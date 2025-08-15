###############################################################################
##
#F  output(degree)
##
##  Selects the output file to log results in.
##  Input:  degree - the degree of the group to log for.
##  Output: the filename of the file to log to.
##
output := function(degree)
    local out;
    out := StringFormatted("out-1.0/{}deg.txt", degree);
    return out;
end;

###############################################################################
##
#F  generateBase(G, bases, base)
##
##  Extends an irredundant base of a group recursively.
##  Input:  G - the group to find base for.
##          bases - list of all the bases of the group.
##          base - current base expanded.
##
generateBase := function(G, bases, base)
    local orbits, orbit, base_l, stab, point;
    # Base case - if G is trivial, we have an irredundant base
    # and can terminate. Add the base to the list of bases.
    if G = Group(()) then 
        Add(bases, base);
    else
    # If G is not trivial we get its orbits.
    # For each orbit we take its first point by which we expand the current
    # base. We also find its stabiliser and recurse with the stabiliser.
        orbits := Orbits(G);
        for orbit in orbits do
            point := orbit[1];
            base_l := ShallowCopy(base);
            # Expand current base.
            Add(base_l, point);
            stab := Stabiliser(G, point);
            # Recurse with stabiliser and current base.
            generateBase(stab, bases, base_l);
        od;
    fi;
end;

###############################################################################
##
#F  findI(G)
##
##  Finds the largest irredundant base size of G, I(G).
##  Input:  G - a group to find I(G) for.
##  Output: the length of the longest irredundant base.
##
findI := function(G)
    local orbits, orbit, bases, lengths, base, stab, i;
    # Initialise the list of irredundant bases.
    bases := [];
    base := [];
    generateBase(G, bases, ShallowCopy(base));

    lengths := [];
    # Find the lengths of all the bases.
    for i in [1, Length(bases)] do
        lengths[i] := Length(bases[i]);
    od;
    #Print("Bases: ", bases, "\n"); # Optional print statement
    # Find the largest base length.
    return Maximum(lengths);
end;        

###############################################################################
##
#F  generateTuple(n, l, points, tuple, tuples)
##
##  Extends a given tuple recursively to generate all tuples with no 
##  repetitions.
##  Input:  n - the upper bound of the set {1,...,n} from which points are
##              taken.
##          l - the remaining length of the tuple to generate.
##          points - the remaining points that have not yet been used in the 
##              tuple.
##          tuple - the tuple currently being generated.
##          tuples - the set of all tuples.
##
generateTuple := function(n, l, points, tuple, tuples)
    local i, tuple_l;
    if l >= 1 then
    # Recursive case: for all points that aren't yet in the tuple add point
    # to tuple.
        for i in [1..n] do
            if points[i] = i then
            # Remove point that we add to tuple.
                points[i] := 0;
                tuple_l := ShallowCopy(tuple);
                Add(tuple_l, i);
                # Recurse on obtained tuple, with a reduced length (l-1).
                generateTuple(n, l-1, ShallowCopy(points), tuple_l, tuples);
                points[i] := i; #Add point back to tuple.
            fi;
        od;
    else
    # Base case, tuple is of desired length, add tuple to list of tuples.
        Add(tuples, tuple);
    fi;
end;

###############################################################################
##
#F  generateTuples(n, l)
##
##  Generates all the tuples of length l from the set {1,...,n} with no
##  repetitions.
##  Input:  n - the upper bound of the set {1,...,n} from which points are
##              taken.
##          l - the length of tuples to generate.
##  Output: tuples - the set of all l-tuples over {1,...,n} with no 
##              repetitions.
## 
generateTuples := function(n, l)
    local tuples, i, points, tuple;
    # Initialise set of tuples and points.
    tuples := [];
    points := [1..n];
    tuple := [];
    generateTuple(n, l, ShallowCopy(points), ShallowCopy(tuple), tuples);
    # Once all recursions have ran return all generated tuples.
    return tuples;
end;

###############################################################################
##
#F  generateOrbits(G, tuples)
##
##  Generates all the orbits of G on the set of non-repeating tuples.
##  Input:  G - the group to generate orbits of.
##          tuples - the set of non-repeating tuples to split into orbits.
##  Output: orbits - the set of orbits of G over tuples. 
## 
generateOrbits := function(G, tuples)
    local tuple, i, j, orbits, orbit, d, el;
    # Marks if a tuple is already in am orbit or not.
    d := NewDictionary(tuples[1], false);
    orbits := [];
    # For each tuple, if it is not already known, get its orbit.
    for tuple in tuples do
        if not KnowsDictionary(d, tuple) then
            orbit := Orbit(G, tuple, OnTuples);
            # Add the orbit to the list of orbits.
            Add(orbits, orbit);
            # Mark each tuple in orbit as already in an orbit.
            for el in orbit do
                AddDictionary(d, el);
            od;
        fi;
    od;
    return orbits;
end;

###############################################################################
##
#F  getOrbitPairs(G, tuples)
##
##  Creates a list of all the unordered pairs of orbit representatives for G
##  and a given list of tuples. The representative is the first item of the 
##  orbit.
##  Input:  G - group under which orbits are calculated.
##          tuples - list of non-repeating tuples on which orbits are 
##              calculated.
##  Output: orbitRepPairs - the list of all the unordered pairs of 
##              representatives of the orbits. It has length (n-1)*(n-1)/2, 
##              where n is the number of orbits, and contains lists of length 2
##              containing a representative each of the orbits in the pair.
##
getOrbitPairs := function(G, tuples)
    local orbits, orbitRepPairs, i, j;
    orbitRepPairs := [];
    # Get all the orbits on the tuples.
    orbits := generateOrbits(G, tuples);
    # Get all the unordered pairs of orbit representatives from different 
    # orbits.
    for i in [1..Length(orbits)] do
        for j in [i+1..Length(orbits)] do
            Add(orbitRepPairs, [orbits[i][1], orbits[j][1]]);
        od;
    od;
    return orbitRepPairs;
end;

###############################################################################
##
#F  RC(G, n)
##
##  Calculates the relational complexity of G acting on set {1,...,n}.
##  Will timeout after 60 seconds. (But will generate tuples until that is
##  completed, which may lead to significantly longer runtime).
##  Input:  G - group for which relational complexity is calculated.
##          n - upper bound of set of points {1,...,n}.
##  Output: the relational complexity of G as a number 2 <= RC(G,n) <= n-1.
##
RC := function(G, n)
    local l, maxSize, i, j, tuples1, tuples2, orbits2, orbits1, orbitRepPairs2,
            pair, I, J, induction, maxFalse, failed, error, str, start_t,
            end_t;
    # Measure the start time.
    start_t := NanosecondsSinceEpoch();
    # Checks for grops for which the relational complexity is known and
    # requires more work to compute to reduce needless computation.
    # RC(S_n, n) = 2, RC(A_n, n) = n-1.
    if IsNaturalSymmetricGroup(G) then
        return 2;
    elif IsNaturalAlternatingGroup(G) then
        return n-1;
    fi;
    # Find I(G), since I(G) + 1 is an upper bound on RC(G, n).
    l := findI(G);
    # Since 2 is a lower bound on RC, if I(G) = 1, then RC(G,n)=2.
    if l = 1 then
        return 2;
    fi;
    # Print out I(G).
    #Print("I = ", l, "\n");
    AppendTo(output(n), StringFormatted(" I = {}\n", l));
    # Generate 2-tuples over {1,...,n}.
    tuples2 := generateTuples(n,2);
    # Initialise list that stores whether 2-equivalence implies 3-equivalence
    # and so on, up until I(G)-equivalence implies I(G)+1-equivalence.
    induction := [];
    for i in [2..l+1] do
        induction[i] := true;
    od;
    for i in [2..l] do
        # Get i-tuples.
        tuples1 := tuples2;
        # Get i+1 tuples.
        tuples2 := generateTuples(n,i+1);
        #Print(tuples2);
        # Get all the orbit rep pairs of i+1_tuples.
        orbitRepPairs2 := getOrbitPairs(G, tuples2);
        # For each pair of orbit representatives (they are not in the same
        # orbit - so one does not map to the other) find elements of G that map
        # subtuples of on orbit rep to the other.
        for pair in orbitRepPairs2 do
        # If already found that ~k !=> ~k+1 then move to next iteration.
            if not induction[i] then
                break;
            fi;
            failed := false;
            # List to hold pair of orbit reps and elements of g that map their
            # subtuples.
            error := [];
            Append(error, [pair[1], ", ", pair[2], " "]);
            # Iterate over which element of the i+1-tuple is removed to create
            # subtuples.
            for j in [1..i+1] do
            # Check end time and whether runtime is longer than specified 
            # timeout.
                end_t := NanosecondsSinceEpoch();
                if end_t - start_t > 60*(10^9) then
                    AppendTo(output(n), StringFormatted("\n Timeout. Runtime = {}s", Float((end_t - start_t)/10^9)));
                    return fail;
                fi;
                # Until there was a fail, check through the next subtuples of I
                # and J, to see if all of them map to each other.
                if not failed then
                    I := ShallowCopy(pair[1]);
                    Remove(I, j);
                    J := ShallowCopy(pair[2]);
                    Remove(J,j);
                    if RepresentativeAction(G, I, J, OnTuples) = fail then
                        failed := true;
                    else
                    # If a subtuple maps, store the group element that maps it.
                        #Append(error, [pair[1], ", ", pair[2], "I= ", I, " J= ", J, " ", RepresentativeAction(G, I, J, OnTuples), "\n"]);
                        Append(error, [RepresentativeAction(G, I, J, OnTuples), ", "]);
                    fi;
                fi;
            od;
            # If all the subtuples mapped ~k !=> ~k+1 and thus RC(G,n) > k.
            if not failed then
                induction[i] := false;
                # Print all the elemnts mapping the subtuples.
                for str in error do
                    #Print(str);
                    AppendTo(output(n), str);
                od;
                #Print("\n");
                AppendTo(output(n), "\n");
                break;
            fi;
        od;
    od;
    #Print(induction, "\n");
    AppendTo(output(n), induction);
    # Find the largest i such that ~i !=> ~i+1. RC(G,n) is i+1.
    maxFalse := 1;
    for i in [2..l+1] do
        if not induction[i] then
            maxFalse := i;
        fi;
    od;
    # Take not of final runtime.
    end_t := NanosecondsSinceEpoch();
    AppendTo(output(n), StringFormatted("\ntime = {}s",
            Float((end_t - start_t)/10^9)));
    return maxFalse+1;
end;

###############################################################################
##
#F  runTests(lower, upper)
##
##  Finds the relationial complexity for all primitive group with degree >= 
##  lower and <= upper.
##  Input:  lower - the lower bound of degree.
##          upper - the upper bound for degree.
##
runTests := function(lower, upper)
    local i, j, num, G;
    # Iterate over all degrees in range.
    for i in [lower..upper] do
        num := NrPrimitiveGroups(i);
        #PrintTo(output(i));
        # Find RC of each primitive group with that degree.
        for j in [13..num] do
            G := PrimitiveGroup(i,j);
            AppendTo(output(i), G);
            AppendTo(output(i), StringFormatted("\nRC = {}\n\n", RC(G, i)));
        od;
    od;
end;
