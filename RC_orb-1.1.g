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
    out := StringFormatted("out-1.1/{}deg.txt", degree);
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
#F  getOrbitRepPairs(orbits)
##
##  Creates a list of all the unordered pairs of orbit representatives for a 
##  given list of orbits. The representative is the first item of the orbit.
##  Input:  orbits - list of orbits
##  Output: orbitRepPairs - the list of all the unordered pairs of 
##              representatives of the orbits. It has length (n-1)*(n-1)/2, 
##              where n is the number of orbits, and contains lists of length 2
##              containing a representative each of the orbits in the pair.
##              The reps do not belong to the same orbit.
##
getOrbitRepPairs := function(orbits)
    local orbitRepPairs, i, j;
    orbitRepPairs := [];
    # This gives all the possible unordered pairs with no repetition.
    for i in [1..Length(orbits)] do
        for j in [i+1..Length(orbits)] do
            Add(orbitRepPairs, [orbits[i], orbits[j]]);
        od;
    od;
    return orbitRepPairs;
end;

###############################################################################
##
#F  extendOrbit(G, orbits, n)
##
##  Extends orbits of k-tuples, to orbits of k+1 tuples. This generates all the
##  k+1 orbits and contains no repetitions.
##  Input:  G - group under which orbits are calculated.
##          orbits - list of orbit representatives of k-tuples.
##          n - degree of G.
##  Output: ext_orbits - list of representatives of all the orbits of 
##              k+1-tuples.
##
extendOrbit := function(G, orbits, n)
    local orb_rep, ext_orbits, to_ext, rep, points, point, tuple_1, tuple_2,
            contained, extended;
    # Initialise list of extended orbits.
    ext_orbits := [];
    for orb_rep in orbits do
    # For each orbit's representative find the set of point that are not in it.
        points := [1..n];
        SubtractSet(points, orb_rep);
        # Initialise set of possible extensions of the representative.
        extended := [];
        for point in points do
            Add(extended, Concatenation(orb_rep, [point]));
        od;
        
        to_ext := [];
        Add(to_ext, extended[1]);
        # For each possible extension of the orbit, check if it can be mapped 
        # to from one of the existing orbit representative. Then it is already
        # contained in an orbit and doesn't need to be included in a new one.
        for tuple_1 in extended do
            contained := false;
            for tuple_2 in to_ext do
                if not RepresentativeAction(G, tuple_1, tuple_2, OnTuples) 
                        = fail then
                    contained := true;
                fi;
            od;
            # If the proposed rep is not in any of the orbits, then choose it
            # as a rep for a new orbit.
            if not contained then
                Add(to_ext, tuple_1);
            fi;
        od;
        # Once all candidates for extended orbits are checked add the ones,
        # that are in fact representatives of different orbits.
        Append(ext_orbits, to_ext);
    od;
    # Return all the the extended orbits.
    return(ext_orbits);
end;

###############################################################################
##
#F  RC(G, n, timeout)
##
##  Calculates the relational complexity of G acting on set {1,...,n}.
##  Will timeout after specified number of second.
##  Input:  G - group for which relational complexity is calculated.
##          n - upper bound of set of points {1,...,n}.
##          timeout - the number of seconds after which the program will stop 
##              the computation.
##  Output: the relational complexity of G as a number 2 <= RC(G,n) <= n-1.
##
RC := function(G, n, timeout)
    local l, alt_size, maxSize, i, j, orbits_1, orbit_reps, orbit_reps_plus_1,
            orbit_rep_pairs, pair, I, J, induction, maxFalse, failed, error,
            str, start_t, end_t, orb;
    # Measure the start time.
    start_t := NanosecondsSinceEpoch();
    # Checks for grops for which the relational complexity is known and
    # requires more work to compute to reduce needless computation.
    # RC(S_n, n) = 2, RC(A_n, n) = n-1, up to isomorphism.
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
    AppendTo(output(n), StringFormatted(" I = {}\n", l));
    # Get the single orbit representative (since group is transitive).
    orbits_1 := [[1]]; # Using the fact that we have transitive groups here!
    # Code if not transitive.
    # for orb in Orbits(G) do
    #     Add(orbits_1, [orb[1]]);
    # od;
    # Extend to orbit representatives on 2_tuples.
    orbit_reps_plus_1 := extendOrbit(G, orbits_1, n);
    # Initialise list that stores whether 2-equivalence implies 3-equivalence
    # and so on, up until I(G)-equivalence implies I(G)+1-equivalence.
    induction := [];
    for i in [2..l+1] do
        induction[i] := true;
    od;
    for i in [2..l] do
        # Get orbit reps of i-tuples.
        orbit_reps := orbit_reps_plus_1;
        # Get orbit reps of i+1-tuples.
        orbit_reps_plus_1 := extendOrbit(G, orbit_reps, n);
        # Get all the orbit rep pairs of i+1_tuples.
        orbit_rep_pairs := getOrbitRepPairs(orbit_reps_plus_1);
        # For each pair of orbit representatives (they are not in the same
        # orbit - so one does not map to the other) find elements of G that map
        # subtuples of on orbit rep to the other.
        for pair in orbit_rep_pairs do
        # If already found that ~k !=> ~k+1 then move to next iteration.
            if not induction[i] then
                break;
            fi;
            # Stores whether there was at least one subtuple of I that does not
            # map to the subtuple of J.
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
                if end_t - start_t > timeout*(10^9) then
                    AppendTo(output(n), StringFormatted("\n Timeout. Runtime = {}s",
                            Float((end_t - start_t)/10^9)));
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
        PrintTo(output(i));
        # Find RC of each primitive group with that degree.
        for j in [1..num] do
            G := PrimitiveGroup(i,j);
            AppendTo(output(i), G);
            AppendTo(output(i), StringFormatted("\nRC = {}\n\n", RC(G, i, 300)));
        od;
    od;
end;
