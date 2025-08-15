#!/bin/sh

GAP_EXE=$GAP_DIR
if [ "x$GAP_DIR" = "x" ]; then
  GAP_DIR=$(cd "/home/ah359/Documents/mt4796/gap-4.13.0" && pwd)
  GAP_EXE="/home/ah359/Documents/mt4796/gap-4.13.0"
fi

exec "$GAP_EXE/gap" -l "$GAP_DIR" "$@"
