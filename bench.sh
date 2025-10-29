#!/bin/sh


usage () {
    echo "Usage: ${0} <cmd>
where <cmd> is:
  perf      run perf record on native.exe (see the report with perf report)
  ref       execute a reference run of native.exe
  diff      compare the output of native.exe with the reference run
  time      benchmark native.exe (fails if the output differs from the reference)
  clean     clean-up generated files (_bench directory and perf.data)
"
}
N=4   #number of runs for benchmark

BASE_DIR="_bench"
mkdir -p "$BASE_DIR"

compare_output () {
    cat "$1" | sed -e 's/[(][0-9a-zA-Z. ]*[)]$//g' | head -n -2 > "$BASE_DIR"/t1.tmp
    cat "$2" | sed -e 's/[(][0-9a-zA-Z. ]*[)]$//g' | head -n -2 > "$BASE_DIR"/t2.tmp
    diff -q "$BASE_DIR"/t1.tmp "$BASE_DIR"/t2.tmp >/dev/null 2>&1
    R=$?
    if [ "$R" -ne 0 ]
    then
        diff --color -U 0 "$BASE_DIR"/t1.tmp "$BASE_DIR"/t2.tmp
    fi
    return $R
}


if [ "$1" = "perf" ]
then
    dune build src/bin/native.exe
    perf record --call-graph=dwarf -- _build/default/src/bin/native.exe tests/*.ml >/dev/null 2>&1
elif [ "$1" = "report" ]
then
    perf report
elif [ "$1" = "ref" ]
then
    dune exec src/bin/native.exe tests/*.ml  > "$BASE_DIR"/timing.ref
    exit 0
elif [ "$1" = "diff" ]
then
     dune exec --display=quiet -- src/bin/native.exe tests/*.ml > "$BASE_DIR"/timing.tmp
     compare_output "$BASE_DIR"/timing.ref "$BASE_DIR"/timing.tmp || exit 1
     exit 0
elif [ "$1" = "time" ]
then
    SUM_T=0.0
    for i in `seq $N`
    do
        echo -n Run "$i ... "
        dune exec --display=quiet -- src/bin/native.exe tests/*.ml > "$BASE_DIR"/timing.tmp
        compare_output "$BASE_DIR"/timing.ref "$BASE_DIR"/timing.tmp || exit 1
        T=`cat "$BASE_DIR"/timing.tmp | grep 'Total time' | grep -o '[0-9]\+[.][0-9]\+'`
        echo "$T"
        SUM_T=`echo "$T" "$SUM_T" + 2 k p | dc`
    done
    TOTAL=`echo 2 k "$SUM_T" "$N" / p | dc`
    echo "Total of $N runs: $TOTAL"
elif [ "$1" = "clean" ]
then
    rm -rf "$BASE_DIR" perf.data
else
    usage
fi
