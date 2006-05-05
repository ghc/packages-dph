#! /bin/bash

GHC=../../../../../../../compiler/stage2/ghc-inplace
OPTS="-package ndp\
      -fglasgow-exts -O2 -funbox-strict-fields\
      -fliberate-case-threshold100 -fno-method-sharing"

if [ "x$@" = "x" ]
then
  tests=`ls *.hs`
else
  tests=$@
fi

for file in $tests
do
  rules=`sed -n 's/-- >[[:space:]]*\([0-9]\+\)[[:space:]]\+\([^[:space:]]\+\)/\1 \2/p' $file`
  log=`echo $file | sed 's/\.hs$/.log/'`
  echo "$GHC $OPTS -c $file -ddump-simpl-stats" > $log
  if $GHC $OPTS -c $file -ddump-simpl-stats >> $log
  then
    oldIFS=$IFS
    IFS='
'
    for rule in `sed -n 's/-- >[[:space:]]*\([0-9]\+\)[[:space:]]\+\([^[:space:]]\+\)/\1 \2/p' $file`
    do
      if ! grep "$rule" $log > /dev/null 2>&1
      then
        echo "FAIL: $file ($rule)"
        break
      fi
    done
    IFS=$oldIFS
  else
    echo FAIL: $file - compiler error
  fi
done
rm -f *.hi *.o

