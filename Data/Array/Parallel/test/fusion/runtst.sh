#! /bin/bash

GHC=../../../../../../../compiler/stage2/ghc-inplace
OPTS="-package ndp\
      -fglasgow-exts -O2 -funbox-strict-fields\
      -fliberate-case-threshold100 -fno-method-sharing"

verbose=no
tests=

exec 6> /dev/null

for arg
do
  case $arg in
    --verbose|-v) exec 6>&1
                  ;;
    *)            tests="$tests $arg"
                  ;;
  esac
done

tests=${tests:=`ls *.hs`}

for file in $tests
do
  echo Testing $file >&6
  rules=`sed -n 's/-- >[[:space:]]*\([0-9]\+\)[[:space:]]\+\([^[:space:]]\+\)/\1 \2/p' $file`
  log=`echo $file | sed 's/\.hs$/.log/'`
  echo "$GHC $OPTS -c $file -ddump-simpl-stats" >&6
  if $GHC $OPTS -c $file -ddump-simpl-stats > $log
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

