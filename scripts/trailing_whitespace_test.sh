source scripts/lib.sh
t () {
  TMP=/tmp/files.txt
  find . -type f -name "*.hs" -exec egrep -l " +$" {} \; > $TMP
  find . -type f -name "*.cabal" -exec egrep -l " +$" {} \; >> $TMP
  if [[ -s $TMP ]]
  then
    code="1"
    echo "These files contain trailing whitespace, please fix: "
  else
    code="0"
  fi

  while read f
  do
    echo $f
  done < $TMP

  return $code
}
check "Trailing whitespace" t
