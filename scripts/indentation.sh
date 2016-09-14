source scripts/lib.sh

indentation () {
  local RESULT_FILE="/tmp/line_length"
  for f in $(find . -not -path '*/\.*' -type f -name '*.hs')
  do
    # White space must be 4 spaces (or more) unless it's a where then it must be more than 2 spaces
    grep -P --line-number '^\s{1,3}(?!where)[^ ].*$' "$f" > "$RESULT_FILE"
    if [[ "$?" == "0" ]]
    then
      print_colored_text RED $f
      echo
      cat "$RESULT_FILE"
      print_colored_text RED "Incorrect whitespace: 4 spaces in general, no tabs. 2 spaces before 'where'"
      echo
      return -1
    fi
  done
}

check "Indentation" indentation
