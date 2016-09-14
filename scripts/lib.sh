ESC_SEQ="\x1b["
COL_RESET=$ESC_SEQ"39;49;00m"
COL_RED=$ESC_SEQ"31;11m"
COL_GREEN=$ESC_SEQ"32;11m"
COL_YELLOW=$ESC_SEQ"33;11m"
COL_BLUE=$ESC_SEQ"34;11m"
COL_MAGENTA=$ESC_SEQ"35;11m"
COL_CYAN=$ESC_SEQ"36;11m"
print_colored_text () {
  local color="$1"
  local text="$2"
  local color_code="COL_$color"
  echo -n -e "${!color_code}$text$COL_RESET"
}

error () {
  local text="$1"
  print_colored_text RED "ERROR: $text\n"
  exit 1
}

warning () {
  local text="$1"
  print_colored_text YELLOW "WARNING: $text\n"
}

good () {
  local text="$1"
  print_colored_text GREEN "CHECK: $text\n"
}

check () {
  name="$1"
  shift
  command="$*"
  echo -n "Check: ${name}... "
  OUT=/tmp/out.txt
  ERR=/tmp/err.txt
  ${command} > $OUT 2> $ERR
  if [[ "$?" == "0" ]]; then
    print_colored_text GREEN "Success\n"
  else
    print_colored_text RED "Failure\n"
    echo ${command}
    cat $OUT
    cat $ERR
    return -1
  fi
}

promptY () {
  prompt="$1"
  print_colored_text BLUE "$prompt [Y/n] > "
  read answer
  if [[ "$answer" == "n" ]]
  then
    return -1
  else
    return 0
  fi
}

promptN () {
  prompt="$1"
  print_colored_text BLUE "$prompt [y/N] > "
  read answer
  if [[ "$answer" == "y" ]]
  then
    return 0
  else
    return -1
  fi
}














