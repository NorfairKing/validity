source scripts/lib.sh

from="$1"
to="$2"

usage () {
  echo "
    refactor FROM TO
  "
}

if [[ "$from" == "" || "$to" == "" ]]
then
  usage
  exit 0
fi

# if ! git diff --exit-code --quiet
# then
#   error "Refactor requires clean git status. Commit current changes first."
# fi

# Check if the string already exists somewhere
grep --color=auto --line-number --word-regexp "$to" --recursive --include \*.hs
if [[ "$?" == "0" ]]
then
  warning "Destination already exists"
  if ! promptN "Go on?"
  then
    exit 1
  fi
else
  good "Destination is not in use yet."
fi

grep --color=auto --line-number --word-regexp "$from" --recursive --include \*.hs
if ! promptY "Above are the matches for $from, continue?"
then
  exit 1
fi


find . -type f -name "*.hs" -exec sed -i "s/\b$from\b/$to/g" {} \;

if promptY "make a commit out of it?"
then
  git add .
  git commit -m "Refactoring $from to $to"
fi
