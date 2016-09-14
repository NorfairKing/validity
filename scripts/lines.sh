source scripts/lib.sh

print_colored_text BLUE "Actual code:\n"
cloc \
  validity/src \
  validity-text/src \
  validity-containers/src \
  genvalidity/src \
  genvalidity-text/src \
  genvalidity-containers/src \
  genvalidity-hspec/src \
