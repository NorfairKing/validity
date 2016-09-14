source scripts/lib.sh
h () {
  hlint \
    validity/src \
    validity-text/src \
    validity-containers/src \
    genvalidity/src \
    genvalidity-text/src \
    genvalidity-containers/src \
    genvalidity-hspec/src \
    --ignore "Reduce duplication"
  # hlint \
  #   --ignore "Use String" \
  #   --ignore "Redundant do" \
  #   --ignore "Redundant $" \
  #   --ignore "Use concatMap" \
  #   --ignore "Reduce duplication" \
  #   --ignore "Use fromMaybe"
}
check "Hlint" h
