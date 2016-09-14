set -e # Abort on error

./scripts/trailing_whitespace_test.sh
./scripts/indentation.sh
./scripts/hlint_health.sh
./scripts/sanity.sh
