set -e # abort on error

./scripts/code_health.sh
./scripts/build.sh
./scripts/install.sh
./scripts/documentation.sh
./scripts/test.sh
./scripts/bench.sh
./scripts/lines.sh
