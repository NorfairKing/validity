#!/bin/bash

# Abort on error
set -e

./scripts/code_health.sh
./scripts/build.sh
./scripts/install.sh
./scripts/test.sh
./scripts/lines.sh
