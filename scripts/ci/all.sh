#!/bin/bash

set -ex

export BUILD_KIND=nix
./scripts/ci/script.sh 


export BUILD_KIND=stack

for s in stack/*.yaml
do
  rm -f "$s.lock"
  export STACK_YAML=$(basename $s)
  ./scripts/ci/script.sh 
done
export RESOLVER=nightly
./scripts/ci/script.sh 
