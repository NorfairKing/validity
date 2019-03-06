#!/bin/bash

set -ex

case $BUILD_KIND in
  stack)
    if [[ "$STACK_YAML" != "" ]]
    then
      stack --no-terminal --stack-yaml "stack/$STACK_YAML" build --haddock --pedantic
      stack --no-terminal --stack-yaml "stack/$STACK_YAML" test --pedantic $TEST_PKGS
    else
      if [[ "$RESOLVER" != "" ]]
      then
        stack --no-terminal --resolver "$RESOLVER" build --haddock --pedantic
        stack --no-terminal --resolver "$RESOLVER" test --pedantic $TEST_PKGS
      else
        echo "Unknown way to build with Stack."
        exit 1
      fi
    fi
    ;;
  nix)
    nix-build --no-out-link
    ;;
  *)
    echo "Unknown build kind."
    exit 1
    ;;
esac
