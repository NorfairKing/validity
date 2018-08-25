#!/bin/bash

set -ex

case $BUILD_KIND in
  stack)
    if [[ "$STACK_YAML" != "" ]]
    then
      stack setup --stack-yaml "$STACK_YAML"
      stack build --only-snapshot --stack-yaml "$STACK_YAML"
    else
      if [[ "$RESOLVER" != "" ]]
      then
        stack setup --resolver "$RESOLVER"
        stack build --only-snapshot --resolver "$RESOLVER"
      else
        echo "Unknown way to install with Stack."
        exit 1
      fi
    fi
    ;;
  nix)
    echo "Nothing to do for install in a nix build."
    ;;
  *)
    echo "Unknown build kind."
    exit 1
    ;;
esac
