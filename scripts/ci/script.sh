#!/bin/bash

set -ex

case $BUILD_KIND in
  stack)
    if [[ "$STACK_YAML" != "" ]]
    then
      stack --no-terminal --stack-yaml "$STACK_YAML" build --haddock --pedantic
      stack --no-terminal --stack-yaml "$STACK_YAML" test --pedantic $TEST_PKGS
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
    nix-build -A haskellPackages.validity
    nix-build -A haskellPackages.validity-bytestring
    nix-build -A haskellPackages.validity-text
    nix-build -A haskellPackages.validity-containers
    nix-build -A haskellPackages.validity-path
    nix-build -A haskellPackages.validity-scientific
    nix-build -A haskellPackages.validity-time
    nix-build -A haskellPackages.validity-unordered-containers
    nix-build -A haskellPackages.validity-uuid
    nix-build -A haskellPackages.validity-vector
    nix-build -A haskellPackages.validity-aeson
    nix-build -A haskellPackages.genvalidity
    nix-build -A haskellPackages.genvalidity-property
    nix-build -A haskellPackages.genvalidity-bytestring
    nix-build -A haskellPackages.genvalidity-text
    nix-build -A haskellPackages.genvalidity-containers
    nix-build -A haskellPackages.genvalidity-path
    nix-build -A haskellPackages.genvalidity-scientific
    nix-build -A haskellPackages.genvalidity-time
    nix-build -A haskellPackages.genvalidity-unordered-containers
    nix-build -A haskellPackages.genvalidity-uuid
    nix-build -A haskellPackages.genvalidity-vector
    nix-build -A haskellPackages.genvalidity-aeson
    nix-build -A haskellPackages.genvalidity-hspec
    nix-build -A haskellPackages.genvalidity-hspec-aeson
    nix-build -A haskellPackages.genvalidity-hspec-binary
    nix-build -A haskellPackages.genvalidity-hspec-cereal
    nix-build -A haskellPackages.genvalidity-hspec-hashable
    nix-build -A haskellPackages.genvalidity-hspec-optics
    ;;
  *)
    echo "Unknown build kind."
    exit 1
    ;;
esac
