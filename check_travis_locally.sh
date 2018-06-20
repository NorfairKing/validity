#!/bin/bash

set -e
set -x

with_lts () {
  stack build --jobs 8 --haddock --pedantic --stack-yaml $1
  stack build --jobs 8 --test --pedantic --stack-yaml $1 $2
}

with_lts  lts-2.yaml \
  "validity validity-bytestring validity-containers validity-text validity-time validity-uuid genvalidity"

with_lts  lts-3.yaml \
  "validity validity-bytestring validity-containers validity-text validity-time validity-uuid genvalidity genvalidity-property"

with_lts  lts-4.yaml \
  "validity validity-bytestring validity-containers validity-text validity-time validity-uuid genvalidity genvalidity-property"

with_lts  lts-5.yaml \
  "validity validity-bytestring validity-containers validity-text validity-time validity-uuid genvalidity genvalidity-property"

with_lts  lts-6.yaml \
  "validity validity-bytestring validity-containers validity-text validity-time validity-uuid genvalidity genvalidity-property"

with_lts  lts-7.yaml
with_lts  lts-8.yaml
with_lts  lts-9.yaml
with_lts lts-10.yaml
with_lts lts-11.yaml
with_lts  stack.yaml
