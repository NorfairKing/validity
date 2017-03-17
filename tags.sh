#!/bin/bash
set -e

ls
for package in $(ls -d */ | cut -f1 -d'/')
do
  cabalfile="$package/$package.cabal"
  if [[ ! -f ${cabalfile} ]]
  then
    continue
  fi
  version=$(grep '^version: \(.*\)' "${cabalfile}" | awk '{print $2}')
  tagname="$package-$version"
  if git tag "${tagname}"
  then
    echo "tag created: ${tagname}"
  else
    echo "tag not created: ${tagname}"
  fi
done
