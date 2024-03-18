#!/bin/bash

if [[ "$1" =~ ^4\.[0-9\.-rc]+$ ]]; then
   printf 'Creating release %s\n' "$1"
else
   echo "Invalid version number: $1. This script can only make v4.x.x releases."
   exit 1;
fi

git checkout -b "release/$1"

php utilities/update-changelog.php $1
php utilities/update-smarty-version-number.php $1

git add changelog CHANGELOG.md libs/Smarty.class.php
git commit -m "version bump"

git checkout support/4
git pull
git merge --no-ff "release/$1"
git branch -d "release/$1"
git tag -a "v$1" -m "Release $1"

printf 'Done creating release %s\n' "$1"
printf 'Run `git push --follow-tags origin` to publish it.\n'
