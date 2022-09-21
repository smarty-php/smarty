#!/bin/bash

if [[ "$1" =~ ^4\.[0-9\.-rc]+$ ]]; then
   printf 'Creating release %s\n' "$1"
else
   echo "Invalid version number: $1. This script can only make v4.x.x releases."
   exit 1;
fi

git checkout -b "release/$1"
sed -i "s/## \\[Unreleased\\]/## \\[Unreleased\\]\\n\\n## \\[$1\\] - $(date +%Y-%m-%d)/" CHANGELOG.md
sed -i "s/const SMARTY_VERSION = '[^']\+';/const SMARTY_VERSION = '$1';/" libs/Smarty.class.php

git add CHANGELOG.md libs/Smarty.class.php
git commit -m "version bump"

git checkout master
git pull
git merge --no-ff "release/$1"
git branch -d "release/$1"
git tag -a "v$1" -m "Release $1"

printf 'Done creating release %s\n' "$1"
printf 'Run `git push --follow-tags origin` to publish it.\n'
