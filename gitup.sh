#!/bin/bash

a=$#
if [ $a -ne 1 ];
then
    echo "Verwendung p.sh kommentar"
    exit 1
fi

git add --all
git status
git commit -m "$1"
git push
