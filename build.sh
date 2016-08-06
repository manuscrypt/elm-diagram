#!/bin/bash

FAIL=0

elm-package install -y &

cd server
npm i &
cd ..


for job in `jobs -p`
do
echo $job
    wait $job || let "FAIL+=1"
done

echo $FAIL

if [ "$FAIL" == "0" ];
then
elm-make --output build/index.html
else
echo "FAILED! ($FAIL)"
fi
