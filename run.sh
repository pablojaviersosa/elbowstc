#!/bin/bash
mv elbowstc elbowstc.zip
pushd priv/static
zip ../../elbowstc.zip index.html
popd
mv elbowstc.zip elbowstc
./elbowstc -l 127.0.0.1 -p 8080
