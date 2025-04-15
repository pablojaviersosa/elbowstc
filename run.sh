#!/bin/bash
pushd priv/static
zip -r ../../elbowstc index.html
popd
./elbowstc -l 127.0.0.1 -p 8080
