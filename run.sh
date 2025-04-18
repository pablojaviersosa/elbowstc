#!/bin/bash
mv elbowstc elbowstc.zip
zip ../../elbowstc.zip index.html
mv elbowstc.zip elbowstc
./elbowstc -l 127.0.0.1 -p 8080
