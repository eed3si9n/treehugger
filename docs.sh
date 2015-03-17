#!/bin/sh

# pf docs www
rm -r www/latest/
mkdir www/latest/
cp -r library/target/scala-2.11/api www/latest/

cd www
tar zcvf ../treehugger.tar.gz **
cd ..
