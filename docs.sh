#!/bin/sh

pf docs www
rm -r www/latest/
mkdir www/latest/
cp -r library/target/scala-2.10/api www/latest/
