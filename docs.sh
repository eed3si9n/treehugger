#!/bin/sh

pf docs www
rm -r www/latest/
mkdir www/latest/
cp -r target/scala-2.9.1/api www/latest/
