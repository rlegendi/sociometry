#!/bin/sh

VERSION=`git describe master`
NAME="sociometry-$VERSION"

if [ ! -d "dist" ]
then
	mkdir "dist"
fi

git archive master --prefix="$NAME/" | gzip > "dist/$NAME.tgz"
git archive master --prefix="$NAME/" --format=zip > "dist/$NAME.zip"
