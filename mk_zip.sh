#!/bin/bash

version=$(git describe --tags --abbrev=0)
release_dir="rings.oneloop-$version"
mkdir "$release_dir" || true
sbt "set test in assembly := {}" clean assembly
cp target/scala-2.12/rings.oneloop-assembly-*.jar "$release_dir"/oneloop.jar
cp oneloop "$release_dir"/
cp mma/oneloop.m "$release_dir"/
zip -r "rings.oneloop-$version".zip "$release_dir"/*
rm -rf "$release_dir"