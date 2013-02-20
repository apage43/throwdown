#!/bin/sh
lein uberjar
s3cmd -P put target/throwdown-*-SNAPSHOT-standalone.jar s3://s3.crate.im/throwdown.jar
