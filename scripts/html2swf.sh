#!/bin/sh

export MXML_COMPILER=/Users/guilespi/Downloads/flex_sdk_4.6/bin/mxmlc
java -jar target/html2swf-0.2.0-standalone.jar html2swf.main --directory $1
