#!/bin/bash
lipo -create -output arrasta arrasta_darwin-Intel-cocoa arrasta_darwin-M1-cocoa \
&& cp -f arrasta ./arrasta.app/Contents/MacOS/
