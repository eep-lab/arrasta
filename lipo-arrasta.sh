#!/bin/bash
lipo -create -output arrasta-universal arrasta_darwin-Intel-cocoa arrasta_darwin-M1-cocoa \
&& cp -f arrasta-universal ./arrasta.app/Contents/MacOS/
