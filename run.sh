#!/bin/sh

# show the commands we're running
set -ex 

# build zurds-fe: clean out pkg dir, recreate it and build wasm stuff, copy over files from res dir to pkg dir
(cd zurds-fe && rm -rf pkg && mkdir pkg && wasm-pack build --target web && python3 copyfiles.py)
# build and run zurds-be (this actually starts up the website)
(cd zurds-be && cargo run)
