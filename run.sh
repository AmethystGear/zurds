#!/bin/sh

# show the commands we're running
set -ex 

# build zurds-fe: clean out pkg directory, recreate
(cd zurds-fe && rm -rf pkg && mkdir pkg && wasm-pack build --target web)
(cd zurds-be && cargo run)
