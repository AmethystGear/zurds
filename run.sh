#!/bin/sh
set -ex
(cd zurds-fe && rm -rf pkg && mkdir pkg && wasm-pack build --target web)
(cd zurds-be && cargo run)
