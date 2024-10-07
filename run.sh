#!/bin/sh
set -ex
(cd zurds-fe && rm -r pkg/* && wasm-pack build --target web)
(cd zurds-be && cargo run)
