#!/bin/sh
set -ex
(cd zurds-fe && wasm-pack build --target web)
(cd zurds-be && cargo run)