#!/bin/bash

cd "$(dirname "$0")"

cd src

export COB_LIBRARY_PATH=bin

./bin/BATCHRUN >> ../batch_result.log 2>&1