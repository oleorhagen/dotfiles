#!/bin/bash

echo "Building..."
make build

echo "Testing..."
make test

echo "Extra code checks..."
make extracheck

echo "Running integration tests..."
echo "Do you wish to run integration tests?"
read INTEGRATION
echo $INTEGRATION
if [[ $INTEGRATION = (Y|y)[e][s] ]]; then
    echo "TODO -- Running integration tests"
    # TDO -- add the ability to choose the tests to run
fi
