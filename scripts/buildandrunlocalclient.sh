#!/bin/bash

set -ex
# First cd to the yocto build directory and build the newest thud environment
cd ~/yocto/qemu-setup/
repo sync -m thud.xml --force-sync
pwd
source poky/oe-init-build-env
nice -n 15 bitbake core-image-full-cmdline

# Then build a client image with the client from the build above
cd ~/mendersoftware/meta-mender/meta-mender-qemu/docker
./build-docker qemux86-64 -t mendersoftware/mender-client-qemu:master

# Start the demo environment in the background, and suppress the output
cd ~/mendersoftware/integration
./demo --client up >/dev/null 2>/dev/null &
demo_env_pid=$!

# Make sure that the demo environment is brought down on SIGINT
exitfunc() {
    set -xe
    echo "Shutting down the demo server"
    cd ~/mendersoftware/integration
    ./demo down
}

trap exitfunc SIGINT

# Wait for the useradm service to come up
echo "Waiting for the useradm service to come up"
# curl --silent -k -X POST -u mender-demo@example.com:1234 \
#      --retry-connrefused \
#      --connect-timeout 5 \
#      https://localhost/api/management/v1/useradm/auth/login
sleep 60

# Connect to the running client
echo "Connecting to the client..."
dockerclientattach

