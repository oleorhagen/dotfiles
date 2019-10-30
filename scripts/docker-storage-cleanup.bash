#!/bin/bash

if [ $(docker ps -a -q -f status=exited | wc -l) -gt 0 ]; then
  echo "removing containers..."
  docker rm -v $(docker ps -a -q -f status=exited)
fi
if [ $(docker images -f "dangling=true" -q | wc -l) -gt 0 ]; then
  echo "removing images..."
  docker rmi $(docker images -f "dangling=true" -q)
fi
if [ $(docker volume ls -qf dangling=true | wc -l) -gt 0 ]; then
  echo "removing volumes..."
  docker volume rm $(docker volume ls -qf dangling=true)
fi
