#!/bin/bash

# Use Docker to install requirements and run Haskell program
docker-compose build && docker-compose run haskell bash -c /app/jarjar_client.hs
