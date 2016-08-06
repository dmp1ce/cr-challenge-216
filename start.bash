#!/bin/bash

# Use Docker to install requirements and run Haskell program
echo "Building Yesod application."
echo "This may take awhile because Yesod framework is around 500 MB."
docker-compose build && docker-compose run --service-ports --rm \
  haskell-yesod bash -c "cd /app && ./jarjar_quotes.hs"
