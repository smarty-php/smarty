#!/bin/bash

# Runs tests for all supported PHP versions
# Usage examples:
# - ./run-tests-for-all-php-versions.sh --group 20221124
# - ./run-tests-for-all-php-versions.sh --exclude-group slow

docker-compose run php71 ./run-tests.sh $@ && \
docker-compose run php72 ./run-tests.sh $@ && \
docker-compose run php73 ./run-tests.sh $@ && \
docker-compose run php74 ./run-tests.sh $@ && \
docker-compose run php80 ./run-tests.sh $@ && \
docker-compose run php81 ./run-tests.sh $@
