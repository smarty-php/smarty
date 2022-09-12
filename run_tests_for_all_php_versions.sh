# Runs tests for all supported PHP versions >= PHP 5.4.
# Cannot get 5.2 and 5.3 to run in docker anymore

docker-compose run php54 && \
docker-compose run php55 && \
docker-compose run php56 && \
docker-compose run php70 && \
docker-compose run php71 && \
docker-compose run php72 && \
docker-compose run php73 && \
docker-compose run php74