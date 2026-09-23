#!/bin/sh

# Runs composer update, echoes php version and runs PHPUnit
# Usage examples:
# - ./run-tests.sh --group 20221124
# - ./run-tests.sh --exclude-group slow

composer update --quiet
echo '------------------------------------------------------------'
php -v
echo '------------------------------------------------------------'
php ./vendor/phpunit/phpunit/phpunit $@
echo