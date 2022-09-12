#!/bin/sh
composer update && php ./vendor/phpunit/phpunit/phpunit -c phpunit.xml tests
