FROM php:8.6-rc-cli

## Basic utilities
RUN apt-get update -yqq && apt-get install -y curl apt-utils git zip unzip

## mbstring is no longer bundled with the official PHP 8.6 image, but PHPUnit requires it
RUN apt-get install -y libonig-dev && docker-php-ext-install mbstring

## Composer
COPY ./utilities/testrunners/shared/install-composer.sh /root/install-composer.sh
WORKDIR /root
RUN sh ./install-composer.sh
RUN mv ./composer.phar /usr/local/bin/composer
