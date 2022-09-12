FROM php:5.6-cli

## Upgrade CA certificates
RUN curl -k https://curl.se/ca/cacert.pem > cacert.crt && cp cacert.crt /usr/local/share/ca-certificates/ && update-ca-certificates

## Basic utilities
RUN apt-get update -yqq && apt-get install --force-yes -y curl apt-utils git zip unzip

## Composer
COPY ./utilities/testrunners/shared/install-composer.sh /root/install-composer.sh
WORKDIR /root
RUN sh ./install-composer.sh
RUN mv ./composer.phar /usr/local/bin/composer
