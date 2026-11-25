FROM php:7.3-cli

## Debian bullseye is end-of-life, its packages have moved to archive.debian.org
RUN printf "deb http://archive.debian.org/debian bullseye main\ndeb http://archive.debian.org/debian-security bullseye-security main\n" > /etc/apt/sources.list

## Basic utilities
RUN apt-get update -yqq && apt-get install -y curl apt-utils git zip unzip

## Composer
COPY ./utilities/testrunners/shared/install-composer.sh /root/install-composer.sh
WORKDIR /root
RUN sh ./install-composer.sh
RUN mv ./composer.phar /usr/local/bin/composer
