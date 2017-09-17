FROM ubuntu:16.04

USER root

RUN apt-get update && apt-get install -y curl && curl -sL https://raw.githubusercontent.com/nodesource/distributions/master/deb/setup_8.x -o nodesource_setup.sh && bash nodesource_setup.sh && apt-get install -y nodejs

EXPOSE 9700
