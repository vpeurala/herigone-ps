FROM ubuntu:16.04

USER root

ENV TERM linux
ENV PATH=$PATH:/home/node/.npm-global/bin

ADD server herigone-ps-server

RUN apt-get update
RUN apt-get install -y apt-utils
RUN apt-get install -y curl
RUN curl -sL https://raw.githubusercontent.com/nodesource/distributions/master/deb/setup_8.x -o nodesource_setup.sh
RUN bash nodesource_setup.sh
RUN apt-get install -y nodejs
RUN addgroup --gid 1000 node
RUN adduser -u 1000 --ingroup node --disabled-password --shell /bin/sh node
RUN chown -R node:node /herigone-ps-server

USER node
WORKDIR herigone-ps-server

RUN npm config set prefix '~/.npm-global'

RUN npm install -g purescript pulp bower
RUN bower install

EXPOSE 9700

CMD ["pulp", "run"]
