FROM ubuntu:16.04

USER root

ENV TERM linux

ADD server herigone-ps-server

RUN apt-get update
RUN apt-get install -y apt-utils
RUN apt-get install -y curl
RUN curl -sL https://raw.githubusercontent.com/nodesource/distributions/master/deb/setup_8.x -o nodesource_setup.sh
RUN bash nodesource_setup.sh
RUN apt-get install -y nodejs
RUN npm install -g purescript pulp bower

WORKDIR herigone-ps-server

CMD ["bower", "--allow-root", "install"]
# CMD ["pulp", "run"]

EXPOSE 9700
