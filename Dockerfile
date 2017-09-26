FROM ubuntu:16.04

USER root

ENV TERM linux
ENV DEBIAN_FRONTEND noninteractive
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV PATH=$PATH:/home/node/.npm-global/bin

# This has to be done first before installing anything.
RUN apt-get update

# Fix locale problems.
RUN apt-get install -y locales
RUN locale-gen en_US.UTF-8
RUN dpkg-reconfigure locales

# Install some necessary tools.
RUN apt-get install -y apt-utils
RUN apt-get install -y curl
RUN apt-get install -y rlwrap

# Install NodeJS 8.4.0.
RUN curl -s -O https://deb.nodesource.com/node_8.x/pool/main/n/nodejs/nodejs_8.4.0-1nodesource1~xenial1_amd64.deb
RUN dpkg -i nodejs_8.4.0-1nodesource1~xenial1_amd64.deb

# Install and setup PostgreSQL and create the herigone database.
RUN apt-get install -y postgresql postgresql-contrib
RUN service postgresql start
# RUN su -l postgres -c 'createuser -s herigone'
# RUN su -l postgres -c 'createdb --encoding=UTF-8 --owner=herigone herigone'
#
# # Create user node:node for running NodeJS.
# RUN addgroup --gid 1000 node
# RUN adduser -u 1000 --ingroup node --disabled-password --shell /bin/sh node
#
# # Copy the server directory to the image and assign node:node as its owner.
# COPY server herigone-ps-server
# RUN chown -R node:node /herigone-ps-server

# RUN echo 'debconf debconf/frontend select Dialog' | debconf-set-selections

# USER node
# WORKDIR /herigone-ps-server
#
# RUN npm config set prefix '~/.npm-global'
#
# RUN npm install -g purescript pulp bower
# RUN bower install
# RUN npm install
#
# EXPOSE 9771
#
# CMD ["pulp", "run"]
