FROM ubuntu:16.04

USER root

ENV TERM linux
ENV DEBIAN_FRONTEND noninteractive
ENV LC_ALL en_US.UTF-8
ENV LANG en_US.UTF-8
ENV JAVA_HOME /usr/lib/jvm/java-8-oracle
ENV PATH=$PATH:/home/node/.npm-global/bin

# This has to be done first before installing anything.
# TODO: Should be combined with apt-get installs, see https://docs.docker.com/engine/userguide/eng-image/dockerfile_best-practices/#run
RUN apt-get update

# Fix locale problems.
RUN apt-get install -y locales && \
  locale-gen en_US.UTF-8 && \
  dpkg-reconfigure locales

# Install some necessary tools.
RUN apt-get install -y apt-utils curl rlwrap software-properties-common python-software-properties sudo git vim

# Install NodeJS 8.4.0.
RUN curl -s -O https://deb.nodesource.com/node_8.x/pool/main/n/nodejs/nodejs_8.4.0-1nodesource1~xenial1_amd64.deb && \
  dpkg -i nodejs_8.4.0-1nodesource1~xenial1_amd64.deb && \
  rm nodejs_8.4.0-1nodesource1~xenial1_amd64.deb

# Install and setup PostgreSQL and create the herigone database.
RUN apt-get install -y postgresql postgresql-client postgresql-contrib
RUN service postgresql start && \
  su -l postgres -c "createuser -s herigone" && \
  su -l postgres -c "createdb --encoding=UTF-8 --template=template0 --owner=herigone herigone" && \
  su -l postgres -c "psql herigone postgres -c \"ALTER USER herigone PASSWORD 'herigone'\""

# Create user node:node for running NodeJS, and add it to sudoers group.
RUN addgroup --gid 1000 node && \
  adduser -u 1000 --ingroup node --disabled-password --shell /bin/sh node && \
  usermod -aG sudo node

# Install Java 8, it is needed by Flyway.
RUN echo oracle-java8-installer shared/accepted-oracle-license-v1-1 select true | debconf-set-selections && \
  add-apt-repository -y ppa:webupd8team/java && \
  apt-get update && \
  apt-get install -y oracle-java8-installer oracle-java8-set-default && \
  rm -rf /var/lib/apt/lists/* && \
  rm -rf /var/cache/oracle-jdk8-installer

# Install Flyway for database migrations.
RUN curl -0 -s -O https://repo1.maven.org/maven2/org/flywaydb/flyway-commandline/4.2.0/flyway-commandline-4.2.0-linux-x64.tar.gz && \
  tar -xvzf flyway-commandline-4.2.0-linux-x64.tar.gz && \
  rm flyway-commandline-4.2.0-linux-x64.tar.gz && \
  ln -s /flyway-4.2.0/flyway /usr/local/bin/flyway && \
  chmod -R 755 /flyway-4.2.0 && \
  chmod 755 /usr/local/bin/flyway

# Give everybody in sudoers group permission to sudo without password.
RUN sed -i 's/^%sudo.*$/%sudo ALL=NOPASSWD: ALL/' /etc/sudoers

# Copy the source directories to the image and assign node:node as their owner.
COPY purescript-herigone-common purescript-herigone-common
COPY purescript-herigone-client purescript-herigone-client
COPY purescript-herigone-server purescript-herigone-server
RUN chown -R node:node /purescript-herigone-common && \
  chown -R node:node /purescript-herigone-client && \
  chown -R node:node /purescript-herigone-server

# Switch to user node to build the sources.
USER node:node

# Install the global npm libraries in ~/.npm-global.
RUN npm config set prefix '~/.npm-global'
RUN npm install -g purescript pulp bower

# Build the "common" subproject.
WORKDIR /purescript-herigone-common
RUN bower install && npm install && pulp build

# Build the "client" subproject.
WORKDIR /purescript-herigone-client
RUN bower install && npm install && pulp build -O --to herigone.js && \
  mv herigone.js /purescript-herigone-server/static

# Build the "server" subproject.
WORKDIR /purescript-herigone-server
RUN bower install && npm install && pulp build

# Perform Flyway migrations.
RUN sudo service postgresql start && flyway migrate

EXPOSE 9771

CMD service postgresql start && su -l node -c 'export PATH=$PATH:/home/node/.npm-global/bin; cd /purescript-herigone-server; pulp run;'
