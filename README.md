# How to set up a development environment

Install the required software:

- Install NVM (https://github.com/creationix/nvm)
- Install Flyway (https://flywaydb.org/)
- Install Docker (https://www.docker.com/)
- Install Yarn (https://yarnpkg.com/en/docs/install), remember to use `--without-node` option if you're on macOS: `brew install yarn --without-node`
- Install PureScript compiler and Pulp build tool with Yarn: `yarn global add purescript pulp`

Use Node.js v8.8.1 via NVM:

`$ npm install -g yarn`  
`$ cd <PROJECT_ROOT>/purescript-herigone-server`  
`$ bower install`  
`$ npm install`  
`$ pulp --watch run --jobs 1`  

Start dockerized PostgreSQL:

`$ cd <PROJECT_ROOT>/purescript-herigone-server/docker`  
`$ ./build.sh`  
`$ ./start.sh`  

Use flyway migrations:

`$ cd <PROJECT_ROOT>/purescript-herigone-server`  
`$ flyway migrate`  

Start server:

`$ cd <PROJECT_ROOT>/purescript-herigone-server`  
`$ pulp run`  