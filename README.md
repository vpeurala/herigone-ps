# How to set up a development environment

Install the required software:

- Install NVM (https://github.com/creationix/nvm)
- Install Flyway (https://flywaydb.org/)
- Install Docker (https://www.docker.com/)
- Install Yarn (https://yarnpkg.com/en/docs/install), remember to use `--without-node` option if you're on macOS: `brew install yarn --without-node`
- Install PureScript compiler and Pulp build tool with Yarn: `yarn global add purescript pulp`

If you want to build the NGINX module, you need to set two environment variables:

`export WWW_HERIGONE_COM_PEM=<secret path to file www_herigone_com.pem>`
`export WWW_HERIGONE_COM_KEY=<secret path to file www_herigone_com.key>`

Use Node.js v8.9.1 via NVM:

`./scripts/build.sh`

Because you probably want to test herigone-ps on your local machine, it makes things easier to add these to your **/etc/hosts**:

    # For testing Herigone locally; remember to remove these!
    0.0.0.0       herigone.com
    0.0.0.0       www.herigone.com
    0.0.0.0       herigone-ps-db
    0.0.0.0       herigone-ps-nginx
    0.0.0.0       herigone-ps-server

