Use Node.js v8.5.0 via NVM:

`$ npm install -g bower pulp purescript`  
`$ cd server`  
`$ bower install`  
`$ npm install`  
`$ pulp --watch run --jobs 1`  

Create local database:

`$ createuser -s herigone`  
`$ createdb --encoding=UTF-8 --owner=arctech arctech`  

Use flyway migrations:

`$ cd server`  
`$ flyway migrate`  
