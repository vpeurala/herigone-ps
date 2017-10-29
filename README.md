Use Node.js v8.8.1 via NVM:

`$ npm install -g bower pulp purescript`  
`$ cd purescript-herigone-server`  
`$ bower install`  
`$ npm install`  
`$ pulp --watch run --jobs 1`  

Create local database:

`$ createuser -s herigone`  
`$ createdb --encoding=UTF-8 --owner=herigone herigone`  

Use flyway migrations:

`$ cd server`  
`$ flyway migrate`  
