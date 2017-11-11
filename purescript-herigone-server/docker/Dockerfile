FROM node:8-alpine

USER root

RUN npm install express
RUN npm install pg

COPY index.js index.js

USER node

CMD ["node", "index.js"]
