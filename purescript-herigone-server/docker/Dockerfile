FROM node:8-alpine

USER node

ENV PATH=$PATH:/home/node/.yarn/bin

RUN yarn global add bower purescript pulp

COPY purescript-herigone-server /purescript-herigone-server

WORKDIR /purescript-herigone-server

RUN pulp build

CMD [ "pulp", "run" ]
