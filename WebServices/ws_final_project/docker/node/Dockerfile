FROM node:14

RUN mkdir -p /home/app
WORKDIR /home/app
COPY ./back/package.json /home/app

ENV NODE_ENV=development
RUN npm install
RUN npm build

COPY ./back /home/app/
EXPOSE 3000

CMD [ "yarn", "start:unix" ]
