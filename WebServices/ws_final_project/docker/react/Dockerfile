FROM node:14

RUN mkdir -p /home/app
WORKDIR /home/app
COPY ./front/package.json /home/app

ENV NODE_ENV=development

RUN yarn global add react-scripts@3.4.1
RUN npm install
RUN npm build
COPY ./front /home/app/

CMD ["yarn", "run", "start"]
