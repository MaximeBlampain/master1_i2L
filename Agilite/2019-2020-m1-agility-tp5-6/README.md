# Traffic web interface

## Description

Web interface of traffic simulator which enables to update network parameters and add vehicles.

This interface is used for passing some Cypress End to End test process.

## How to use it

```bash
git clone https://gitlab.com/jbuisine/2019-2020-m1-agility-tp5-6.git
```

Run the backend REST API:
```
java -jar traffic.jar
```

Download web app dependencies:
```bash
cd web-app
yarn install
```

Then run the app:
```bash
yarn start
```

Go to [http://127.0.0.1:3000](http://127.0.0.1:3000) to see the app home page.

## Running Cypress

```
cd web-app
./node_modules/.bin/cypress open
```