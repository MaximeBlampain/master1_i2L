const { Sequelize } = require('sequelize');

// get mySQL infos from config file
const { MYSQL } = require("../config")
const {
    HOST_SQL,
    USER_SQL,
    PASSWORD_SQL,
    DATABASE_SQL,
} = MYSQL

// Create a Sequelize object for mySQL
const sequelize = new Sequelize(
    DATABASE_SQL,
    USER_SQL,
    PASSWORD_SQL,
    {
        dialect: "mysql",
        host: HOST_SQL,
        logging: false, 
    },
);

async function tryConnection(){
    try {
        await sequelize.authenticate();
        console.log(`[${DATABASE_SQL}] MySQL database connection success.`);
    } catch (error) {
        console.log(` /!\ Unable to connect to the database: [${DATABASE_SQL}] /!\ `);
    }
}



module.exports  = {sequelize, tryConnection}