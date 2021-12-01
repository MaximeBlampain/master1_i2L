const config = {
    MONGO: {
        URL_MONGO: "mongodb://" + process.env.MONGO_HOST + ":" + process.env.MONGO_PORT + "/" + process.env.MONGO_DATABASE+ "?authSource=admin",
        USERNAME_MONGO: process.env.MONGO_ROOT_USER,
        PASSWORD_MONGO: process.env.MONGO_ROOT_PASSWORD
    },
    MYSQL: {
        HOST_SQL: process.env.BDD_HOST,
        PORT_SQL: process.env.BDD_PORT,
        USER_SQL: process.env.BDD_USER,
        PASSWORD_SQL: process.env.BDD_PASSWORD,
        DATABASE_SQL: process.env.BDD_DATABASE,
    },
    API_PORT: 3000,
    CLIENT_PORT: 3001,
    API_URL: "http://localhost:3000/",
    SECRET: "3ec16131-d30e-4f03-8f79-ee6a129584e0"
}

module.exports = config