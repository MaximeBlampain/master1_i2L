const mongoose = require('mongoose');

// import config file
const { MONGO } = require('../config');
const { USERNAME_MONGO, PASSWORD_MONGO } = MONGO;

const mongoOptions = {
    useCreateIndex: true,
    useNewUrlParser: true,
    useUnifiedTopology: true,
    useFindAndModify: true,
    user: USERNAME_MONGO,
    pass: PASSWORD_MONGO,
}
const { URL_MONGO } = MONGO;
mongoose.connect(
    URL_MONGO, 
    mongoOptions);
mongoose.Promise = global.Promise;
module.exports = {
    Recette: require('../recettes/recettes.model.js'),
    
}