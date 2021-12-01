const { SECRET } = require("../config");
const bcrypt = require('bcryptjs')
const Users = require('./users.model');
const jwt = require('jsonwebtoken');

module.exports = {
    getAllUsers,
    auth,
    authWithGoogle,
    getSingle,
    create,
    createWithGoogle,
    edit,
    deleteUser,
};

function generateJwtToken(user) {
    // create a jwt token containing the user id that expires in 7 days
    return jwt.sign({ sub: user.id, id: user.id }, SECRET, { expiresIn: '7d' });
}

async function getAllUsers(){
    const users = await Users.findAll();
    const usersToReturn = users.map(({id, firstName, lastName, email, provider}) => ({
        idUser: id,
        firstName: firstName,
        lastName: lastName,
        email: email,
        provider: provider,
    }));

    return {
        status: 200,
        type: "success",
        message: "Users successfully getted !",
        usersGetted: usersToReturn,
    };
}

async function auth({email, password}){
    
    const user = await Users.findOne({
        where: {
            email: email
        }
    });

    if (!user || !bcrypt.compareSync(password, user.password))
        return {
            status: 403,
            type: "warning",
            message: "Cannot authenticate, Email or password is incorrect",
        };
    
    // generate token
    const jwtToken = generateJwtToken(user);

    return {
        status: 200,
        type: "success",
        message: "User successfully authenticated !",
        userGetted: {
            idUser: user.id,
            firstName: user.firstName,
            lastName: user.lastName,
            email: user.email,
            token: jwtToken,
        }
    };
}

async function authWithGoogle({providerID}){
    const user = await Users.findOne({
        where: {
            providerID: providerID
        }
    });

    if (!user) 
        return {
            status: 403,
            type: "warning",
            message: "Cannot authenticate with Google, Email not in the database",
        };
        
    // generate token
    const jwtToken = generateJwtToken(user);

    return {
        status: 200,
        type: "success",
        message: "Google user successfully authenticated !",
        userGetted: {
            idUser: user.id,
            firstName: user.firstName,
            lastName: user.lastName,
            email: user.email,
            token: jwtToken,
        }
    };
}

async function getSingle({ idUser }){
    const user = await Users.findOne({
        where: {
            id: idUser
        }
    });

    if(!user)
        return {
            status: 404,
            type: "error",
            message: "Cannot get user, cannot find target user to get",
        };
    
    return {
        status: 200,
        type: "success",
        message: "User successfully getted !",
        userGetted: {
            idUser: user.id,
            firstName: user.firstName,
            lastName: user.lastName,
            email: user.email,
        },
    };
}

async function create({firstName, lastName, email, password}){

    const user = await Users.create({
        firstName: firstName,
        lastName: lastName,
        email: email,
        password: bcrypt.hashSync(password, 5),
        provider: 'Email + password',
    });

    return {
        status: 201,
        type: "success",
        message: "User successfully created !",
        userCreated: {
            idUser: user.id,
            firstName: user.firstName,
            lastName: user.lastName,
            email: user.email,
            provider: user.provider,
        },
    };
}

async function createWithGoogle({firstName, lastName, email, provider, providerID}) {
    const user = await Users.create({
        firstName: firstName,
        lastName: lastName,
        email: email,
        password: bcrypt.hashSync(`${Math.random()*1000}`, 5),
        provider: provider,
        providerID: providerID,
    });

    return {
        status: 201,
        type: "success",
        message: "Google user successfully created !",
        userCreated: {
            idUser: user.id,
            firstName: user.firstName,
            lastName: user.lastName,
            email: user.email,
            provider: user.provider,
        },
    };
}

async function edit({ idUser, fieldsToEdit }){
    const userToEdit = await Users.findOne({
        where: {
            id: idUser
        }
    });

    if(!userToEdit)
        return {
            status: 404,
            type: "error",
            message: "Cannot update user, cannot find target user to update",
        };

    if(Object.keys(fieldsToEdit).length === 0)
        return {
            status: 406,
            type: "error",
            message: "Cannot update user, missing informations to update",
        }
    
    //edit field are introduce in fieldsToEdit
    for(field in fieldsToEdit){
        userToEdit[field] = fieldsToEdit[field];
    }

    await userToEdit.save();

    return {
        status: 202,
        type: "success",
        message: "User successfully updated !",
        userEdited:{
            idUser: userToEdit.id,
            firstName: userToEdit.firstName,
            lastName: userToEdit.lastName,
            email: userToEdit.email,
        }
    };
}

async function deleteUser({ idUser }) {
    const user = await Users.findOne({
        where: {
            id: idUser
        }
    });

    if(!user) 
        return {
            status: 404,
            type: "error",
            message: "Error with delete request, Cannot get user",
        }

    await user.destroy();
    
    return {
        status: 202,
        type: "success",
        message: "Successful delete",
    }
}

