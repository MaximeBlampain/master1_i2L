const express = require('express');
const router = express.Router();
const Joi = require('@hapi/joi');
const userService = require('./users.service');
const validateRequest = require('../middleware/validate_request')
const verifyToken = require('../middleware/verify_token')
const { getRouteFromRequest, logging } = require('../utils/helpers')

// routes
router.route('/all')
    .get(verifyToken, getAllUsers);
router.route('/auth')
    .post(authSchema, auth);
router.route('/auth_google')
    .post(loginWithGoogleSchema, loginWithGoogle);
router.route('/:idUser')
    .get(verifyToken, getSingle);
router.route('/create')
    .post(createSchema, create);
router.route('/create_google')
    .post(createWithGoogleSchema, createWithGoogle);
router.route('/edit')
    .patch(verifyToken, editSchema, edit);
router.route('/delete')
    .delete(verifyToken, deleteSchema, deleteUser);

module.exports = router;

function getAllUsers(req, res, next) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - GET - All Users`);

    userService.getAllUsers()
        .then((response) => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
                usersGetted: response.usersGetted ? response.usersGetted : null,
            })
        });
}

function authSchema(req, res, next) {
    const schema = Joi.object({
        email: Joi.string().required(),
        password: Joi.string().required()
    });
    validateRequest(req, next, schema);
}

function auth(req, res, next) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - POST - Authentification`);

    const { email, password } = req.body;

    userService.auth({ email, password })
        .then((response) => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
                userGetted: response.userGetted ? response.userGetted : null,
            })
        })
}

function getSingle(req, res, next) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - GET - Single User`);
    
    const idUser = req.params.idUser;

    userService.getSingle({ idUser })
        .then((response) => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
                userGetted: response.userGetted ? response.userGetted : null,
            })
        });
}

function createSchema(req, res, next) {
    const schema = Joi.object({
        firstName: Joi.string().required(),
        lastName: Joi.string().required(),
        email: Joi.string().required(),
        password: Joi.string().required()
    });

    validateRequest(req, next, schema);
}

function create(req, res) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - POST - User Creation`);

    const { firstName, lastName, email, password } = req.body;

    userService.create({ firstName, lastName, email, password })
        .then((response) => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
                userCreated: response.userCreated ? response.userCreated : null,
            })
        });
}

function editSchema(req, res, next) {
    const schema = Joi.object({
        idUser: Joi.number().required(),
        fieldsToEdit: Joi.object({
            firstName: Joi.string(),
            lastName: Joi.string(),
            email: Joi.string(),
            password: Joi.string()
        }).required()
    });

    validateRequest(req, next, schema);
}

function edit(req, res) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - PATCH - User Edition`);

    const { idUser, fieldsToEdit } = req.body

    userService.edit({ idUser, fieldsToEdit })
        .then((response) => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
                userEdited: response.userEdited ? response.userEdited : null,
            })
        });
}

function deleteSchema (req, res, next) {
    const schema = Joi.object({
        idUser: Joi.number().required(),
    });

    validateRequest(req, next, schema);
}

function deleteUser (req, res) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - DELETE - Delete User`);

    const { idUser } = req.body;

    userService.deleteUser({ idUser })
        .then((response) => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
            })
        });
}

function loginWithGoogleSchema (req,res, next) {
    const schema = Joi.object({
        providerID: Joi.string().required(),
    })

    validateRequest(req, next, schema);
}

function loginWithGoogle (req,res) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - POST - Login with Google`);
    
    const { providerID } = req.body;

    userService.authWithGoogle({ providerID })
        .then((response) => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
                userGetted: response.userGetted ? response.userGetted : null,
            })
        })
}

function createWithGoogleSchema(req, res, next) {
    const schema = Joi.object({
        firstName: Joi.string().required(),
        lastName: Joi.string().required(),
        email: Joi.string().required(),
        provider: Joi.string().required(),
        providerID: Joi.string().required(),
    });

    validateRequest(req, next, schema);
}

function createWithGoogle(req, res) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - POST - Google User Creation`);

    const { firstName, lastName, email, provider, providerID } = req.body;

    userService.createWithGoogle({ firstName, lastName, email, provider, providerID})
        .then((response) => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
                userCreated: response.userCreated ? response.userCreated : null,
            })
        });
}