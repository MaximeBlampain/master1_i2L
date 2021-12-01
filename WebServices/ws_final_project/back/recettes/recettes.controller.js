const express = require('express');
const router = express.Router();
const Joi = require('@hapi/joi');
const recetteService = require('./recettes.service');
const validateRequest = require('../middleware/validate_request')
const verifyToken = require('../middleware/verify_token')
const { getRouteFromRequest, logging } = require('../utils/helpers')

// routes
router.route('/')
    .get(verifyToken, getAllRecettes);
router.route('/new')
    .post(verifyToken, newRecetteSchema, newRecette);
router.route('/:idRecette')
    .get(verifyToken, getIDRecette)
    .patch(verifyToken, editSchema, editRecette)
    .delete(verifyToken, deleteRecette);

module.exports = router;

function getAllRecettes(req, res) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - GET - Get All Recettes`);

    recetteService.getAllRecettes()
        .then(response => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
                recettes: response.recettes ? response.recettes : null,
            });
        });
}

function newRecetteSchema(req, res, next) {
    const schema = Joi.object({
        title: Joi.string().required(),
        description: Joi.string().required(),
        steps: Joi.array().required(),
    })
    validateRequest(req, next, schema)
}

function newRecette(req, res) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - POST - Add New Recette`);

    const { title, description, steps, photos, commentary} = req.body

    recetteService.newRecette({title, description, steps, photos, commentary})
        .then(response => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
                recette: response.recette ? response.recette : null,
            })
        })
    
}

function getIDRecette(req, res) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - GET - Get One Recette`);

    const idRecette = req.params.idRecette;
    console.log('ID RECETTE ', idRecette)
    recetteService.getIDRecette(idRecette)
        .then(response => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
                recette: response.recette ? response.recette : null,
            });
        });
}

function editSchema(req, res, next) {
    const schema = Joi.object({
        title: Joi.string(),
        description: Joi.string(),
        steps: Joi.array()
    }).required();
    validateRequest(req, next, schema)
}

function editRecette(req, res) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - PATCH - Edit Recette`);

    const fieldsToEdit = {...req.body};

    recetteService.editRecette({idRecette: req.params.idRecette, fieldsToEdit})
        .then(response => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
                recette: response.recette ? response.recette : null,
            })
        });
}

function deleteRecette(req, res) {
    var route = getRouteFromRequest(req);
    console.info(`[route] '${route}' - DELETE - Delete Recette`);

    recetteService.deleteRecette({ idRecette: req.params.idRecette })
        .then(response => {
            logging(route, response);
            res.status(response.status).json({
                type: response.type,
                message: response.message,
            })
        });
}
