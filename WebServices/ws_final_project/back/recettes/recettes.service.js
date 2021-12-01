const mongoose = require('mongoose');
const mongo = require('../utils/mongo');
const ObjectId = mongoose.Types.ObjectId
module.exports= {
    getAllRecettes,
    newRecette,
    getIDRecette,
    editRecette,
    deleteRecette,
};

async function getAllRecettes() {
    const recettes = await mongo.Recette.find({});

    const recettesToReturn = recettes.map(({_id, title, description, steps}) => (
        {
            _id, 
            title, 
            description, 
            steps
        }
    ))

    return {
        status: 200,
        type: "success",
        message: "Recettes successfully getted !",
        recettes: recettesToReturn,
    };
}

async function newRecette({ title, description, steps, photos, commentary }) {

    const recette = await mongo.Recette.create({
        title: title,
        description: description,
        steps: steps,
    });

    return {
        status: 201,
        type: "success",
        message: "Recette added to the database",
        recette,
    };
}

async function getIDRecette(idRecette) {
    const recette = await mongo.Recette.findById(idRecette);
    if(!recette)
        return {
            status: 404,
            type: "error",
            message: "Cannot get recette, cannot find target recette to get",
        };
    
    return {
        status: 200,
        type: "success",
        message: "Recette successfully getted !",
        recette: recette,
    };
}

async function editRecette({ idRecette, fieldsToEdit }) {
    await mongo.Recette.findByIdAndUpdate(idRecette, fieldsToEdit);

    const recette = await mongo.Recette.findById(idRecette);

    if(!recette)
        return {
            status: 404,
            type: "error",
            message: "Cannot get recette, cannot find target recette to get",
        };
    
    if(Object.keys(fieldsToEdit).length === 0)
        return {
            status: 406,
            type: "error",
            message: "Cannot update recette, missing informations to update",
        };

    return {
        status: 202,
        type: "success",
        message: "Recette successfully updated !",
        recette: recette,
    };
}

async function deleteRecette({ idRecette }) {
    var recette = await mongo.Recette.findById(idRecette);
    
    if(!recette) 
        return {
            status: 404,
            type: "error",
            message: "Error with delete request, Cannot get recette",
        }

    await mongo.Recette.findByIdAndDelete(idRecette);

    recette = await mongo.Recette.findById(idRecette);
    if(!recette)
        return {
            status: 202,
            type: "success",
            message: "Successful delete",
        }
    
    return {
        status: 400,
        type: "error",
        message: "Error during delete request, Please try again",
    }
}