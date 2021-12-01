const mongoose = require('mongoose');
const Schema = mongoose.Schema;

const stepSchema = new Schema({
    idStep: { type: String, required: true },
    step: { type: String, required: true },
});


const recetteSchema = new Schema({
    title: { type: String, required: true },
    description: { type: String, required: false },
    steps: { type: [stepSchema], required: true },
});

recetteSchema.set('toJSON', {
    virtuals: true,
    versionKey: false,
});

module.exports = mongoose.model('Recette', recetteSchema);