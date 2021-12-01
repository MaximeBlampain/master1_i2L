import {
    GET_ALL_RECETTES,
    GET_ALL_RECETTES_SUCCESS,
    NEW_RECETTE,
    GET_RECETTE,
    GET_RECETTE_SUCCESS,
    EDIT_RECETTE,
    DELETE_RECETTE,
    SET_LOADING,
} from './constants'

export function getAllRecettesAction(){
    return {
        type: GET_ALL_RECETTES,
    }
}
export function getAllRecettesSuccessAction(payload){
    return {
        type: GET_ALL_RECETTES_SUCCESS,
        payload: payload,
    }
}

export function getSingleRecetteAction(payload){
    return {
        type: GET_RECETTE,
        payload: payload,
    }
}
export function getSingleRecetteSuccessAction(payload){
    return {
        type: GET_RECETTE_SUCCESS,
        payload: payload,
    }
}

export function newRecetteAction(payload){
    return {
        type: NEW_RECETTE,
        payload: payload,
    }
}

export function editRecetteAction(payload){
    return {
        type: EDIT_RECETTE,
        payload: payload,
    }
}

export function deleteRecetteAction(payload){
    return {
        type: DELETE_RECETTE,
        payload: payload,
    }
}

export function setLoading(payload){
    return {
        type: SET_LOADING,
        payload: payload,
    }

}