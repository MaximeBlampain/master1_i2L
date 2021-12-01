import { GET_CATEGORIES, GET_CATEGORIES_SUCCESS, GET_CATEGORY_RECETTES, GET_CATEGORY_RECETTES_SUCCESS, GET_SELECTED_RECETTE, GET_SELECTED_RECETTE_SUCCESS } from './constants'

export function getCategoriesAction(){
  return {
    type: GET_CATEGORIES
  }
}

export function getCategoriesSuccessAction(response){
  return {
    type: GET_CATEGORIES_SUCCESS,
    payload: response,
  }
}

export function getCategoryRecettesAction(payload){
  return {
    type: GET_CATEGORY_RECETTES,
    payload: payload,
  }
}

export function getCategoryRecettesSuccessAction(response){
  return {
    type: GET_CATEGORY_RECETTES_SUCCESS,
    payload: response,
  }
}

export function getSelectedRecette(payload){
  return {
    type: GET_SELECTED_RECETTE,
    payload: payload,
  }
}

export function getSelectedSuccessRecette(response){
  return {
    type: GET_SELECTED_RECETTE_SUCCESS,
    payload: response,
  }
}