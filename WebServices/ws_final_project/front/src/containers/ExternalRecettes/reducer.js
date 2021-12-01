import { GET_CATEGORIES_SUCCESS, GET_CATEGORY_RECETTES_SUCCESS, GET_SELECTED_RECETTE_SUCCESS } from './constants'

const INITIAL_STATE = {
  categories: [],
  recettes: [],
  recette: {},
}

function externalRecettesReducer (state = INITIAL_STATE, { type, payload }) {
  switch (type) {
    case GET_CATEGORIES_SUCCESS:
      return {
        ...state,
        categories: payload.categories,
      }
    case GET_CATEGORY_RECETTES_SUCCESS:
      return {
        ...state,
        recettes: payload.meals,
      }
    case GET_SELECTED_RECETTE_SUCCESS:
      return {
        ...state,
        recette: payload.recette,
      }
    default:
      return state
  }
}
  
export default externalRecettesReducer
  