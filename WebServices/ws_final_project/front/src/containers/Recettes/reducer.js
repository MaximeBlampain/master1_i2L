import { 
    GET_ALL_RECETTES_SUCCESS,
    GET_RECETTE_SUCCESS,
    SET_LOADING,
} from './constants'

const INITIAL_STATE = {
    recettes: [],
    selectedRecette: {},
    isLoading: false,
}

function recettesReducer (state = INITIAL_STATE, { type, payload }) {
  switch (type) {
    case GET_ALL_RECETTES_SUCCESS:
        return {
            ...state,
            recettes: payload.recettes,
        }
    case GET_RECETTE_SUCCESS:
        return {
            ...state,
            selectedRecette: payload.recette,
        }
    case SET_LOADING:
        return {
            ...state,
            isLoading: payload,
        }
    default:
      return state
  }
}
  
export default recettesReducer
  