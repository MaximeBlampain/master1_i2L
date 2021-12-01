import { takeEvery, put, call } from "redux-saga/effects"

// Types
import { GET_CATEGORIES, GET_CATEGORY_RECETTES, GET_SELECTED_RECETTE } from './constants'

// Actions
import { getCategoriesSuccessAction, getCategoryRecettesSuccessAction, getSelectedSuccessRecette } from './actions'

// Utils
import request from '../../utils/request'

const API_URL = 'https://www.themealdb.com/api/json/v1/1'
const customGet = {
  method: "GET"
}
// Functions
function* getCategories() {
  const requestURL = `${API_URL}/list.php?c=list`
  try {
    const response = yield call(request, requestURL, customGet)
    const { meals } = response
    const arrayToReturn = []

    // if data from external api, format it to French
    if(meals.length !== 0){
      meals.map(({strCategory}) => {
        switch(strCategory){
          case "Beef":
            arrayToReturn.push({
              id: "Beef",
              name: "Boeuf"
            })
            break
          case "Breakfast":
            arrayToReturn.push({
              id: "Breakfast",
              name: "Petit Déjeuner"
            })
            break
          case "Chicken":
            arrayToReturn.push({
              id: "Chicken",
              name: "Poulet"
            })
            break
          case "Dessert":
            arrayToReturn.push({
              id: "Dessert",
              name: "Dessert"
            })
            break
          case "Goat":
            arrayToReturn.push({
              id: "Goat",
              name: "Mouton"
            })
            break
          case "Lamb":
            arrayToReturn.push({
              id: "Lamb",
              name: "Agneau"
            })
            break
          default: return 0
        }

        return 0
      })
    }

    yield put(getCategoriesSuccessAction({categories: arrayToReturn}))
  } catch (err) {
    console.error('err', err)
  }
}

function* getCategoryRecettes({ payload }) {
  const requestURL = `${API_URL}/filter.php?c=${payload.category}`
  try {
    const response = yield call(request, requestURL, customGet)

    yield put(getCategoryRecettesSuccessAction(response))
  } catch (err) {
    console.error('err', err)
  }
}

function* getSelectedRecette({ payload }) {
  const requestURL = `${API_URL}/lookup.php?i=${payload.idMeal}`
  try {
    const response = yield call(request, requestURL, customGet)
    let objectToReturn = {}
    if(response.meals.length !== 0)
      objectToReturn = response.meals[0]


    yield put(getSelectedSuccessRecette({recette: objectToReturn}))
  } catch (err) {
    console.error('err', err)
  }
}

function* externalRecettesSaga() {
  yield takeEvery(GET_CATEGORIES, getCategories)
  yield takeEvery(GET_CATEGORY_RECETTES, getCategoryRecettes)
  yield takeEvery(GET_SELECTED_RECETTE, getSelectedRecette)
}

export default externalRecettesSaga
