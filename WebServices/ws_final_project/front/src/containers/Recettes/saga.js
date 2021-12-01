import { takeEvery, put, call, select } from "redux-saga/effects"

// Types
import { 
    GET_ALL_RECETTES,
    NEW_RECETTE,
    GET_RECETTE,
    EDIT_RECETTE,
    DELETE_RECETTE,
 } from './constants'

// Actions
import { 
    getAllRecettesAction,
    getAllRecettesSuccessAction,
    getSingleRecetteSuccessAction,
    setLoading,
} from './actions'

// Selectors
import { selectToken } from '../Login/selectors'

// Utils
import request from '../../utils/request'
import { API_URL } from "../../utils/config"
import { getCors, postCors, patchCors, deleteCors } from "../../utils/cors"

// Functions
function* getAllRecettes() {
  const token = yield select(selectToken)
  const requestURL = `${API_URL}/recettes/`

  yield put(setLoading(true))
  try {
    const response = yield call(request, requestURL, getCors({token}))
    yield put(setLoading(false))
    yield put(getAllRecettesSuccessAction(response))
  } catch (err) {
    yield put(setLoading(false))
    console.error('err', err)
  }
}

function* newRecette({ payload }) {
    const token = yield select(selectToken)
    const requestURL = `${API_URL}/recettes/new`
    
    yield put(setLoading(true))
    try {
      yield call(request, requestURL, postCors({...payload.recetteToCreate},{token}))
      yield put(setLoading(false))
      yield put(getAllRecettesAction())
    } catch (err) {
      yield put(setLoading(false))
      console.error('err', err)
    }
}

function* getSingleRecette({ payload }) {
    const token = yield select(selectToken)
    const requestURL = `${API_URL}/recettes/${payload.idRecette}`

    yield put(setLoading(true))
    try {
        const response = yield call(request, requestURL, getCors({token}))
      yield put(setLoading(false))
        yield put(getSingleRecetteSuccessAction(response))
    } catch (err) {
      yield put(setLoading(false))
    console.error('err', err)
    }
}

function* editRecette({ payload }) {
    const token = yield select(selectToken)
    const requestURL = `${API_URL}/recettes/${payload.idRecette}`

    yield put(setLoading(true))
    try {
        yield call(request, requestURL, patchCors({...payload.recetteToUpdate},{token}))
      yield put(setLoading(false))
        yield put(getAllRecettesAction())
    } catch (err) {
      yield put(setLoading(false))
    console.error('err', err)
    }
}

function* deleteRecette({ payload }) {
    const token = yield select(selectToken)
    const requestURL = `${API_URL}/recettes/${payload.idRecette}`

    yield put(setLoading(true))
    try {
        yield call(request, requestURL, deleteCors({}, {token}))
      yield put(setLoading(false))
        yield put(getAllRecettesAction())
    } catch (err) {
      yield put(setLoading(false))
    console.error('err', err)
    }
}

function* recettesSaga() {
  yield takeEvery(GET_ALL_RECETTES, getAllRecettes)
  yield takeEvery(NEW_RECETTE, newRecette)
  yield takeEvery(GET_RECETTE, getSingleRecette)
  yield takeEvery(EDIT_RECETTE, editRecette)
  yield takeEvery(DELETE_RECETTE, deleteRecette)
}

export default recettesSaga