import { takeEvery, put, call, select } from "redux-saga/effects"

// Types
import { 
    GET_ALL_USERS,
    GET_USER,
    ADD_USER,
    EDIT_USER,
    DELETE_USER,
 } from './constants'

// Actions
import { 
    getAllUsersAction,
    getAllUsersSuccessAction,
    getSingleUserSuccessAction,
    setLoading,
} from './actions'

// Selectors
import { selectToken } from '../Login/selectors'

// Utils
import request from '../../utils/request'
import { API_URL } from "../../utils/config"
import { getCors, postCors, patchCors, deleteCors } from "../../utils/cors"

// Functions
function* getAllUsers() {
  const token = yield select(selectToken)
  const requestURL = `${API_URL}/users/all`

  yield put(setLoading(true))
  try {
    const response = yield call(request, requestURL, getCors({token}))
    yield put(setLoading(false))
    yield put(getAllUsersSuccessAction(response))
  } catch (err) {
    yield put(setLoading(false))
    console.error('err', err)
  }
}

function* addUser({ payload }) {
    const token = yield select(selectToken)
    const requestURL = `${API_URL}/users/create`
    
    yield put(setLoading(true))
    try {
      yield call(request, requestURL, postCors({...payload.userToCreate},{token}))
      yield put(setLoading(false))
      yield put(getAllUsersAction())
    } catch (err) {
      yield put(setLoading(false))
      console.error('err', err)
    }
}

function* getSingleUser({ payload }) {
    const token = yield select(selectToken)
    const requestURL = `${API_URL}/users/${payload.idUser}`

    yield put(setLoading(true))
    try {
        const response = yield call(request, requestURL, getCors({token}))
        yield put(setLoading(false))
        yield put(getSingleUserSuccessAction(response))
    } catch (err) {
      yield put(setLoading(false))
    console.error('err', err)
    }
}

function* editUser({ payload }) {
    const token = yield select(selectToken)
    const requestURL = `${API_URL}/users/edit`

    yield put(setLoading(true))
    try {
        yield call(request, requestURL, patchCors({idUser: payload.idUser, fieldsToEdit: payload.userToUpdate},{token}))
        yield put(setLoading(false))
        yield put(getAllUsersAction())
    } catch (err) {
      yield put(setLoading(false))
    console.error('err', err)
    }
}

function* deleteUser({ payload }) {
    const token = yield select(selectToken)
    const requestURL = `${API_URL}/users/delete`

    yield put(setLoading(true))
    try {
        yield call(request, requestURL, deleteCors({idUser: payload.idUser},{token}))
        yield put(setLoading(false))
        yield put(getAllUsersAction())
    } catch (err) {
      yield put(setLoading(false))
    console.error('err', err)
    }
}

function* recettesSaga() {
  yield takeEvery(GET_ALL_USERS, getAllUsers)
  yield takeEvery(GET_USER, getSingleUser)
  yield takeEvery(ADD_USER, addUser)
  yield takeEvery(EDIT_USER, editUser)
  yield takeEvery(DELETE_USER, deleteUser)
}

export default recettesSaga