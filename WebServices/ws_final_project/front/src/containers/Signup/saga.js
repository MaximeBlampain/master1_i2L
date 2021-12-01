import { takeEvery, put, call } from "redux-saga/effects"

// Types
import { SIGNUP, SIGNUP_GOOGLE } from './constants'

// Actions
import { signupSuccessAction, signupWithGoogleSuccessAction } from './actions'
import { loginAction, loginWithGoogleAction } from '../Login/actions'
// Utils
import request from '../../utils/request'
import { API_URL } from "../../utils/config"
import { postCors } from "../../utils/cors"

// Functions
function* signupUser({ payload }) {
  const requestURL = `${API_URL}/users/create`
  try {
    const response = yield call(request, requestURL, postCors(payload))
    if(response.type === 'warning')
      console.warn(`${response.message}`)
    else {
      yield put(signupSuccessAction(response))
      yield put(loginAction({email: payload.email, password: payload.password}))
    }
  } catch (err) {
    console.error('err', err)
  }
}

function* signupWithGoogle({ payload }) {
  const requestURL = `${API_URL}/users/create_google`
  try {
    const response = yield call(request, requestURL, postCors(payload))
    
    if(response.type === 'warning')
      console.warn(`${response.message}`)
    else {
      yield put(signupWithGoogleSuccessAction(response))
      yield put(loginWithGoogleAction({providerID: payload.providerID}))
    }
  } catch (err) {
    console.error('err', err)
  }
}

function* signupSaga() {
  yield takeEvery(SIGNUP, signupUser)
  yield takeEvery(SIGNUP_GOOGLE, signupWithGoogle)
}

export default signupSaga
