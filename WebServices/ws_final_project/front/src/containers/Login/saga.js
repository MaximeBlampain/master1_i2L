import { takeEvery, put, call } from "redux-saga/effects"

// Types
import { LOGIN, LOGIN_GOOGLE } from './constants'

// Actions
import { loginSuccessAction, loginWithGoogleSuccessAction } from './actions'

// Utils
import request from '../../utils/request'
import { API_URL } from "../../utils/config"
import { postCors } from "../../utils/cors"

// Functions
function* loginUser({ payload }) {
  const requestURL = `${API_URL}/users/auth`
  try {
    const response = yield call(request, requestURL, postCors(payload))

    if(response.type === 'warning')
      console.warn(`${response.message}`)
    else {
      yield put(loginSuccessAction(response))
    }
  } catch (err) {
    console.error('err', err)
  }
}

function* loginWithGoogle({ payload }) {
  const requestURL = `${API_URL}/users/auth_google`
  try {
    const response = yield call(request, requestURL, postCors(payload))
    
    if(response.type === 'warning')
      console.warn(`${response.message}`)
    else {
      yield put(loginWithGoogleSuccessAction(response))
    }
  } catch (err) {
    console.error('err', err)
  }
}

function* loginSaga() {
  yield takeEvery(LOGIN, loginUser)
  yield takeEvery(LOGIN_GOOGLE, loginWithGoogle)
}

export default loginSaga
