import { all } from 'redux-saga/effects'

// Saga
import loginSaga from '../containers/Login/saga'
import signupSaga from '../containers/Signup/saga'
import usersSaga from '../containers/Users/saga'
import recettesSaga from '../containers/Recettes/saga'
import externalRecettesSaga from '../containers/ExternalRecettes/saga'



export default function* rootSaga() {
  yield all([
    loginSaga(),
    signupSaga(),
    usersSaga(),
    recettesSaga(),
    externalRecettesSaga(),
  ])
}