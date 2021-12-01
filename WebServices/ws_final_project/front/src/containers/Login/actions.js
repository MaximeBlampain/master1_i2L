import { LOGIN, LOGIN_SUCCESS, LOGOUT, LOGIN_GOOGLE, LOGIN_GOOGLE_SUCCESS } from './constants'

export function loginAction(payload){
    return {
        type: LOGIN,
        payload: payload,
    }
}

export function loginSuccessAction(response){
    return {
        type: LOGIN_SUCCESS,
        payload: response,
    }
}

export function loginWithGoogleAction(payload){
    return {
        type: LOGIN_GOOGLE,
        payload: payload,
    }
}

export function loginWithGoogleSuccessAction(response){
    return {
        type: LOGIN_GOOGLE_SUCCESS,
        payload: response,
    }
}

export function logoutAction(){
    return {
        type: LOGOUT,
    }
}