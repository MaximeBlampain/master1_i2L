import { SIGNUP, SIGNUP_SUCCESS, SIGNUP_GOOGLE, SIGNUP_GOOGLE_SUCCESS } from './constants'

export function signupAction(payload){
    return {
        type: SIGNUP,
        payload: payload,
    }
}

export function signupSuccessAction(response){
    return {
        type: SIGNUP_SUCCESS,
        payload: response,
    }
}

export function signupWithGoogleAction(payload){
    return {
        type: SIGNUP_GOOGLE,
        payload: payload,
    }
}

export function signupWithGoogleSuccessAction(response){
    return {
        type: SIGNUP_GOOGLE_SUCCESS,
        payload: response,
    }
}
