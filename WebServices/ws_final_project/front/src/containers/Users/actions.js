import {
    GET_ALL_USERS,
    GET_ALL_USERS_SUCCESS,
    GET_USER,
    GET_USER_SUCCESS,
    ADD_USER,
    EDIT_USER,
    DELETE_USER,
    SET_LOADING,
} from './constants'

export function getAllUsersAction(){
    return {
        type: GET_ALL_USERS,
    }
}
export function getAllUsersSuccessAction(payload){
    return {
        type: GET_ALL_USERS_SUCCESS,
        payload: payload,
    }
}

export function getSingleUserAction(payload){
    return {
        type: GET_USER,
        payload: payload,
    }
}
export function getSingleUserSuccessAction(payload){
    return {
        type: GET_USER_SUCCESS,
        payload: payload,
    }
}

export function addUserAction(payload){
    return {
        type: ADD_USER,
        payload: payload,
    }
}

export function editUserAction(payload){
    return {
        type: EDIT_USER,
        payload: payload,
    }
}

export function deleteUserAction(payload){
    return {
        type: DELETE_USER,
        payload: payload,
    }
}

export function setLoading(payload){
    return {
        type: SET_LOADING,
        payload: payload,
    }
}