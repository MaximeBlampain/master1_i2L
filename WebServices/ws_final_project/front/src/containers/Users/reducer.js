import {
    GET_ALL_USERS_SUCCESS,
    GET_USER_SUCCESS,
    SET_LOADING,
} from './constants'

const INITIAL_STATE = {
    users: [],
    userSelected: {},
    isLoading: false,
}

function usersReducer (state= INITIAL_STATE, { type, payload }) {
    switch(type) {
        case GET_ALL_USERS_SUCCESS:
            return {
                ...state,
                users: payload.usersGetted
            }
        case GET_USER_SUCCESS:
            return {
                ...state,
                userSelected: payload.userGetted
            }
        case SET_LOADING:
            return {
                ...state,
                isLoading: payload
            }
        default:
            return state
    }
}

export default usersReducer