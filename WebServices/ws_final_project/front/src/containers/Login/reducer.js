import { LOGIN_SUCCESS, LOGIN_GOOGLE_SUCCESS, LOGOUT } from './constants'

const INITIAL_STATE = {
    connectedUser: {},
    token: '',
}

function loginReducer (state = INITIAL_STATE, { type, payload }) {
  switch (type) {
    case LOGIN_SUCCESS:
    case LOGIN_GOOGLE_SUCCESS:
      return {
        ...state,
        connectedUser: payload.userGetted,
        token: payload.userGetted.token,
      }
    case LOGOUT:
      return {
        ...state,
        connectedUser: {},
        token: "",
      }
    default:
      return state
  }
}
  
export default loginReducer
  