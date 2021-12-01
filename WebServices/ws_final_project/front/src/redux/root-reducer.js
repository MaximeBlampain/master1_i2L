import { combineReducers } from 'redux';
import { persistReducer } from "redux-persist"
import storage from "redux-persist/lib/storage"

// Reducers
import loginReducer from '../containers/Login/reducer'
import usersReducer from '../containers/Users/reducer'
import recettesReducer from '../containers/Recettes/reducer'
import externalRecettesReducer from '../containers/ExternalRecettes/reducer'

// Persist
const persistConfig = {
    key: "root",
    storage,
    whitelist: ["login"],
    blacklist: ["message"],
}
  
const rootReducer = combineReducers({
    login: loginReducer,
    users: usersReducer,
    recettes: recettesReducer,
    externalRecettes: externalRecettesReducer,
})

export default persistReducer(persistConfig, rootReducer)