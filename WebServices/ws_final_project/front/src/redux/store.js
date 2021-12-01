import { createStore, applyMiddleware, compose } from 'redux'
import createSagaMiddleware from 'redux-saga'
import { persistStore } from "redux-persist"

// Root Reducer && Root Saga
import persistReducer from './root-reducer'
import rootSaga from './root-saga'

const sagaMiddleware = createSagaMiddleware()

const composeEnhancers = 
    typeof window === "object" && window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__
        ? window.__REDUX_DEVTOOLS_EXTENSION_COMPOSE__({})
        : compose

// Middlewares
const middlewares = [sagaMiddleware]
const enhancers = composeEnhancers(applyMiddleware(...middlewares))



export default (INITIAL_STATE = {}) => {
    let store = createStore(persistReducer, INITIAL_STATE, enhancers)
    let persistor = persistStore(store)
    
    sagaMiddleware.run(rootSaga)

    return { store, persistor }
}