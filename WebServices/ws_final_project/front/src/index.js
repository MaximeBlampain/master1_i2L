import React from "react"
import ReactDOM from "react-dom"
import { Provider } from "react-redux"
import { PersistGate } from "redux-persist/integration/react"

import "./global.css"

// Components
import App from "./containers/App"

// Store
import configStore from "./redux/store"

const { store, persistor } = configStore()



ReactDOM.render(
  <Provider store={store}>    
  <PersistGate persistor={persistor}>
    <App />
  </PersistGate>
  </Provider>,
  document.getElementById("root")
)