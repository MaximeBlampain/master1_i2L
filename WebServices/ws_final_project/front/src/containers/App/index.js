import React, { Component, lazy, Suspense, Fragment } from "react"
import { connect } from "react-redux"
import { createStructuredSelector } from "reselect"
import {
  BrowserRouter as Router,
  Switch,
  Route,
  Redirect,
} from "react-router-dom"

// Containers
import Layout from "../Layout"
import Login from "../Login"
import SignUp from "../Signup"

// Selectors
import {
  selectConnectedUser,
  selectToken,
} from "../Login/selectors"

// Lazy containers
const Users = lazy(() => import("../Users"))
const Recettes = lazy(() => import("../Recettes"))
const ExternalRecettes = lazy(() => import("../ExternalRecettes"))

const PrivateRoute = ({ component: Component, connected, ...rest }) => {
  return (
    <Route
      {...rest}
      render={props =>
        connected ? (
          <Layout>
            <Component {...props} />
          </Layout>
        ) : (
          <Redirect
            to={{
              pathname: "/login",
              state: { from: props.location },
            }}
          />
        )
      }
    />
  )
}

const PublicRoute = ({ component: Component, connected, ...rest }) => {
  return (
    <Route
      {...rest}
      render={props =>
        !connected ? (
          <Fragment>
            <Component {...props} />
          </Fragment>
        ) : (
          <Redirect
            to={{
              pathname: "/",
              state: { from: props.location },
            }}
          />
        )
      }
    />
  )
}

class App extends Component {
  render() {
    const { token } = this.props

    return (
      <Router>
          <Suspense fallback={<h3> Loading...</h3>}>
            <Switch>
              <PrivateRoute
                exact
                path="/"
                component={Recettes}
                connected={!!token}
              />
              <PrivateRoute
                exact
                path="/external_recettes"
                component={ExternalRecettes}
                connected={!!token}
              />
              <PrivateRoute
                exact
                path="/users"
                component={Users}
                connected={!!token}
              />
              <PublicRoute
                path="/login"
                component={Login}
                connected={!!token}
              />
              <PublicRoute
                path="/signup"
                component={SignUp}
                connected={!!token}
              />
              <PublicRoute path="/:others" component={<h3>Not Found</h3>} />
            </Switch>
          </Suspense>
      </Router>
    )
  }
}

const mapStateToProps = createStructuredSelector({
  connectedUser: selectConnectedUser,
  token: selectToken,
})

export default connect(mapStateToProps)(App)
