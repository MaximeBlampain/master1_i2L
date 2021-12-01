import { createStructuredSelector } from "reselect"
import { withRouter } from "react-router"
import { connect } from "react-redux"
import { compose } from "redux"

// Component
import LoginComponent from '../../components/Login'

// Actions
import { loginAction, loginWithGoogleAction } from './actions'

// Redux
const mapDispatchToProps = dispatch => ({
    login: core => dispatch(loginAction(core)),
    loginWithGoogle: core => dispatch(loginWithGoogleAction(core)),
})

const mapStateToProps = createStructuredSelector({
})

const withConnect = connect(
    mapStateToProps, 
    mapDispatchToProps
)

// Compose
const Login = compose(withRouter, withConnect)(LoginComponent)

export default Login