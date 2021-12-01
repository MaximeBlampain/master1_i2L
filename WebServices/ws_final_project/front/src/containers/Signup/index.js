import { createStructuredSelector } from "reselect"
import { withRouter } from "react-router"
import { connect } from "react-redux"
import { compose } from "redux"

// Component
import SignupComponent from '../../components/Signup'

// Actions
import { signupAction, signupWithGoogleAction } from './actions'

// Redux
const mapDispatchToProps = dispatch => ({
    signup: core => dispatch(signupAction(core)),
    signupWithGoogle: core => dispatch(signupWithGoogleAction(core)),
})

const mapStateToProps = createStructuredSelector({
})

const withConnect = connect(
    mapStateToProps, 
    mapDispatchToProps
)

// Compose
const Signup = compose(withRouter, withConnect)(SignupComponent)

export default Signup