import { createStructuredSelector } from "reselect"
import { withRouter } from "react-router"
import { connect } from "react-redux"
import { compose } from "redux"

// Component
import LayoutComponent from '../../components/Layout'

// Actions
import { logoutAction } from '../Login/actions'

// Selectors
import { selectConnectedUser } from '../Login/selectors'

// Redux
const mapDispatchToProps = dispatch => ({
    logout: () => dispatch(logoutAction())
})

const mapStateToProps = createStructuredSelector({
    connectedUser: selectConnectedUser
})

const withConnect = connect(
    mapStateToProps, 
    mapDispatchToProps
)

// Compose
const Layout = compose(withRouter, withConnect)(LayoutComponent)

export default Layout