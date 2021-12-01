import { createStructuredSelector } from "reselect"
import { withRouter } from "react-router"
import { connect } from "react-redux"
import { compose } from "redux"

// Component
import UsersComponent from '../../components/Users'

// Actions
import { getAllUsersAction, getSingleUserAction, addUserAction, editUserAction, deleteUserAction } from './actions'

// Selectors
import { selectAllUsers, selectSelectedUser } from './selectors'

// Redux
const mapDispatchToProps = dispatch => ({
    getUsers: () => dispatch(getAllUsersAction()),
    getSingleUser: idUser => dispatch(getSingleUserAction(idUser)),
    createUser: createdUser => dispatch(addUserAction(createdUser)),
    updateUser: updatedUser => dispatch(editUserAction(updatedUser)),
    deleteUser: idUser => dispatch(deleteUserAction(idUser)),

})

const mapStateToProps = createStructuredSelector({
    users: selectAllUsers,
    selectedUser : selectSelectedUser
})

const withConnect = connect(
    mapStateToProps, 
    mapDispatchToProps
)

// Compose
const Users = compose(withRouter, withConnect)(UsersComponent)

export default Users