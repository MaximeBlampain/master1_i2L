import { createStructuredSelector } from "reselect"
import { withRouter } from "react-router"
import { connect } from "react-redux"
import { compose } from "redux"

// Component
import RecettesComponent from '../../components/Recettes'

// Actions
import { getAllRecettesAction, getSingleRecetteAction, newRecetteAction, editRecetteAction, deleteRecetteAction } from './actions'

// Selectors
import { selectAllRecettes, selectIsLoading, selectSelectedRecette } from './selectors'

// Redux
const mapDispatchToProps = dispatch => ({
    getRecettes: () => dispatch(getAllRecettesAction()),
    getSelectedRecette: core => dispatch(getSingleRecetteAction(core)),
    createRecette: core => dispatch(newRecetteAction(core)),
    updateRecette: core => dispatch(editRecetteAction(core)),
    deleteRecette: core => dispatch(deleteRecetteAction(core)),
})

const mapStateToProps = createStructuredSelector({
    recettes: selectAllRecettes,
    selectedRecette: selectSelectedRecette,
    isLoading: selectIsLoading
})

const withConnect = connect(
    mapStateToProps, 
    mapDispatchToProps
)

// Compose
const Recettes = compose(withRouter, withConnect)(RecettesComponent)

export default Recettes