import { createStructuredSelector } from "reselect"
import { withRouter } from "react-router"
import { connect } from "react-redux"
import { compose } from "redux"

// Component
import ExternalRecettesComponent from '../../components/ExternalRecettes'

// Actions
import { getCategoriesAction, getCategoryRecettesAction, getSelectedRecette } from './actions'

// Selectors
import { selectCategories, selectCategoryRecettes, selectSelectedRecette } from './selectors'

// Redux
const mapDispatchToProps = dispatch => ({
    getCategories: () => dispatch(getCategoriesAction()),
    getCategoryRecettes: core => dispatch(getCategoryRecettesAction(core)),
    getSelectedRecette: core => dispatch(getSelectedRecette(core)),
})

const mapStateToProps = createStructuredSelector({
    categories: selectCategories,
    externalRecettes: selectCategoryRecettes,
    selectedRecette: selectSelectedRecette,
})

const withConnect = connect(
    mapStateToProps, 
    mapDispatchToProps
)

// Compose
const ExternalRecettes = compose(withRouter, withConnect)(ExternalRecettesComponent)

export default ExternalRecettes