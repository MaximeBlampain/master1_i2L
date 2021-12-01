import { createSelector } from "reselect"

const selectExternalRecettes = state => state.externalRecettes

export const selectCategories = createSelector(
  [selectExternalRecettes],
  externalRecettes => externalRecettes.categories
)

export const selectCategoryRecettes = createSelector(
  [selectExternalRecettes],
  externalRecettes => externalRecettes.recettes
)

export const selectSelectedRecette = createSelector(
  [selectExternalRecettes],
  externalRecettes => externalRecettes.recette
)