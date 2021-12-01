import { createSelector } from "reselect"

const selectRecettes = state => state.recettes

export const selectAllRecettes = createSelector(
  [selectRecettes],
  recettes => recettes.recettes
)

export const selectSelectedRecette = createSelector(
  [selectRecettes],
  recettes => recettes.selectedRecette
)

export const selectIsLoading = createSelector(
  [selectRecettes],
  recettes => recettes.isLoading
)