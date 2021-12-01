import { createSelector } from "reselect"

const selectLogin = state => state.login

export const selectConnectedUser = createSelector(
  [selectLogin],
  login => login.connectedUser
)

export const selectToken = createSelector(
  [selectLogin],
  login => login.token
)