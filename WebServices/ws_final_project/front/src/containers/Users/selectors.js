import { createSelector } from "reselect"

const selectUsers = state => state.users

export const selectAllUsers = createSelector(
  [selectUsers],
  users => users.users
)

export const selectSelectedUser = createSelector(
  [selectUsers],
  users => users.userSelected
)

export const selectIsLoading = createSelector(
  [selectUsers],
  users => users.isLoading
)