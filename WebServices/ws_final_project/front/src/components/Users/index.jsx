import React, { useState, useEffect} from 'react'

// Components
import Header from './components/header'
import Item from './components/item'
import Button from '../UtilsComponents/button'
import Popup from './components/popup'

// Styled Components
import {
  UserContainer,
  UserTableContainer,
  UserHeader,
  TableBody,
} from './users.styles'

const Users = ({
  // Props
  users,
  selectedUser,
  // Function
  getUsers,
  getSingleUser,
  createUser,
  updateUser,
  deleteUser,
}) => {
  /** @description State */
  const [typePopup, setTypePopup] = useState('')
  const [userEditID, setUserEditID] = useState('')
  const [isModaleOpen, setModaleOpen] = useState(false)
  const [userToCreate, setUserToCreate] = useState({ lastName: '', firstName: '', email: '', password: '', confirmPassword: ''})
  const [userToUpdate, setUserToUpdate] = useState(selectedUser)
  const [invalidFields, setInvalidFields] = useState({ lastName: false, firstName: false, email: false, password: false, confirmPassword: false})
  
  /** @description ComponentDidMount */
  useEffect(() => {
    getUsers()
  }, [])

  /** @description ComponentDidUpdate */
  useEffect(() => {
    setUserToUpdate(selectedUser)
  }, [selectedUser])

  /** @description Constants */
  const headerList = ['Nom', 'Prénom', 'Email', "Type d'inscription", 'Options']
  const itemKeys = ['lastName','firstName','email', 'provider','options']
  const popupFields = [
    { name: 'lastName', type: 'text', title: 'Nom', placeholder: 'Valjean' },
    { name: 'firstName', type: 'text', title: 'Prénom', placeholder: 'Jean' },
    { name: 'email', type: 'email', title: 'Email', placeholder: 'email@example.com' },
    { name: 'password', type: 'password', title: `${typePopup !== 'edit' ? 'Mot de passe' : 'Nouveau mot de passe'}`, placeholder: 'My wonderfull password' },
    { name: 'confirmPassword', type: 'password', title: `${typePopup !== 'edit' ? 'Confirmer mot de passe' : 'Confirmer nouveau mot de passe'}`, placeholder: 'My wonderfull password' },
  ]
  const ErrorMessages = {
    lastName: 'Veuillez entrer un nom',
    firstName: 'Veuillez entrer un prénom',
    email: 'Veuillez entrer un email correcte',
    password: 'Veuillez entrer un mot de passe',
    confirmPassword: 'Veuillez entrer le même mot de passe que le précédent',
  }
  const isEmailValid = new RegExp('[^@ \t\r\n]+@[^@ \t\r\n]+\.[^@ \t\r\n]+')

  /** @description Functions */

  function onOpenPopup(type, idUser = 0){
    setTypePopup(type)

    if(type === 'edit')
      setUserEditID(idUser)
    
    setModaleOpen(true)
  }

  function onClosePopup() {
    // reset fields
    setUserToCreate({ lastName: '', firstName: '', email: '', password: '', confirmPassword: ''})
    setUserToUpdate({})
    // close modal
    setModaleOpen(false)
  }

  function onChangeInput({target}){
    // remove error message
    if(invalidFields[target.name]) setInvalidFields({...invalidFields, [target.name]: false})
    
    if(typePopup === 'new')
      setUserToCreate({
        ...userToCreate,
        [target.name]: target.value,
      })
    else
      setUserToUpdate({
        ...userToUpdate,
        [target.name]: target.value
      })
  }

  function onCreateUser() {
    const {firstName, lastName, email, password, confirmPassword} = userToCreate

    const areAllFieldsFilled = firstName !== '' && lastName !== '' && email !== '' && password !== '' && confirmPassword !== ''
    const samePassword = password === confirmPassword
    
    if(areAllFieldsFilled && isEmailValid.test(email) && samePassword){
      const createdUser = {
        firstName,
        lastName,
        email,
        password,
      }
      createUser({userToCreate: createdUser})
      onClosePopup()
    } else {
      let firstNameWrong = false,
        lastNameWrong = false,
        emailWrong = false,
        passwordWrong = false,
        confirmPasswordWrong = false

      if(firstName === '') {
        firstNameWrong = true
      }
      if(lastName === '') {
        lastNameWrong = true
      }
      if(email === '') {
        emailWrong = true
      }
      if(password === '') {
        passwordWrong = true
      }
      if(confirmPassword === '' || password !== confirmPassword) {
        confirmPasswordWrong = true
      }

      setInvalidFields({
        firstName: firstNameWrong,
        lastName: lastNameWrong,
        email: emailWrong,
        password: passwordWrong,
        confirmPassword: confirmPasswordWrong
      })
    }
  }

  function onUpdateUser() {
    const {idUser, firstName, lastName, email, password, confirmPassword} = userToUpdate

    const areAllFieldsFilled = firstName !== '' && lastName !== '' && email !== ''
    const isPasswordChanged = password !== ''
    const samePassword = password === confirmPassword
    
    if(isPasswordChanged) {
      if(areAllFieldsFilled && isEmailValid.test(email) && samePassword){
        const updatedUser = {
          firstName,
          lastName,
          email,
          password,
        }
        updateUser({idUser: idUser, userToUpdate: updatedUser})
        onClosePopup()
      } else {
      let firstNameWrong = false,
        lastNameWrong = false,
        emailWrong = false,
        passwordWrong = false,
        confirmPasswordWrong = false

      if(firstName === '') {
        firstNameWrong = true
      }
      if(lastName === '') {
        lastNameWrong = true
      }
      if(email === '') {
        emailWrong = true
      }
      if(password === '') {
        passwordWrong = true
      }
      if(confirmPassword === '' || password !== confirmPassword) {
        confirmPasswordWrong = true
      }

      setInvalidFields({
        firstName: firstNameWrong,
        lastName: lastNameWrong,
        email: emailWrong,
        password: passwordWrong,
        confirmPassword: confirmPasswordWrong
      })
    }
    }
    else {
      if(areAllFieldsFilled && isEmailValid.test(email)){
        const updatedUser = {
          firstName,
          lastName,
          email,
        }
        updateUser({idUser: idUser, userToUpdate: updatedUser})
        onClosePopup()
      } else {
      let firstNameWrong = false,
        lastNameWrong = false,
        emailWrong = false,
        passwordWrong = false,
        confirmPasswordWrong = false

      if(firstName === '') {
        firstNameWrong = true
      }
      if(lastName === '') {
        lastNameWrong = true
      }
      if(email === '') {
        emailWrong = true
      }
      if(password === '') {
        passwordWrong = true
      }
      if(confirmPassword === '' || password !== confirmPassword) {
        confirmPasswordWrong = true
      }

      setInvalidFields({
        firstName: firstNameWrong,
        lastName: lastNameWrong,
        email: emailWrong,
        password: passwordWrong,
        confirmPassword: confirmPasswordWrong
      })
    }
    }
  }

  return (
    <UserContainer>
      <UserHeader>
        <h3> Liste des utilisateurs </h3>
        <Button 
          id='btn_new_user'
          icon={<i className='fas fa-user-plus' />}
          text='Ajouter un utilisateur'
          backgroundColor="#48A31D"
          color="#FFFFFF"
          onClick={() => onOpenPopup('new')}
        />
      </UserHeader>
      <UserTableContainer>
        <Header 
          headerList={headerList}
        />
        <TableBody>
          {
            React.Children.toArray(users.map((user, index) => (
              <Item 
                keys={itemKeys}
                user={user}
                index={index}
                onEditItem={() => onOpenPopup('edit', user.idUser)}
                onDeleteItem={() => deleteUser({idUser: user.idUser})}
              />
            )))
          }
        </TableBody>
      </UserTableContainer>

      {isModaleOpen && 
        <Popup 
          //Props
          type={typePopup}
          idUser={userEditID}
          user={typePopup === 'new' ? userToCreate : userToUpdate}
          fields={popupFields}
          invalidFields={invalidFields}
          invalidFieldsMessages={ErrorMessages}
          //Functions
          onClose={() => onClosePopup()}
          onSave={typePopup === 'new' ? () => onCreateUser() : () => onUpdateUser()}
          onChangeInput={e => onChangeInput(e)}
          getUserSelected={getSingleUser}
        />
      }
    </UserContainer>
  )
}

export default Users
