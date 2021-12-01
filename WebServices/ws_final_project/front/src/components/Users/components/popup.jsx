import React, { useEffect } from 'react'

// Component
import Input from '../../UtilsComponents/input'
import Button from '../../UtilsComponents/button'
import Modal from '../../UtilsComponents/modal'

const Popup = ({
  //Props
  type,
  user,
  idUser,
  fields,
  invalidFields,
  invalidFieldsMessages,
  //Functions
  onClose,
  onSave,
  onChangeInput,
  getUserSelected,
}) => {
  
  useEffect(() => {
    if(type === 'edit')
      getUserSelected({idUser: idUser})
  }, [])
  return (
    <Modal 
      title={type === 'new' ? 'Ajouter un utilisateur' : 'Modifier un utilisateur'}
      buttons={
        <React.Fragment>
          <Button
            text='Annuler'
            backgroundColor="#B3B9B0"
            color="#FFFFFF"
            onClick={onClose}
          />
          <Button
            text={type === 'new' ? 'Ajouter' : 'Modifier'}
            backgroundColor="#48A31D"
            color="#FFFFFF"
            onClick={onSave}
          />
        </React.Fragment>
      }
      onClosePopup={onClose}
    >
      {
        React.Children.toArray(fields.map(field => (
          <Input 
            name={field.name}
            value={user[field.name]}
            type={field.type}
            labelTitle={field.title}
            placeholder={field.placeholder}
            isInvalid={invalidFields[field.name]}
            invalidErrorMessage={invalidFieldsMessages[field.name]}
            onChange={onChangeInput}
          />
        )))
      }
    </Modal>
  )
}

export default Popup