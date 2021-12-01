import React, { useEffect } from 'react'

// Component
import Input from '../../UtilsComponents/input'
import Button from '../../UtilsComponents/button'
import Modal from '../../UtilsComponents/modal'

const Popup = ({
  // Props
  type,
  recette,
  idRecette,
  fields,
  invalidFields,
  invalidFieldsMessages,
  isLoading,
  //Functions
  onClose,
  onSave,
  onDelete,
  onChangeInput,
  getRecetteSelected,
}) => {
  useEffect(() => {
    if(type === 'edit')
      getRecetteSelected({idRecette: idRecette})
  }, [])
  return (
    <Modal 
      title={type === 'new' ? 'Ajouter une recette' : 'Modifier une recette'}
      buttons={
        <React.Fragment>
          <Button
            text='Supprimer'
            backgroundColor="#B72929"
            color="#FFFFFF"
            onClick={onDelete}
          />
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
      {!isLoading ?
        React.Children.toArray(fields.map(field => (
          <Input 
            name={field.name}
            value={recette[field.name]}
            type={field.type}
            labelTitle={field.title}
            placeholder={field.placeholder}
            isInvalid={invalidFields[field.name]}
            invalidErrorMessage={invalidFieldsMessages[field.name]}
            onChange={onChangeInput}
          />
        ))) :
        <React.Fragment></React.Fragment>
      }
    </Modal>
  )
}

export default Popup