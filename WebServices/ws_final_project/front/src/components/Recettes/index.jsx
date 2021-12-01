import React, { useState, useEffect} from 'react'

// Components
import Button from '../UtilsComponents/button'
import Popup from './components/popup'

import {
  RecetteContainer,
  RecetteHeader,
  RecetteListContainer,
  RecetteCard,
} from './recettes.styles'

const Recettes = ({
  // Props
  recettes,
  selectedRecette,
  isLoading,
  // Functions
  getRecettes,
  getSelectedRecette,
  createRecette,
  updateRecette,
  deleteRecette,
}) => {
  /** @description State */
  const [typePopup, setTypePopup] = useState('')
  const [recetteEditID, setRecetteEditID] = useState('')
  const [isModaleOpen, setModaleOpen] = useState(false)
  const [recetteToCreate, setRecetteToCreate] = useState({ title: '', description: '', steps: ''})
  const [recetteToUpdate, setRecetteToUpdate] = useState(selectedRecette)
  const [invalidFields, setInvalidFields] = useState({ title: false, description: false, steps: false})
  
  /** @description ComponentDidMount */
  useEffect(() => {
    getRecettes()
  }, [])

  /** @description ComponentDidUpdate */
  useEffect(() => {
    setRecetteToUpdate(selectedRecette)
  }, [selectedRecette])

  /** @description Constants */
  const popupFields = [
    { name: 'title', type: 'text', title: 'Titre de la recette', placeholder: 'Ma superbe recette' },
    { name: 'description', type: 'text', title: 'Description', placeholder: 'Lorem ipsum ...' },
    { name: 'steps', type: 'text', title: 'Etapes', placeholder: 'Faire préchauffer le four' },
  ]
  const ErrorMessages = {
    title: 'Veuillez renseigner le titre de la recette',
    description: 'Veuillez renseigner une description',
    steps: 'Vous devez au moins donner une étape à suivre !',
  }
  
  /** @description Functions */
  function onOpenPopup(type, idRecette = 0){
    setTypePopup(type)
    if(type === 'edit')
      setRecetteEditID(idRecette)
    
    setModaleOpen(true)
  }

  function onClosePopup() {
    // reset fields
    setRecetteToCreate({ title: '', description: '', steps: ''})
    setRecetteToUpdate({})
    setInvalidFields({title: false, description: false, steps: false})
    // close modal
    setModaleOpen(false)
  }

  function onChangeInput({target}){
    // remove error message
    if(invalidFields[target.name]) setInvalidFields({...invalidFields, [target.name]: false})
    
    if(typePopup === 'new')
      setRecetteToCreate({
        ...recetteToCreate,
        [target.name]: target.value,
      })
    else
      setRecetteToUpdate({
        ...recetteToUpdate,
        [target.name]: target.value
      })
  }

  function onSave() {
    const recette = typePopup !== 'edit' ? recetteToCreate : recetteToUpdate

    const {title, description} = recette

    const areAllFieldsFilled = title !== '' && description !== ''

    if(areAllFieldsFilled){
      const recetteToSend = {
        title,
        description,
        steps: [],
      }
      if(typePopup !== 'edit')
        createRecette({recetteToCreate: recetteToSend})
      else
        updateRecette({idRecette: recetteEditID, recetteToUpdate: recetteToSend})
      onClosePopup()
    } else {
      let titleWrong= false,
        descriptionWrong= false
      
      if(title === '') titleWrong = true

      if(description === '') descriptionWrong = true

      setInvalidFields({
        title: titleWrong,
        description: descriptionWrong,
      })
    }
  }

  function onDelete() {
    deleteRecette({idRecette: recetteEditID})
    onClosePopup()
  }

  return (
    <RecetteContainer>
      <RecetteHeader>
        <h3> Liste des recettes </h3>
        <Button 
          id='btn_new_recette'
          icon={<i className='fas fa-book' />}
          text='Ajouter une recette'
          backgroundColor="#48A31D"
          color="#FFFFFF"
          onClick={() => onOpenPopup('new')}
        />
      </RecetteHeader>
      <RecetteListContainer>
      {
        React.Children.toArray(recettes.map((recette, index) => (
          <RecetteCard>
              <h3>{recette.title}</h3>
              <p>{recette.description}</p>
              <Button 
                id='btn_show_recette'
                text='Afficher la recette'
                backgroundColor="#009BDE"
                color="#FFFFFF"
                onClick={() => onOpenPopup('edit', recette._id)}
              />
          </RecetteCard>
        )))
      }
      </RecetteListContainer>
      {isModaleOpen && 
        <Popup 
          // Props
          type={typePopup}
          idRecette={recetteEditID}
          recette={typePopup === 'new' ? recetteToCreate : recetteToUpdate}
          fields={popupFields}
          invalidFields={invalidFields}
          invalidFieldsMessages={ErrorMessages}
          isLoading={isLoading}
          // Functions
          onClose={() => onClosePopup()}
          onSave={() => onSave()}
          onDelete={() => onDelete()}
          onChangeInput={e => onChangeInput(e)}
          getRecetteSelected={getSelectedRecette}
        />
      }
    </RecetteContainer>
  )
}

export default Recettes
