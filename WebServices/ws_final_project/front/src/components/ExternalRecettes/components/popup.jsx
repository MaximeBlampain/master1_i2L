import React, { useState, useEffect } from 'react'

// Component
import Button from '../../UtilsComponents/button'
import Modal from '../../UtilsComponents/modal'

// Styled Component
import {
  PopupBody,
  PopupItem,
  PopupStepsContainer,
  StepCard,
} from '../external_recettes.styles'
const Popup = ({
  // Props
  selectedRecette,
  idRecette,
  // Functions
  onClose,
  getSelectedRecette,
}) => {
  /** @description State */
  const [steps, setSteps] = useState([])
  /** @description ComponentDidMount */
  useEffect(() => {
    getSelectedRecette({idMeal: idRecette})
  },[])

  useEffect(() => {
    if(selectedRecette.hasOwnProperty('strInstructions')){
      const listStep = selectedRecette.strInstructions.split('\r\n')
      setSteps(listStep)
    }
  },[selectedRecette])
  return (
    <Modal 
      title='Afficher la recette'
      buttons={
        <React.Fragment>
          <Button
            text='Retour'
            backgroundColor="#B3B9B0"
            color="#FFFFFF"
            onClick={onClose}
          />
        </React.Fragment>
      }
    >
      {selectedRecette 
        ? 
        <PopupBody>
          <PopupItem>
            <h3>Titre de la recette</h3>
            <p>{selectedRecette.strMeal ? selectedRecette.strMeal : ''}</p>
          </PopupItem>
          <PopupItem>
            <h3>Image de la recette</h3>
            <img src={selectedRecette.strMealThumb} alt=''/>
          </PopupItem>
          <PopupItem>
            <h3> Etapes à suivre </h3>
            <PopupStepsContainer>
              {React.Children.toArray(steps.map(step => (
                <StepCard>{step}</StepCard>
              )))}
            </PopupStepsContainer>
          </PopupItem>

        </PopupBody>
        : <React.Fragment></React.Fragment>
      }
    </Modal>
  )
}

export default Popup