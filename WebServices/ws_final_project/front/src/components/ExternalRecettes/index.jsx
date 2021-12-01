import React, { useState, useEffect } from 'react'

// Components
import Button from '../UtilsComponents/button'
import Popup from './components/popup'

import {
  Container,
  HeaderList,
  CardListContainer,
  RecetteCard,
} from './external_recettes.styles'

const ExternalRecette = ({
  // Props
  categories,
  externalRecettes,
  selectedRecette,
  // Functions
  getCategories,
  getCategoryRecettes,
  getSelectedRecette,
}) => {
  /** @description State */
  const [isModaleOpen, setModaleOpen] = useState(false)
  const [actualCategory, setActualCategory] = useState("")
  const [idRecette,setIdRecette] = useState("")
  
  /** @description ComponentDidMount */
  useEffect(() => {
    getCategories()
  }, [])

  /** @description ComponentDidUpdate */
  useEffect(() => {
    if(categories.length > 0) setActualCategory(categories[0].id)
  },[categories])

  useEffect(() => {
    if(actualCategory !== "") getCategoryRecettes({category: actualCategory})
  },[actualCategory])


  /** @description Functions */
  function onOpenPopup(idRecette) {
    setIdRecette(idRecette)
    setModaleOpen(true)
  }

  function onClosePopup() {
    setIdRecette('')
    setModaleOpen(false)
  }
  return (
    <Container>
      <HeaderList>
        {React.Children.toArray(categories.map(category => (
          <h4 onClick={() => setActualCategory(category.id)} className={category.id === actualCategory ? 'active' : ''}>{`${category.name}`}</h4>
        )))}
      </HeaderList>
      <CardListContainer>
        {React.Children.toArray(externalRecettes.map(recette => (
          <RecetteCard>
            <h3>{recette.strMeal}</h3>
            <img src={recette.strMealThumb} alt=''/>
            <Button 
              id='btn_show_recette'
              text='Afficher la recette'
              backgroundColor="#23180d"
              color="#FFFFFF"
              onClick={() => onOpenPopup(recette.idMeal)}
            />
          </RecetteCard>
        )))}
      </CardListContainer>
      {isModaleOpen &&
        <Popup 
          // Props
          selectedRecette={selectedRecette}
          idRecette={idRecette}
          // Functions
          onClose={() => onClosePopup()}
          getSelectedRecette={getSelectedRecette}
        />
      }
    </Container>    
  )
}

export default ExternalRecette

