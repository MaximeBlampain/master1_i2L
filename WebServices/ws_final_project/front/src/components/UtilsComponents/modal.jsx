import React from 'react'
import styled from 'styled-components'

export const ContainerModal = styled.div`
  display: grid;
  grid-template-columns: 55vw 45vw;
  height: 100%;
  width: 100%;
  position: fixed;
  top:0;
  left: 0;
  z-index: 100;
`

export const OutputSide = styled.div`
  background-color: #FFFFFF88;
  height:100%;
  width:100%;

`

export const ContentModal = styled.div`
  display: flex;
  flex-direction: column;
  background-color: #FFFFFF;
`

export const HeaderModal = styled.div`
  height: 100px;
  width: 100%;
  display: flex;
  justify-content: space-between;
  align-items: center;
  margin-bottom: 2rem;
  padding: 1rem 2rem;
  box-shadow: 0px 5px 10px -4px rgba(0, 0, 0, 0.1);
`

export const ButtonContainer = styled.div`
  display: flex;
  button {
    margin: 0 1rem;
  }
`

export const BodyModal = styled.div`
  height: 100%;
  width: 100%;
  display: flex;
  flex-direction: column;
  align-items: center;
  padding: 1rem 2rem;
`


const Modal = ({
  title,
  buttons,
  children,
  // Function
  onClosePopup,
}) => (
  <ContainerModal>
    <OutputSide onClick={onClosePopup}/>
    <ContentModal>
      <HeaderModal>
        <h3>{title}</h3>
        <ButtonContainer>
          {buttons}
        </ButtonContainer>
      </HeaderModal>
      <BodyModal>
        {children}
      </BodyModal>
    </ContentModal>
  </ContainerModal>
)

export default Modal