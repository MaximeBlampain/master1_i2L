import styled from 'styled-components'

export const Container = styled.div`
  height: calc(100vh - 75px);
  width: 100%;
  display: grid;
  grid-template-rows: 1fr 6fr;
  justify-content: center;
`

export const HeaderList = styled.div`
  padding-top: 2rem;
  display: flex;
  justify-content: space-around;
  align-items: center;
  padding: 0 3rem;

  h4 {
    &.active {
      text-decoration: underline;
    }
    cursor: pointer;
    margin: 0 1rem;
  }
`
export const CardListContainer = styled.div`
  width: 100%;
  height: 100%;
  overflow-y: auto;
  display: flex;
  flex-wrap: wrap;
  padding: 0 50px;
  padding-top: 2rem;
`

export const RecetteCard = styled.div`
  height: 400px;
  width: 400px;
  background-color: #FFFFFF;

  border-radius: 10px;
  margin: 1.5rem 0.5rem;
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  align-items: center;

  h3 {
    margin: 0 0.5rem;
    margin-top: 1rem;
    text-align: center;
  }
  img {
    max-height: 200px;
    max-width: 200px;
  }
  button{
    margin-bottom: 1rem;
    text-transform: initial;
  }
`

export const PopupBody = styled.div`
  width: 100%;
  height: 100%;
  overflow-y: auto;
  display: flex;
  flex-direction: column;
  align-items: flex-start;
`
export const PopupItem = styled.div`
  margin: 1rem 0;
  max-height: 200px;
  display: flex;
  flex-direction: column;
  justify-content: space-between;

  img {
    max-height: 175px;
    max-width: 175px;
    margin: 0 0.5rem;
  }
`

export const PopupStepsContainer = styled.div`
  display: flex;
  flex-direction: column;
`
export const StepCard = styled.div`
  margin: 1rem 0;
  padding: 0.5rem;
  background-color: #BEBEBE;
  border-radius: 10px;
`