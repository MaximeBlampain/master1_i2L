import styled from 'styled-components'

export const RecetteContainer = styled.div`
  height: calc(100vh - 75px);
  width: 100%;
  display: grid;
  grid-template-rows: 1fr 6fr;
  justify-content: center;
`
export const RecetteHeader = styled.div`
  margin-top: 2rem;
  width: 100%;
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0 3rem;

  h3 {
    text-decoration: underline;
  }
`

export const RecetteListContainer = styled.div`
  width: 100%;

  display: flex;
  flex-wrap: wrap;
  padding: 0 80px;

`

export const RecetteCard = styled.div`
  height: 300px;
  width: 400px;
  background-color: #FFFFFF;

  border-radius: 10px;
  margin: 2rem 1rem;
  display: flex;
  flex-direction: column;
  justify-content: space-between;
  align-items: center;

  h3 {
    margin-top: 1rem;
  }
  button{
    margin-bottom: 1rem;
    text-transform: initial;
  }
`

