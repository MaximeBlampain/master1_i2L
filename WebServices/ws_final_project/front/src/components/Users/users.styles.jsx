import styled from 'styled-components'

export const UserContainer = styled.div`
  height: calc(100vh - 75px);
  width: 100%;
  display: grid;
  grid-template-rows: 1fr 6fr;
  justify-content: center;
`

export const UserHeader = styled.div`
  width: 100%;
  display: flex;
  justify-content: space-between;
  align-items: center;
  padding: 0 3rem;

  h3 {
    text-decoration: underline;
  }
`

export const UserTableContainer = styled.div`
  width: 100%;
  padding: 0 3rem;

  display: flex;
  flex-direction: column;
  justify-content: center;
  align-items: center;
`

export const TableHeader = styled.div`
  width: 1000px;
  background-color: #FFFFFF;
  padding: 0.5rem 0;
  display: grid;
  grid-template-columns: repeat(5, 1fr);
  
  .header-item{
    font-size: 0.9rem;
    text-align: center;
    font-weight: 500;
  }
`
export const TableBody = styled.div`
  width: 1000px;
  display: flex;
  flex-direction: column;

`


export const TableItem = styled.div`
  background-color: ${(props) => props.isClearbackground === 0 ? "#EDEDED" : "#FFFFFF"};
  padding: 0.5rem 0;
  display: grid;
  grid-template-columns: repeat(5, 1fr);
  
  .item-col{
    font-size: 0.9rem;
    text-align: center;
  }

  .item-options{
    display: flex;
    justify-content: center;
    align-items: center;

    i{
      margin: 0 0.50rem;
      cursor: pointer;
    }
    #deleteItem {
      color: #B72929;
    }
  }
`
