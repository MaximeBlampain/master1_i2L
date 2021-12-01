import styled from 'styled-components'

export const LoginContainer = styled.main`
    height: 100%;
    width: 100%;

    display: flex;
    justify-content: flex-end;
    align-items: center;

    background-repeat: no-repeat;
    background-size: cover;




    #login_card {
        width: 750px;
        height: 100vh;

        display: flex;
        flex-direction: column;
        justify-content: space-around;
        text-align: center;
        
        background-color: #FFFFFF88;

        padding: 2rem;

        overflow-y: auto;
    
    }
    
    #login_header {
        display: flex;
        flex-direction: column;
        justify-content: center;
        align-items: center;
        
        margin: 1rem 0;

        img {
            height: 200px;
            width: 200px;
            margin-bottom: 30px;
        }

        h2 {
            margin-top: 0;
            font-weight: 500;
            font-style: italic;
            
        }
    }
`

export const FormLogin = styled.form`
    display: flex;
    flex-direction: column;
    justify-content: space-around;
`

export const InputContainer = styled.div`
    padding: 10px 0;

    display:flex;
    flex-direction:column;
    border-radius: 5px;
    width: 100%;
    border: ${props => props.unvalidValue ? "1px solid red" : "none"};

    label {
        text-align: left;
        font-weight: 400;
        font-size: 1.25rem !important;
        text-transform: none;
        margin-bottom: 0.5rem;
    }

    input {
        padding-left: 1.5rem !important;
        height: 50px;
        border: 0;
        border-radius: 10px;
        &:invalid {
          box-shadow: none;
        }
        &::placeholder {
          font-size: 1rem !important;
        }
    }
`

export const ErrorMessage = styled.div `
  margin-top: 10px;
  font-size: 1rem;
  font-style: italic;
  color: red;
  text-align: left;
`

export const ButtonContainer = styled.div`
    display: flex;
    justify-content: center;
    align-items: center;
    button {
        height: 50px !important;
        width: 200px !important;
        border-radius: 10px !important;
        box-shadow: rgba(0, 0, 0, 0.24) 0px 2px 2px 0px, rgba(0, 0, 0, 0.24) 0px 0px 1px 0px;
        margin: 0 10px;
        font-size: 14px;
    }
`