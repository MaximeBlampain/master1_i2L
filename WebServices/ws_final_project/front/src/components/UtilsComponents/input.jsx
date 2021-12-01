import React from 'react'
import PropTypes from 'prop-types'
import styled from 'styled-components'

// Styled Components
const InputContainer = styled.div`
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

const Input = ({
  // Props
  name,
  value,
  type,
  labelTitle,
  placeholder,
  isInvalid,
  invalidErrorMessage,
  // Functions
  onChange,
}) => (
  <InputContainer>
    <label htmlFor={ name }>{ labelTitle }</label>
    <input 
      type={ type } 
      name={ name } 
      value={ value }
      placeholder={ placeholder } 
      onChange={ onChange }/>
    {isInvalid &&
      <ErrorMessage>{ invalidErrorMessage }</ErrorMessage>}
  </InputContainer>
)

export default Input

Input.defaultProps = {
  // Props
  name: 'input',
  value: '',
  type: 'text',
  labelTitle: 'Input',
  placeholder: '',
  isInvalid: false,
  invalidErrorMessage: 'This is a wrong test',
  // Functions
  onChange: () => console.info('onChange Input'),
}

Input.propTypes = {
  // Props
  name: PropTypes.string,
  value: PropTypes.string,
  type: PropTypes.string,
  labelTitle: PropTypes.string,
  placeholder: PropTypes.string,
  isInvalid: PropTypes.bool,
  invalidErrorMessage: PropTypes.string,
  // Functions
  onChange: PropTypes.func,
}