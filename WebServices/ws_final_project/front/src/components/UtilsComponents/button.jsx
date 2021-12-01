import React from 'react'

import styled from 'styled-components'

const ButtonContainer = styled.button`
  height: 50px;
  width: ${props => props.onlyIcon ? "50px" : ""};
  font-family: Roboto;
  display: flex;
  justify-content: center;
  align-items: center;
  padding: ${props => props.onlyIcon ? "" : "0 20px"};
  background-color: ${(props) => props.backgroundColor};
  color: ${(props) => props.color};
  border: 1px solid ${(props) => props.borderColor};
  text-transform: uppercase;
  font-weight: ${props => props.fontWeight ? props.fontWeight : "500"};
  font-size: 14px;
  border-radius: 5px;
  cursor: pointer;
  transition: 0.2s;
  span {
    margin-right: ${props => props.onlyIcon ? "" : "20px"};
  }
  &:hover{
    background-color: ${props => props.backgroundColorHover};
    color: ${props => props.colorHover};
  }
`

const Button = ({
    id,
    icon,
    text, 
    backgroundColor,
    borderColor,
    color, 
    backgroundColorHover,
    borderColorHover,
    colorHover, 
    style,
    onClick,
    fontWeight,

}) => {

    const newId = id || "button" + ( Math.random()*1000 )

    const calculateColorHover = backgroundColor === "white" 
      ? borderColor + "33" 
      : backgroundColor + "80"
  
    return (
      <ButtonContainer 
        id={newId}
        backgroundColor={backgroundColor}
        borderColor={borderColor !== undefined ? borderColor : backgroundColor}
        color={color} 
        backgroundColorHover={backgroundColorHover !== undefined ? backgroundColorHover : calculateColorHover}
        borderColorHover={
          borderColorHover !== undefined 
          ? borderColorHover 
          : borderColor !== undefined 
            ? borderColor 
            : backgroundColor
        }
        colorHover={colorHover ? colorHover : color}
        onlyIcon={text === undefined}
        fontWeight={fontWeight}
        onClick={onClick}
        style={style}
      >
        {icon !== undefined && <span>{icon}</span>}
        {text}
      </ButtonContainer>
    )
  }
  
  Button.defaultProps = {
    color: "#fff",
    backgroundColor: "#FF5F55",
    onClick: () => {},
  }
  
  export default Button
  
