import React, { useState } from 'react'
import { Link } from 'react-router-dom'
import GoogleLogin from 'react-google-login'

// Images
import logoULCO from '../../assets/images/logo_ulco.png'
import BackgroundImg from '../../assets/images/login_background.jpg'

// Components
import Button from '../UtilsComponents/button'

// Styled Components
import {
    SignupContainer,
    FormSignup,
    InputContainer,
    ErrorMessage,
    ButtonContainer,
} from './signup.styles'

const SignUp = ({
  // Functions
  signup,
  signupWithGoogle,
}) => {

  /** @description State */
  const [signupForm, setSignupForm] = useState({firstName: '', lastName: '', email: '', password: ''})
  const [invalidInputs, setInvalidInputs] = useState({firstName: false, lastName: false, email: false, password: false})

  /** @decription Functions */
  function onChangeInputSignup(event){
    const { target, key } = event
    if(invalidInputs[target.name]) setInvalidInputs({...invalidInputs, [target.name]: false})
       
    if(key === 'Enter') handleSubmit()
    else setSignupForm({ ...signupForm, [target.name]: target.value })
  }

  function handleSubmit() {
    const isEmailValid = new RegExp('[^@ \t\r\n]+@[^@ \t\r\n]+\.[^@ \t\r\n]+')
    const areFullFilled = signupForm.firstName !== '' && signupForm.lastName !== '' && signupForm.email !== '' && signupForm.password !== ''
    
    if(isEmailValid.test(signupForm.email) && areFullFilled){
      signup({...signupForm})
    } else {
      let isFirstNameWrong = false,
        isLastNameWrong = false,
        isPasswordWrong = false, 
        isEmailWrong = false

      if(signupForm.firstName === '')
        isFirstNameWrong = true
      if(signupForm.lastName === '')
        isLastNameWrong = true    
      if(isEmailValid.test(signupForm.email) === false  || signupForm.email === '') 
        isEmailWrong = true
      if(signupForm.password === '')
        isPasswordWrong = true
       
      setInvalidInputs({
        firstName: isFirstNameWrong,
        lastName: isLastNameWrong,
        email: isEmailWrong,
        password: isPasswordWrong,
      })
    }
  }

  function handleGoogleSignup(response) {
    const profileObject = response.profileObj

    const userInfos = {
      firstName: profileObject.givenName,
      lastName: profileObject.familyName,
      email: profileObject.email,
      provider: 'Google',
      providerID: `${profileObject.googleId}`,
    }

    signupWithGoogle({...userInfos})
}
  return (
    <SignupContainer style={{backgroundImage: `url(${BackgroundImg})`}}>
      <div id="signup_card">
        <div id="signup_header">
          <img src={logoULCO} alt='logo ULCO'/>
          <h2>S'inscrire</h2>
        </div>
        <FormSignup>
          <InputContainer>
            <label htmlFor='firstName'>Prénom</label>
            <input type="text" name='firstName' id="firstName" placeholder="Jean" onChange={event => onChangeInputSignup(event)}/>
            {invalidInputs.firstName &&
                <ErrorMessage> Veuillez renseigner un prénom </ErrorMessage>}
          </InputContainer>
          <InputContainer>
            <label htmlFor='lastName'>Nom</label>
            <input type="text" name='lastName' id="lastName" placeholder="Valjean" onChange={event => onChangeInputSignup(event)}/>
            {invalidInputs.lastName &&
              <ErrorMessage> Veuillez renseigner un nom </ErrorMessage>}
          </InputContainer>
          <InputContainer>
            <label htmlFor="email">Email</label>
            <input type="email" name='email' id="email" placeholder="name@example.com" onChange={event => onChangeInputSignup(event)}/>
            {invalidInputs.email &&
              <ErrorMessage> Veuillez entrer un email correcte </ErrorMessage>}
          </InputContainer>
          <InputContainer>
            <label htmlFor="password">Mot de passe</label>
            <input type="password" name='password' id="password" placeholder="password123" onChange={event => onChangeInputSignup(event)}/>
            {invalidInputs.password &&
              <ErrorMessage> Veillez renseigner un mot de passe </ErrorMessage>}
          </InputContainer>
        </FormSignup>
        <ButtonContainer>
          <Button 
            id='btn_signup'
            icon={<i className='fas fa-grin-beam' />}
            text="S'inscrire"
            backgroundColor="#009BDE"
            color="#FFFFFF"
            onClick={() => handleSubmit()}
          />
          <GoogleLogin 
            clientId='742895937065-0ocob2o174i9hinjfl05rti3o107otsv.apps.googleusercontent.com'
            onSuccess={handleGoogleSignup}
            buttonText="SIGN IN WITH GOOGLE"
          ></GoogleLogin> 
        </ButtonContainer>
        <Link to='/login'> Déjà un compte ? Connecte toi ! </Link>
      </div>
    </SignupContainer>
  )
}

export default SignUp