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
    LoginContainer,
    FormLogin,
    InputContainer,
    ErrorMessage,
    ButtonContainer,
} from './login.styles'

const Login = ({
    // Functions
    login,
    loginWithGoogle,
}) => {
    
    /** @description State */
    const [loginForm, setLoginForm] = useState({email: '', password: ''})
    const [invalidInputs, setInvalidInputs] = useState({email: false, password: false})

    const isEmailValid = new RegExp('[^@ \t\r\n]+@[^@ \t\r\n]+\.[^@ \t\r\n]+')

    /** @decription Functions */
    function onChangeInputLogin({ target }){
        if(invalidInputs[target.name]) setInvalidInputs({...invalidInputs, [target.name]: false})
        setLoginForm({ ...loginForm, [target.name]: target.value })
    }

    function handleSubmit(){
        
        if(isEmailValid.test(loginForm.email) && loginForm.email !== ''){
            login({...loginForm})
        } else {
            let isPasswordWrong = false, isEmailWrong = false
            if(isEmailValid.test(loginForm.email) === false  || loginForm.email === '') 
                isEmailWrong = true
            if(loginForm.password === '')
                isPasswordWrong = true
            
            setInvalidInputs({
                email: isEmailWrong,
                password: isPasswordWrong,
            })
        }
    }

    function handleGoogleLogin(response) {
        const profileObject = response.profileObj

        const userInfos = {
            firstName: profileObject.givenName,
            lastName: profileObject.familyName,
            email: profileObject.email,
            provider: 'Google',
            providerID: profileObject.googleId,
        }

        loginWithGoogle(userInfos)
    }

    return (
        <LoginContainer style={{backgroundImage: `url(${BackgroundImg})`}}>
            <div id="login_card">
                <div id="login_header">
                    <img src={logoULCO} alt='logo ULCO'/>
                    <h2>Identifiez vous</h2>
                </div>
                <FormLogin>
                    <InputContainer>
                        <label htmlFor="email">Email</label>
                        <input type="email" name='email' id="email" placeholder="name@example.com" onChange={event => onChangeInputLogin(event)}/>
                        {invalidInputs.email &&
                            <ErrorMessage> Veuillez entrer un email correcte </ErrorMessage>}
                    </InputContainer>
                    <InputContainer>
                        <label htmlFor="password">Mot de passe</label>
                        <input type="password" name='password' id="password" placeholder="password123" onChange={event => onChangeInputLogin(event)}/>
                        {invalidInputs.password &&
                            <ErrorMessage> Veillez renseigner un mot de passe </ErrorMessage>}
                    </InputContainer>
                </FormLogin>
                <ButtonContainer>
                    <Button 
                        id='btn_login'
                        icon={<i className='fas fa-sign-in-alt' />}
                        text='Se Connecter'
                        backgroundColor="#009BDE"
                        color="#FFFFFF"
                        onClick={() => handleSubmit()}
                    />
                    <GoogleLogin 
                        clientId='742895937065-0ocob2o174i9hinjfl05rti3o107otsv.apps.googleusercontent.com'
                        onSuccess={handleGoogleLogin}
                        buttonText="Se Connecter via Google"
                    ></GoogleLogin>
                </ButtonContainer>
                <Link to='/signup'> Pas de compte ? Inscrivez vous </Link>
            </div>
        </LoginContainer>
    )
}

export default Login