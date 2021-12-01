import React, { useState, Fragment, Children } from 'react'
import { Link } from 'react-router-dom'

// Components
import Button from '../UtilsComponents/button'

// Styled Components
import {
    NavBar,
    Li,
} from './layout.styles'
const Layout = ({
    // Props
    history,
    children,
    // Functions
    logout,
}) => {
    const actualRoute = history.location.pathname

    const [activePath, setActivePath] = useState(actualRoute)

    const links = [
        {
            route: '/',
            name: ' Recettes',
            icon: 'fa-cookie-bite',
        },
        {
            route: '/users',
            name: ' Users',
            icon: 'fa-users',
        },
        {
            route: '/external_recettes',
            name: " Recettes de l'API",
            icon: 'fa-blender',
        }
    ]
    return (
        <Fragment>
            <NavBar> 
                <h5> Marmytho </h5>
                <ul>
                    {Children.toArray(links.map(link => (
                        <Li>
                            <Link to={link.route} className={`${activePath === link.route ? 'active' : ''} nav_link`} onClick={() => setActivePath(link.route)} >
                                <i className={`fas ${link.icon}`} /> 
                                {link.name}
                            </Link>
                        </Li>
                    )))}
                </ul>
                <div>
                        <Button 
                            text={'Déconnexion'}
                            backgroundColor={"#B72929"}
                            icon={<i className='fas fa-door-open'/>}
                            onClick={logout}
                        />
                </div>
            </NavBar>
            {children}
        </Fragment>
    )
}

export default Layout