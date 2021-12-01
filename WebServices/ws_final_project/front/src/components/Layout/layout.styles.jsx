import styled from 'styled-components'

export const NavBar = styled.nav`
    height: 75px;
    display: flex;
    flex-direction: row;
    justify-content: space-between;
    align-items: center;

    padding: 0.5rem 5rem;

    background-color: #FFFFFF;

    h5 {
        margin: 0;
        font-style: italic;
    }
    ul {
        list-style-type: none;
        margin: 0;
        padding: 0;

        width: 100%;
        display: flex;
        flex-direction: row;
        justify-content: center;
        align-items: center;

        li {
            margin: 0 3rem;
        }
    }
`

export const Li = styled.li`
    font-size: 1.2rem;
    a {
        color: #000 !important;
        text-decoration: none;
    }
    .active {
        border-bottom: 1px solid #000;
    }
`