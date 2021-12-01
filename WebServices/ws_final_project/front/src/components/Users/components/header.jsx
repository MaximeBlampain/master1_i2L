import React from 'react'
import PropTypes from 'prop-types'

// Styled Components
import {
  TableHeader,
} from '../users.styles'

const Header = ({ headerList }) => (
  <TableHeader>
    {
      React.Children.toArray(headerList.map(item => (
        <div className="header-item"> {item} </div>
      )))
    }
  </TableHeader>
)

Header.propTypes = {
  headerList: PropTypes.arrayOf(PropTypes.string),
}

Header.defaultProps = {
  headerList: [''],
}

export default Header