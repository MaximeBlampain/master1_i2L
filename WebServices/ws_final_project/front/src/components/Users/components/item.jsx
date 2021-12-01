import React from 'react'
import PropTypes from 'prop-types'

// Styled Components
import {
  TableItem,
} from '../users.styles'


const Item = ({
  // Props
  keys,
  user,
  index,
  // Functions
  onEditItem,
  onDeleteItem,
}) => (
  <TableItem isClearbackground={index % 2}>
    {
      React.Children.toArray(keys.map(item => {
        // If option, display button, instead render value
        if(item === 'options')
          return (
            <div className="item-options">
                <i className='fas fa-pencil-alt' onClick={() => onEditItem(user.userID)} />
                <i id='deleteItem' className='fas fa-trash-alt' onClick={() => onDeleteItem(user.userID)}/>
            </div>  
          )
        else 
          return (
            <div className="item-col"> {user[item]}</div>
          )
      }))
    }
  </TableItem>
)

Item.propTypes = {
  // Props
  keys: PropTypes.arrayOf(PropTypes.string),
  user: PropTypes.object,
  index: PropTypes.number,
  // Functions
  onEditItem: PropTypes.func,
  onDeleteItem: PropTypes.func,
}

Item.defaultProps = {
  // Props
  keys: [''],
  user: {},
  index: 0,
  // Functions
  onEditItem: () => {},
  onDeleteItem: () => {},
}
export default Item