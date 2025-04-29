import React from 'react'
import { Text } from 'shared/components'
import { Link as Link_ } from '@react-navigation/native'
import Hoverable from 'shared/Hoverable'

export const Link = ({
  to,
  action = undefined,
  hoverable = false,
  ...props
}) => {
  const [state, setState] = React.useState(false)
  const getProps = React.useCallback(() => {
    if (state && hoverable)
      return {
        textDecorationLine: 'underline',
        textDecorationColor: 'black',
        textDecorationStyle: 'solid',
      }
  }, [state])

  return (
    <Hoverable
      onHoverIn={() => setState(true)}
      onHoverOut={() => setState(false)}
    >
      <Link_ to={to} action={action}>
        <Text {...props} {...getProps()} />
      </Link_>
    </Hoverable>
  )
}
export default Link
