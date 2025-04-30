import React from 'react'
import { View } from 'react-native'

interface Props {
  style?: Object
  children?: any
}

function isFunction(functionToCheck: Function) {
  return (
    functionToCheck && {}.toString.call(functionToCheck) === '[object Function]'
  )
}

const Hoverable = (props: Props) => {
  const childrenWithHoverState = isFunction(props.children)
    ? props.children(false)
    : props.children
  return <View style={props.style}>{childrenWithHoverState}</View>
}

export default Hoverable
