import React from 'react'
import * as Native from 'react-native'
import { AntDesign, SimpleLineIcons, Feather } from '@expo/vector-icons'
import Svg, { Path } from 'react-native-svg'
import {
  createBox,
  createText,
  createRestyleComponent,
  createVariant,
  VariantProps,
} from '@shopify/restyle'

import { Theme } from 'Shared/theme'

export * from 'react-native'

const card = createVariant({ themeKey: 'cardVariants' })
const shadow = createVariant({
  property: 'shadow',
  themeKey: 'shadowVariants',
})

export const Text = createText<Theme>()
export const Box = createBox<Theme>()
export const Card = createRestyleComponent<
  VariantProps<Theme, 'cardVariants'> &
    VariantProps<Theme, 'shadowVariants', 'shadow'> &
    React.ComponentProps<typeof Box>,
  Theme
>([card, shadow], Box)

export const CheckBox = ({
  state = false,
  setState,
  color = undefined,
  size = 20,
  children,
  ...props
}) => (
  <Native.TouchableOpacity onPress={() => setState(!state)}>
    <Box flexWrap="wrap" flexDirection="row" {...props}>
      <Box
        bg="white"
        width={size}
        height={size}
        borderWidth={1}
        borderColor="black"
        alignItems="center"
        justifyContent="center"
      >
        <Feather name={state ? 'check' : undefined} size={size} color={color} />
      </Box>
      {children}
    </Box>
  </Native.TouchableOpacity>
)

export const Divider = (props: any) => {
  if (props.vertical)
    return <Box bg="light-gray" height="100%" width={2} {...props} />
  else return <Box bg="light-gray" height={2} width="100%" {...props} />
}

export const CallToAction = ({ onPress = () => {}, children, ...props }) => (
  <Native.TouchableOpacity onPress={onPress} style={{ alignItems: 'center' }}>
    <Box
      bg="pri-light"
      alignItems="center"
      justifyContent="center"
      p="md"
      {...props}
    >
      <Text
        variant="subheader"
        textTransform="uppercase"
        color="white"
        fontSize={16}
        children={children}
      />
    </Box>
  </Native.TouchableOpacity>
)

export const AspectView = ({
  aspectRatio = 1,
  size = 100,
  dimension = 'width',
  style = {},
  ...props
}) => {
  const otherDimension = dimension === 'width' ? 'height' : 'width'
  const [otherDimensionSize, setOtherDimensionSize] = React.useState<number>()

  return (
    <Box
      {...props}
      style={{
        ...style,
        [dimension]: size + '%',
        [otherDimension]: otherDimensionSize,
      }}
      aspectRatio={aspectRatio}
      onLayout={({ nativeEvent: { layout } }) =>
        setOtherDimensionSize(layout[dimension] * aspectRatio)
      }
    />
  )
}

export const Icons = {
  Facebook: ({ icon }) => (
    <SimpleLineIcons size={20} {...icon} name="social-facebook" />
  ),
  Instagram: ({ icon }) => (
    <SimpleLineIcons size={20} {...icon} name="social-instagram" />
  ),
  Twitter: ({ icon }) => (
    <SimpleLineIcons size={20} {...icon} name="social-twitter" />
  ),
  TikTok: ({ size, color = '#000', ...props }) => (
    <Svg viewBox="0 0 48 48" width={size} height={size} {...props}>
      <Path
        fill={color}
        d="M38.4,21.68V16c-2.66,0-4.69-.71-6-2.09a8.9,8.9,0,0,1-2.13-5.64V7.86L24.9,7.73s0,.23,0,.54V30.8a5,5,0,1,1-3.24-5.61v-5.5a10.64,10.64,0,0,0-1.7-.14A10.36,10.36,0,1,0,30.32,29.91a10.56,10.56,0,0,0-.08-1.27V19.49A14.48,14.48,0,0,0,38.4,21.68Z"
      />
    </Svg>
  ),
}

export const ScrollUp = ({ scrollRef, size: size_ = undefined, ...props }) => {
  const size: number =
    size_ ??
    Native.Platform.select({
      native: 48,
      default: 64,
    })

  return (
    <Native.TouchableOpacity
      onPress={() => scrollRef.current?.scrollTo({ y: 0, animated: true })}
    >
      <Box
        right={0}
        bottom={0}
        position="absolute"
        borderRadius={999}
        borderWidth={1}
        bg="white"
        alignItems="center"
        justifyContent="center"
        borderColor="black"
        width={size}
        height={size}
        {...Native.Platform.select({
          native: {
            mr: 'sm',
            mb: 'sm',
          },
          default: {
            mr: 'lg',
            mb: 'sm',
          },
        })}
        {...props}
      >
        <AntDesign size={size / 2} name="up" />
      </Box>
    </Native.TouchableOpacity>
  )
}

/* TODO: Can we do lodaing animation based on progress of graphql query? */
export const GraphQLLoading = () => {
  const rotation = React.useRef(new Native.Animated.Value(1)).current

  const animation = Native.Animated.sequence([
    Native.Animated.timing(rotation, {
      useNativeDriver: true,
      toValue: 0,
      duration: 0,
      delay: 500,
    }),
    Native.Animated.timing(rotation, {
      useNativeDriver: true,
      toValue: 360,
      duration: 1000,
    }),
  ])
  const loop = Native.Animated.loop(animation)

  React.useEffect(() => {
    loop.start()
    return () => loop.stop()
  }, [])

  return (
    <Box flex={1} height="100%" justifyContent="center" alignItems="center">
      <Box
        position="absolute"
        top={0}
        right={0}
        left={0}
        bottom={0}
        bg="gray1"
        opacity={0.5}
      />
      <Box height={128} width={128}>
        <Native.Animated.Image
          source={require('assets/brand/Transparent-Hanger.png')}
          style={{
            height: '100%',
            width: '100%',
            zIndex: 10,
            transform: [
              {
                rotate: rotation.interpolate({
                  inputRange: [0, 360],
                  outputRange: ['0deg', '360deg'],
                }),
              },
            ],
          }}
        />
      </Box>
    </Box>
  )
}

export const GraphQLError = () => <Box></Box>

export const Loading = () => <Box />

export const SplashScreen = () => <></>
