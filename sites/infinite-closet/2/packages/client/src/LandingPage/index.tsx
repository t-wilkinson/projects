import React from 'react'
import { Entypo } from '@expo/vector-icons'
import axios from 'axios'
import { useSafeAreaInsets } from 'react-native-safe-area-context'
import { useTheme } from '@shopify/restyle'

import {
  Icons,
  Divider,
  Linking,
  TouchableOpacity,
  CallToAction,
  Box,
  CheckBox,
  ImageBackground,
  Text,
  TextInput,
  ScrollView,
} from 'Shared/components'
import { extras, socialMediaLinks } from 'Shared/constants'
import { Default } from 'Shared/Breakpoints'
import Wrapper from 'Shared/Wrapper'
import Link from 'Shared/Link'

import Header from './Header'
import Footer from './Footer'

import { AboutUs } from './AboutUs'
import { howDidYouFindUs } from './constants'

export default function ({ navigation }) {
  const insets = useSafeAreaInsets()

  return (
    <ScrollView>
      <Box bg="white" flex={1} style={{ paddingTop: insets.top }}>
        <Banner />
        <Header navigation={navigation} />

        <Box mb="lg">
          <ImageBackground source={require('assets/brand/Facebook-Banner.png')}>
            <Wrapper
              outer={{ style: { backgroundColor: 'rgba(0,0,0,.5)' } }}
              inner={{ alignItems: 'center' }}
            >
              <Box
                style={{ backgroundColor: 'rgba(255, 255, 255, 0.80)' }}
                alignItems="center"
                my={{ tablet: '5xl' }}
                p="md"
                width="100%"
                maxWidth={{ tablet: 500 }}
              >
                <JoinWaitlist />
              </Box>
            </Wrapper>
          </ImageBackground>
        </Box>

        <Divider />
        <AboutUs />
        <Default>
          <Divider />
        </Default>
        <Footer />
      </Box>
    </ScrollView>
  )
}

const WaitlistItem = (props: React.ComponentProps<typeof Box>) => (
  <Box width="100%" my="sm" {...props} />
)

enum Status {
  None,
  ServerError,
  Error,
  Submitted,
  Submitting,
}

const JoinWaitlist = () => {
  const [state] = React.useState({
    status: { value: Status.None },
    layout: { value: null },
  })

  const [layout, setLayout] = React.useState({})

  if (state.status.value === Status.Submitting) {
    return (
      <Box
        alignItems="center"
        justifyContent="center"
        width={layout.width}
        height={layout.height}
      >
        <Text variant="subheader">Submitting</Text>
      </Box>
    )
  } else if (state.status.value === Status.Submitted) {
    return (
      <Box
        alignItems="center"
        justifyContent="center"
        width={layout.width}
        height={layout.height}
      >
        <Text>Form was successfully submitted.</Text>
        <Text variant="subheader">Thank You!</Text>
      </Box>
    )
  } else {
    return (
      <Box
        width="100%"
        onLayout={({ nativeEvent: { layout } }) => setLayout(layout)}
      >
        <WaitlistForm />
      </Box>
    )
  }
}

const WaitlistForm = () => {
  const [state, setState] = React.useState({
    subscribe: { value: false },
    checkbox: { value: null },
    email: { placeholder: 'Email', value: '' },
    status: { value: Status.None },
    name: { placeholder: 'Name', value: '' },
    other: { placeholder: 'Leave a comment', value: '' },
  })

  const getProps = (field: string) => ({
    state: state[field],
    setState: (value: any) =>
      setState((s) => {
        const newState = { ...s }
        newState[field].value = value
        return newState
      }),
  })

  const subscribe = getProps('subscribe')
  const checkbox = getProps('checkbox')
  const status = getProps('status')

  const onSubmit = () => {
    status.setState(Status.Submitting)

    const marketing = howDidYouFindUs.find(
      (v) => v.value == state.checkbox.value,
    )?.label
    const body = {
      name: state.name.value.trim(),
      email: state.email.value.trim(),
      comment: state.other.value.trim(),
    }

    if (body.name.length === 0 || body.email.length === 0) {
      status.setState(Status.Error)
      return
    }

    axios
      .post(extras.api + '/accounts/waitlist', {
        ...body,
        subscribe: state.subscribe.value,
        marketing: marketing,
      })
      .then(() => status.setState(Status.Submitted))
      .catch(() => status.setState(Status.ServerError))
      .then(() => {})
  }

  return (
    <Box>
      <Box my="md" width="100%" alignItems="center" zIndex={10}>
        <Text variant="subheader" color="pri-light">
          Join The Waitlist
        </Text>
        <WaitlistItem>
          <Text variant="body-bold" fontSize={16}>
            Name
          </Text>
          <Input
            textContentType="name"
            autoCompleteType="name"
            autoCapitalize="words"
            {...getProps('name')}
          />
        </WaitlistItem>

        <WaitlistItem>
          <Text variant="body-bold" fontSize={16}>
            Email
          </Text>
          <Input
            autoCompleteType="email"
            textContentType="emailAddress"
            autoCapitalize="none"
            {...getProps('email')}
          />
        </WaitlistItem>

        <WaitlistItem>
          <Text variant="body-bold" fontSize={16} my="sm">
            How did you first learn about our website?
          </Text>
          <CheckBoxes {...checkbox} />
          <Input width="100%" {...getProps('other')} />
        </WaitlistItem>

        <Box flexDirection={{ tablet: 'row' }} alignItems="center" my="sm">
          <CheckBox state={subscribe.state.value} setState={subscribe.setState}>
            <Text>&nbsp;&nbsp;I want to subscribe to the newsletter. </Text>
          </CheckBox>
          <Link to="/privacy-policy">
            <Text textDecorationLine="underline">View terms.</Text>
          </Link>
        </Box>

        <Box my="sm">
          <CallToAction onPress={onSubmit}>
            <Text>Join</Text>
          </CallToAction>
        </Box>
        {state.status.value === Status.ServerError && (
          <Box alignItems="center">
            <Text color="warning">
              There was a problem communicating with the server.
            </Text>
          </Box>
        )}
        {state.status.value === Status.Error && (
          <Box alignItems="center">
            <Text color="warning">Please fill out all fields.</Text>
          </Box>
        )}
      </Box>

      <Box
        flexDirection="row"
        justifyContent="space-between"
        width="100%"
        alignItems="center"
      >
        <Box>
          <Text color="dark-gray">London, UK</Text>
          <Text color="dark-gray">info@infiniteclosetuk.com</Text>
        </Box>

        <Box flexDirection="row" alignItems="center">
          <Box flexDirection={{ tablet: 'row' }} alignItems="center">
            <SocialMediaIcon name="facebook" />
            <Box mt={{ base: 'sm', tablet: '0' }}>
              <SocialMediaIcon name="instagram" />
            </Box>
          </Box>
          <Box flexDirection={{ tablet: 'row' }} alignItems="center">
            <SocialMediaIcon name="twitter" />
            <Box mt={{ base: 'sm', tablet: '0' }}>
              <TouchableOpacity
                onPress={() => Linking.openURL(socialMediaLinks.tiktok)}
              >
                <Box
                  width={28}
                  height={28}
                  alignItems="center"
                  justifyContent="center"
                >
                  <Icons.TikTok size={28} />
                </Box>
              </TouchableOpacity>
            </Box>
          </Box>
        </Box>
      </Box>
    </Box>
  )
}

const CheckBoxes = ({ state, setState }) => (
  <>
    {howDidYouFindUs.map((v) => (
      <CheckBox
        key={v.value}
        state={state.value === v.value}
        setState={(s: null | string) => s && setState(v.value)}
        my="xs"
      >
        <Text>&nbsp;&nbsp;{v.label}</Text>
      </CheckBox>
    ))}
  </>
)

const SocialMediaIcon = ({ name }) => (
  <TouchableOpacity onPress={() => Linking.openURL(socialMediaLinks[name])}>
    <Box mx="sm">
      <Entypo size={24} name={name} />
    </Box>
  </TouchableOpacity>
)

const Input = ({ state, setState, style = {}, ...props }) => {
  const theme = useTheme()
  const [focused, setFocused] = React.useState(false)

  return (
    <Box
      borderBottomColor={focused ? 'sec-light' : 'gray5'}
      borderBottomWidth={2}
      flexDirection="row"
      alignItems="center"
      width="100%"
      my="sm"
      style={style}
    >
      <TextInput
        {...props}
        returnKeyType="next"
        placeholderTextColor={theme.gray5}
        placeholder={state.placeholder}
        value={state.value}
        onChangeText={(text) => setState(text)}
        style={{
          fontFamily: theme.textVariants.body.fontFamily,
          flex: 1,
          paddingVertical: 4,
          paddingHorizontal: 4,
        }}
        onBlur={() => setFocused(false)}
        onFocus={() => setFocused(true)}
      />
    </Box>
  )
}

const Banner = () => (
  <Box width="100%" alignItems="center" bg="sec" py="lg" px="sm">
    <Text variant="header" fontSize={28}>
      COMING SPRING 2021
    </Text>
    <Box height={8} />
    <Text
      variant="subheader"
      textAlign="center"
      fontSize={20}
      textTransform="none"
    >
      Discover and rent the latest trends from fashions rising{' '}
    </Text>
  </Box>
)
