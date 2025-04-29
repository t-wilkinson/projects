import React from 'react'
import { MaterialCommunityIcons, SimpleLineIcons } from '@expo/vector-icons'
import axios from 'axios'

import {
  Icons,
  CallToAction,
  Linking,
  TextInput,
  Box,
  Text,
  Divider,
  TouchableOpacity,
} from './components'
import { extras, socialMediaLinks } from './constants'
import { Default, Mobile } from './Breakpoints'
import Link from './Link'

export const Footer = () => (
  <>
    <Mobile>
      <Box>
        <Divider my="md" />
        <Subscribe />
        <Divider my="md" />
        <FooterLinks />
        <Divider my="md" />
        <FollowUs />
      </Box>
    </Mobile>

    <Default>
      <Divider my="md" />
      <Box
        flexDirection="row"
        justifyContent="space-between"
        alignItems="center"
        pb="sm"
      >
        <Subscribe flex={1} />
        <FooterLinks flex={1} />
        <FollowUs flex={1} />
      </Box>
    </Default>
  </>
)
export default Footer

type Status = 'None' | 'Error' | 'Submitted' | 'Submitting'

export const Subscribe = (props) => {
  const [state, setState] = React.useState<{
    status: Status
    focused: boolean
    value: string
  }>({
    status: 'None',
    focused: false,
    value: '',
  })

  const onSubmit = () => {
    axios
      .post(extras.api + '/accounts/newsletter', {
        email: state.value,
      })
      .then(() => setState({ ...state, status: 'Submitted' }))
      .catch(() => setState({ ...state, status: 'Error' }))
      .then(() => {})
  }

  if (state.status === 'Submitted') {
    return (
      <Box alignItems="center" {...props}>
        <Text variant="subheader" fontSize={28}>
          Newsletter
        </Text>
        <Box
          borderWidth={1}
          borderColor="black"
          alignItems="center"
          justifyContent="center"
          px="sm"
          my="sm"
        >
          <Box
            flexDirection="row"
            alignItems="center"
            justifyContent="center"
            width={{ base: 200, tablet: 200, laptop: 300 }}
            height={50}
            m="md"
            p="sm"
          >
            <Text>Thanks for submitting!</Text>
          </Box>
        </Box>
      </Box>
    )
  } else if (state.status === 'Submitting') {
    return (
      <Box alignItems="center" {...props}>
        <Text variant="subheader" fontSize={28}>
          Newsletter
        </Text>
        <Box
          borderWidth={1}
          borderColor="black"
          alignItems="center"
          justifyContent="center"
          px="sm"
          my="sm"
        >
          <Box
            flexDirection="row"
            alignItems="center"
            justifyContent="center"
            width={{ base: 200, tablet: 200, laptop: 300 }}
            height={50}
            m="md"
            p="sm"
          >
            <Text>Submitting...</Text>
          </Box>
        </Box>
      </Box>
    )
  } else {
    return (
      <Box alignItems="center" {...props}>
        <Text variant="subheader" fontSize={28}>
          Newsletter
        </Text>
        <Box
          borderWidth={1}
          borderColor="black"
          alignItems="center"
          px="sm"
          my="sm"
        >
          <Box
            flexDirection="row"
            alignItems="center"
            width={{ base: 200, tablet: 200, laptop: 300 }}
            mt="md"
            p="sm"
            borderBottomColor={state.focused ? 'sec-light' : 'light-gray'}
            borderBottomWidth={2}
          >
            <MaterialCommunityIcons
              name="email-outline"
              size={24}
              color="black"
            />
            <TextInput
              placeholder="Email"
              value={state.value}
              onChangeText={(text) => setState((s) => ({ ...s, value: text }))}
              style={{ flex: 1, paddingVertical: 4, paddingHorizontal: 4 }}
              onSubmitEditing={() => onSubmit()}
              onBlur={() => setState((s) => ({ ...s, focused: false }))}
              onFocus={() => setState((s) => ({ ...s, focused: true }))}
            />
          </Box>
          <Box my="md">
            <CallToAction onPress={() => onSubmit()}>Submit</CallToAction>
          </Box>
        </Box>
      </Box>
    )
  }
}

const FooterLinks = (props) => (
  <Box alignItems="center" {...props}>
    <FooterLink to="/contact" label="Contact" />
    <FooterLink to="/FAQ" label="FAQ" />
    <FooterLink to="/legal" label="Legal Terms and Conditions" />
    <FooterLink to="/data-protection" label="Data Protection" />
    <FooterLink to="/accessibility" label="Accessibility" />
  </Box>
)

export const FooterLink = ({ to, label }) => (
  <Link to={to} style={{ marginVertical: 4 }}>
    <Box
      borderBottomColor="black"
      borderBottomWidth={1}
      borderStyle="solid"
      my="sm"
    >
      <Text fontSize={20}>{label}</Text>
    </Box>
  </Link>
)

export const FollowUs = (props) => (
  <Box alignItems="center" {...props}>
    <Text variant="subheader" fontSize={28}>
      Follow Us
    </Text>
    <Box
      flexDirection="row"
      width="100%"
      maxWidth={300}
      justifyContent="space-evenly"
      my="md"
    >
      <SocialMediaIcon name="facebook" />
      <SocialMediaIcon name="instagram" />
      <SocialMediaIcon name="twitter" />
      <TouchableOpacity
        onPress={() => Linking.openURL(socialMediaLinks.tiktok)}
      >
        <Box borderWidth={1} borderRadius={999} p="md">
          <Icons.TikTok size={24} />
        </Box>
      </TouchableOpacity>
    </Box>
  </Box>
)

export const SocialMediaIcon = ({
  name,
  ...props
}: {
  name: 'facebook' | 'instagram' | 'twitter'
  props?: object
}) => (
  <TouchableOpacity onPress={() => Linking.openURL(socialMediaLinks[name])}>
    <Box borderWidth={1} borderRadius={999} p="md">
      <SimpleLineIcons
        size={20}
        // @ts-ignore
        name={'social-' + name}
        {...props}
      />
    </Box>
  </TouchableOpacity>
)
