import React from 'react'
import { useSafeAreaInsets } from 'react-native-safe-area-context'
import { SimpleLineIcons } from '@expo/vector-icons'

import {
  Linking,
  TouchableOpacity,
  Box,
  Image,
  Text,
  Divider,
  Icons,
  ScrollView,
  Dimensions,
} from 'Shared/components'
import { socialMediaLinks } from 'Shared/constants'
import Link from 'Shared/Link'

export default ({}) => {
  const insets = useSafeAreaInsets()
  const { height } = Dimensions.get('window')

  return (
    <ScrollView
      style={{
        backgroundColor: 'white',
        paddingTop: insets.top,
      }}
    >
      <Box alignItems="center" bg="white" height={height}>
        <Banner />
        <Link to="/landing-page">
          <Box
            width={{ base: 300, tablet: 400 }}
            style={{
              marginBottom: -50 + 16,
              marginTop: 16,
              height: 200,
            }}
          >
            <Image
              style={{
                width: '100%',
                height: '100%',
                resizeMode: 'cover',
              }}
              source={require('assets/brand/Logo-Lockup-(Transparent).png')}
            />
          </Box>
        </Link>

        {/* <Box alignItems="center" my="lg"> */}
        {/*   <Image */}
        {/*     style={{ width: 80, height: 50 }} */}
        {/*     source={require('assets/brand/Transparent-Hanger.png')} */}
        {/*   /> */}
        {/*   <Text variant="header" fontSize={{ base: 40, tablet: 48 }}> */}
        {/*     Infinite Closet */}
        {/*   </Text> */}
        {/*   <Text variant="subheader" fontSize={20}> */}
        {/*     Less is More */}
        {/*   </Text> */}
        {/* </Box> */}

        <Divider />
        {/* <Divider mt="lg" /> */}

        <Box flex={1} justifyContent="center" alignItems="center">
          <Text
            variant="subheader"
            textAlign="center"
            padding="md"
            fontSize={{ base: 48, mobile: 64 }}
            my="xl"
          >
            MORE COMING SOON!
          </Text>
          <Link to="/landing-page">
            <Text textDecorationLine="underline">Go Back</Text>
          </Link>
        </Box>

        <Divider />

        <Box
          flexDirection="row"
          width="100%"
          maxWidth={300}
          justifyContent="space-evenly"
          my="lg"
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
    </ScrollView>
  )
}

type SocialMedia<T extends string> = `social-${T}`
export const SocialMediaIcon = ({
  name,
  ...props
}: {
  name: 'facebook' | 'instagram' | 'twitter'
} & Partial<typeof SimpleLineIcons>) => (
  <TouchableOpacity onPress={() => Linking.openURL(socialMediaLinks[name])}>
    <Box borderWidth={1} borderRadius={999} p="md">
      <SimpleLineIcons
        size={20}
        name={('social-' + name) as SocialMedia<typeof name>}
        {...props}
      />
    </Box>
  </TouchableOpacity>
)

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
