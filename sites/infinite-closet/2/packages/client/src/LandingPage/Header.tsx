import React from 'react'
import { Ionicons } from '@expo/vector-icons'

import { Box, Text, TouchableOpacity, Platform, Image } from 'Shared/components'
import { Default, Mobile } from 'Shared/Breakpoints'
import Wrapper from 'Shared/Wrapper'
import Link from 'Shared/Link'
import NavBar from 'Shared/NavBar'

export const Header = ({ navigation }) =>
  Platform.select({
    native: <SmallHeader navigation={navigation} />,
    default: (
      <>
        <Mobile>
          <SmallHeader navigation={navigation} />
        </Mobile>
        <Default>
          <LargeHeader />
        </Default>
      </>
    ),
  })
export default Header

const SmallHeader = ({ navigation }) => (
  <Wrapper
    outer={{
      p: 'md',
      borderBottomWidth: 2,
      borderBottomColor: 'light-gray',
      mb: 'lg',
    }}
  >
    <Box flexDirection="row">
      <Box
        width="100%"
        flexDirection="row"
        justifyContent="space-between"
        alignItems="center"
      >
        <Box flexDirection="row" alignItems="center">
          <Box mr="sm">
            <TouchableOpacity onPress={() => navigation.toggleDrawer()}>
              <Ionicons name="menu-outline" size={32} />
            </TouchableOpacity>
          </Box>
          <Link to="/landing-page">
            <Text ml="md" variant="header" fontSize={20}>
              INFINITE CLOSET
            </Text>
          </Link>
        </Box>
      </Box>
    </Box>
  </Wrapper>
)

const LargeHeader = () => (
  <Box
    borderBottomWidth={2}
    borderBottomColor="light-gray"
    py="md"
    mb="lg"
    zIndex={10}
    pb="0"
  >
    <Box alignItems="center" justifyContent="center" width="100%">
      <Link to="/landing-page">
        <Image
          style={{ height: 200, width: 400, resizeMode: 'cover' }}
          source={require('assets/brand/Logo-Lockup-(Transparent).png')}
        />
      </Link>
    </Box>
    <NavBar />
  </Box>
)
