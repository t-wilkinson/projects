import React from 'react'
import { Ionicons } from '@expo/vector-icons'

import {
  CallToAction,
  Image,
  Box,
  Text,
  TouchableOpacity,
  TextInput,
  Platform,
} from 'shared/components'
import { Default, Mobile } from 'shared/Breakpoints'
import { Link } from 'shared/Link'
import { Wrapper } from 'shared/Wrapper'
import NavBar from 'shared/NavBar'

export const MainHeader = ({ navigation }) => {
  const [searchEnabled, setSearchEnabled] = React.useState(false)
  const [searchFocused, setSearchFocused] = React.useState(false)
  const [query, setQuery] = React.useState('')
  const refs: any = {}

  React.useEffect(() => {
    searchEnabled && refs.search.focus()
  }, [searchEnabled])

  return (
    <>
      <Mobile>
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
                <TouchableOpacity onPress={() => navigation.toggleDrawer()}>
                  <Ionicons name="menu-outline" size={32} />
                </TouchableOpacity>
                <Link to="/landing-page">
                  <Text ml="md" variant="header" fontSize={20}>
                    INFINITE CLOSET
                  </Text>
                </Link>
              </Box>
              <Box flexDirection="row" alignItems="center">
                <Box bg={searchEnabled ? 'sec-light' : undefined} p="2xs">
                  <TouchableOpacity
                    onPress={() => setSearchEnabled(!searchEnabled)}
                  >
                    <Text color={searchEnabled ? 'white' : undefined}>
                      <Ionicons name="search-outline" size={24} />
                    </Text>
                  </TouchableOpacity>
                </Box>
                {/* <Box width={{ base: 8, tablet: 16 }} /> */}
                {/* <Ionicons name="heart-outline" size={24} /> */}
                {/* <Box width={{ base: 8, tablet: 16 }} /> */}
                {/* <SimpleLineIcons name="bag" size={20} /> */}
              </Box>
            </Box>
          </Box>

          <SearchBar
            query={query}
            searchEnabled={searchEnabled}
            searchFocused={searchFocused}
            refs={refs}
            setQuery={setQuery}
            setSearchFocused={setSearchFocused}
            setSearchEnabled={setSearchEnabled}
          />
        </Wrapper>
      </Mobile>

      <Default>
        <Box
          borderBottomWidth={2}
          borderBottomColor="light-gray"
          mt="md"
          mb="lg"
          zIndex={10}
        >
          <Box alignItems="center" justifyContent="center">
            <Box position="absolute" left={0}>
              <SearchBar
                query={query}
                searchEnabled={searchEnabled}
                searchFocused={searchFocused}
                refs={refs}
                setQuery={setQuery}
                setSearchFocused={setSearchFocused}
                setSearchEnabled={setSearchEnabled}
              />
            </Box>

            <Link to="/landing-page">
              <Text variant="header" fontSize={{ base: 40, tablet: 48 }}>
                INFINITE CLOSET
              </Text>
            </Link>

            {/* <Box position="absolute" right={0} flexDirection="row"> */}
            {/*   <Box width={{ base: 8, tablet: 16 }} /> */}
            {/*   <AntDesign name="user" size={24} /> */}
            {/*   <Box width={{ base: 8, tablet: 16 }} /> */}
            {/*   <Ionicons name="heart-outline" size={24} /> */}
            {/*   <Box width={{ base: 8, tablet: 16 }} /> */}
            {/*   <SimpleLineIcons name="bag" size={20} /> */}
            {/* </Box> */}
          </Box>

          <Box flexDirection="row" alignItems="center" justifyContent="center">
            <NavBar />
            <Box position="absolute" right={0} mr="md" zIndex={10} pb="md">
              <CallToAction
                onPress={() => {}}
                fontSize={{ tablet: 14, laptop: 18 }}
              >
                Subscribe
              </CallToAction>
            </Box>
          </Box>
        </Box>
      </Default>
    </>
  )
}
export default MainHeader

const SearchBar = ({
  query,
  refs,
  searchEnabled,
  searchFocused,
  setQuery,
  setSearchEnabled,
  setSearchFocused,
}) => {
  return (
    <>
      <Mobile>
        <Box
          visible={searchEnabled}
          ref={(ref) => (refs.searchContainer = ref)}
          borderBottomColor={searchFocused ? 'sec-light' : 'light-gray'}
          borderBottomWidth={2}
          flexDirection="row"
          alignItems="center"
        >
          <TextInput
            placeholder="Search"
            selectTextOnFocus
            returnKeyType="search"
            ref={(ref) => (refs.search = ref)}
            value={query}
            onChangeText={(text) => setQuery(text)}
            style={{ flex: 1, paddingVertical: 4, paddingHorizontal: 4 }}
            onBlur={() => {
              setSearchFocused(false)
              setSearchEnabled(false)
            }}
            onFocus={() => setSearchFocused(true)}
          />
        </Box>
      </Mobile>

      <Default>
        <Box
          ref={(ref) => (refs.searchContainer = ref)}
          flexDirection="row"
          alignItems="center"
          width={{ base: 150, laptop: 250 }}
          zIndex={10}
          borderBottomColor={searchFocused ? 'sec-light' : 'light-gray'}
          borderBottomWidth={2}
          ml="md"
        >
          <Ionicons name="search-outline" size={24} />
          {Platform.OS === 'web' && <style>{`input { outline: none; }`}</style>}
          <TextInput
            selectTextOnFocus
            placeholder="Search"
            returnKeyType="search"
            ref={(ref) => (refs.search = ref)}
            value={query}
            onChangeText={(text) => setQuery(text)}
            style={{ flex: 1, paddingVertical: 4, paddingHorizontal: 4 }}
            onBlur={() => {
              setSearchFocused(false)
              setSearchEnabled(false)
            }}
            onFocus={() => setSearchFocused(true)}
          />
        </Box>
      </Default>
    </>
  )
}

const LandingPageHeaderSmall = ({ navigation }) => (
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

const LandingPageHeaderLarge = () => (
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

export const LandingPageHeader = ({ navigation }) =>
  Platform.select({
    native: <LandingPageHeaderSmall navigation={navigation} />,
    default: (
      <>
        <Mobile>
          <LandingPageHeaderSmall navigation={navigation} />
        </Mobile>
        <Default>
          <LandingPageHeaderLarge />
        </Default>
      </>
    ),
  })
