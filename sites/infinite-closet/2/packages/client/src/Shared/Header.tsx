import React from 'react'
import { Ionicons } from '@expo/vector-icons'

import {
  CallToAction,
  Box,
  Text,
  TouchableOpacity,
  TextInput,
  Platform,
} from 'Shared/components'
import { Default, Mobile } from 'Shared/Breakpoints'
import Link from 'Shared/Link'
import Wrapper from 'Shared/Wrapper'
import NavBar from 'Shared/NavBar'

export const Header = ({ navigation }) => {
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
export default Header

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
