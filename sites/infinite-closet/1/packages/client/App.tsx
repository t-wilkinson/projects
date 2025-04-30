import { ApolloClient, ApolloProvider, InMemoryCache } from '@apollo/client'
import {
  Barlow_200ExtraLight,
  Barlow_300Light,
} from '@expo-google-fonts/barlow'
import { Cinzel_400Regular } from '@expo-google-fonts/cinzel'
import { Lato_400Regular, Lato_700Bold } from '@expo-google-fonts/lato'
import { AntDesign } from '@expo/vector-icons'
import {
  createDrawerNavigator,
  DrawerContentScrollView,
} from '@react-navigation/drawer'
import { NavigationContainer } from '@react-navigation/native'
import { ThemeProvider } from '@shopify/restyle'
import AppLoading from 'expo-app-loading'
import { Asset } from 'expo-asset'
import { useFonts } from 'expo-font'
import React from 'react'
import { SafeAreaProvider } from 'react-native-safe-area-context'
import { Provider } from 'react-redux'

import {
  SplashScreen,
  Box,
  Divider,
  Image,
  Linking,
  Platform,
  Text,
  TouchableOpacity,
} from 'shared/components'
import { extras, routes } from 'shared/constants'
import store from 'shared/store'
import { theme } from 'shared/theme'
import { Wrapper } from 'shared/Wrapper'

const client = new ApolloClient({
  uri: extras.graphql,
  cache: new InMemoryCache(),
})

const RootDrawer = createDrawerNavigator()

const pages = {
  Home: React.lazy(() => import(/* webpackPrefetch: true */ './src/home')),
  ComingSoon: React.lazy(() => import('./src/coming-soon')),
  LandingPage: React.lazy(() => import('./src/landing-page')),
  Listing: React.lazy(() =>
    import(/* webpackPrefetch: true */ './src/product'),
  ),
  PrivacyPolicy: React.lazy(() => import('./src/privacy-policy')),
  Shop: React.lazy(() => import(/* webpackPrefetch: true */ './src/products')),
}

export const App = () => {
  const linking = {
    prefixes: extras.prefixes,
    config: {
      screens: {
        Home: '',
        LandingPage: 'landing-page',
        ComingSoon: 'coming-soon',
        PrivacyPolicy: 'privacy-policy',
        ItemPage: 'shop/listings/:designer_uid/:name_uid',
        Shop: {
          path: 'shop/:product',
          screens: {
            Main: '',
          },
        },
      },
    },
  }

  const [fontsLoaded] = useFonts({
    Barlow_200ExtraLight,
    Barlow_300Light,
    Cinzel_400Regular,
    Lato_400Regular,
    Lato_700Bold,
  })

  const [assetsLoaded, setAssetsLoaded] = React.useState(false)
  const cacheImages = (images) => {
    return images.map((image) => {
      if (typeof image === 'string') return Image.prefetch(image)
      else return Asset.fromModule(image).downloadAsync()
    })
  }

  const _loadAssetsAsync = async () => {
    const imageAssets = cacheImages([
      require('assets/brand/Logo-Lockup---Gray.jpg'),
      require('assets/brand/Logo-Lockup-(Transparent).png'),
    ])
    await Promise.all([...imageAssets])
  }

  if ((!fontsLoaded || !assetsLoaded) && Platform.OS !== 'web') {
    return (
      <AppLoading
        startAsync={_loadAssetsAsync}
        onFinish={() => setAssetsLoaded(false)}
        onError={console.warn}
      />
    )
  } else {
    return (
      <SafeAreaProvider>
        <ThemeProvider theme={theme}>
          <React.Suspense fallback={<SplashScreen />}>
            {Platform.OS === 'web' && (
              <style>{`input { outline: none; }`}</style>
            )}
            <Provider store={store}>
              <ApolloProvider client={client}>
                <NavigationContainer
                  linking={linking}
                  fallback={<SplashScreen />}
                  theme={{
                    dark: false,
                    colors: {
                      primary: theme.colors['sec-light'],
                      background: theme.colors.white,
                      card: theme.colors.white,
                      text: theme.colors.black,
                      border: theme.colors['light-gray'],
                      notification: theme.colors['light-gray'],
                    },
                  }}
                >
                  <RootDrawer.Navigator
                    initialRouteName="LandingPage"
                    screenOptions={{ headerShown: false }}
                    drawerContent={(props) => <RootDrawerContent {...props} />}
                  >
                    <RootDrawer.Screen
                      options={{ title: 'Infinite Closet' }}
                      name="Home"
                      component={pages.Home}
                    />
                    <RootDrawer.Screen
                      options={{ title: 'Welcome!' }}
                      name="LandingPage"
                      component={pages.LandingPage}
                    />
                    <RootDrawer.Screen
                      options={{ title: 'Coming Soon' }}
                      name="ComingSoon"
                      component={pages.ComingSoon}
                    />
                    <RootDrawer.Screen
                      options={{ title: 'Privacy Policy' }}
                      name="PrivacyPolicy"
                      component={pages.PrivacyPolicy}
                    />
                    <RootDrawer.Screen
                      options={{ title: 'Shop' }}
                      name="Shop"
                      component={pages.Shop}
                    />
                    <RootDrawer.Screen
                      name="ItemPage"
                      component={pages.Listing}
                    />
                  </RootDrawer.Navigator>
                </NavigationContainer>
              </ApolloProvider>
            </Provider>
          </React.Suspense>
        </ThemeProvider>
      </SafeAreaProvider>
    )
  }
}
export default App

const DrawerHeader = ({ navigation }) => {
  /* const [searchFocused, setSearchFocused] = React.useState(false) */
  /* const [query, setQuery] = React.useState('') */
  /* const refs = {} */

  return (
    <Wrapper
      outer={{ p: 'md', borderBottomWidth: 1, borderBottomColor: 'light-gray' }}
    >
      <Box width="100%" flexDirection="row" alignItems="center">
        <Box mr="sm">
          <TouchableOpacity onPress={() => navigation.toggleDrawer()}>
            <AntDesign name="close" size={24} />
          </TouchableOpacity>
        </Box>

        <TouchableOpacity onPressOut={() => Linking.openURL('/')}>
          <Text variant="header" fontSize={18}>
            Infinite Closet
          </Text>
        </TouchableOpacity>

        {/* <Box flexDirection="row" alignItems="center"> */}
        {/*   <Box width={4} /> */}
        {/*   <Ionicons name="heart-outline" size={24} /> */}
        {/*   <Box width={4} /> */}
        {/*   <SimpleLineIcons name="bag" size={20} /> */}
        {/* </Box> */}
      </Box>

      {/* <Box */}
      {/*   ref={(ref) => (refs.searchContainer = ref)} */}
      {/*   borderBottomColor={searchFocused ? 'sec-light' : 'light-gray'} */}
      {/*   borderBottomWidth={2} */}
      {/*   flexDirection="row" */}
      {/*   alignItems="center" */}
      {/* > */}
      {/*   <Box p="xs"> */}
      {/*     <Ionicons name="search-outline" size={20} /> */}
      {/*   </Box> */}
      {/*   <TextInput */}
      {/*     selectTextOnFocus */}
      {/*     placeholder="Search" */}
      {/*     returnKeyType="search" */}
      {/*     ref={(ref) => (refs.search = ref)} */}
      {/*     value={query} */}
      {/*     onChangeText={(text) => setQuery(text)} */}
      {/*     style={{ flex: 1, paddingVertical: 4, paddingHorizontal: 4 }} */}
      {/*     onBlur={() => setSearchFocused(false)} */}
      {/*     onFocus={() => setSearchFocused(true)} */}
      {/*   /> */}
      {/* </Box> */}
    </Wrapper>
  )
}

const RenderItem = ({ navigation, selectedSection, item, section, index }) => {
  return (
    <Box
      visible={selectedSection === section.value}
      bg="light-gray"
      pb={index + 1 === section.data[0].data.length ? 'sm' : undefined}
    >
      <TouchableOpacity onPress={() => navigation.navigate(item.to, {})}>
        {/* <TouchableOpacity onPress={() => {}}> */}
        <Text px="md" py="sm">
          {item.label}
        </Text>
      </TouchableOpacity>
    </Box>
  )
}

const RenderSectionHeader = ({
  setSelectedSection,
  selectedSection,
  section,
}) => {
  return (
    <TouchableOpacity
      onPress={() =>
        setSelectedSection(
          selectedSection === section.value ? null : section.value,
        )
      }
    >
      <Box
        flexDirection="row"
        justifyContent="space-between"
        p="md"
        alignItems="center"
      >
        <Text
          textTransform="uppercase"
          variant={selectedSection === section.value ? 'body-bold' : 'body'}
        >
          {section.label}
        </Text>
        <AntDesign
          size={12}
          name={selectedSection === section.value ? 'down' : 'up'}
        />
      </Box>
    </TouchableOpacity>
  )
}

const RootDrawerContent = ({ navigation, ...props }) => {
  const [selectedSection, setSelectedSection] = React.useState(null)

  return (
    <DrawerContentScrollView {...props}>
      <DrawerHeader navigation={navigation} />
      {routes.map((section, i) => (
        <Box key={section.value + '' + i}>
          <RenderSectionHeader
            key={section.value + '' + i}
            setSelectedSection={setSelectedSection}
            selectedSection={selectedSection}
            section={section}
          />
          {section.data[0].data.map((v, i) => (
            <RenderItem
              navigation={navigation}
              key={v.label + '' + i}
              selectedSection={selectedSection}
              section={section}
              item={v}
              index={i}
            />
          ))}
        </Box>
      ))}

      <Divider my="sm" />
      {/* <DrawerLink navigation={navigation} label='My Account' to='ComingSoon' /> */}
      <DrawerLink navigation={navigation} label="About Us" to="ComingSoon" />
      <DrawerLink navigation={navigation} label="Help" to="ComingSoon" />
      <Divider my="sm" />
      <DrawerLink navigation={navigation} label="Sign In" to="ComingSoon" />
      <DrawerLink navigation={navigation} label="Register" to="ComingSoon" />
    </DrawerContentScrollView>
  )
}

const DrawerLink = ({ navigation, label, to }) => (
  <TouchableOpacity onPress={() => navigation.navigate(to)}>
    <Text p="md" textTransform="uppercase">
      {label}
    </Text>
  </TouchableOpacity>
)
