import React from 'react'
import {
  createDrawerNavigator,
  DrawerContentScrollView,
} from '@react-navigation/drawer'
import { useMediaQuery } from 'react-responsive'

import {
  CallToAction,
  ScrollUp,
  GraphQLLoading,
  GraphQLError,
  Dimensions,
  ScrollView,
  Text,
  Box,
} from 'shared/components'
import { MainHeader as Header } from 'shared/Header'
import { MainFooter as Footer } from 'shared/Footer'
import { Wrapper } from 'shared/Wrapper'
import { breakpoints } from 'shared/theme'
import { useSelector, useDispatch } from 'shared/store'

import { shopActions } from './slice'
import { FILTERS_ASIDE_WIDTH } from './constants'
import useQueryFilter from './useQueryFilter'
import ShopFilters from './ShopFilters'
import Products from './Products'

const ShopDrawer = createDrawerNavigator()

export default ({ navigation, route }) => {
  const shop = useSelector((state) => state.shop)
  const routeName = route.params.product
  const query = useQueryFilter(shop.filters, shop.pageNumber)

  const Shop_ = React.useCallback(
    (props) => (
      <Shop
        query={query}
        rootNavigation={navigation}
        routeName={routeName}
        {...props}
      />
    ),
    [query],
  )
  const DrawerContent_ = React.useCallback(
    (props) => <DrawerContent query={query} routeName={routeName} {...props} />,
    [query],
  )

  return (
    <ShopDrawer.Navigator
      drawerContent={DrawerContent_}
      initialRouteName="Main"
    >
      <ShopDrawer.Screen
        name="Main"
        options={{ title: 'Clothing' }}
        component={Shop_}
      />
    </ShopDrawer.Navigator>
  )
}

const DrawerContent = ({ routeName, query }) => {
  return (
    <DrawerContentScrollView>
      <ShopFilters routeName={routeName} query={query} />
    </DrawerContentScrollView>
  )
}

const Shop = ({ routeName, rootNavigation, navigation, query }) => {
  const isLargeScreen = useMediaQuery({ minWidth: breakpoints.laptop + 1 })

  return (
    <ShopWrapper rootNavigation={rootNavigation}>
      {isLargeScreen && (
        <Box
          height="100%"
          width={FILTERS_ASIDE_WIDTH}
          pr="lg"
          justifyContent="flex-start"
        >
          <ShopFilters routeName={routeName} query={query} />
        </Box>
      )}
      {query.loading && !query.data ? (
        <GraphQLLoading />
      ) : query.error ? (
        <GraphQLError />
      ) : (
        <ShopProducts
          routeName={routeName}
          navigation={navigation}
          query={query}
        />
      )}
    </ShopWrapper>
  )
}

const ShopWrapper = ({ rootNavigation, children }) => {
  const scrollRef = React.useRef()

  return (
    <Box flex={1}>
      <ScrollView
        style={{ flexGrow: 1 }}
        // @ts-ignore
        ref={scrollRef}
      >
        <Box bg="white" minHeight={Dimensions.get('window').height}>
          <Box flex={1}>
            <Header navigation={rootNavigation} />
            <Wrapper
              outer={{ flex: 1 }}
              inner={{
                flex: 1,
                px: 'sm',
                flexDirection: 'row',
                alignItems: 'flex-start',
              }}
            >
              {children}
            </Wrapper>
          </Box>
          <Footer />
        </Box>
      </ScrollView>
      <ScrollUp scrollRef={scrollRef} />
    </Box>
  )
}

const ShopProducts = ({ query, navigation, routeName }) => {
  const dispatch = useDispatch()

  if (!query.data) return null
  if (query.data.shopItemsCount === 0) {
    return (
      <Box
        px="sm"
        alignItems="center"
        height={400}
        justifyContent="center"
        flex={1}
      >
        <Text variant="body-bold" fontSize={28} py="xl">
          Sorry, we couldn't find any matches.
        </Text>
        <CallToAction
          onPress={() => {
            dispatch(shopActions.unfocusFilter())
            dispatch(shopActions.resetFilters())
          }}
        >
          Browse All
        </CallToAction>
      </Box>
    )
  }

  return (
    <Products routeName={routeName} query={query} navigation={navigation} />
  )
}
