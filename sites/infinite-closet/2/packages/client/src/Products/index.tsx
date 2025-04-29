import React from 'react'
import { useMediaQuery } from 'react-responsive'
import {
  createDrawerNavigator,
  DrawerContentScrollView,
} from '@react-navigation/drawer'
import { useNavigationState } from '@react-navigation/native'

import {
  CallToAction,
  ScrollUp,
  GraphQLError,
  Dimensions,
  ScrollView,
  Text,
  Box,
} from 'Shared/components'
import { breakpoints } from 'Shared/theme'
import { useSelector, useDispatch } from 'Shared/store'
import Wrapper from 'Shared/Wrapper'
import Header from 'Shared/Header'
import Footer from 'Shared/Footer'

import { shopActions } from './slice'
import { FILTERS_ASIDE_WIDTH } from './constants'
import { resetParams } from './helpers'
import useQueryFilter from './useQueryFilter'
import Filters from './filters'
import ProductItems from './ProductItems'

const ProductsDrawer = createDrawerNavigator()

// TODO the state organization is poor which is causing most issues.
export default ({ navigation: rootNavigation, route }) => {
  // TODO this causes 3 extra rerenders on param change
  // url query params
  const params =
    useNavigationState(
      (state) => state.routes[state.index].state!.routes[0].params,
    ) ?? {}
  const shop = useSelector((state) => state.shop)
  const routeName = route.params.product
  const query = useQueryFilter(shop.pageNumber, params, routeName)

  const ProductsPage_ = React.useCallback(
    (props) => (
      <ProductsPage
        params={params}
        query={query}
        rootNavigation={rootNavigation}
        routeName={routeName}
        {...props}
      />
    ),
    [query],
  )

  const DrawerContent_ = React.useCallback(
    (props) => (
      <DrawerContent
        routeName={routeName}
        query={query}
        params={params}
        {...props}
      />
    ),
    [query],
  )

  return (
    <>
      {DrawerContent_}
      {ProductsPage_}
    </>
  )
}

const DrawerContent = ({ query, routeName, childNavigation }) => {
  return (
    <DrawerContentScrollView>
      <Filters
        routeName={routeName}
        query={query}
        childNavigation={childNavigation}
      />
    </DrawerContentScrollView>
  )
}

const ProductsPage = ({
  rootNavigation,
  navigation,
  query,
  routeName,
  route,
}) => {
  const isLargeScreen = useMediaQuery({ minWidth: breakpoints.laptop + 1 })
  const dispatch = useDispatch()

  return (
    <ProductsWrapper rootNavigation={rootNavigation}>
      {isLargeScreen && (
        <Box
          height="100%"
          width={FILTERS_ASIDE_WIDTH}
          pr="lg"
          justifyContent="flex-start"
        >
          <Filters
            routeName={routeName}
            query={query}
            childNavigation={navigation}
          />
        </Box>
      )}
      {query.error ? (
        <GraphQLError />
      ) : query.data?.shopItemsCount === 0 ? (
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
              resetParams(routeName, navigation)
            }}
          >
            Browse All
          </CallToAction>
        </Box>
      ) : (
        <ProductItems
          routeName={routeName}
          query={query}
          navigation={navigation}
        />
      )}
    </ProductsWrapper>
  )
}

const ProductsWrapper = ({ rootNavigation, children }) => {
  const scrollRef = React.useRef()
  return (
    <Box flex={1}>
      <ScrollView
        style={{ flexGrow: 1 }}
        // @ts-ignore
        ref={scrollRef}
      >
        <Box bg="white" minHeight={Dimensions.get('window').height}>
          <Box flex={1} height="100%">
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
