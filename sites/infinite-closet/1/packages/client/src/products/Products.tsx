import React from 'react'
import { useLinkProps } from '@react-navigation/native'
import { SimpleLineIcons, AntDesign, Ionicons } from '@expo/vector-icons'

import { FlatList, Box, Text, Image, TouchableOpacity } from 'shared/components'
import { breakpoints } from 'shared/theme'
import { useSelector, useDispatch } from 'shared/store'
import { extras } from 'shared/constants'
import { useBreakpoints } from 'shared/useBreakpoints'
import Hoverable from 'shared/Hoverable'

import { PAGE_SIZE, FILTERS_ASIDE_WIDTH } from './constants'
import { FiltersCount } from './helpers'
import { shopActions, shopSelectors } from './slice'
import { useDimensions } from './useDimensions'

export const Products = ({ query, navigation, routeName }) => {
  const dimensions = useDimensions()
  const {
    isSmallScreen,
    isLargeScreen,
    isLargerScreen,
    isLargestScreen,
  } = useBreakpoints({
    smallScreen: { maxWidth: breakpoints.tablet },
    largeScreen: {
      minWidth: breakpoints.tablet + 1,
      maxWidth: breakpoints.laptop,
    },
    largerScreen: {
      minWidth: breakpoints.laptop + 1,
      maxWidth: breakpoints.max,
    },
    largestScreen: { minWidth: breakpoints.max + 1 },
  })

  const listWidth =
    isSmallScreen ?? isLargeScreen
      ? dimensions.window.width
      : isLargerScreen
      ? dimensions.window.width - FILTERS_ASIDE_WIDTH
      : breakpoints.max - FILTERS_ASIDE_WIDTH
  const productWidth = isSmallScreen ? listWidth * 0.44 : listWidth * 0.3
  const numColumns = isSmallScreen ? 2 : 3
  const totalPages = Math.ceil(query.data.shopItemsCount / PAGE_SIZE)

  const Product_ = React.useCallback(
    (props) => (
      <Product
        width={productWidth}
        style={{
          // add marginRight iff it is the last item in its row
          marginRight:
            (props.index + 1) % numColumns === 0
              ? 0
              : (listWidth - productWidth * numColumns - 32) / (numColumns - 1),
        }}
        height={productWidth * 1.25}
        {...props}
      />
    ),
    [listWidth, productWidth, numColumns],
  )

  return (
    <Box flex={1}>
      <Box
        flexDirection="row"
        alignItems="center"
        justifyContent="space-between"
        flex={1}
      >
        <Box>
          <Text variant="subheader">
            {routeName} ({query.data.shopItemsCount})
          </Text>
        </Box>
        <PageNavigation totalPages={totalPages} />
      </Box>

      {(isSmallScreen ?? isLargeScreen) && (
        <Box flexDirection="row" justifyContent="flex-end" width="100%" mt="lg">
          <TouchableOpacity onPress={() => navigation.openDrawer()}>
            <Box flexDirection="row" alignItems="center">
              <Ionicons name="options-outline" size={24} />
              <FiltersCount fontSize={12} />
            </Box>
          </TouchableOpacity>
        </Box>
      )}

      <FlatList
        key={numColumns}
        renderItem={Product_}
        data={query.data.shopItems}
        keyExtractor={(item) => item.id}
        numColumns={numColumns}
        columnWrapperStyle={
          totalPages < numColumns
            ? { justifyContent: 'flex-start' }
            : { justifyContent: 'space-between' }
        }
        style={{
          width: isLargerScreen ?? isLargestScreen ? listWidth : '100%',
        }}
        contentContainerStyle={{
          alignItems: 'flex-start',
          width: '100%',
        }}
      />

      <PageNavigation totalPages={totalPages} mt="xl" />
    </Box>
  )
}
export default Products

const Product = ({ width, height, item, ...props }: any) => {
  const [, setHover] = React.useState(false)
  const { onPress, ...linkProps } = useLinkProps({
    to: `/shop/listings/${item.designer.name_uid}/${item.name_uid}`,
  })

  return (
    <Box
      width={width}
      my="sm"
      justifyContent="center"
      alignItems="stretch"
      style={{ cursor: 'pointer' }}
      onPress={onPress}
      {...linkProps}
      {...props}
    >
      <Hoverable
        onHoverIn={() => setHover(true)}
        onHoverOut={() => setHover(false)}
        style={{ flex: 1 }}
      >
        <ProductImage height={height} images={item.images} />
        <ProductInfo item={item} />
      </Hoverable>
    </Box>
  )
}

const ProductImage = ({ height, images }) => (
  <Box mt="md" p="xl" bg="light-gray" height={height}>
    <Box m="xs" p="sm" position="absolute" right={0} top={0}>
      <Ionicons name="heart-outline" size={24} />
    </Box>
    <Image
      source={{ uri: extras.api + images[0].url }}
      resizeMode="cover"
      resizeMethod="scale"
      defaultSource={{ uri: extras.api + images[0].url }}
      style={{ flex: 1 }}
    />
  </Box>
)

const ProductInfo = ({ item }) => (
  <Box flexDirection="row" justifyContent="space-between" mt="md">
    <Box flex={1}>
      <Text fontSize={14} variant="body-bold">
        {item.designer.name}
      </Text>
      <Text fontSize={14}>{item.name}</Text>
      <Box flexDirection="row" mt="md">
        <Text fontSize={14} variant="body-bold">
          £{item.rental_price}
        </Text>
        <Text fontSize={14} color="gray5">
          &nbsp;{'| '} ${item.retail_price} retail
        </Text>
      </Box>
    </Box>
    <Box
      p="md"
      borderRadius={999}
      borderWidth={1}
      borderColor="black"
      alignSelf="flex-end"
    >
      <SimpleLineIcons name="bag" size={16} />
    </Box>
  </Box>
)

const PageNavigation = ({ totalPages, ...props }) => {
  const pageNumber = useSelector((state) => shopSelectors.pageNumber(state))
  const dispatch = useDispatch()

  return (
    <Box
      flexDirection="row"
      justifyContent="flex-end"
      alignItems="center"
      flex={1}
      {...props}
    >
      <TouchableOpacity
        onPress={() => dispatch(shopActions.decreasePageNumber())}
      >
        <Box borderColor="light-gray" borderWidth={1} p="sm">
          <AntDesign size={16} name="left" />
        </Box>
      </TouchableOpacity>
      <Text mx="sm">
        {pageNumber + 1} / {totalPages}
      </Text>
      <TouchableOpacity
        onPress={() => dispatch(shopActions.increasePageNumber(totalPages))}
      >
        <Box borderColor="light-gray" borderWidth={1} p="sm">
          <AntDesign size={16} name="right" />
        </Box>
      </TouchableOpacity>
    </Box>
  )
}
