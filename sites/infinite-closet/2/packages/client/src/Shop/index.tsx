import React from 'react'
import { useQuery } from '@apollo/client'
import { useMediaQuery } from 'react-responsive'
import { FontAwesome, Ionicons } from '@expo/vector-icons'

import {
  Box,
  Platform,
  Divider,
  GraphQLError,
  GraphQLLoading,
  ScrollView,
  Text,
} from 'Shared/components'
import Header from 'Shared/Header'
import Footer from 'Shared/Footer'
import Wrapper from 'Shared/Wrapper'

import { items, QUERY } from './constants'
import { State } from './types'
import ProductImages from './ProductImages'
import ProductDetails from './ProductDetails'
import ProductRentHeaders from './ProductRentHeaders'
import ProductRentContents from './ProductRentContents'

export default ({ navigation, route }) => {
  return (
    <ScrollView>
      <Header navigation={navigation} />
      <ProductWrapper navigation={navigation} route={route} />
      <Divider mt="lg" height={0} />
      <Footer />
    </ScrollView>
  )
}

const ProductWrapper = ({ route, navigation }) => {
  const isSmallMobile = useMediaQuery({
    maxWidth: 600,
  })
  const query = useQuery(QUERY, {
    variables: { name_uid: route.params?.name_uid },
  })

  if (query.loading) {
    return <GraphQLLoading />
  } else if (query.error || query.data?.shopItems?.length === 0) {
    return <GraphQLError />
  }

  return (
    <Wrapper outer={{ mx: 'md' }}>
      <Box flexDirection={!isSmallMobile ? 'row' : undefined} width="100%">
        <ProductImages images={query.data.shopItems[0].images} />
        <Box
          flex={1}
          mt={isSmallMobile ? 'lg' : '0'}
          ml={!isSmallMobile ? 'xl' : '0'}
          width="100%"
          minWidth={250}
          maxWidth={!isSmallMobile ? 400 : undefined}
        >
          <Product query={query} navigation={navigation} />
        </Box>
      </Box>
    </Wrapper>
  )
}

const Product = ({ query, navigation }) => {
  const [state, setState] = React.useState<State>({
    rentType: 'OneTime',
    oneTime: 'Short',
    membership: 'Short',
  })

  React.useLayoutEffect(() => {
    if (query.loading || query.error || query.data.shopItems.length === 0)
      return
    addOpenGraphTags(query.data.shopItems[0])

    const { name, designer } = query.data.shopItems[0]
    const site_name = 'Infinite Closet'
    const title = name + ' by ' + designer.name
    navigation.setOptions({
      title: `${title} | ${site_name}`,
    })
  }, [query.loading, navigation, query.data])

  const shopItem = query.data.shopItems[0]

  return (
    <>
      <Box
        flexDirection="row"
        justifyContent="space-between"
        alignItems="center"
      >
        <Rating rating={4.5} />
        <Ionicons name="heart-outline" size={24} />
      </Box>
      <Text pt="md" pb="sm" variant="subheader">
        {shopItem.designer.name}
      </Text>
      <Text pb="sm">{shopItem.name}</Text>
      <Text pb="sm" color="dark-gray">
        Retail £{shopItem.retail_price}
      </Text>
      <Divider mb="md" />
      <ProductRentHeaders
        shopItem={shopItem}
        membership_price={query.data.setting.membership_price}
        setState={setState}
        state={state}
      />
      <ProductRentContents
        shopItem={shopItem}
        setState={setState}
        state={state}
      />
      {items.dropdown.map((item) => (
        <ProductDetails
          key={item.key}
          shopItem={shopItem}
          item={item}
          selected={item.key === state.moreInfo}
          setState={setState}
        />
      ))}
    </>
  )
}

const Rating = ({ rating }: any) => (
  <Box flexDirection="row">
    <FontAwesome name="star" size={18} color="black" />
    <FontAwesome name="star" size={18} color="black" />
    <FontAwesome name="star" size={18} color="black" />
    <FontAwesome name="star-half-empty" size={18} color="black" />
    <FontAwesome name="star-o" size={18} color="black" />
  </Box>
)

const addOpenGraphTags = async ({
  name,
  designer,
  rental_price,
  title,
  site_name,
  images,
  sizes,
  retail_price,
}) => {
  if (Platform.OS !== 'web') return

  const url = window.location.href
  const head = document.querySelector('head')
  head?.appendChild(document.createComment('open graph'))

  const description = `Rent ${name} by ${designer} for only ${rental_price} only at Infinite Closet.`
  const quantity = Object.values(sizes as { quantity: number }[]).reduce(
    (acc, { quantity }) => acc + quantity,
    0,
  )

  // open graph meta information
  const og = [
    { property: 'og:url', content: url },
    { property: 'og:type', content: 'og:product' },
    { property: 'og:title', content: title },
    { property: 'og:description', content: description },
    images[0] && { property: 'og:image', content: images[0] },
    { property: 'og:site_name', content: site_name },
    { property: 'product:price:amount', content: String(retail_price) },
    { property: 'product:price:currency', content: 'GBP' },
    {
      property: 'og:availability',
      content: quantity > 0 ? 'instock' : 'out of stock',
    },
  ]

  // create <meta /> tags from 'og' and apend them to <head />
  og.forEach(({ property, content }) => {
    const meta = document.createElement('meta')
    meta.setAttribute('property', property)
    meta.setAttribute('content', content)
    head?.appendChild(meta)
  })
}
