import React from 'react'
import { SimpleLineIcons, Ionicons } from '@expo/vector-icons'
import {
  Card,
  Image,
  ImageBackground,
  Text,
  Divider,
  Box,
  AspectView,
  CallToAction,
} from 'shared/components'
import { Wrapper } from 'shared/Wrapper'
import { Default, Mobile } from 'shared/Breakpoints'
import { MainHeader as Header } from 'shared/Header'
import { MainFooter as Footer } from 'shared/Footer'

export default ({ navigation }) => {
  return (
    <Box bg="white">
      <Header navigation={navigation} />
      <Membership />
      <FilterCards />
      <FeaturedCards />
      <Shop />
      <Break1 />
      <Footer />
    </Box>
  )
}

const Break1 = () => (
  <Box
    justifyContent="center"
    alignItems="center"
    bg="sec"
    height={400}
    my="sm"
  >
    <Text variant="subheader" my="md">
      Amet Quis Unde
    </Text>
    <Text mb="md">Sapiente voluptatem</Text>
    <Box my="md">
      <CallToAction>
        <Text>Doloribus Aliquid </Text>
      </CallToAction>
    </Box>
  </Box>
)

const Membership = () => (
  <Box mb="lg">
    <ImageBackground
      source={{
        uri:
          'https://static01.nyt.com/images/2020/10/01/fashion/30PFW-DIOR-dior-5/20PFW-DIOR-dior-5-mobileMasterAt3x.jpg',
      }}
    >
      <Wrapper
        outer={{ style: { backgroundColor: 'rgba(0,0,0,.5)' } }}
        inner={{ py: '2xl', height: 400 }}
      >
        <Box
          justifyContent="space-around"
          alignItems="flex-start"
          flex={1}
          maxWidth={500}
          px={{ base: 'lg', laptop: 'none' }}
        >
          <Text color="white" variant="subheader">
            Lorem ipsum dolor
          </Text>
          <Text color="white">
            Ipsum necessitatibus minima similique atque eaque, debitis.
            Explicabo facilis eligendi velit fugit nam Alias illum architecto
            voluptatem quis repellat.{' '}
          </Text>
          <Text color="white">
            Repellendus obcaecati fugiat dicta reprehenderit
          </Text>
          <CallToAction>Lorem Ipsum</CallToAction>
        </Box>
      </Wrapper>
    </ImageBackground>
  </Box>
)

const FilterCards = () => {
  const size = 8
  const selected = 1
  const data = [
    {
      filter: 'Clothing',
      item: 'Lorem ipsum dolor sit amet',
      uri:
        'https://cdn.cliqueinc.com/posts/276577/black-and-white-clothes-trend-276577-1548699908418-promo.700x0c.jpg',
    },
    {
      filter: 'Occasions',
      item: 'Lorem ipsum dolor sit amet',
      uri:
        'https://cdn.cliqueinc.com/posts/276577/black-and-white-clothes-trend-276577-1548699908418-promo.700x0c.jpg',
    },
    {
      filter: 'Accessories',
      item: 'Lorem ipsum dolor sit amet',
      uri:
        'https://cdn.cliqueinc.com/posts/276577/black-and-white-clothes-trend-276577-1548699908418-promo.700x0c.jpg',
    },
    {
      filter: 'Trending',
      item: 'Lorem ipsum dolor sit amet',
      uri:
        'https://cdn.cliqueinc.com/posts/276577/black-and-white-clothes-trend-276577-1548699908418-promo.700x0c.jpg',
    },
    {
      filter: 'Sale',
      item: 'Lorem ipsum dolor sit amet',
      uri:
        'https://cdn.cliqueinc.com/posts/276577/black-and-white-clothes-trend-276577-1548699908418-promo.700x0c.jpg',
    },
  ]

  return (
    <Wrapper outer={{ pb: 'lg' }}>
      <Box
        flexDirection="row"
        pb="md"
        overflow="scroll"
        justifyContent="center"
      >
        {data.map((item) => (
          <FilterCard key={item.filter} {...item} />
        ))}
      </Box>
      <Mobile>
        <Box alignItems="center" my="md">
          <Box
            justifyContent="space-between"
            alignItems="center"
            flexDirection="row"
            width={(size + 4) * data.length}
          >
            {Array(data.length)
              .fill(0)
              .map((_, i) => (
                <FilterCircle
                  key={i}
                  size={i === selected ? size + 2 : size}
                  filled={i === selected}
                />
              ))}
          </Box>
        </Box>
      </Mobile>
      <Default>
        <Box
          alignItems="center"
          my="md"
          position="absolute"
          bottom={0}
          right={0}
          style={{ transform: [{ translateY: -140 }] }}
        >
          <Box
            justifyContent="space-between"
            alignItems="center"
            flexDirection="row"
            width={(size + 4) * data.length}
          >
            {Array(data.length)
              .fill(0)
              .map((_, i) => (
                <FilterCircle
                  key={i}
                  size={i === selected ? size + 2 : size}
                  filled={i === selected}
                />
              ))}
          </Box>
        </Box>
      </Default>
    </Wrapper>
  )
}

const FilterCircle = ({ size = 16, filled = false }) => (
  <Box
    borderRadius={999}
    bg={filled ? 'black' : 'gray5'}
    height={size}
    width={size}
  />
)

const FilterCard = ({ uri, filter, item }) => (
  <>
    <Mobile>
      <Box
        shadowColor="black"
        shadowOffset={{ width: 0, height: 7 }}
        shadowOpacity={0.43}
        shadowRadius={9.51}
        elevation={15}
        mx="sm"
        width={300}
        height={500}
      >
        <Image source={uri} style={{ flex: 60 }} />
        <Box flex={40} alignItems="center" px="md" py="md">
          <Text color="gray5">{filter}</Text>
          <Text
            mt="md"
            textTransform="uppercase"
            fontSize={20}
            textAlign="center"
          >
            {item}
          </Text>
          <Box height={8} />
          <Order my="lg" />
        </Box>
      </Box>
    </Mobile>
    <Default>
      <Box width="100%">
        <Image source={uri} style={{ height: 400 }} />
        <Box alignItems="flex-start" py="md" flex={1}>
          <Text color="gray5">{filter}</Text>
          <Text
            mt="sm"
            textTransform="uppercase"
            fontSize={20}
            textAlign="center"
          >
            {item}
          </Text>
          <Order my="lg" />
        </Box>
      </Box>
    </Default>
  </>
)

const FeaturedCards = ({ ...props }) => {
  const size = 8
  const selected = 1
  const data = [
    {
      uri:
        'https://media.gq.com/photos/5ab151dcd668df704470b18f/master/w_2000,h_2285,c_limit/Not-Normal-High-Fashion-Gets-Serious-About-Regular-Clothes-20-Edit.jpg',
    },
    {
      uri:
        'https://media.gq.com/photos/5ab151dcd668df704470b18f/master/w_2000,h_2285,c_limit/Not-Normal-High-Fashion-Gets-Serious-About-Regular-Clothes-20-Edit.jpg',
    },
    {
      uri:
        'https://media.gq.com/photos/5ab151dcd668df704470b18f/master/w_2000,h_2285,c_limit/Not-Normal-High-Fashion-Gets-Serious-About-Regular-Clothes-20-Edit.jpg',
    },
  ]

  return (
    <Wrapper
      inner={{
        bg: 'light-gray',
        px: { tablet: '3xl' },
        height: { base: '100vh', tablet: 800 },
        maxHeight: 900,
      }}
    >
      <Box alignItems="center" mx="lg" my="xl">
        <Text variant="subheader" fontSize={48}>
          Featured
        </Text>
      </Box>
      <Box
        {...props}
        overflow="scroll"
        flexDirection="row"
        justifyContent="center"
      >
        {data.map((item, i) => (
          <FeaturedCard key={i} {...item} />
        ))}
      </Box>
      <Box flex={1} />
      <Box alignItems="center" my="md">
        <Box
          justifyContent="space-between"
          alignItems="center"
          flexDirection="row"
          width={(size + 4) * data.length}
        >
          {Array(data.length)
            .fill(0)
            .map((_, i) => (
              <FilterCircle
                key={i}
                size={i === selected ? size + 2 : size}
                filled={i === selected}
              />
            ))}
        </Box>
      </Box>
    </Wrapper>
  )
}

const FeaturedCard = ({ uri, ...props }) => (
  <Box
    {...props}
    width={256}
    maxWidth="100%"
    maxHeight="100%"
    mx="md"
    position="relative"
  >
    <Box height={256}>
      <Image source={{ uri }} style={{ width: '100%', height: '100%' }} />
      <Box position="absolute" top={0} right={0} mt="sm" mr="sm">
        <Ionicons name="heart-outline" size={24} />
      </Box>
    </Box>
    <Box alignItems="center" my="lg">
      <Text my="sm" fontSize={20} variant="body-bold">
        Adipisicing sit{' '}
      </Text>
      <Text my="sm" fontSize={20} variant="body" textAlign="center">
        Amet cum aspernatur quisquam atque
      </Text>
      <Text my="sm" fontSize={20} variant="price">
        £16.00
      </Text>
      <Order />
    </Box>
  </Box>
)

const Shop = () => (
  <Wrapper inner={{ alignItems: 'center' }} outer={{ mx: 'md' }}>
    <ItemHeader
      title="Lorem Ipsum"
      description="Consectetur dolorem quos doloribus exercitationem ipsa"
    />
    <LargeItem
      label="Adipisicing blanditiis"
      uri="https://coveteur.com/wp-content/uploads/2019/12/PCL_MFWAW18_Day1_Image-18-2010s-fashion-trends-decade-homepage-1280x720.jpg"
      label2="Adipisicing blanditiis"
      uri2="https://miro.medium.com/max/10370/1*FbAVZD_gQJtd2FX21S7c5w.jpeg"
    />
    <Box
      flexDirection={{ base: 'column', tablet: 'row' }}
      width="100%"
      flex={1}
    >
      <SmallItem
        flex={1}
        mr="md"
        left={{
          label: 'Error mollitia',
          uri:
            'https://coveteur.com/wp-content/uploads/2019/12/PCL_MFWAW18_Day1_Image-18-2010s-fashion-trends-decade-homepage-1280x720.jpg',
        }}
        right={{
          label: 'sint dignissimos',
          uri:
            'https://coveteur.com/wp-content/uploads/2019/12/PCL_MFWAW18_Day1_Image-18-2010s-fashion-trends-decade-homepage-1280x720.jpg',
        }}
      />
      <SmallItem
        flex={1}
        left={{
          label: 'Error mollitia',
          uri:
            'https://coveteur.com/wp-content/uploads/2019/12/PCL_MFWAW18_Day1_Image-18-2010s-fashion-trends-decade-homepage-1280x720.jpg',
        }}
        right={{
          label: 'sint dignissimos',
          uri:
            'https://coveteur.com/wp-content/uploads/2019/12/PCL_MFWAW18_Day1_Image-18-2010s-fashion-trends-decade-homepage-1280x720.jpg',
        }}
      />
    </Box>

    <Divider />

    <ItemHeader
      title="Lorem Ipsum"
      description="modi! Odio provident sapiente quae"
    />
    <LargeItem
      label="Adipisicing blanditiis"
      uri="https://coveteur.com/wp-content/uploads/2019/12/PCL_MFWAW18_Day1_Image-18-2010s-fashion-trends-decade-homepage-1280x720.jpg"
      label2="Adipisicing blanditiis"
      uri2="https://miro.medium.com/max/10370/1*FbAVZD_gQJtd2FX21S7c5w.jpeg"
    />
    <Box
      flexDirection={{ base: 'column', tablet: 'row' }}
      width="100%"
      flex={1}
    >
      <SmallItem
        flex={1}
        mr="md"
        left={{
          label: 'Error mollitia',
          uri:
            'https://coveteur.com/wp-content/uploads/2019/12/PCL_MFWAW18_Day1_Image-18-2010s-fashion-trends-decade-homepage-1280x720.jpg',
        }}
        right={{
          label: 'sint dignissimos',
          uri:
            'https://coveteur.com/wp-content/uploads/2019/12/PCL_MFWAW18_Day1_Image-18-2010s-fashion-trends-decade-homepage-1280x720.jpg',
        }}
      />
      <SmallItem
        flex={1}
        left={{
          label: 'Error mollitia',
          uri:
            'https://coveteur.com/wp-content/uploads/2019/12/PCL_MFWAW18_Day1_Image-18-2010s-fashion-trends-decade-homepage-1280x720.jpg',
        }}
        right={{
          label: 'sint dignissimos',
          uri:
            'https://coveteur.com/wp-content/uploads/2019/12/PCL_MFWAW18_Day1_Image-18-2010s-fashion-trends-decade-homepage-1280x720.jpg',
        }}
      />
    </Box>
  </Wrapper>
)

const ItemHeader = ({ title, description }) => (
  <Box alignItems="center" mb="lg">
    <Text variant="subheader" fontSize={48} my="sm">
      {title}
    </Text>
    <Text textAlign="center" fontSize={20}>
      {description}
    </Text>
  </Box>
)

const LargeItem = ({ label, uri, label2, uri2 }) => (
  <Box flexDirection="row" width="100%">
    <ItemImage
      flex={1}
      uri={uri}
      label={label}
      text={{ variant: 'subheader', fontSize: 32 }}
    />
    <ItemImage
      visible={{ base: false, tablet: true }}
      ml={{ tablet: 'md' }}
      flex={1}
      uri={uri2}
      label={label2}
      text={{ variant: 'subheader', fontSize: 32 }}
    />
  </Box>
)

const SmallItem = ({ left, right, ...props }) => (
  <Box flexDirection="row" {...props}>
    <ItemImage uri={left.uri} label={left.label} mr="md" />
    <ItemImage uri={right.uri} label={right.label} />
  </Box>
)

const ItemImage = ({ uri, label, text = {}, ...props }) => (
  <Box flex={1} width="100%" mb="md" {...props}>
    <AspectView aspectRatio={1} dimension="width" size={100}>
      <Image style={{ flex: 1 }} source={{ uri }} />
    </AspectView>
    <Text my="sm" fontSize={20} textTransform="uppercase" {...text}>
      {label}
    </Text>
  </Box>
)

export const Order = ({ ...props }) => (
  <Box mt="md" {...props}>
    <CallToAction onPress={() => console.log('TODO')}>
      <SimpleLineIcons name="bag" size={16} />
      <Text>&nbsp;Order</Text>
    </CallToAction>
  </Box>
)
