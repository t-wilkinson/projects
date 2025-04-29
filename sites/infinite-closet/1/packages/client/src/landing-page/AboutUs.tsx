import React from 'react'
import { Wrapper } from 'shared/Wrapper'
import { Text, Box } from 'shared/components'
import { values } from './constants'

export default AboutUs
export const AboutUs = () => (
  <Wrapper
    outer={{ mx: { base: 'sm', tablet: 0 } }}
    inner={{ alignItems: 'center', p: 'lg' }}
  >
    <Text textAlign="center" variant="subheader-light" my="md" fontSize={42}>
      Discover. Rent. Love.
    </Text>
    <AboutItem header="Our Mission">
      <Text fontSize={20} textAlign="center">
        To help women feel confident and fashionable while creating a more
        sustainable future.
      </Text>
    </AboutItem>
    <AboutItem header="Our Vision">
      <Text fontSize={20} textAlign="center">
        Our platform allows customers to hire independent brands while cutting
        their carbon footprint and supporting “slow” fashion – making it
        affordable for the average consumer. We take the guesswork out of the
        fashion industry by only partnering with brands who are sustainable,
        ethical, and minority and/or women owned. By creating an “unlimited”
        designer closet, we allow women to feel great every day.
      </Text>
    </AboutItem>
    <AboutItem header="Our Values">
      <Box
        flexDirection={{ tablet: 'row' }}
        flexWrap="wrap"
        justifyContent="center"
        alignContent="center"
        m="md"
        width="100%"
      >
        {values.map(({ header, text }) => (
          <ValueItem key={header} header={header} text={text} />
        ))}
      </Box>
    </AboutItem>
  </Wrapper>
)

const AboutItem = ({ header, children }) => (
  <Box my="lg" width={{ base: '100%', tablet: 500 }} alignItems="center">
    <Text variant="subheader" fontSize={32} color="pri-light">
      {header}
    </Text>
    {children}
  </Box>
)

// TODO use restyle card
const ValueItem = ({ header, text }) => (
  <Box
    m="sm"
    bg="light-gray"
    width={{ base: 250, tablet: 200, laptop: 225 }}
    height={{ tablet: 250 }}
    alignItems="center"
    p="lg"
    shadowColor="black"
    shadowOffset={{
      width: 0,
      height: 2,
    }}
    shadowOpacity={0.25}
    shadowRadius={3.84}
    elevation={5}
    borderRadius={3.84}
  >
    <Text variant="body-bold" mb="md" color="pri-light" fontSize={20}>
      {header}
    </Text>
    <Text textAlign="center" fontSize={16}>
      {text}
    </Text>
  </Box>
)
