import React from 'react'
import { useSafeAreaInsets } from 'react-native-safe-area-context'

import { ScrollView, Box, Divider, Text } from 'shared/components'
import { LandingPageHeader as Header } from 'shared/Header'
import { LandingPageFooter as Footer } from 'shared/Footer'
import { Wrapper } from 'shared/Wrapper'
import { Default } from 'shared/Breakpoints'

import { terms } from './constants'

export default ({ navigation }) => {
  const insets = useSafeAreaInsets()

  return (
    <ScrollView>
      <Box bg="white" flex={1} style={{ paddingTop: insets.top }}>
        <Header navigation={navigation} />
        <Wrapper inner={{ px: { base: 'sm', laptop: 0 } }}>
          <PolicyTerms />
        </Wrapper>
        <Default>
          <Divider />
        </Default>
        <Footer />
      </Box>
    </ScrollView>
  )
}

const PolicyTerms = () => (
  <>
    <Box alignItems="center" mb="md">
      <Text variant="subheader" textAlign="center">
        Privacy & Cookie Policy
      </Text>
      <Text color="dark-gray" fontSize={14}>
        Last Updated: 5/2/21
      </Text>
    </Box>
    {terms.map((term) => (
      <React.Fragment key={term.header}>
        <TermHeader header={term.header} text={term.text} />
        {term.data.map((content: { header: string; text: string }) => (
          <TermContent
            key={content.header}
            subheader={content.header}
            text={content.text}
          />
        ))}
      </React.Fragment>
    ))}
  </>
)

const TermHeader = ({ header = '', text = '' }) => (
  <>
    <Text textAlign="center" variant="subheader" fontSize={32}>
      {header}
    </Text>
    <Divider my="md" />
    <Text variant="body">{`${text}

`}</Text>
  </>
)

const TermContent = ({ subheader = '', text = '' }) => (
  <>
    <Text variant="body-bold">{subheader}</Text>
    <Text variant="body">{`${text}

`}</Text>
  </>
)
