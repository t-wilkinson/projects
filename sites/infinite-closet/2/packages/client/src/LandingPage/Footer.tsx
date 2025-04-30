import React from 'react'

import { Box, Divider } from 'Shared/components'
import { Default, Mobile } from 'Shared/Breakpoints'
import { FooterLink, FollowUs, Subscribe } from 'Shared/Footer'

export const Footer = () => (
  <>
    <Mobile>
      <Box>
        <Divider my="md" />
        <Subscribe />
        <Divider my="md" />
        <Box alignItems="center">
          <FooterLink to="/landing-page" label="Landing Page" />
          <FooterLink to="/privacy-policy" label="Legal Terms and Conditions" />
        </Box>
        <Divider my="md" />
        <FollowUs />
      </Box>
    </Mobile>

    <Default>
      <Box
        flexDirection="row"
        justifyContent="space-between"
        alignItems="center"
        pb="sm"
        my="md"
      >
        <Subscribe flex={1} />
        <Box alignItems="center">
          <FooterLink to="/landing-page" label="Landing Page" />
          <FooterLink to="/privacy-policy" label="Legal Terms and Conditions" />
        </Box>
        <FollowUs flex={1} />
      </Box>
    </Default>
  </>
)
export default Footer
