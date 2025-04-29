import React from 'react'
import { Image, Linking, TouchableOpacity } from 'Shared/components'
import { extras } from 'Shared/constants'

interface ShareFacebookConfig {
  url: string
  description: string
}

interface SharePinterestConfig {
  url: string
  description: string
  imageURL: string
}

const share = (url: string) => {
  if (url) {
    Linking.openURL(url)
      .then((data) => console.info(data))
      .catch((err) => console.error(err))
  }
}

const createParams = (obj: { [key: string]: string | undefined }) =>
  Object.entries(obj)
    .filter(([, v]) => v !== undefined)
    .map(([k, v]) => k + '=' + encodeURI(v as string))
    .join('&')

const ratio = 32 / 13
const size = 28
const shareStyle = { width: size * ratio, height: size }

export default {
  Facebook: ({ url, description }: ShareFacebookConfig) => (
    <TouchableOpacity
      onPress={() =>
        share(
          'https://www.facebook.com/sharer/sharer.php?' +
            createParams({
              u: url,
              quote: description,
              app_id: extras.facebook_app_id,
              redirect_uri: url,
              display: 'popup',
              kid_directed_site: '0',
            }),
        )
      }
    >
      <Image
        style={shareStyle}
        resizeMode="contain"
        source={require('assets/images/icons/facebook-share.png')}
      />
    </TouchableOpacity>
  ),

  Pinterest: ({ url, description, imageURL }: SharePinterestConfig) => (
    <TouchableOpacity
      onPress={() =>
        share(
          'https://www.pinterest.com/pin/create/button/?' +
            createParams({
              url,
              description,
              media: imageURL,
              method: 'button',
            }),
        )
      }
    >
      <Image
        style={shareStyle}
        resizeMode="contain"
        source={require('assets/images/icons/pinit.jpg')}
      />
    </TouchableOpacity>
  ),
}
