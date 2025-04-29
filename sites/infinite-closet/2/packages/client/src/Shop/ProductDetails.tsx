import React from 'react'
import { AntDesign } from '@expo/vector-icons'

import { Divider, Box, Text, TouchableOpacity } from 'Shared/components'
import Share from 'Shared/share'
import { extras } from 'Shared/constants'

import { State } from './types'

export const ProductDeatils = ({ selected, setState, item, shopItem }) => (
  <>
    <TouchableOpacity
      onPress={() =>
        setState((s: State) => ({
          ...s,
          moreInfo: item.key === s.moreInfo ? undefined : item.key,
        }))
      }
    >
      <Box px="sm" py="md" flexDirection="row" justifyContent="space-between">
        <Text variant={selected ? 'body-bold' : 'body'}>{item.label}</Text>
        <AntDesign size={12} name={selected ? 'down' : 'up'} />
      </Box>
    </TouchableOpacity>
    {item.key === 'share' ? (
      <Box flexDirection="row" visible={selected} px="sm" pb="md">
        <Box mr="sm">
          <Share.Facebook
            url={createProductURL(shopItem)}
            description={shopItem.description}
          />
        </Box>
        <Share.Pinterest
          url={createProductURL(shopItem)}
          description={shopItem.description}
          imageURL={shopItem.images[0]}
        />
      </Box>
    ) : (
      <Box bg="light-gray" px="sm" py="md" visible={selected}>
        <Text>{shopItem[item.key]}</Text>
      </Box>
    )}
    <Divider />
  </>
)
export default ProductDeatils

const createProductURL = ({ name_uid, designer: { name: designer_uid } }) =>
  `${extras.domain}/Shop/listings/${designer_uid}/${name_uid}`
