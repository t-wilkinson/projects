import React from 'react'

import { Box, Text, TouchableOpacity } from 'Shared/components'

import { State } from './types'

export const ProductRentHeaders = ({
  shopItem,
  membership_price,
  state,
  setState,
}) => {
  return (
    <Box
      flexDirection="row"
      borderColor="gray3"
      borderWidth={1}
      borderRadius={4}
      bg="light-gray"
    >
      {Object.keys(productRentHeaders).map((rentType, i) => {
        return (
          <TouchableOpacity
            key={rentType}
            style={{ flex: 1 }}
            onPress={() =>
              setState((s: State) => ({
                ...s,
                rentType,
              }))
            }
          >
            <Box
              flex={1}
              p="sm"
              borderRightColor="gray3"
              borderRightWidth={
                i !== Object.keys(productRentHeaders).length - 1 ? 1 : 0
              }
              {...(Number(rentType) === state.rentType && { bg: 'sec' })}
            >
              <Box flex={1} alignItems="center" justifyContent="space-between">
                {productRentHeaders[rentType]({
                  shopItem,
                  membership_price,
                  state,
                  setState,
                })}
              </Box>
            </Box>
          </TouchableOpacity>
        )
      })}
    </Box>
  )
}
export default ProductRentHeaders

const productRentHeaders = {
  OneTime: ({ shopItem }) => (
    <>
      <Text fontSize={13} variant="body-bold" textAlign="center">
        One-time rental
      </Text>
      <Text variant="body-bold">£{shopItem.rental_price}</Text>
    </>
  ),

  Membership: ({ membership_price }) => (
    <>
      <Text fontSize={13} variant="body-bold">
        Membership
      </Text>
      {/* <Text variant="body-bold">from £{membership_price}</Text> */}
    </>
  ),

  Purchase: ({ shopItem }) => (
    <>
      <Text fontSize={13} variant="body-bold">
        Purchase
      </Text>
      {/* <Text variant="body-bold">£{shopItem.purchase_price}</Text> */}
    </>
  ),
}
