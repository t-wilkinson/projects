import React from 'react'
import { TouchableOpacity, Box, Text } from 'Shared/components'
import { merge } from './FilterItem'

export type SortBy = 'Recommended' | 'Newest' | 'PriceLowHigh' | 'PriceHighLow'

// TODO Sort is NOT a filter. Don't treat it as such.
export const Sort = merge({
  label: 'Sort',
  filter: 'sorts',
  size(params) {
    return 0
    // return Number(this.fromQuery(params) !== 'Recommended')
  },

  toFilter(params) {
    return { [this.filter]: filterValue(params[this.filter]) }
  },

  toQuery(value) {
    return { [this.filter]: value ?? 'Recommended' }
  },

  fromQuery(params) {
    return params[this.filter] ?? 'Recommended'
  },

  render({ params, setParams }) {
    return (
      <>
        <Box>
          {data.map(({ field }, i) => (
            <Box key={field} my="sm">
              <TouchableOpacity onPress={() => setParams(field)}>
                <Text variant={params === field ? 'body-bold' : 'body'}>
                  {data[i].field}
                </Text>
              </TouchableOpacity>
            </Box>
          ))}
        </Box>
      </>
    )
  },
})
export default Sort

const data = [
  { field: 'Recommended', label: 'Recommended', value: 'created_by' },
  { field: 'Newest', label: 'Newest', value: 'created_by' },
  {
    field: 'PriceLowHigh',
    label: 'Price (Low to High)',
    value: 'rental_price:ASC',
  },
  {
    field: 'PriceHighLow',
    label: 'Price (High to Low)',
    value: 'rental_price:DESC',
  },
] as const

const filterValue = (sortBy: SortBy) =>
  data.find(({ field }) => field === sortBy)?.value
