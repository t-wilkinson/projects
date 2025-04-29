import { gql } from '@apollo/client'
import { Filter } from './types'

export const PAGE_SIZE = 6 // should be a multiple of 2 and 3 (possible column numbers)
export const FILTERS_ASIDE_WIDTH = 332

export const QUERY = gql`
  query(
    $sort: String
    $start: Int
    $PAGE_SIZE: Int
    $colors: [String]
    $occasions: [String]
    $styles: [String]
    $weathers: [String]
    $designers: [String]
  ) {
    designers {
      id
      name
      name_uid
    }
    shopItemsCount(
      where: {
        color_in: $colors
        occasion_in: $occasions
        style_in: $styles
        weather_in: $weathers
        designer: { name_uid_in: $designers }
      }
    )
    shopItems(
      sort: $sort
      start: $start
      limit: $PAGE_SIZE
      where: {
        color_in: $colors
        occasion_in: $occasions
        style_in: $styles
        weather_in: $weathers
        designer: { name_uid_in: $designers }
      }
    ) {
      id
      name
      name_uid
      retail_price
      rental_price
      purchase_price
      designer {
        name
        name_uid
      }
      images {
        url
      }
    }
  }
`

// ensure we construct an object containing data for every Filter
export type Verify = Pick<typeof filterData, Filter>

// TODO normalize field names? ({field: string, label: string, value?: string}[]
export type FilterData = typeof filterData
export const filterData = {
  Weather: {
    filterName: 'weathers',
    data: [
      { field: 'cold_weather', label: 'Cold Weather' },
      { field: 'warm_weather', label: 'Warm Weather' },
      { field: 'year_round', label: 'Year-Round' },
    ],
  },
  Occasions: {
    filterName: 'occasions',
    data: [
      { field: 'date', label: 'Date' },
      { field: 'formal_affair', label: 'Formal Affair' },
      { field: 'party', label: 'Party' },
      { field: 'vacation', label: 'Vacation' },
      { field: 'wedding', label: 'Wedding' },
      { field: 'weekend', label: 'Weekend' },
      { field: 'work', label: 'Work' },
    ],
  },
  Colors: {
    filterName: 'colors',
    data: [
      { color: 'white', label: 'White', value: '#ffffff' },
      { color: 'gray', label: 'Gray', value: '#cccccc' },
      { color: 'black', label: 'Black', value: '#000000' },
      { color: 'red', label: 'Red', value: '#ff0000' },
      { color: 'blue', label: 'Blue', value: '#0000ff' },
      { color: 'green', label: 'Green', value: '#00ff00' },
    ],
  },
  Style: {
    filterName: 'styles',
    data: [
      { field: 'apple', label: 'Apple' },
      { field: 'athletic', label: 'Athletic' },
      { field: 'bump_friendly', label: 'Bump Friendly' },
      { field: 'full_bust', label: 'Full Bust' },
      { field: 'hourglass', label: 'Hourglass' },
      { field: 'pear', label: 'Pear' },
      { field: 'petite', label: 'Petite' },
      { field: 'straight_narrow', label: 'Straight & Narrow' },
    ],
  },
  Sort: {
    filterName: 'sort',
    data: [
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
    ],
  },
  DatesAvailable: {
    filterName: undefined,
    label: 'Dates Available',
  },
  Designers: {
    filterName: 'designers',
  },
  Favorites: { filterName: undefined },
} as const

// TODO extend
type ShopRoutes = 'clothing' | 'plans' | 'trending'
export const filtersByRoute: { readonly [key in ShopRoutes]: Filter[] } = {
  clothing: [
    'Sort',
    'Designers',
    'Colors',
    'DatesAvailable',
    'Occasions',
    'Favorites',
    'Weather',
    'Style',
  ],
  plans: ['Designers', 'Sort'],
  trending: [],
}
