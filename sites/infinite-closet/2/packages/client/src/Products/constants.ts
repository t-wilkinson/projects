import { gql } from '@apollo/client'
import { Filter } from './Filters/types'

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

// TODO extend
export type ProductRoutes = 'clothing' | 'plans' | 'trending'
export const filtersByRoute: { readonly [key in ProductRoutes]: Filter[] } = {
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
