export type SortBy = 'Recommended' | 'Newest' | 'PriceLowHigh' | 'PriceHighLow'

export const Filter = [
  'Designers',
  'Sort',
  'Colors',
  'DatesAvailable',
  'Occasions',
  'Favorites',
  'Weather',
  'Style',
] as const

export type Filter = typeof Filter[number]

export type Filters = {
  [key in Filter]: { filter: unknown }
}
