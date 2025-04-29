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
