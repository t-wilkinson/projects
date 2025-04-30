import React from 'react'
import { Text } from 'Shared/components'
import { Filter } from './types'

interface FilterProps {
  query: unknown
  filter: Filter
  state: unknown
  setState: (field: unknown, value: unknown) => void
  params: Set<string>
  setParams: (params: Set<string>) => void
}

export interface FilterItem {
  label: string
  filter?: string
  size: (params: object) => number
  toFilter: (params: object) => object // Takes filter data in params to graphql filter variables
  toQuery: (value: any) => object // Filters data -> url query params
  fromQuery: (params: object) => any // url query params -> filter data
  render: (props: FilterProps) => React.ReactNode
}

// TODO move parsing information to linking?
export const FilterItem: (label: string, filter?: string) => FilterItem = (
  label,
  filter,
) => {
  return {
    label,
    filter,
    size(params) {
      return this.fromQuery(params).size
    },
    toFilter(params) {
      if (!filter) return {}
      if (!params[filter]) return {}
      return { [filter]: Array.from(this.fromQuery(params)) }
    },
    toQuery(value) {
      if (!filter) return {}
      if (!value) return {}
      return { [filter]: Array.from(value) }
    },
    fromQuery(params) {
      let values: Set<string>
      if (!filter) values = new Set()
      else if (!params[filter]) values = new Set()
      else if (typeof params[filter] === 'string')
        values = new Set(params[filter].split(','))
      else values = new Set(params[filter])
      values.delete('')
      return values
    },
    render: () => <Text>Coming Soon...</Text>,
  }
}
export default FilterItem

// TODO each filter item stores its own data in params. show this in type information.
// TODO default single filter per component but allow more?
export function merge<T extends { label: string; filter?: string }>(item: T) {
  return { ...FilterItem(item.label, item.filter), ...item }
}
