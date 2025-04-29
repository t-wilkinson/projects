import React from 'react'
import { FilterCheckBoxes } from './FilterCheckBox'

import { merge } from './FilterItem'

export default merge({
  label: 'Occasions',
  filter: 'occasions',
  render({ params, setParams }) {
    return <FilterCheckBoxes state={params} setState={setParams} data={data} />
  },
})

const data = [
  { field: 'date', label: 'Date' },
  { field: 'formal_affair', label: 'Formal Affair' },
  { field: 'party', label: 'Party' },
  { field: 'vacation', label: 'Vacation' },
  { field: 'wedding', label: 'Wedding' },
  { field: 'weekend', label: 'Weekend' },
  { field: 'work', label: 'Work' },
] as const
