import React from 'react'
import { FilterCheckBoxes } from './FilterCheckBox'
import { merge } from './FilterItem'

export default merge({
  label: 'Style',
  filter: 'styles',

  render({ params, setParams }) {
    return (
      <>
        <FilterCheckBoxes state={params} setState={setParams} data={data} />
      </>
    )
  },
})

const data = [
  { field: 'apple', label: 'Apple' },
  { field: 'athletic', label: 'Athletic' },
  { field: 'bump_friendly', label: 'Bump Friendly' },
  { field: 'full_bust', label: 'Full Bust' },
  { field: 'hourglass', label: 'Hourglass' },
  { field: 'pear', label: 'Pear' },
  { field: 'petite', label: 'Petite' },
  { field: 'straight_narrow', label: 'Straight & Narrow' },
] as const
