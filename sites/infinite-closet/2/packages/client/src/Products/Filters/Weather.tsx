import React from 'react'
import { FilterCheckBoxes } from './FilterCheckBox'
import { merge } from './FilterItem'

export default merge({
  label: 'Weather',
  filter: 'weathers',

  render({ params, setParams }) {
    return (
      <>
        <FilterCheckBoxes state={params} setState={setParams} data={data} />
      </>
    )
  },
})

const data = [
  { field: 'cold_weather', label: 'Cold Weather' },
  { field: 'warm_weather', label: 'Warm Weather' },
  { field: 'year_round', label: 'Year-Round' },
] as const
