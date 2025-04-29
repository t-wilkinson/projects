import React from 'react'
import { CheckBox, Text } from 'Shared/components'

export const FilterCheckBox = ({ field, setState, state, label }) => {
  const setCheckBoxState = () => {
    state.has(field) ? state.delete(field) : state.add(field)
    setState(state)
  }

  return (
    <CheckBox p="sm" setState={setCheckBoxState} state={state.has(field)}>
      <Text>&nbsp;&nbsp;{label}</Text>
    </CheckBox>
  )
}

export default FilterCheckBox
export const FilterCheckBoxes = ({ data, state, setState, ...props }) =>
  data.map((v: { field: string; label: string }) => (
    <FilterCheckBox
      key={v.field}
      state={state}
      setState={setState}
      {...props}
      {...v}
    />
  ))
