import React from 'react'
import { Ionicons, Feather } from '@expo/vector-icons'
import debounce from 'lodash/debounce'

import {
  TouchableOpacity,
  Text,
  Box,
  TextInput,
  CheckBox,
} from 'shared/components'
import Hoverable from 'shared/Hoverable'

import { pickFgColorFromBgColor } from './helpers'
import { filterData } from './constants'
import { Filter } from './types'
import { State } from './slice'

type ProductFilter = {
  [key in Filter]: (props: {
    query: unknown
    filter: Filter
    state: State['filters'][key]
    // TODO `value' does not have the right type
    // instead it is the union of all fields
    // oh how I want dependent types
    setState: (
      field: keyof State['filters'][key],
      value: State['filters'][key][typeof field],
    ) => void
  }) => any
}

export const productFilter: ProductFilter = {
  Designers: ({ query, state, setState }) => {
    // TODO this causes graphql fetching by calling setMatches
    const designers = query.data.designers
    const debounceMatches = React.useCallback(
      debounce(searchDesignerMatches, 300),
      [],
    )

    React.useLayoutEffect(() => {
      debounceMatches(
        (v: any) => setState('matches', v),
        state.search,
        designers,
      )
    }, [state.search])

    return (
      <>
        <Box
          borderBottomColor={state.searchFocused ? 'sec-light' : 'light-gray'}
          borderBottomWidth={2}
          flexDirection="row"
          alignItems="center"
        >
          <Box p="xs">
            <Ionicons name="search-outline" size={20} />
          </Box>
          <TextInput
            autoFocus={true}
            placeholder="Designer"
            returnKeyType="search"
            value={state.search}
            onChangeText={(text) => {
              setState('search', text)
            }}
            style={{ flex: 1, paddingVertical: 4, paddingHorizontal: 4 }}
            onBlur={() => setState('searchFocused', false)}
            onFocus={() => setState('searchFocused', true)}
          />
        </Box>
        {state.matches.map((_, index: number) => {
          const key = designers[index].name_uid
          return (
            <CheckBox
              key={key}
              setState={() => {
                const filter = new Set(state.filter)
                filter.has(key) ? filter.delete(key) : filter.add(key)
                setState('filter', filter)
              }}
              state={state.filter.has(key)}
              p="sm"
            >
              <Text>&nbsp;&nbsp;{designers[index].name}</Text>
            </CheckBox>
          )
        })}
      </>
    )
  },

  Colors: ({ state, setState }: any) => {
    return (
      <>
        <Box flexDirection="row" flexWrap="wrap">
          {filterData.Colors.data.map((v) => (
            <Hoverable key={v.color}>
              <Box m="sm">
                <TouchableOpacity
                  onPress={() => {
                    const filter = new Set(state.filter)
                    filter.has(v.color)
                      ? filter.delete(v.color)
                      : filter.add(v.color)
                    setState('filter', filter)
                  }}
                >
                  <Box
                    borderRadius={999}
                    width={32}
                    height={32}
                    style={{ backgroundColor: v.value }}
                    alignItems="center"
                    justifyContent="center"
                  >
                    {state.filter.has(v.color) && (
                      <Feather
                        name="check"
                        size={24}
                        style={{
                          color: pickFgColorFromBgColor(
                            v.value,
                            '#ffffff',
                            '#000000',
                          ),
                        }}
                      />
                    )}
                  </Box>
                </TouchableOpacity>
              </Box>
            </Hoverable>
          ))}
        </Box>
      </>
    )
  },

  DatesAvailable: () => (
    <>
      <Text>Coming Soon...</Text>
    </>
  ),

  Occasions: ({ setState, state }) => (
    <>
      <FilterCheckBoxes
        state={state}
        setState={setState}
        data={filterData.Occasions.data}
      />
    </>
  ),

  Favorites: () => (
    <>
      <Text>Coming Soon...</Text>
    </>
  ),

  Weather: ({ setState, state }) => (
    <>
      <FilterCheckBoxes
        state={state}
        setState={setState}
        data={filterData.Weather.data}
      />
    </>
  ),

  Style: ({ setState, state }) => {
    return (
      <>
        <FilterCheckBoxes
          state={state}
          setState={setState}
          data={filterData.Style.data}
        />
      </>
    )
  },

  Sort: ({ state, setState }) => {
    return (
      <>
        <Box>
          {Object.values(filterData.Sort.data).map(({ field }, i) => (
            <Box key={field} my="sm">
              <TouchableOpacity onPress={() => setState('filter', field)}>
                <Text variant={field === state.filter ? 'body-bold' : 'body'}>
                  {filterData.Sort.data[i].field}
                </Text>
              </TouchableOpacity>
            </Box>
          ))}
        </Box>
      </>
    )
  },
}
export default productFilter

async function searchDesignerMatches(
  setMatches: (matches: any) => void,
  search: string,
  designers: { name_uid: string; name: string }[],
) {
  const emptyRegExp = String(new RegExp('', 'i'))
  const keywords: RegExp[] = search
    .replace(/[^a-zA-Z0-9_\s]/g, '')
    .split(/\s+/g)
    .map((v) => new RegExp(v, 'i'))

  /* iterate through each keyword, removing keyword if exists.
   * if replacement was made, continue, otherwise, short-circuit
   * compare to `emptyRegExp' because this will not affect `newName.length'
   */
  const matches = designers.reduce((acc: number[], designer, index) => {
    const keywordsFound =
      null !==
      keywords.reduce((name: string | null, keyword) => {
        if (name === null) return null
        if (String(keyword) === emptyRegExp) return name
        const newName = name.replace(keyword, '')
        return newName.length === name.length ? null : newName
      }, designer.name)
    if (keywordsFound) acc.push(index)
    return acc
  }, [])
  setMatches(matches ?? [])
}

const FilterCheckBoxes = ({ data, state, setState, ...props }) =>
  data.map((v: { field: string; label: string }) => (
    <FilterCheckBox
      key={v.field}
      state={state}
      setState={setState}
      {...props}
      {...v}
    />
  ))

const FilterCheckBox = ({ field, setState, state, label }) => {
  const setCheckBoxState = () => {
    const filter = new Set(state.filter)
    filter.has(field) ? filter.delete(field) : filter.add(field)
    setState('filter', filter)
  }

  return (
    <CheckBox
      p="sm"
      setState={setCheckBoxState}
      state={state.filter.has(field)}
    >
      <Text>&nbsp;&nbsp;{label}</Text>
    </CheckBox>
  )
}
