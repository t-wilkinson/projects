import React from 'react'
import { Ionicons } from '@expo/vector-icons'
// import debounce from 'lodash/debounce'
import { Text, CheckBox, Box, TextInput } from 'Shared/components'
import { merge } from './FilterItem'

export default merge({
  label: 'Designers',
  filter: 'designers',

  render({ params, setParams, state, setState, query }) {
    // TODO this causes graphql fetching by calling setMatches
    const designers = query.data.designers
    // const debounceMatches = React.useCallback(
    //   debounce(searchDesignerMatches, 300),
    //   [],
    // )

    React.useLayoutEffect(() => {
      // debounceMatches(
      //   (v: any) => setState('matches', v),
      //   state.search,
      //   designers,
      // )
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
        {state.matches.map((_: any, index: number) => {
          const key = designers[index].name_uid
          return (
            <CheckBox
              key={key}
              setState={() => {
                params.has(key) ? params.delete(key) : params.add(key)
                setParams(params)
              }}
              params={params.has(key)}
              p="sm"
            >
              <Text>&nbsp;&nbsp;{designers[index].name}</Text>
            </CheckBox>
          )
        })}
      </>
    )
  },
})

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
