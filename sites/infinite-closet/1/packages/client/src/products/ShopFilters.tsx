import React from 'react'
import { AntDesign } from '@expo/vector-icons'

import { TouchableOpacity, Text, Box, Divider } from 'shared/components'
import { Link } from 'shared/Link'
import { useSelector, useDispatch } from 'shared/store'
import { routes } from 'shared/constants'

import { filtersByRoute, filterData } from './constants'
import { FiltersCount } from './helpers'
import { shopActions, shopSelectors } from './slice'
import shopFilter from './ShopFilter'
import { Filter } from './types'

export const ShopFilters = ({ query, routeName }) => {
  const dispatch = useDispatch()
  const setFilterState = (filter: Filter) => (field: string, value: any) =>
    dispatch(
      shopActions.setFilterState({
        filter,
        field,
        payload: value,
      }),
    )

  return (
    <>
      <Box alignItems="center">
        <BreadCrumbs routeName={routeName} px="md" />
        <ShopFilterHeader />
        <Divider />
      </Box>
      {routeName &&
        query.data &&
        filtersByRoute[routeName].map((filter: Filter) => (
          <ShopFilterWrapper
            key={filter}
            query={query}
            filter={filter}
            Filter={shopFilter[filter]}
            setState={setFilterState(filter)}
            selectFilter={() => dispatch(shopActions.focusFilter(filter))}
          />
        ))}
    </>
  )
}

export default ShopFilters

const BreadCrumbs = ({ routeName, ...props }) => {
  return (
    <Box alignItems="flex-start" mb="sm" width="100%" {...props}>
      <Text mb="md">
        <Text fontSize={14}>Shop / &nbsp;</Text>
        <Link fontSize={14} to={routeName} variant="body-bold" hoverable>
          {routeName}
        </Link>
      </Text>
      {routes
        .find((el) => el.value === routeName)
        ?.data[0].data.map((el) => (
          <Link key={el.label} to={el.to} hoverable fontSize={14} pb="sm">
            {el.label}
          </Link>
        ))}
    </Box>
  )
}

const ShopFilterHeader = () => {
  const dispatch = useDispatch()
  return (
    <Box
      flexDirection="row"
      alignItems="flex-end"
      justifyContent="space-between"
      width="100%"
      my="md"
      px="md"
    >
      <FiltersCount variant="subheader" />
      <TouchableOpacity
        onPress={() => {
          dispatch(shopActions.unfocusFilter())
          dispatch(shopActions.resetFilters())
        }}
      >
        <Text
          fontSize={14}
          textDecorationStyle="solid"
          textDecorationLine="underline"
        >
          clear all
        </Text>
      </TouchableOpacity>
    </Box>
  )
}

const ShopFilterWrapper = React.memo(
  ({ selectFilter, filter, query, setState, Filter }: any) => {
    const state = useSelector((state) =>
      shopSelectors.filterSelector(state, filter),
    )
    const selected = useSelector((state) =>
      shopSelectors.isFilterSelected(state, filter),
    )
    const numToggled = useSelector((state) =>
      shopSelectors.numToggledFilter(state, filter),
    )

    return (
      <>
        <Box>
          <TouchableOpacity onPress={() => selectFilter()}>
            <Box
              flexDirection="row"
              justifyContent="space-between"
              p="md"
              alignItems="center"
            >
              <Text
                textTransform="uppercase"
                variant={selected ? 'body-bold' : 'body'}
              >
                {filterData[filter].label ?? filter}
                {numToggled > 0 && ` (${numToggled})`}
              </Text>
              <AntDesign size={12} name={selected ? 'down' : 'up'} />
            </Box>
          </TouchableOpacity>
          <Box bg="light-gray" visible={selected} p="md">
            <Filter
              key={filter}
              filter={filter}
              query={query}
              state={state}
              setState={setState}
            />
          </Box>
        </Box>
        <Divider />
      </>
    )
  },
  (p, n) => {
    if (p.selected !== n.selected ?? p.query !== n.query) {
      return false
    }
    return true
  },
)
