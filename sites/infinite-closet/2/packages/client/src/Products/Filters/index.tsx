import React from 'react'
import { AntDesign } from '@expo/vector-icons'
import { useRoute } from '@react-navigation/native'

import { TouchableOpacity, Text, Box, Divider } from 'Shared/components'
import { Link } from 'Shared/Link'
import { useSelector, useDispatch } from 'Shared/store'
import { routes } from 'Shared/constants'
import { filtersByRoute } from 'Products/constants'
import { FiltersCount, resetParams } from 'Products/helpers'
import { shopActions, shopSelectors } from 'Products/slice'

import { Filter } from './types'
import FilterItems from './FilterItems'

export const Filters = ({ query, routeName, childNavigation }) => {
  const dispatch = useDispatch()

  return (
    <>
      <Box alignItems="center">
        <BreadCrumbs routeName={routeName} px="md" />
        <ShopFilterHeader
          routeName={routeName}
          childNavigation={childNavigation}
        />
        <Divider />
      </Box>
      {routeName &&
        query.data &&
        filtersByRoute[routeName].map((filter: Filter) => (
          <ShopFilterWrapper
            key={filter}
            query={query}
            filter={filter}
            filterItem={FilterItems[filter]}
            selectFilter={() => dispatch(shopActions.focusFilter(filter))}
            childNavigation={childNavigation}
          />
        ))}
    </>
  )
}

export default Filters

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
        ?.data[0].data.map((el: { label: string; to: string }) => (
          <Link key={el.label} to={el.to} hoverable fontSize={14} pb="sm">
            {el.label}
          </Link>
        ))}
    </Box>
  )
}

const ShopFilterHeader = ({ routeName, childNavigation }) => {
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
      <FiltersCount variant="subheader" routeName={routeName} />
      <TouchableOpacity
        onPress={() => {
          dispatch(shopActions.unfocusFilter())
          resetParams(routeName, childNavigation)
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
  ({
    selectFilter,
    filter,
    query,
    filterItem,
    childNavigation,
  }: {
    selectFilter: any
    filter: Filter
    query: unknown
    filterItem: any
    childNavigation: any
  }) => {
    const dispatch = useDispatch()

    const selected = useSelector((state) =>
      shopSelectors.isFilterSelected(state, filter),
    )

    const state =
      useSelector((state) => shopSelectors.filterSelector(state, filter)) ?? {}
    const setState = (field: string, value: any) =>
      dispatch(
        shopActions.setFilterState({
          filter,
          field,
          payload: value,
        }),
      )

    const params = useRoute().params ?? {}
    const filterParams = filterItem.fromQuery(params)
    const setFilterParams = (values: any) =>
      childNavigation.setParams(filterItem.toQuery(values))

    const numToggled = filterItem.size(params)
    const FilterItemRender = filterItem.render

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
                {filterItem.label}
                {numToggled > 0 && ` (${numToggled})`}
              </Text>
              <AntDesign size={12} name={selected ? 'down' : 'up'} />
            </Box>
          </TouchableOpacity>
          <Box bg="light-gray" visible={selected} p="md">
            <FilterItemRender
              key={filter}
              filter={filter}
              query={query}
              state={state}
              setState={setState}
              params={filterParams}
              setParams={setFilterParams}
            />
          </Box>
        </Box>
        <Divider />
      </>
    )
  },
  (p, n) => {
    if (p.query !== n.query) {
      return false
    }
    return true
  },
)
