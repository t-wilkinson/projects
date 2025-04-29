import { createSlice, createSelector, PayloadAction } from '@reduxjs/toolkit'
import { RootState } from 'Shared/store'
import { Filter } from './Filters/types'

export interface State {
  pageNumber: number
  focusedFilter?: Filter
  filters: {
    Designers: {
      search: string
      searchFocused: boolean
      matches: string[]
    }
    Sort: {}
    Colors: {}
    DatesAvailable: {}
    Occasions: {}
    Favorites: {}
    Weather: {}
    Style: {}
  }
}

const initialState: State = {
  pageNumber: 0,
  filters: {
    Designers: {
      search: '',
      searchFocused: false,
      matches: [],
    },
    Sort: {},
    Colors: {},
    DatesAvailable: {},
    Occasions: {},
    Favorites: {},
    Weather: {},
    Style: {},
  },
}

export const shopSlice = createSlice({
  name: 'shop',
  initialState,
  reducers: {
    increasePageNumber(state, { payload: totalPages }) {
      const n = state.pageNumber
      state.pageNumber = n + 1 < totalPages ? n + 1 : n
    },
    decreasePageNumber(state) {
      const n = state.pageNumber
      state.pageNumber = n > 0 ? n - 1 : 0
    },
    focusFilter(state, { payload: filter }: PayloadAction<Filter>) {
      state.focusedFilter = state.focusedFilter === filter ? undefined : filter
    },
    unfocusFilter(state) {
      state.focusedFilter = undefined
    },
    setFilterState(
      state,
      {
        payload: { filter, field, payload },
      }: PayloadAction<{ filter: Filter; field: string; payload: any }>,
    ) {
      state.filters[filter][field] = payload
    },
    resetFilters(state) {
      state.filters = initialState.filters
    },
  },
})

const shopSelector = (state: RootState) => state.shop
const filtersSelector = createSelector(shopSelector, (shop) => shop.filters)
const focusedFilter = createSelector(shopSelector, (shop) => shop.focusedFilter)

const shopSelectors = {
  focusedFilter,
  pageNumber: createSelector(shopSelector, (shop) => shop.pageNumber),
  isFilterSelected: createSelector(
    shopSelector,
    (_: any, filter: Filter) => filter,
    (shop, filter) => shop.focusedFilter === filter,
  ),
  filterSelector: createSelector(
    filtersSelector,
    (_: any, filter: Filter) => filter,
    (filters, filter) => filters[filter],
  ),
}

export { shopSelectors }
export const shopActions = shopSlice.actions
export default shopSlice.reducer
