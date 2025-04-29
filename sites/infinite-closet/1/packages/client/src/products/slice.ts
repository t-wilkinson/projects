import { createSlice, createSelector, PayloadAction } from '@reduxjs/toolkit'
import { RootState } from 'shared/store'
import { SortBy, Filter, Filters } from './types'

type NestedFilters = { filters: Filters }
export interface State extends NestedFilters {
  pageNumber: number
  focusedFilter?: Filter
  filters: {
    Designers: {
      search: string
      searchFocused: boolean
      matches: string[]
      filter: Set<string>
    }
    Sort: {
      filter: SortBy
    }
    Colors: {
      filter: Set<string>
    }
    DatesAvailable: {
      filter: Set<string>
    }
    Occasions: {
      filter: Set<string>
    }
    Favorites: {
      filter: Set<string>
    }
    Weather: {
      filter: Set<string>
    }
    Style: {
      filter: Set<string>
    }
  }
}

const initialState: State = {
  pageNumber: 0,
  filters: {
    Designers: {
      search: '',
      searchFocused: false,
      matches: [],
      filter: new Set(),
    },
    Sort: {
      filter: 'Recommended',
    },
    Colors: {
      filter: new Set(),
    },
    DatesAvailable: {
      filter: new Set(),
    },
    Occasions: {
      filter: new Set(),
    },
    Favorites: {
      filter: new Set(),
    },
    Weather: {
      filter: new Set(),
    },
    Style: {
      filter: new Set(),
    },
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
const filterSize = (
  filters: typeof initialState['filters'],
  filter: Filter,
): number => {
  if (filter === 'Sort') return Number(filters[filter].filter !== 'Recommended')
  else return filters[filter].filter.size
}

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
  numToggled: createSelector(filtersSelector, (filters) =>
    Object.values(Filter).reduce((acc, filter) => {
      return acc + filterSize(filters, filter)
    }, 0),
  ),
  numToggledFilter: createSelector(
    filtersSelector,
    (_: any, filter: Filter) => filter,
    filterSize,
  ),
}

export { shopSelectors }
export const shopActions = shopSlice.actions
export default shopSlice.reducer
