import { fetchAPI, getAttributes } from "$lib/api"

export async function getStakes() {
  const response = await fetchAPI(`/stakes?populate=*`, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    }
  })
  const stakes = getAttributes(response)

  return stakes
}

export async function getStake(stakeName: string) {
  const response = await fetchAPI(`/stakes/${stakeName}?populate[events]=true&populate[resources]=true&populate[contacts]=true&populate[wards][populate][]=wardPhoto`, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    }
  })
  const stake = getAttributes(response)

  return stake
}

