import { fetchAPI, getAttributes } from "$lib/api"

export async function getWardData(wardName: string) {
  if (wardName === 'favicon.ico') {
    return undefined // Not sure why this happens...
  }

  const response = await fetchAPI(`/wards/${wardName}?populate=*`, {
    method: 'GET',
    headers: {
      'Content-Type': 'application/json',
    }
  })
  const ward = getAttributes(response)

  const today = new Date()
  ward.events = ward.events.filter(event => new Date(event.end) > today)

  return ward
}
