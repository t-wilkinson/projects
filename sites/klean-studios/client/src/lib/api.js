import { variables } from '$lib/variables'

export function toBackendUrl(url) {
  return `${variables.apiUrl}${url}`
}

export function toApiUrl(url) {
  return `${variables.apiUrl}/api${url}`
}

export async function fromResponse(response) {
  const data = response.ok && (await response.json()).data || undefined
  return {
    data,
    status: response.status
  }
}

export async function fetchApi(url, data=undefined) {
  const response = await fetch(toApiUrl(url), {
    ...data,
    headers: {
      'Content-Type': 'application/json',
      ...data.headers,
    },
    body: JSON.stringify(data?.body),
  })
  return await fromResponse(response)
}

export async function getApi(url) {
  const response = await fetch(toApiUrl(url))
  return await fromResponse(response)
}

export function getAttributes(data=[]) {
  return data.map(({attributes}) => attributes)
}
