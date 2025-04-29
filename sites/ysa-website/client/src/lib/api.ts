export function getAttributes(item: any) {
  if (!item.data) {
    return item
  } else if (Array.isArray(item.data)) {
    return item.data.map(({attributes}) => ({...attributes}))
  } else {
    return item.data.attributes
  }
}

export async function fetchAPI(url: string, request: object) {
  const response = await fetch(`${import.meta.env.VITE_API_URL}${url}`, request)

  const data = await response.json()
  if (data.error) {
    throw new Error(data.error)
  }
  return data
}

export function getImgUrl(image: string): string {
  return `${import.meta.env.VITE_BACKEND_URL}${getAttributes(image).url}`
}
