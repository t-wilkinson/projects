// A typeof function which can distinguish between different object types (Array, Function, etc)
export const typeOf = (arg: any) =>
  Object.prototype.toString.call(arg).slice(8, -1)

type Keys = (keyof any)[]

export const getDeep = (obj: object, keys: Keys): object => {
  return keys.reduce(
    (acc, key) => (acc && acc[key] !== undefined ? acc[key] : undefined),
    obj,
  )
}

export const getBreadth = (obj: object, keys: Keys): object => {
  let res = {}
  for (const key of keys) {
    if (obj[key] !== undefined) {
      res[key] = obj[key]
    }
  }
  return res
}
