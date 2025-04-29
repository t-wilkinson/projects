import React from 'react'

const toTitleCase = (value: string) => {
  let titlecase = value.replace(/([A-Z])/g, ' $1').replace(/^ /, '')
  titlecase = titlecase.charAt(0).toUpperCase() + titlecase.slice(1)
  return titlecase
}

export type FieldConfig<Value=FieldValue> = Partial<{
  default: any
  constraints: string
  label: string
  onChange: (value: Value) => void
  type: string
  placeholder: string
  autocomplete: string
  errorMessage: string    // Provide custom error messages
  changed: (field: UseField<Value>) => [boolean, Value?]
}>

export type FieldsConfig<Keys> = {
  [field in keyof Keys]: FieldConfig<Keys[field]>
}

export type FieldErrors = { [field: string]: FieldError[] }
export type FieldError = string
export type Valid = true | string

export type FormStatus = 'success' | 'submitting' | 'error' | null
export type UseFormField = UseField<FormStatus>

export type FieldValue = number | string | boolean

export type DateOfBirthFields = {
  bday: UseField<FieldValue>
  bmonth: UseField<FieldValue>
  byear: UseField<FieldValue>
}

const autocompleteValues = {
  name: 'name',
  fullName: 'name',
  firstName: 'given-name',
  lastName: 'family-name',
  nickName: 'nickname',
  billingName: 'cc-name',

  email: 'email',
  emailAddress: 'email',
  username: 'username',
  password: 'current-password',
  currentPassword: 'current-password',
  newPassword: 'new-password',
  confirmPassword: 'new-password',

  address: 'street-address',
  streetAddress: 'street-address',
  country: 'country',
  postcode: 'postal-code',
  countryName: 'country-name',
  addressLine1: 'address-line1',
  addressLine2: 'address-line2',
  addressLine3: 'address-line3',

  town: 'address-level2',
  city: 'address-level2',
  state: 'address-level1',
  province: 'address-level1',

  bday: 'bday-day',
  bmonth: 'bday-month',
  byear: 'bday-year',
}

export class UseField<Value = FieldValue> {
  name: string
  label: string
  value: Value
  default: any
  constraints: string
  placeholder: string
  setValue: (value: Value) => void
  autocomplete: string
  errorMessage: string
  errors: string[]
  setErrors: (...errors: (string | string[])[]) => void
  setError: (error: string) => void
  clearErrors: () => void

  constructor(name: string, config: FieldConfig<Value>) {
    Object.keys(config).forEach(
      (key) => config[key] === undefined && delete config[key]
    )
    config = Object.assign(
      {
        label: toTitleCase(name),
        constraints: '',
        onChange: () => {},
        default: '',
        placeholder: '',
        autocomplete: autocompleteValues[name] || 'on',
      },
      config
    )

    const [state, setState] = React.useState(config.default)
    const [errors, setErrors] = React.useState([])

    this.name = name
    this.label = config.label
    this.default = config.default
    this.constraints = config.constraints
    this.placeholder = config.placeholder
    this.value = state
    this.setValue = (value: Value) => {
      setState(value)
      config.onChange(value)
    }
    this.errorMessage = config.errorMessage
    this.errors = errors
    this.setErrors = (...errors) => {
      setErrors(errors.flat().filter((v) => v))
    }
    this.setError = this.setErrors
    this.clearErrors = () => this.setErrors()
  }

  // TODO: improve this
  // TODO: preprocess the constraints
  getErrors(): FieldError[] {
    const isError = (value: Value, constraint: string): Valid => {
      constraint = constraint.replace(/<space>/g, ' ')
      const label = this.label.toLowerCase()
      const label_ = label.charAt(0).toUpperCase() + label.slice(1)
      const v = (value || '').toString()

      let [operation, ...props] = constraint.split(':')
      let negate = false
      if (/^!/.test(operation)) {
        operation = operation.slice(1)
        negate = true
      }

      let optional = false
      if (/^\?/.test(operation)) {
        operation = operation.slice(1)
        optional = true
      }

      const err = (test: boolean, message: string) => (negate ? !test : test) || message

      // prettier-ignore
      switch (operation) {
        case 'selected': return err(v === 'true', `${label_} must be selected`)
        case 'enum': return err(props[0].split(',').includes(v), `Value must be one of ${props[0]}`)
        case 'between': return err(RegExp(`[${props[0]}-${props[1]}]`).test(v), `Value must be between ${props[0]} and ${props[1]}`)
        case 'email': return err(/^.+@.+\..+$/.test(v) , `Please enter a valid ${label}`)
        case 'required': return err(Boolean(v) , `Missing ${label}`)
        case 'decimal': return err(/^\d*\.?\d{0,2}$/.test(v) , `${label_} must be a number`)
        case 'integer': return err(/^-?\d*$/.test(v) , `${label_} must be a number`)
        case 'number': return err(/^\d*\.?\d*$/.test(v) , `${label_} must be a number`)
        case 'min': return err(Number(value) > Number(props[0]), `${label_} must be be greater than ${props[0]}`)
        case 'max': return err(Number(value) < Number(props[0]), `${label_} must be be smaller than ${props[0]}`)
        case 'max-width': return err(
          v.length <= Number(props[0]) || (v.length === 0 && optional), `${label_} must be at most ${props[0]} characters long`)
        case 'min-width': return err(v.length >= Number(props[0]) || (v.length === 0 && optional), `${label_} must be at least ${props[0]} characters long`)
        case 'regex': return err(RegExp(props[0]).test(v) , `${label_} does not have the correct format`)
        case 'contains':
          let search = props[0]
          search = search.replace(']', '\\]')
          return err(RegExp(`[${search}]`).test(v) || v.length === 0 && optional, `${label_} must contain one of ${props[0].split('').join(' ')}`)
        case '': return true
        default: return true
      }
    }

    const errors = this.constraints
      .replace(
        /(^|\s)password($|\s)/,
        'min-width:8 max-width:30 contains:~!@#$%^*-_=+[{]}/;,.?'
      )
      .replace(
        /(^|\s)optional-password($|\s)/,
        '?min-width:8 max-width:30 ?contains:~!@#$%^*-_=+[{]}/;,.?'
      )
      .split(' ')
      .map((constraint) => isError(this.value, constraint))
      .filter((v) => v !== true) as FieldError[]

    if (this.errorMessage && errors.length > 0) {
      return [this.errorMessage]
    } else {
      return errors
    }
  }

  hasErrors(): boolean {
    return this.getErrors().length !== 0
  }

  clean(): Value {
    if (typeof this.value === 'string') {
      return this.value.trim() as any
    } else {
      return this.value
    }
  }
}

type Fields<Keys> = {
  [field in keyof Keys]: UseField<Keys[field]>
}

export class UseFields<Keys = { [key: string]: any }> {
  form: UseFormField
  fields: Fields<Keys>
  status: FormStatus

  constructor(config: FieldsConfig<Keys>) {
    const fields: Fields<Keys> = Object.entries(config).reduce(
      (acc, [field, fieldConfig]) => {
        acc[field] = useField(field, fieldConfig)
        return acc
      },
      {} as any
    )

    this.fields = fields
    this.form = useField('form', {})
    this.status = this.form.value
  }

  setStatus(status: FormStatus): void {
    this.form.setValue(status)
  }

  setError(error: string): void {
    this.setErrors(error)
  }

  setErrors(...errors: (string | string[])[]): void {
    errors = errors.flat().filter((v) => v)
    if (errors.length > 0) {
      this.setStatus('error')
    }
    this.form.setErrors(...errors)
  }

  get(field: keyof Keys): UseField<Keys[typeof field]> {
    return this.fields[field]
  }

  setValue(field: keyof Keys, value: Keys[typeof field]): void {
    this.get(field).setValue(value)
  }

  value(field: keyof Keys): Keys[typeof field] {
    return this.get(field).value
  }

  getErrors(): FieldErrors {
    return Object.fromEntries(
      Object.entries(this.fields).map(
        ([k, field]: [string, UseField<typeof k>]) => [k, field.getErrors()]
      )
    )
  }

  hasErrors(fieldErrors?: FieldErrors): boolean {
    if (!fieldErrors) {
      return Object.values(this.fields).some((field: UseField) =>
        field.hasErrors()
      )
    } else {
      return Object.values(fieldErrors).some(
        (errors: FieldError[]) => errors.length !== 0
      )
    }
  }

  attachErrors(fieldErrors: FieldErrors) {
    for (const field in this.fields) {
      this.fields[field].setErrors(fieldErrors[field])
    }
  }

  // Returns true if fields don't have errors
  update(): boolean {
    const fieldErrors = this.getErrors()
    this.attachErrors(fieldErrors)
    return !this.hasErrors(fieldErrors)
  }

  clearErrors(): void {
    for (const field in this.fields) {
      this.fields[field].setErrors([])
    }
  }

  clean(): Keys {
    return Object.entries(this.fields).reduce(
      (acc, [k, field]) => {
        acc[k] = (field as any).clean()
        return acc
      },
      {} as any
    )
  }

  changed() {
    const cleaned = this.clean()
    let changed: { [key in keyof Keys]?: Keys[key] } = {}
    for (const field in cleaned) {
      if (cleaned[field] != this.fields[field].default) {
        changed[field] = cleaned[field]
      }
    }
    return changed
  }

  map(fn: (value: UseField<any>, index: number) => any): any[] {
    return Object.values(this.fields).map(fn)
  }
}

type useFields<Keys> = UseFields<{ [key in keyof Keys]: Keys[key] }>
//   Keys extends null
// ? {[key in keyof typeof config]: FieldValue}
// : {
//   // look at AddToCart
//   // [key in keyof typeof config]: FieldValue
//   // [key in keyof typeof config]: Keys[key] extends unknown ? Keys[key] : FieldValue
//   [key in keyof typeof config]: Keys[key]
// }>

export const useFields = <Keys>(config: FieldsConfig<Keys>): useFields<Keys> =>
  new UseFields(config)
// useFields<{[key in keyof Keys]: Keys[key] extends unknown ? FieldValue : Keys[key]
export const useField = (name: string, config: FieldConfig): UseField<any> =>
  new UseField(name, config)
