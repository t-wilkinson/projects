import React from "react"
import loadjs from "loadjs"
import produce, {enableES5} from "immer"
import {useHistory, NavLink} from "react-router-dom"
import * as api from "../api"
import {dispatch} from "../store"
import "./Register.css"
import {
  get, createForm,
  Checkbox, Required, Input, Button,
} from "../components"

enableES5()

export default () => {
  let [state, ldispatch] = React.useReducer(reducer, initState())
  let history = useHistory()
  let refs = {
    autocomplete: React.useRef(null),
  }

  React.useLayoutEffect(() => {
    // Don't load script multiple times
    let exists = Array
      .from(document.querySelectorAll("script"))
      .map(src=>src.src)
      .includes("https://maps.googleapis.com/maps/api/js?key=GOOGLE_API_KEY&callback=initMap&libraries=places")
    if (exists) return

    (window as any).initMap = () => {
      let autocomplete = new (window as any).google.maps.places.Autocomplete(document.getElementById('autocomplete'), {})
      autocomplete.addListener( "place_changed", () => {
        let address = autocomplete.getPlace().address_components
        const get_ = (props) => get(address, props, "")
        ldispatch({type: "place-changed", place: {
          city: get_([2, "long_name"]),
          state: get_([6, "short_name"]),
          zipCode: `${get_([7, "short_name"])}`,
          streetAddress: `${get_([0, "long_name"])} ${get_([1, "long_name"])}`,
        }})
      })}
    loadjs("https://maps.googleapis.com/maps/api/js?key=AIzaSyAU_nol5aAtfwUwOkZn8VDnzW3Wn4Kfe-c&callback=initMap&libraries=places")

  }, [ldispatch])

  // Values to pass to <Input />
  let input = {validate: validate, state: state, dispatch: ldispatch}

  return <article
    className="relative bg-pri-2 col justify-center h-full min-h-screen py-8"
  >
    <form
      className="relative bg-white w-full sm:w-96 px-8 py-8 sm:rounded-lg col border border-gray"
    >
      <h3>Register</h3>
      <div className="pt-4" />
      <Input field="email" {...input} />
      <Input field="password" {...input} />
      <label className="flex flex-col w-full relative my-4 z-10">
        <Required label="Address" required={true} />
        <button
          type="button"
          onClick={() => ldispatch({type: "toggle-address"})}
          className="text-pri absolute top-0 right-0"
        >
          {state.address ? "Hide" : "Edit address"}
        </button>
        <input
          id="autocomplete"
          ref={refs.autocomplete}
          className="border-b-2 py-1 px-2 focus:border-pri w-full outline-none border-gray-3"
          type="text"
        />
      </label>

      <div className={`w-full ${state.address ? "block" : "hidden"}`} >
        <InputStreet autoComplete="address-line1" label="Street Address" name="streetAddress" {...input} />
        <InputStreet autoComplete="address-level2" label="City" name="city" {...input} />
        <InputStreet autoComplete="address-level1" label="State" name="state" {...input} />
        <InputStreet autoComplete="postal-code" label="Zip Code" name="zipCode" {...input} />
      </div>
      <div className="pt-4" />
      <Button
        type="submit"
        className="px-4 py-2 text-xl bg-pri hover:bg-pri-3"
        onClick={(e) => {
          submit(history, state, ldispatch)
          e.preventDefault()
        }}
      >Register</Button>
      <div className="w-full flex-col pt-8">
        <label className="flex items-center cursor-pointer">
          Remember me:&nbsp;
          <Checkbox
            checked={state.rememberMe}
            onChange={(e) => ldispatch({type: "toggle-check", rememberMe: e.target.checked})}
          />
        </label>
        <span>
          Already have an account?
          <NavLink
            to={(l) => ({...l, pathname: "/login"})}
            className="mt-4 text-pri"
          >{" "}Login</NavLink>
        </span>
      </div>
    </form>
  </article>
}

const InputStreet = ({label, name, state, dispatch, autoComplete}) => {
  let f = state.form[name]
  return <label className="flex flex-col w-full relative my-4">
    <Required label={label} required={f.required} />
    <input
      // @ts-ignore
      autoComplete={autoComplete}
      className="relative border-b-2 py-1 px-2 focus:border-pri w-full outline-none border-gray-3"
      name={name}
      type={f.type}
      value={f.value}
      onChange={(e) => dispatch({type: "update-field", target: e.target})}
    />
  </label>
}

function validate(state, ldispatch, target) {
  api.postSessionValidate(
    { [target.name]: state.form[target.name].value,
    },
    (errors) => ldispatch({type: "validate-form", errors: errors, target: target}),
    () => ldispatch({type: "validate-form", errors: {"email": ["Error communicating with server."]}, target: {name: "email"}}),
  )
}

function submit(history, state, ldispatch) {
  const f = (field) => state.form[field].value
  let zipCode = parseInt(f("zipCode"))
  if (isNaN(zipCode)) {
    ldispatch({type: "validate-form", target: {name: "email"}, errors: {email: ["Zip Code must be a number."]}})
    return
  }
  dispatch({
    filter: "root", type: "register",
    body: {
      remember: state.rememberMe,
      email: f("email"),
      password: f("password"),
      streetAddress: f("streetAddress"),
      city: f("city"),
      state: f("state"),
      zipCode: zipCode,
    },
    onSuccess: () => {
      history.push("/calendar")
    },
    onError: () => {
      ldispatch({type: "validate-form", target: {name: "email"}, errors: {email: ["Email or password is incorrect."]}})
    },
  })
}

const reducer = produce((s,a) => {
  const f = s.form[get(a, ["target","name"])]
  switch (a.type) {
    case "validate-form":     f.errors = a.errors[a.target.name] ?? []; break
    case "update-field":      f.value = a.target.value; break
    case "toggle-visibility": f.visible = !f.visible; break
    case "toggle-check":      s.rememberMe = a.rememberMe; break
    case "toggle-address":    s.address = !s.address; break
    case "place-changed":
      for (const field of ["city", "state", "zipCode", "streetAddress"]) {
        s.form[field].value = a.place[field]
      }
      break
    default: break;
  }
})

const initState = () => {
  let state = {
    address:false,
    rememberMe: false,
    form: createForm({
      email: {placeHolder: "Enter your email"},
      password: {placeHolder: "Enter a password"},
      streetAddress: {},
      city: {},
      state: {},
      zipCode: {},
    })
  }

  return state
}
