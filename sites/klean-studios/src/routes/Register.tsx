import React from "react"
import produce, {enableES5} from "immer"
import * as api from "../api"
import {useHistory} from "react-router-dom"
import {dispatch} from "../store"
import {
  A, Button, Checkbox, Input,
  get, createForm,
} from "../components"

enableES5()

export default () => {
  let [state, ldispatch] = React.useReducer(reducer, initState())
  let history = useHistory()
  let input = {onBlur: validate, state: state, dispatch: ldispatch}

  React.useEffect(() => {
    document.title = "Register | Klean Studios"
  }, [])

  return <article
    className="relative bg-pri-1 col justify-center"
    style={{height: "calc(100vh)"}}
  >
    <form
      className="relative bg-white w-full sm:w-96 px-8 py-8 col border border-gray"
    >
      <h2>Register</h2>
      {/* <strong>Request a service in seconds.</strong> */}
      <div className="pt-4" />
      <Input {...input} field="name" />
      <Input {...input} field="email" />
      <Input {...input} field="password" />
      <div className="pt-4" />
      <Button
        bg="pri"
        className="mt-4"
        onClick={(e) => {
          submit(history, state, ldispatch)
          e.preventDefault()
        }}
      >Register</Button>
      <div className="w-full flex-col pt-8">
        <label className="flex items-center">
          Remember me:&nbsp;
          <Checkbox
            checked={state.checked}
            onChange={(e) => ldispatch({type: "toggle-check", checked: e.target.checked})}
          />
        </label>
        <span>
          Already have an account?{" "}
          <A to="/login" className="font-sec">Login</A>
        </span>
      </div>
    </form>
  </article>
}

function validate(state, ldispatch, target) {
  let f = state.form
  api.postSessionValidate({
      [target.name]: f[target.name].value,
    },
    (errors) => ldispatch({type: "validate-form", errors: errors, target: target}),
    () => ldispatch({type: "validate-form", errors: {email: ["Error communicating with server."]}, target: {name: "email"}}),
  )
}

function submit(history, state, ldispatch) {
  const f = (field) => state.form[field].value
  dispatch({
    filter: "root", type: "register",
    body: {
      name: f("name"),
      email: f("email"),
      password: f("password"),
      remember: state.checked,
    },
    onSuccess: () => {
      history.push("/dashboard" || history.location.s)
    },
    onError: (err) => {
      ldispatch({type: "validate-form", target: {name: "name"}, errors: {name: [err]}})
    },
  })
}

const reducer = produce((s, a) => {
  const f = s.form[get(a, ["target", "name"])]
  switch (a.type) {
    case "validate-form": f.errors = a.errors[a.target.name] || []; break
    case "update-field": f.value = a.target.value; break
    case "toggle-visibility": f.visible = !f.visible; break
    case "toggle-check": s.checked = a.checked; break
    default: break
  }
})

const initState = () => {
  let state = {
    address: false,
    checked: false,
    form: createForm({
      name: {},
      email: {},
      password: {},
    })
  }
  return state
}

