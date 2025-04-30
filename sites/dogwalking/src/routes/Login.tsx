import React from "react"
import produce, {enableES5} from "immer"
import {useHistory, NavLink} from "react-router-dom"
import * as api from "../api"
import {dispatch, actions} from "../store"
import {
  get, createForm,
  Checkbox, Input, Button,
} from "../components"

enableES5()

const {header: {form}} = actions

export default () => {
  let [state, ldispatch] = React.useReducer(reducer, initState())
  let history = useHistory()

  React.useEffect(() => {
    dispatch({
      filter: "root", type: "getUsers",
      onSuccess: (res) => ldispatch({type: "get-users", payload: res}),
      onError: () => {},
    })
  }, [])

  return <article
    className="relative col justify-center bg-pri-2 h-full min-h-screen py-8"
  >
      <div
        className="bg-white w-full sm:w-96 px-8 py-4 sm:rounded-lg border border-gray"
      >
        {state.pickingAccounts ?
          <PickAccounts state={state} ldispatch={ldispatch} history={history} />
          : <Form state={state} ldispatch={ldispatch} history={history} />
        }
      </div>
    </article>
}

const PickAccounts = ({state, ldispatch, history}) =>
  <div
    className="relative col py-4 rounded-lg space-y-8"
  >
    <span className="col">
      <h3>Login</h3>
      <strong>Select an existing account.</strong>
    </span>
    <div
      className="space-y-6 my-6 w-full"
    >
      {state.users.map(u=><Account key={u} user={u} history={history} />)}
    </div>
    <Button
      className="bg-white hover:bg-pri"
      onClick={() => ldispatch({type: "pick-account", payload: false})}
    >
      <strong className="text-pri">+</strong>Add another account
    </Button>
    <span className="mt-8 w-full">
      Don't have an account?{" "}
      <NavLink
        to={(l) => ({...l, pathname: "/register"})}
        className="mt-4 text-pri"
      >Register</NavLink>
    </span>
  </div>

const Account = ({user, history}) => {
  let [token, email] = user
  return <Button
    className="w-full border-solid border px-4 py-2 bg-pri-2
    cursor-pointer flex justify-center bg-gray-3 hover:bg-sec"
    onClick={(e) => {
      dispatch({filter: "root", type: "changeUser", history: history, token: token})
    }}
    >
    {email}
    </Button>
}

const Form = ({state, ldispatch, history}) => {
  // Values to pass to <Input />
  let input = {validate: validate, state: state, dispatch: ldispatch}

  return <form
    className="relative col py-8 rounded-lg"
  >
    <h3>Login</h3>
    <Input field="email" {...input} />
    <Input field="password" {...input} />
    <div className="pt-4" />
    <Button
      type="submit"
      className="px-4 py-2 text-xl bg-pri hover:bg-pri-3"
      onClick={(e) => {
        submit(history, state, ldispatch)
        e.preventDefault()
      }}
    >Login</Button>
    <div className="w-full flex-col pt-8">
      <label className="flex items-center cursor-pointer">
        Remember me:&nbsp;
        <Checkbox
          checked={state.rememberMe}
          onChange={(e) => ldispatch({type: "toggle-check", rememberMe: e.target.checked})}
        />
      </label>
      <span>
        Don't have an account?{" "}
        <NavLink
          to={(l) => ({...l, pathname: "/register"})}
          className="mt-4 text-pri"
        >Register</NavLink>
      </span>
    </div>
  </form>
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
    filter: "root", type: "login",
    body: {
      email: f("email"),
      password: f("password"),
      remember: state.rememberMe,
    },
    onSuccess: () => {
      history.push("/calendar")
    },
    onError: () => {
      ldispatch({type: "validate-form", target: {name: "email"}, errors: {email: ["Email or password is incorrect."]}})
    },
  })
}

const reducer = produce((s, a) => {
  const f = s.form[get(a, ["target","name"])]
  switch (a.type) {
    case "validate-form":     f.errors = a.errors[a.target.name] || []; break
    case "update-field":      f.value = a.target.value; break
    case "toggle-visibility": f.visible = !f.visible; break
    case "toggle-check":      s.rememberMe = a.rememberMe; break
    case "pick-account":      s.pickingAccounts = a.payload; break
    case "get-users":
      s.users = a.payload
      s.pickingAccounts = Boolean(a.payload.length)
      break
    default: break;
  }
})

const initState = () => {
  let state = {
    accounts: [],
    pickingAccounts: false,
    rememberMe: false,
    form: createForm({
      email: {placeHolder: "Enter your email"},
      password: {placeHolder: "Enter a password"},
    })
  }

  return state
}
