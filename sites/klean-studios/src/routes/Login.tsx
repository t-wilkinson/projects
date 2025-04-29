import produce, {enableES5} from "immer"
import React from "react"
import {useHistory} from "react-router-dom"
import * as api from "../api"
import {dispatch} from "../store"
import {
  get, createForm,
  A, Button, Checkbox, Input,
} from "../components"

enableES5()

export default () => {
  let [state, ldispatch] = React.useReducer(reducer, initState())
  let history = useHistory()

  React.useEffect(() => {
    document.title = "Login | Klean Studios"
  }, [])

  React.useEffect(() => {
    dispatch({
      filter: "root", type: "getUsers",
      onSuccess: (res) => ldispatch({type: "get-users", payload: res}),
      onError: () => {},
    })
  }, [])

  return <article
    className="relative col justify-center bg-pri-1"
    style={{height: "calc(100vh)"}}
  >
      <div
        className="relative bg-white w-full sm:w-96 px-8 py-8 col border border-gray"
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
    className="relative col"
  >
    <h2>Login</h2>
    <strong>Select an existing account.</strong>
    <div
      className="space-y-6 my-6 w-full"
    >
      {state.users.map(u=><Account key={u} user={u} history={history} />)}
    </div>
    <Button
      onClick={() => ldispatch({type: "pick-account", payload: false})}
    >
      + Add another account
    </Button>
  </div>

const Account = ({user, history}) => {
  let [token, email] = user
  return <Button
    className="w-full border-solid border px-4 py-2 bg-pri-2
    cursor-pointer flex justify-center bg-gray hover:bg-sec"
    onClick={(e) => {
      dispatch({filter: "root", type: "changeUser", history: history, token: token})
    }}
    >
    {email}
    </Button>
}

const Form = ({state, ldispatch, history}) => {
  let input = {onBlur: validate, state: state, dispatch: ldispatch}

  return <form
    className="relative col w-full"
  >
    <h2>Login</h2>
    <div className="pt-4" />
    <Input {...input} field="email" />
    <Input {...input} field="password" />
    <div className="pt-4" />
    <Button
      type="submit"
      className="mt-4 px-4 py-2"
      bg="pri"
      onClick={(e) => {
        submit(history, state, ldispatch)
        e.preventDefault()
      }}
    >Login</Button>
    <div className="w-full flex-col pt-8">
      <span>
        <label className="flex items-center cursor-pointer inline">
          Remember me:&nbsp;
          <Checkbox
            checked={state.checked}
            onChange={(e) => ldispatch({type: "toggle-check", checked: e.target.checked})}
          />
        </label>
      </span>
      Don't have an account?{" "}
      <A to="/register" className="font-sec">Register</A>
    </div>
  </form>
}


function validate(state, ldispatch, target) {
  let f = state.form
  api.postSessionValidate(
    { [target.name]: f[target.name].value,
    },
    (errors) => ldispatch({type: "validate-form", errors: errors, target: target,}),
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
      remember: state.checked,
    },
    onSuccess: () => {
      history.push("/dashboard" || history.location.s)
    },
    onError: () => {
      ldispatch({type: "validate-form", target: {name: "email"}, errors: {email: ["Email or password is incorrect."]}})
    },
  })
}

type field = {
  type: string,
  value: string,
  dispatch: (type: string, field: string, value: string) => null,
  validate: (type: string) => [string],
}

const reducer = produce((s, a) => {
  const name = get(a, ["target", "name"])
  const f = s.form[name]
  switch (a.type) {
    case "get-users":
      s.users = a.payload
      s.pickingAccounts = Boolean(a.payload.length)
      break
    case "validate-form":
      if (a.errors && f)
        f.errors = a.errors[name] ?? []
      break
    case "update-field":      f.value = a.target.value; break
    case "toggle-visibility": f.visible = !f.visible; break
    case "toggle-check":      s.checked = a.checked; break
    case "pick-account":      s.pickingAccounts = a.payload; break
    default: break;
  }
})

const initState = () => {
  let state = {
    users: [],
    pickingAccounts: false,
    checked: false,
    form: createForm({
      email: {},
      password: {},
    })
  }
  return state
}
