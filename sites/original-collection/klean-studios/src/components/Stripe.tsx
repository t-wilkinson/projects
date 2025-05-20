import produce, {enableES5} from "immer"
import React from "react"
import {useStore, dispatch} from '../store'
import {
  Elements,
  CardElement,
  useStripe,
  useElements
} from "@stripe/react-stripe-js"
import {
  Button, AnimPlane,
  isAdmin, validateCents,
} from "../components"
import { promise } from "../components/StripeKey"

enableES5()

export default ({className="", ...props}) => {
  return <div
    className={`col justify-center ${className}`}
    {...props}
  >
    <Elements stripe={promise}>
      <CheckoutForm />
    </Elements>
  </div>
}

const reducer = produce((s,a) => {
  switch(a.type) {
    case 'succeeded':     s.processing = false; s.succeeded = true; break
    case 'error':         s.error = a.payload; break
    case 'processing':    s.processing = a.payload; break
    case 'disabled':      s.disabled = a.payload; break
    case 'client-secret': s.clientSecret = a.payload; break
    case 'charge':        s.charge = a.payload; break
    case 'paying':        s.paying = a.payload ?? !s.paying; break
    default:              break
  }
})

export function CheckoutForm() {
  const [state, ldispatch] = React.useReducer(reducer, {
    succeeded: false,
    error: null,
    processing: false,
    disabled: true,
    clientSecret: null,
    charge: 'LOADING',
    paying: false,
  })

  const calendar = useStore(s=>s.calendar)
  const root = useStore(s => s.root)
  const stripe   = useStripe()
  const elements = useElements()

  // Create payment intent on server
  React.useEffect(() => {
    if (state.paying) {
      dispatch({
        filter: 'stripe', type: 'createPayment',
        onSuccess: (res) => {
          ldispatch({type: 'client-secret', payload: res.clientSecret})
          ldispatch({type: 'charge', payload: res.charge})
        },
        onError: (e) => ldispatch({type: 'error', payload: 'Could not communicate with server.'}),
      })
    } else {
      dispatch({
        filter: "dashboard", type: "getCharge",
        onSuccess: c=>ldispatch({type: 'charge', payload: c}),
          onError: e=>ldispatch({type: 'charge', payload: "ERROR"}),
      })
    }
  }, [state.paying, root.user, calendar.ownedDays])

  // Listen for changes in the CardElement
  const handleChange = async (event) => {
    ldispatch({type: 'disabled', payload: event.empty})
    ldispatch({type: 'error', payload: event.error ? event.error.message : ""})
  }

  const handleSubmit = async ev => {
    ev.preventDefault()
    ldispatch({type: 'processing', payload: true})
    const payload = await stripe.confirmCardPayment(state.clientSecret, {
      receipt_email: root.user,
      payment_method: {
        card: elements.getElement(CardElement),
        billing_details: {
          name: ev.target.name.value
        }
      }
    })
    if (payload.error) {
      ldispatch({type: 'error', payload: `Payment failed ${payload.error.message}`})
      ldispatch({type: 'processing', payload: false})
    } else {
      ldispatch({type: 'error', payload: null})
      ldispatch({type: 'processing', payload: false})
      ldispatch({type: 'succeeded', payload: true})
    }
  }

  if (!validateCents(state.charge) || isAdmin(root.user)) {
    return null

  } else if (!state.paying) {
    return <div className="col space-y-4">
      <span>
        You currently owe{" "}
        <strong>{"$" + state.charge}</strong>
      </span>
      <Button
        onClick={() => ldispatch({type: 'paying'})}
      >Make a payment</Button>
    </div>

  } else if (!state.paying) {
    return <span>Your payments are all caught up!</span>

  } else if (state.succeeded) {
    return <p
      className={`h-full w-96 sm:w-128 text-xl text-center`}>
      Success! Thank you for choosing &nbsp;<strong className="block">Klean Studios.</strong>
    </p>

  } else {
    return <form
      onSubmit={handleSubmit}
      className="h-32 w-96 sm:w-128 flex flex-col justify-between items-center
      px-4 py-2 text-black"
    >
      <div className="flex flex-col w-full">
        <CardElement
          options={{
            classes: {
              base: "text-black font-base placeholder-gray",
              invalid: "text-err fill-current",
            },
          }}
          onChange={handleChange}
        />
      </div>
      <div className="w-full mx-4 bg-black h-px" />
      {state.error && <div role="alert" className="text-err">{state.error}</div>}
      {state.processing ?
        <AnimPlane className="w-8 h-8 bg-pri" />
        : <Button
          disabled={state.disabled || state.succeeded}
          type="submit"
        >
          <span>Pay{" "}
            <strong>${state.charge}</strong>
          </span>
        </Button>
      }
    </form>
  }
}


