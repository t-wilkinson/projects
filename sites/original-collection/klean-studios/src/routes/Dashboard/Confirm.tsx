import produce, {enableES5} from "immer"
import React from "react"
import { loadStripe } from "@stripe/stripe-js";
import {useStore} from '../../store'
import { Elements, CardElement, useStripe, useElements} from "@stripe/react-stripe-js"
import {
  A, Button, AnimPlane,
  parseCents, validateCents, basicAuth, isAdmin, toyyyymmdd, ymdToUTC, isEmpty,
} from "../../components"
import * as api from "../../api"

enableES5()

const promise = loadStripe("pk_test_51H8AIrBDyoEFvA4xHLGQAs22IXsjwF76ng5spTRo37RCLAI7Rb3eZU1XkVBrgB7bTerH6W4EN0WaBeQHKkbGrmb600jyeC8TYB");

export default ({className="", ...props}) => {
  const confirmService = useStore(s=>s.confirmService)
  React.useEffect(() => {
    document.title = "Dashboard | Klean Studios"
  }, [])

  if (isEmpty(confirmService)) {
    props.history.push("/dashboard")
    return <></>
  }

  return <main className="bg-gray-3 w-full h-128 col justify-center">
    <div className="col justify-center">
      <Elements stripe={promise}>
        <CheckoutForm confirmService={confirmService} />
      </Elements>
    </div>
  </main>
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
    case 'createdIntent': s.createdIntent = true; break
    case 'service-token': s.token = a.payload; break;
    default:              break
  }
})

export function CheckoutForm({confirmService}) {
  const [state, ldispatch] = React.useReducer(reducer, {
    succeeded: false,
    error: null,
    processing: false,
    disabled: true,
    clientSecret: null,
    charge: 'LOADING',
    paying: false,
    createdIntent: false,
  })

  const calendar = useStore(s=>s.calendar)
  const root = useStore(s => s.root)
  const stripe   = useStripe()
  const elements = useElements()

  // Create payment intent on server
  React.useEffect(() => {
      let date = toyyyymmdd(calendar)
      let utc = ymdToUTC(date, confirmService.start, confirmService.end)
      let [year, month, day] = utc.date.split(/-/).map(v=>parseInt(v))
      let {description, tracks, mixing, mastering, recording} = confirmService
      if (state.createdIntent) return

      if (typeof confirmService.token === "string") { // creating a new service
        api.postPaymentFeeIntentByToken(
          confirmService.token,
          { description,
            date: {day, month, year},
            tracks: tracks,
            start: utc.start, end: utc.end,
            mixing, mastering, recording,
          },
          basicAuth(root),
          (res)=> {
            ldispatch({type: "createdIntent"})
            ldispatch({type: "charge", payload: parseCents(res.charge)})
            ldispatch({type: "client-secret", payload: res.clientSecret})
            ldispatch({type: "service-token", payload: res.token})
          },
          (e)=>ldispatch({type: 'error', payload: 'Could not communicate with server.'}),
        )

      } else { // Create payment intent for existing service
        api.postPaymentFeeIntent(
          { description,
            date: {day, month, year},
            tracks: tracks,
            start: utc.start, end: utc.end,
            mixing, mastering, recording,
          },
          basicAuth(root),
          (res)=> {
            ldispatch({type: "createdIntent"})
            ldispatch({type: "charge", payload: parseCents(res.charge)})
            ldispatch({type: "client-secret", payload: res.clientSecret})
            ldispatch({type: "service-token", payload: res.token})
          },
          (e)=>ldispatch({type: 'error', payload: 'Could not communicate with server.'}),
        )

      }

  }, [state.createdIntent, state.paying, root, calendar, confirmService])

  // Listen for changes in the CardElement
  const handleChange = async (event) => {
    ldispatch({type: 'disabled', payload: event.empty})
    ldispatch({type: 'error', payload: event.error ? event.error.message : ""})
  }

  const handleSubmit = async ev => {
    ev.preventDefault()
    ev.persist()
    ldispatch({type: 'processing', payload: true})
    const payload: any = await new Promise((res,rej)=>{
    api.putServiceProcessingByToken(
      state.token,
      basicAuth(root),
      ()=>{
        const payload = stripe.confirmCardPayment(state.clientSecret, {
          receipt_email: root.user,
          payment_method: {
            card: elements.getElement(CardElement),
            billing_details: {
              name: ev.target.name.value
            }
          }
        })
        res(payload)
      },
      (err)=> {
        rej({error: err})
      }
    )})

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
        Please pay{" "}
        <strong>{"$" + state.charge}</strong>
        {" "}to secure this session.
      </span>
        <small className="text-xs">Charge is non-refundable.</small>
      <Button
        onClick={() => ldispatch({type: 'paying'})}
      >Make a payment</Button>
    </div>

  } else if (state.succeeded) {
    return <div className="space-y-8">
      <p
        className={`h-full w-96 sm:w-128 text-xl text-center`}>
        Success! Thank you for choosing &nbsp;<strong className="block">Klean Studios.</strong>
      </p>
      <p className="text-center w-full">
        Return to{" "}<A to="/dashboard">dashboard</A>?
      </p>
    </div>

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


