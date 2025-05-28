import React from "react"
import {
  Button,
  basicAuth,
} from "./"
import * as api from "../api"
import {dispatch, useStore} from "../store"

const square = (window as any) // for square

function uuidv4() {
  return 'xxxxxxxx-xxxx-4xxx-yxxx-xxxxxxxxxxxx'.replace(/[xy]/g, function(c) {
    var r = Math.random() * 16 | 0, v = c === 'x' ? r : ((r & 0x3) | 0x8)
    return v.toString(16)
  })
}

export default (props) => {
  const [state, setState]: any = React.useState({
    form: null,
    charge: null,
  })
  const root = useStore(s=>s.root)
  const idempotency_key = uuidv4()

  function onGetCardNonce(e) {
    e.preventDefault()
    state.form.requestCardNonce()
  }

//   function setPayButtonDisableState(newState) {
//     var payButton = document.getElementById("sq-creditcard")
//     // @ts-ignore
//     payButton.disabled = newState
//     var buttonContent = payButton.innerHTML; // redraw
//     payButton.innerHTML = buttonContent;
//   }

  React.useEffect(() => {
    dispatch({
      filter: "dashboard", type: "getCharge",
      onSuccess: (c)=>setState(s=>({...s, charge: c}))
    })
  }, [root])

  React.useEffect(() => {
    if (state.form !== null) return

    const paymentForm = new square.SqPaymentForm({
      applicationId: "sandbox-sq0idb-BghWCgkaKuNJZh7KA1fJXw",
      autoBuild: false,
      // Initialize the credit card placeholders
      card: {
        elementId: "sq-card",
        inputStyle: {
          autoFillColor: "#ccc",
          color: "#000",
          fontSize: "16px",
          fontWeight: 500,
          cardIconColor: "#A5A5A5",
          // boxShadow: "0 0 0 0",
          borderRadius: "0",
        },
        error: {
          color: "#ee2244",
        },
        details: {
          errors: {
          },
        },
      },
      // SqPaymentForm callback functions
      callbacks: {
        // Triggered when: SqPaymentForm completes a card nonce request
        cardNonceResponseReceived: function (errors, nonce, cardData) {
          if (errors) {
            return
          }
          api.postPaymentConfirm(
            { nonce,
              uuid: idempotency_key,
            },
            basicAuth(root),
            () => {},
            () => {},
          )
        }
      }
    })
    paymentForm.build()

    setState(s=>({...s, form: paymentForm}))
  }, [state.form, setState, root, idempotency_key])

  return <div
    className="h-32 w-96 sm:w-128 flex flex-col justify-between items-center
    px-4 py-2 text-black"
  >
    <div className="flex flex-col w-full">
      <div id="form-container">
        <div id="sq-card"/>
      </div>
    </div>
    <Button id="sq-creditcard" onClick={(e) => onGetCardNonce(e)}>
      Pay{" "}<strong>${state.charge}</strong>
    </Button>
  </div>
}

