import React from 'react'
import { theme, Button, Input } from '../components'
import { useRouter } from 'next/router'

export default function Retrieve() {
  return <main>
    <h2 style={{fontFamily: theme.fontPri, textAlign: 'center', fontSize: '4em'}}>Retrieve Styles</h2>
    <Form />
  </main>
}

const def = (value, fallback) => value === null || value === undefined ? fallback : value

function Form() {
  const [images, setImages] = React.useState({})
  const router = useRouter()

  React.useEffect(() => {
    let user = router.query.user
    if (user === null || user === undefined || user === '') return
    retrieveTokens(def(router.query.user, ''), images, setImages, router)
  }, [router.query.user])

  return <form
    onSubmit={(e) => {
      e.preventDefault()
      retrieveTokens(e.target.email.value, images, setImages, router)
    }}
  >
    <style jsx>{`
      form {
        display: grid;
        justify-items: center;
        gap: 4rem;
      }
    `}</style>
    <Input type='email' name='email' label='Email' defaultValue={router.query.user ?? ''} />
    <Button>Submit</Button>
    {Object.entries(images).map(([token, url]) => {
      return <div key={token}>
        <img src={url} alt='Loading...' />
        </div>
    })}
  </form>
}

async function retrieveTokens(email, images, setImages, router) {
  console.log(email)
  if (email !== router.query.user)
    router.replace(`${router.pathname}?user=${email}`)

  const tokens = await fetch('/api/tokens',{
    method: 'POST',
    body: JSON.stringify({
      email: email,
    })
  })
    .then((res) => res.json())
    .then((res) => res.tokens)
    .catch((err) => [])

  tokens.forEach((token) => {
    // Don't send request for existing images. They will never change.
    if (token in images) return

    fetch('/api/retrieve', {
      method: 'POST',
      body: JSON.stringify({token}),
    })
      .then((res) => {
        if (res.ok) return res.blob()
        else throw new Error('Response is not ok')
      })
      .then((res) => {
        if (res.length === 0) throw new Error('Response has no value')
        setImages(images => ({[token]: URL.createObjectURL(res), ...images}))
      })
      .catch((err) => {})
  })
}
