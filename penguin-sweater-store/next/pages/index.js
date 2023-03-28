/** @jsx jsx */
import Head from 'next/head'
import Link from 'next/link'
import React from 'react'
import { ApolloClient, InMemoryCache, gql } from '@apollo/client'
import {jsx, css} from '@emotion/react'
import { FontAwesomeIcon } from '@fortawesome/react-fontawesome'
import { useMediaQuery } from 'react-responsive'

// TODO
// const uri = 'http://penguins.treywilkinson.xyz:1337'
const uri = 'http://penguins.treywilkinson.com/strapi'

const theme = {
  font1: `font-family: 'Playfair Display', serif;`,
  font2: `font-family: Raleway, sans-serif;`,
  gray: `rgb(239, 239, 239)`,
  maxWidth: `1200px`,
  sm: `786px`,
}

const LG = ({ children }) => useMediaQuery({ minWidth: 992 }) ? children : null
const MD = ({ children }) => useMediaQuery({ minWidth: 851, maxWidth: 991 }) ? children : null
const SM = ({ children }) => useMediaQuery({ maxWidth: 850 }) ? children : null
const Default = ({ children }) => useMediaQuery({ minWidth: 850 }) ? children : null

export async function getStaticProps() {
  const client = new ApolloClient({
    uri: uri + '/graphql',
    cache: new InMemoryCache()
  })

  const {data} = await client.query({
    query: gql`
      query GetShopItems {
        shopItems(limit: 10) {
          id
          name
          price
          image {
            url
          }
        }
        newShopItem: shopItem(id: 3) {
          id
          name
          price
          image {
            url
          }
        }
      }
    `
  })

  return {
    props: {
      shopItems: data.shopItems,
      newShopItem: data.newShopItem,
    }
  }
}

const Button = ({...props}) => {
  return <button css={{
    border: '1px solid tomato',
    borderRadius: '1px',
    fontSize: '1.2rem',
    width: '100%',
    padding: '0.5rem 0.5rem',
    '&:hover': {
      background: 'tomato',
      cursor: 'pointer',
    },
  }}
    type='button'
    {...props}
  >
  </button>
}

const H3 = ({children, ...props}) => {
  return <h3 css={css`font-size: 4rem; display: inline-block; text-decoration: underline tomato;
    @media(max-width: 768px) {font-size: 3rem;}
    `} {...props}>
    {children}
  </h3>
}

// markup
export default function Home({newShopItem, shopItems}) {
  const [hidden, setHidden] = React.useState(true)
  const [buttonPressed, setButtonPressed] = React.useState(false)
  const [cart, setCart] = React.useState(null)

  React.useEffect(() => {
    setCart(JSON.parse(window.localStorage.getItem('cart')) || {})
  }, [setCart])

  React.useEffect(() => {
    window.localStorage.setItem('cart', JSON.stringify(cart || {}))
  }, [setCart, cart])

  return (<div>
    <Head>
      <link rel="preconnect" href="https://fonts.gstatic.com" />
      <link href="https://fonts.googleapis.com/css2?family=Playfair+Display&family=Raleway&display=swap" rel="stylesheet" />
      <meta name="viewport" content="initial-scale=1.0, width=device-width" />
      <script src="https://cdnjs.cloudflare.com/ajax/libs/gsap/3.5.1/gsap.min.js" />
      <title>Penguin Sweaters</title>
    </Head>
    { cart === null
      ? null
      : <ShoppingCart buttonPressed={buttonPressed} setButtonPressed={setButtonPressed} hidden={hidden} setHidden={setHidden} cart={cart} setCart={setCart} />
    }
      <Header setButtonPressed={setButtonPressed} setHidden={setHidden} />
    <main css={css`
    color: #232129;
    font-family: Raleway, sans-serif;
    display: grid;
    place-items: center;
    text-align: center;
    /* padding: 0 1rem; */
    `}>
      <img src="https://www.treehugger.com/thmb/iKvoLTs4LRWxUrZqL4xRM63lPN0=/768x0/filters:no_upscale():max_bytes(150000):strip_icc()/__opt__aboutcom__coeus__resources__content_migration__mnn__images__2017__10__shutterstock_516950872-7950e29400bd42768a3d4eacdcbd6d73.jpg"
        alt="penguins wearing sweaters"
        css={css`
          width: 100%;
          max-height: 500px;
          object-fit: contain;
          background: #9eb2f1;
          padding: 1rem;
        `}
      />
      <div css={css`
    max-width: ${theme.maxWidth};
    width: 100%;
    width: 100vw;
    padding: 0 1rem;
    box-sizing: border-box;
  `}>
        <NewItem newShopItem={newShopItem} setCart={setCart} />
        <section css={css`margin-top: 1rem;
            @media(max-width:  ${theme.maxWidth - 400}){ display: flex; flex-direction: column;
            }
          `}>
          <H3>penguins need entertainment</H3>
          <div css={css`clear: both;`} />
          <video loop autoPlay muted css={css`margin-left: 8rem; float: right; height: 400px;
            @media(max-width:  ${theme.maxWidth}){ margin-left: 4rem; height: 300px; }
            @media(max-width:  ${1200 - 400}px){ margin-left: 0; float: none; width: 100%; height: auto; }
            `}>
            <source src='/penguin-fall.mp4' type='video/mp4' />
          </video>
          <p>
            The Antarctic is an incredibly hostile and uneventful environment. So much so that penguins regularly turn on each other for what little entertainment they can find. If we give them (ugly) sweaters, they can instead make fun of each others' sweaters, saving countless penguin lives.
          </p>
        </section>
        <div css={css`clear: both;`} />
        <Sweaters shopItems={shopItems} setCart={setCart} />
        <Testimonials />
        <NoSweaters />
      </div>
    </main>
  </div>
  )
}

const Header = ({setHidden, setButtonPressed}) => {
  const [scrollY, setScrollY] = React.useState(0)
  React.useEffect(() => {
    const onscroll = () => setScrollY(window.scrollY)
    document.addEventListener('scroll', onscroll)
    return () => document.removeEventListener('scroll', onscroll)
  }, [setScrollY])

  return <header css={css`
        ${scrollY > 532
          ? 'position: fixed; background: #9eb2f1; box-shadow: 0 1px 8px 0px rgb(0 0 0 / 20%); '
          : 'position: absolute;'
        }
        top: 0;
        width: 100%;
        display: flex;
        justify-content: center;
        color: white;
      `}>
    <div css={css`
        display: flex;
        width: 100%;
        max-width: ${theme.maxWidth};
        justify-content: space-between;
        padding: 1rem 1rem;
        ul { margin: 0; padding: 0; }
        list-style: none;
        `}>
      <Default>
          <ul css={css`
            list-style: none;
            display: flex;
            align-items: center;
            li { margin-right: 1rem; display: flex; align-items: center; }
            a { text-decoration: none; color: white; }
            `}>
            <li><Link href='/no-sweaters'><a>
                  shop&nbsp;<FontAwesomeIcon icon="angle-down" />
            </a></Link></li>
            <li><Link href='/no-sweaters'><a>
                  about us&nbsp;<FontAwesomeIcon icon="angle-down" />
            </a></Link></li>
          </ul>
          <h2 css={css`
            text-align: center;
            font-weight: 1200;
            ${theme.font1}
            font-size: 2.5em;
            margin: 0;
            `}>Penguin Sweaters</h2>
          <ul css={css`
            list-style: none;
            display: flex;
            align-items: center;
            li { margin-left: 1rem; display: flex; align-items: center; }
            `}>
            <li>USD&nbsp;<FontAwesomeIcon icon="dollar-sign" /></li>
            <li><button css={css`color: white;`} onClick={()=>{setHidden(false);setButtonPressed(true)}}>
              cart&nbsp;<FontAwesomeIcon icon="shopping-cart" />
            </button></li>
          </ul>
      </Default>
      <SM>
        <li><Link href='/no-sweaters'><a>
            <button css={css`color: white;`}>
            <FontAwesomeIcon icon="bars" />
            </button></a></Link></li>
          <li><button css={css`color: white;`} onClick={()=>{setHidden(false);setButtonPressed(true)}}>
            <FontAwesomeIcon icon="shopping-cart" />
          </button></li>
      </SM>
    </div>
  </header>
}

const NewItem = ({setCart, newShopItem}) => {
  const [amount, setAmount] = React.useState(1)

  React.useEffect(() => {
    if (typeof amount !== "number") {
      setAmount(parseInt(amount) || 1)
    } else if (amount <= 0) {
      setAmount(1)
    } else {
      setAmount(Math.round(amount) )
    }
  }, [amount, setAmount])

  return <section>
    <H3>penguins need sweaters</H3>
    <div css={css`display: flex;
      @media(max-width:  ${theme.sm}){ flex-direction: column; }
      `}>
      <div css={css`
    align-items: center;
    margin-right: 8rem;
    @media(max-width:  ${theme.maxWidth}){ margin-right: 4rem;}
    @media(max-width:  ${theme.maxWidth - 400}){ margin-right: 2rem;}
    width: 100%;
    flex: 1 1;
    img { object-fit: contain; width: 100%;}
    `}>
        <img alt="penguin wearing a sweater" src={uri + newShopItem.image.url} />
      </div>
      <div
        css={css`
      display: grid;
      justify-items: start;
      grid-gap: 2rem;
      flex: 1 3;
      `}
      >
        <strong css={css`font-size: 2rem; text-align: left;`}>
          {newShopItem.name} - new!
        </strong>
        <span>
          ${newShopItem.price * amount}
        </span>
        <span css={css`text-transform: uppercase;`}>quantity</span>
        <SelectAmount amount={amount} setAmount={setAmount} />
        <AddToCart item={newShopItem} setCart={setCart} amount={amount} />
        <p>
          The natural habitat of penguins is Antartica which one of the coldest places on Earth. Temperatures average around -20°C (or -20°C but in Fahrenheit for Americans) and can fall as low as -50°C (again -50°C but in Fahrenheit for Americans). Winds can reach up to 200km (or 200km but in mph for Americans).
        </p>
      </div>
    </div>
  </section>
}

const SelectAmount = ({setAmount, amount, ...props}) => {
  return <span css={css`
          display: inline-block;
          margin: 1px;
          &:focus-within { border: none; outline: none; border: 1px solid #232129; margin: 0; }

          button { border: none; background: ${theme.gray}; height: 100%; padding: 0.25rem;}
          button:focus { outline: none; border: none; }
          button:hover { cursor: pointer; background: tomato; }

          input { width: 2rem; border: none; text-align: center; padding: 0.25rem 0.25rem; }
          input:focus { outline: none; border; none; }
          `}>
    <button onClick={()=>setAmount(a=>a-1)}>-</button>
    <input value={amount} onBlur={(e)=>setAmount(e.target.value)} onChange={(e) => setAmount(e.target.value)} />
    <button onClick={()=>setAmount(a=>a+1)}>+</button>
  </span>
}

const Sweaters = ({shopItems, setCart}) => {
  return <section css={css`margin-top: 1rem;`}>
    <H3 css={css`margin-bottom: 0;`}>sweaters</H3>
    <ul css={css`padding: 0; margin: 1rem; justify-content: space-around; display: flex; flex-wrap: wrap; list-style: none; `}>
      {shopItems.map(item=><li key={item.id} css={css`width: 300px; margin: 2rem; 0.5rem;`}>
        <div css={css`height: 200px; padding: 1rem; background: ${theme.gray}; display: flex; align-items: center; `}>
          <img css={css`height: 100%; width: 100%; object-fit: contain; `} src={uri + item.image.url} alt="" />
        </div>
        <div css={css`margin: 1rem 0; display: grid; grid-gap: 0.5rem; place-items: center;`}>
          <strong css={css`${theme.font2};`}>{item.name}</strong>
          <ul css={css`list-style: none; margin: 0; padding: 0;`}>{Array(5).fill(0).map((v,i)=><i key={i} css={css`color: #f2bc47;`} className="fas fa-star" />)} {Math.floor(Math.random()*100)} reveiws</ul>
          <div>${item.price}</div>
        </div>
        <AddToCart item={item} setCart={setCart} />
      </li>)}
    </ul>
    <small>*Disclaimer: reviews are fake*</small>
  </section>
}

const AddToCart = ({setCart, item, amount=1, ...props}) => {
  return <Button {...props}
    onClick={(e)=>{
      setCart(cart=>{
        const prevItem = cart[item.id]
        const quantity = (prevItem
          ? prevItem.quantity || 1
          : 1)
          + amount
        return {...cart, [item.id]: {name: item.name, price: item.price, url: item.image.url, quantity: quantity}}
      })
    }}
  >
    add to cart
  </Button>
}

const Testimonials = () => {
  const testimonials = [
    {name: 'Donald Trump', testimonial: `I'm telling you these are GREAT! Don't listen to the FAKE NEWS saying that the penguin sweaters don't work, they do! By the way have I ever mentioned how smart I am? Because I am! Hillary's Emails.`},
    {name: 'Elon Musk', testimonial: `Let's nuke mars! Guess what?!? The penguin sweaters fit on my Baby Yoda doll.`},
    {name: 'Mumble', testimonial: `I'm Mumble (the guy from Happy Feet). These sweaters saved my colonies lives. I've bought over 9000 sweaters.`},
  ]
  return <section css={css`margin-top: 1rem;`}>
    <H3>testimonials</H3>
    <div css={css`overflow-x: auto;`}>
      <ul css={css`list-style: none; display: flex; height: 300px;
      li {
        position: relative;
        display: flex; flex-direction: column; justify-content: start; align-items: start;
        flex-basis: 400px; flex-grow: 0; flex-shrink: 0;
        border-radius: 0.5rem; padding: 1rem; margin: 1rem;
        background: ${theme.gray};
        * + * { margin-top: 1rem; }
        i { font-size: 2rem; }
        span { text-align: left; }
        span:nth-of-type(2) { font-weight: 900; }
      }
      `}>
        {testimonials.map((v,i) => <li key={i}>
          <FontAwesomeIcon icon="quote-left" />
          <span>{v.testimonial}</span>
          <span>- {v.name}</span>
        </li>)}
      </ul>
    </div>
  </section>
}

const NoSweaters = () => {
  return <section css={css`margin-top: 1rem;`}>
    <H3>penguins need help</H3>
    <p>
        OK but seriously, climate change is hurting artic creatures the most. Would you like to donate to penguin conservation efforts? If so please visit the
        {' '}<a target='_blank' href='https://penguinfoundation.org.au/get-involved/penguin-jumpers'>Penguin Foundation</a>
      {' '}or <a target='_blank' href='https://www.worldwildlife.org/'>World Wild Life</a> website.
    </p>
  </section>

}

const ShoppingCart = ({buttonPressed, setButtonPressed, hidden, setHidden, cart, setCart}) => {
  let refs = {}
  let tl = gsap.timeline({defaults: {duration: 0.5, delay: 0}})

  React.useEffect(() => {
    if (gsap === undefined
    || buttonPressed === false
    || refs.aside === null
    ) return

    if (hidden) {
      tl.to(refs.aside, {xPercent: 0, ease: 'power2.out'})
    } else {
      tl
        .to(refs.aside, {xPercent: -100, ease: 'power2.in'})
        .fromTo(refs.aside.children, {y: 50}, {stagger: 0.15, y: 0, ease: 'power1.out', duration: 0.35}, 0.10)
        .fromTo(refs.aside.children, {opacity: 0}, {stagger: 0.15, opacity: 1, ease: 'power1.in', duration: 0.35}, 0.10)
    }
    setButtonPressed(false)
  }, [hidden, refs, buttonPressed])

  return <aside ref={ref=>{refs.aside = ref}} css={css`
      position: fixed;
      right: 0; top: 0; bottom: 0;
      width: 400px;
      display: flex; flex-direction: column;
      z-index: 10;
      padding: 1rem;
      background: ${theme.gray};
      transform: translateX(100%);
      .close { position: absolute; top: 0; right: 0; margin: 1rem; z-index: 20; }
      .cart { font-size: 2rem; margin-bottom: 4rem; }
      ul { padding: 0; margin: 0; }
      @media(max-width: 800px) {width: 100%; box-sizing: border-box;}
      `}>
    <button className="close" onClick={()=>{setHidden(true);setButtonPressed(true)}}>
      <FontAwesomeIcon icon="times" />
    </button>
    <strong className='cart'>Cart</strong>
    {/* <ul css={css`flex: 1 0; display: flex; flex-direction: column; align-items: center; list-style:none; li + li { margin-top: 1rem; } `}> */}
      {Object.entries(cart).map(([id,v])=><CartItem key={id} setCart={setCart} id={id} {...v} />)}
    {/* </ul> */}
    <div css={{flex: '1 0'}} />
    <div css={css`display: flex; justify-content: space-between;`}>
      <span css={css`text-transform: uppercase;`}>subtotal</span>
      <span>${Object.values(cart).reduce((acc,v)=>acc+parseFloat(v.price * v.quantity), 0)}</span>
    </div>
    <Link href='/checkout'
    ><a
      css={{
        marginTop: '1rem',
        boxSizing: 'border-box',
        textAlign: 'center',
        border: '1px solid tomato',
        borderRadius: '1px',
        fontSize: '1.2rem',
        width: '100%',
        padding: '0.5rem 0.5rem',
        '&:hover': {
          background: 'tomato',
          cursor: 'pointer',
        },
      }}
      onClick={()=>window.localStorage.removeItem('cart')}
    >Check out</a></Link>
  </aside>
}

const CartItem = ({id, name, price, url, quantity, setCart}) => {
  const setQuantity = React.useCallback((f) => {
    if (typeof f === 'function') {
      setCart(cart=>{
        return {...cart, [id]: {name, price, url, quantity: f(quantity)}}
      })
    } else {
      setCart(cart=>{
        return {...cart, [id]: {name, price, url, quantity}}
      })
    }
  }, [id, name, price, url, setCart, quantity])

  React.useEffect(() => {
    if (typeof quantity !== "number") {
      setQuantity(parseInt(quantity) || 1)
    } else if (quantity <= 0) {
      setQuantity(1)
    } else {
      setQuantity(Math.round(quantity) )
    }
  }, [quantity, setQuantity])

  return <li css={css` display: flex; `}>
    <img alt={name} src={uri + url} css={css`width: 100px; object-fit: contain; `}/>
    <p css={css`display: flex; flex-direction: column; justify-content: start; margin-left: 1rem; `}>
      <span>{name}</span>
      <span css={css`display: flex; justify-content: space-between; margin-top: 0.5rem; `} >
        <SelectAmount amount={quantity} setAmount={setQuantity} />
        <span>${price * quantity}</span>
      </span>
    </p>
  </li>
}

