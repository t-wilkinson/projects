import React from 'react'
import Link from 'next/link'

export const theme = {
  colPri: '#16ff03',
  colError: '#ff3000',
  fontPri: `'Sarpanch', sans-serif`,
  fontSec: `'Raleway', sans-serif`,
}

function A({children, href}) {
  return <Link href={href}>
    {children}
  </Link>
}

export function Header() {
  return <header>
    <style jsx>{`

      header {
        position: fixed;
        display: grid;
        grid-template-columns: 1fr 500px 1fr;
        align-items: center;
        width: 100%;
      }

      nav { text-transform: uppercase; font-family: ${theme.fontPri}; grid-column: 2; }
      ul { display: flex; justify-content: space-between; }
      li { font-family: ${theme.fontPri}; }

      `}</style>
    <nav>
      <ul>
        <li><A href='/'>Home</A></li>
        <li><A href='/styles'>Styles</A></li>
        <li><A href='/about'>About</A></li>
      </ul>
    </nav>
  </header>
}

export function Button({children, disabled=false, ...props}) {
  return <button tabIndex={0} className='box' disabled={disabled} {...props}>
    <style jsx>{`
      button {
        position: relative;
        display: inline-block;
        width: 20rem;
        padding: 0.5rem 1rem;
        cursor: pointer;
        font-family: ${theme.fontPri};
        font-size: 1.25rem;
        background: ${theme.colPri};
        color: black;
      }

      button:focus { outline: none; }
      button:active::after, button:focus::after, button:hover::after {
        transform: translate(5px, 5px);
      }

    `}</style>
    {children}
  </button>
}

export function Input({type, name, label, ...props}) {
  return <label className='field'>
    <style jsx>{`
    .field {
      position: relative;
      width: 20em;
      height: 3em;
    }
    .field__input { padding-left: 1em; }
    .field__input:focus ~ .field__bg { transform: translate(5px, 5px); }
    .field__input:focus { outline: none; }
    .field__input:focus ~ .field__label { transform: translateY(-0.75em); font-size: 0.7em; }
    .field__input:valid ~ .field__label { transform: translateY(-0.75em); font-size: 0.7em; }
    .field__input:invalid { border: 2px solid ${theme.colError}; outline: none; box-shadow: none; }
    .field__bg { transform: translate(10px, 10px); background: black; z-index: -1; transition: all 0.25s ease-in-out; }
    .field__bg, .field__input {
      border: 2px solid black;
      position: absolute;
      top: 0;
      width: 100%;
      height: 100%;
    }
    .field__label {
      transition: all 0.25s ease-in-out;
      color: black;
      position: absolute;
      left: 0;
      top: 0;
      line-height: 3em;
      margin-left: 1em;
    }
    `}</style>
    <input className='field__input box' type={type} name={name} required {...props} />
    <div className='field__bg' />
    <span className='field__label'>{label}</span>
  </label>
}
