import React from "react"
import {Link} from "react-router-dom"
import {
  SocialMedia, Logo, Button,
} from "./"

export default (props: any) => {
  return <footer
    className="col justify-evenly bg-pri-2 py-16 w-full space-y-8
    border-t-2 md:flex-row"
  >
    <div className="col">
      <Link to="/" className="flex items-center text-black hover:text-pri">
        <Logo className="h-8" />
        <h1 className="text-4xl text-black">Paw Pals</h1>
      </Link>
      <div className="flex space-x-6">
        <SocialMedia
          className="w-10 text-black fill-current"
        />
      </div>
    </div>
    <Link
      to="/calendar"
      className="text-2xl text-black"
    >
      <Button className="bg-white hover:bg-pri">Schedule a service</Button>
    </Link>
  </footer>
}
