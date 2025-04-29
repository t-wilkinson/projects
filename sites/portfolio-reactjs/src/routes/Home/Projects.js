import React from 'react'
import Section from './Section'
import Project from './Project'

function PortfolioVideo({ src, type = 'video/mp4', ...props }) {
  return (
    <video
      key={src}
      src={'/assets/sites/' + src}
      autoPlay
      muted
      loop
      className="video"
    />
  )
}

export default function () {
  return (
    <Section to="projects">
      <Project
        views={[
          <img
            key="1"
            alt="Home page"
            src="/assets/sites/infinite-closet/home.png"
          />,
          <img
            key="2"
            alt="Clothing section"
            src="/assets/sites/infinite-closet/clothing.png"
          />,
        ]}
        alt="Infinite Closet fashion rental"
        src="/assets/sites/infinite-closet/intro.png"
        title="Infinite Closet"
        subtitle="Fashion rental"
        href="https://infinitecloset.co.uk"
        tools="Node.js,ReactJS,Next.js,Strapi,Responsive"
        color="#16ff03"
        img="contain"
      >
        <ul>
          <li>
            Platform which allows individuals to wear nice clothes while being
            sustainable and cost-friendly.
          </li>
          <li>
            The site integrates with a sustainable delivery and cleaning service
            to automate many actions required in the order lifecycle.
          </li>
        </ul>
      </Project>

      <Project
        views={[
          <img
            key="0"
            alt="Klean studios home page"
            src="/assets/sites/max.k-studio/home.png"
          />,
          <PortfolioVideo key="1" src="max.k-studio/home.mp4" />,
          <PortfolioVideo key="2" src="max.k-studio/portfolio.mp4" />,
        ]}
        alt="Klean Studios website"
        src="/assets/sites/max.k-studio/klean-studios.png"
        title="Klean Studios"
        subtitle="A modern website which allows clients to schedule music recoding time and to promote their work. "
        href="https://kleanstudio.com"
        tools="Haskell,TypeScript,ReactJS,RxJS,GSAP,Stripe API,Responsive"
        color="#000"
        img="contain"
      >
        <ul>
          <li>
            Provides a user dashboard to simplify life for the business owner.
          </li>
          <li>
            Clients can schedule recording times and get expected cost for
            session.
          </li>
          <li>After sessions, clients can pay online.</li>
          <li>
            Admin can easily view scheduled sessions, update/change existing
            ones and change service rates.
          </li>
        </ul>
      </Project>

      <Project
        views={[
          <img
            key="1"
            alt="Orange penguin sweater"
            src="/assets/sites/trey.w-penguins/orange-penguin-sweater.png"
          />,
          <img
            key="2"
            alt="Penguin sweaters store"
            src="/assets/sites/trey.w-penguins/penguin-sweaters-store.png"
          />,
          <img
            key="3"
            alt="Penguin sweaters"
            src="/assets/sites/trey.w-penguins/penguin-sweaters.jpg"
          />,
        ]}
        alt="Penguin sweaters store"
        src="/assets/sites/trey.w-penguins/home.png"
        title="Penguin Sweaters"
        subtitle="Sell hand-knitted penguin sweaters for the greater good of Antartica."
        href="http://penguins.treywilkinson.com"
        tools="Node.js,ReactJS,Next.js,Strapi,GraphQL,Responsive"
        color="#9eb2f1"
        img="contain"
      >
        <ul>
          <li>Sell awesome sweaters for penguins.</li>
          <li>Simple yet attractive UI.</li>
          <li>
            Makes anyone, whether a penguin owner or not, to want a penguin
            sweater.
          </li>
          <li>By the way I'm not actually selling penguin sweaters.</li>
        </ul>
      </Project>

      {/*       <Project */}
      {/*         views={[ */}
      {/*           <img */}
      {/*             key="1" */}
      {/*             alt="Home page" */}
      {/*             src="/assets/sites/trey.w-ai/home.png" */}
      {/*           />, */}
      {/*         ]} */}
      {/*         alt="Neural Style Transfer algorithm" */}
      {/*         src="/assets/sites/trey.w-ai/home.png" */}
      {/*         title="Neural Style Transfer Online" */}
      {/*         subtitle="NST implemented with PyTorch" */}
      {/*         href="http://ai.treywilkinson.com" */}
      {/*         tools="Node.js,PyTorch,ReactJS,Next.js" */}
      {/*         color="#16ff03" */}
      {/*         img="cover" */}
      {/*       > */}
      {/*         <ul> */}
      {/*           <li> */}
      {/*             A neural style transfer (nst) implementation using{' '} */}
      {/*             <a */}
      {/*               href="https://pytorch.org/" */}
      {/*               rel="noopener noreferrer" */}
      {/*               target="_blank" */}
      {/*             > */}
      {/*               PyTorch */}
      {/*             </a>{' '} */}
      {/*             and{' '} */}
      {/*             <a */}
      {/*               href="https://arxiv.org/abs/1508.06576" */}
      {/*               rel="noopener noreferrer" */}
      {/*               target="_blank" */}
      {/*             > */}
      {/*               this paper */}
      {/*             </a> */}
      {/*             . */}
      {/*           </li> */}
      {/*           <li>Written in Next.js and ReactJS.</li> */}
      {/*         </ul> */}
      {/*         <div style={{ textAlign: 'center', width: '100%' }}> */}
      {/*           <small>*The link may not work.*</small> */}
      {/*         </div> */}
      {/*       </Project> */}

      {/* <Project */}
      {/*   views={[ */}
      {/*     <img key="1" alt="dogwalking home page" src="/assets/sites/nyah.w-pawpals/home.png" />, */}
      {/*     <img key="2" alt="dogwalking calendar page" src="/assets/sites/nyah.w-pawpals/calendar.png" />, */}
      {/*   ]} */}
      {/*   alt="Paw Pals website" */}
      {/*   src="/assets/sites/nyah.w-pawpals/home.png" */}
      {/*   title="Paw Pals" */}
      {/*   subtitle="Enable clients to hire a dog walker at their convenience." */}
      {/*   href="https://pawpals.treywilkinson.com" */}
      {/*   tools="Haskell,TypeScript,ReactJS,Responsive" */}
      {/*   color="#20c9c6" */}
      {/*   img="cover" */}
      {/*   // color="#fff" */}
      {/* > */}
      {/*   <ul> */}
      {/*     <li>Enable clients to schedule a dog walking service through a custom made calendar.</li> */}
      {/*     <li>Clients may view, update and remove existing appointments through the calendar.</li> */}
      {/*   </ul> */}
      {/* </Project> */}
    </Section>
  )
}
