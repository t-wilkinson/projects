// @ts-nocheck
import React from 'react'
import { A, Href, Logo, social } from './'
import { useStore } from '../store'

export const Footer = (props) => {
  const refs = {
    text: React.useRef(null),
  }
  const root = useStore((s) => s.root)

  return (
    <footer
      className="
    relative text-lg h-screen bg-white text-black space-y-20
    z-20
    justify-center col
    px-8 py-16
    sm:px-24 sm:py-24
    md:px-32 md:py-32
    "
      style={{
        ...(window.innerWidth > 700 && window.innerHeight < 400
          ? { margin: '0', padding: '0.5rem 0.5rem' }
          : {}),
      }}
    >
      <div className="w-full max-w-screen-md h-full col justify-between">
        <div className="flex flex-col md:flex-row justify-start w-full md:h-96">
          <div
            className="flex flex-col items-start justify-between
          w-full md:w-128"
          >
            <A to="/">
              <Logo height="40px" fill="black" />
            </A>
            <h4>Klean Studios</h4>
          </div>
          <div className="flex flex-col items-end justify-between w-full">
            <span
              ref={refs.text}
              className="text-2xl pt-4 sm:pt-0 sm:text-3xl
            text-left
            md:text-right w-full
            "
            >
              Collaborate and Create
            </span>
            <div
              className={'w-full flex mt-32 md:mt-0 md:justify-end space-x-4'}
            >
              <A
                to="/projects"
                className={`text-xl
              ${root.userAgent('safari') ? 'mr-2' : ''}
                `}
              >
                projects
              </A>
              <A to="/dashboard" className="text-xl">
                dashboard
              </A>
            </div>
          </div>
        </div>
        <div
          className="w-full flex justify-between
        items-start flex-col-reverse
        sm:flex-row sm:items-end
        "
        >
          <nav className="space-x-2">
            <span>follow me on</span>
            <social.Spotify />
            <social.Instagram />
          </nav>
          <div
            className="col pb-4 sm:pb-0 justify-between items-start"
            style={{
              ...(window.innerWidth > 700 && window.innerHeight < 400
                ? { position: 'absolute', right: '0', bottom: '4rem' }
                : {}),
            }}
          >
            <ul className="space-y-4 col w-full items-start md:items-end">
              <Href
                className={`font-sec
              ${root.userAgent('safari') ? 'mt-2' : ''}
                `}
                to="tel:+5408162061"
              >
                (540) 816-2061
              </Href>
              <Href
                className={`font-sec
              ${root.userAgent('safari') ? 'mt-2' : ''}
                `}
                to="mailto:studio.klean@gmail.com"
              >
                studio.klean@gmail.com
              </Href>
              <span
                className={`font-sec ${root.userAgent('safari') ? 'mt-2' : ''}`}
              >
                Roanoke - Staunton VA
              </span>
            </ul>
          </div>
        </div>
      </div>
      <div className="text-sm px-4 py-2 absolute right-0 bottom-0 flex bg-gray-3 justify-end w-full">
        made by&nbsp;
        <a
          href="https://treywilkinson.com"
          rel="noopener noreferrer"
          target="_blank"
          className="flex items-center"
        >
          <TreyWilkinson />
          &nbsp;&nbsp;<span className="underline">Trey Wilkinson</span>
        </a>
      </div>
    </footer>
  )
}

const TreyWilkinson = () => (
  <svg viewBox="0 0 138.71225 89.033352" className="w-6">
    <g transform="translate(-60.66768,-43.929754)">
      <path
        // @ts-ignore
        // prettier-ignore
        style={{ color: '#000000', fontStyle: 'normal', fontVariant: 'normal', fontWeight: 'normal', fontStretch: 'normal', fontSize: 'medium', lineHeight: 'normal', fontFamily: 'sansSerif', fontVariantLigatures: 'normal', fontVariantPosition: 'normal', fontVariantCaps: 'normal', fontVariantNumeric: 'normal', fontVariantAlternates: 'normal', fontVariantEastAsian: 'normal', fontFeatureSettings: 'normal', fontVariationSettings: 'normal', textIndent: '0', textAlign: 'start', textDecoration: 'none', textDecorationLine: 'none', textDecorationStyle: 'solid', textDecorationColor: '#000000', letterSpacing: 'normal', wordSpacing: 'normal', textTransform: 'none', writingMode: 'lrTb', direction: 'ltr', textOrientation: 'mixed', dominantBaseline: 'auto', baselineShift: 'baseline', textAnchor: 'start', whiteSpace: 'normal', shapePadding: '0', shapeMargin: '0', inlineSize: '0', clipRule: 'nonzero', display: 'inline', overflow: 'visible', visibility: 'visible', isolation: 'auto', mixBlendMode: 'normal', colorInterpolation: 'sRGB', colorInterpolationFilters: 'linearRGB', solidColor: '#000000', solidOpacity: '1', vectorEffect: 'none', fill: '#000000', fillOpacity: '1', fillRule: 'nonzero', stroke: 'none', strokeWidth: '13.2292', strokeLinecap: 'square', strokeLinejoin: 'miter', strokeMiterlimit: '4', strokeDasharray: 'none', strokeDashoffset: '0', strokeOpacity: '1', paintOrder: 'stroke fill markers', colorRendering: 'auto', imageRendering: 'auto', shapeRendering: 'auto', textRendering: 'auto', enableBackground: 'accumulate', stopColor: '#000000', }}
        d="m 130.59674,43.929754 c -21.1962,21.19624 -44.79063,44.79013 -69.92906,69.928546 l 8.7936,8.7938 24.925101,-23.918946 V 132.9631 H 107.61449 V 85.505014 l 21.52168,-21.52116 v 65.088526 l 0.0104,0.007 c 0.003,6.52743 0.005,-2.66668 0.008,3.86074 l 28.23724,-23.14117 22.41206,23.14122 19.57607,-20.22718 -9.25886,-9.25888 -11.17502,11.16263 L 157.3639,91.333414 142.36483,103.70526 V 55.697854 Z"
      />
    </g>
  </svg>
)

export default Footer
