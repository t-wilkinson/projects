import React from 'react'
import styled from 'styled-components'
import { C, ExternalLink } from '../../components'

function Project(props) {
  const refs = {
    info: React.useRef(null),
    view: React.useRef(null),
  }
  const [slidepos, setSlidepos] = React.useState(0)
  // const [videoBlocked, setVideoBlocked] = React.useState(false)

  React.useEffect(() => {}, [slidepos])
  // React.useEffect(() => {
  //   const view = refs.view.current
  //   if (view === null) return
  //   var blocked = false
  //   view.querySelectorAll('video').forEach(v=> {
  //     v.play()
  //       .then(()=>{})
  //       .catch(()=>{setVideoBlocked(true)})
  //   })
  // }, [refs.view, setVideoBlocked])

  return (
    <div className={props.className}>
      <div className="wrapper">
        <div
          className="teaser"
          onClick={(e) => refs.info.current.classList.toggle('active')}
        >
          <div className="read-more">Read more</div>
          <img
            className={`project-img ${props.img}`}
            src={props.src}
            alt={props.alt}
          />
          <ul className="overview">
            {props.tools.split(/,/).map((v) => (
              <li key={v}>{v}</li>
            ))}
          </ul>
          <span
            style={{
              position: 'absolute',
              top: '0',
              left: '0',
              fontSize: '0.75rem',
              margin: '0.5rem 0 0 0.5rem',
              padding: '0.25rem',
              background: '#0003',
              color: 'white',
              borderRadius: '0.25rem',
            }}
          >
            Click to read more
          </span>
        </div>
      </div>
      <div className="info" ref={refs.info}>
        <div className="info__wrapper">
          <div className="slider">
            <div ref={refs.view} className="view">
              {[props.views[Math.abs(slidepos % props.views.length)]]}
            </div>
            {/* {videoBlocked */}
            {/*   ? <div className="video-blocked">Video autoplay is disabled ;(</div> */}
            {/*   : null */}
            {/* } */}
            <nav className="nav">
              <button
                className="left"
                onClick={() => setSlidepos(slidepos - 1)}
              >
                {C.arrow}
              </button>
              <button
                className="right"
                onClick={() => setSlidepos(slidepos + 1)}
              >
                {C.arrow}
              </button>
            </nav>
          </div>
          <div className="content">
            <div className="title">{props.title}</div>
            <div className="subtitle">{props.subtitle}</div>
            <div className="sep" />
            <div className="description">{props.children}</div>
            <nav className="nav">
              <div
                className="close"
                onClick={() => refs.info.current.classList.toggle('active')}
              >
                {C.close}
              </div>
              <a
                className="link"
                rel="noopener noreferrer"
                target="_blank"
                href={props.href}
              >
                <ExternalLink /> view site
              </a>
            </nav>
          </div>
        </div>
      </div>
    </div>
  )
}

export default styled(Project)`
  .wrapper {
    padding: 0.25rem;
    background: #ccc;
    border-radius: 0.25rem;
  }
  .teaser {
    // margin: 1.5rem 0;
    border-radius: 0.25rem;
    height: 100%;
    width: 100%;
    max-width: 700px;
    display: flex;
    flex-direction: column;
    justify-content: center;
    position: relative;
    cursor: pointer;
    overflow: hidden;

    .overview {
      position: absolute;
      bottom: 0;
      display: flex;
      flex-wrap: wrap-reverse;
      margin: 0;
      padding: 0;
      width: 100%;

      li {
        margin-left: 8px;
      }
      li {
        display: inline-block;
        list-style: none;
        background: ${(p) => p.theme.sec};
        box-shadow: 0 1px 0 2px #041f06;
        border-radius: 4px;
        margin-bottom: 8px;
        padding: 4px 16px;
        font-size: 1rem;
      }

      @media (max-width: 550px) {
        li {
          font-size: 0.8rem;
          padding: 2px 8px;
        }
      }
    }
    .project-img.contain {
      object-fit: contain;
    }
    .project-img.cover {
      object-fit: cover;
    }
    .project-img {
      height: 100%;
      width: 100%;

      background: ${(p) => p.color};
      cursor: pointer;
    }
    img {
      transition: 0.2s ease-in-out;
    }

    .read-more {
      z-index: 1;
      position: absolute;
      top: 0;
      left: 0;
      right: 0;
      bottom: 0;
      display: grid;
      place-items: center;
      opacity: 0;
      transition: 0.2s ease-in-out;
      font-weight: 900;
      background-color: rgb(255 255 255 / 80%);
    }
    .read-more:hover {
      transition: 0.3s;
      opacity: 1;
    }
  }

  .teaser:hover img {
    transition: 0.3s ease-in-out;
    transform: scale(1.1);
  }

  .info.active {
    display: flex;
  }
  .info__wrapper {
    width: 100%;
    position: relative;
    max-width: 700px;
    border: 0.25rem solid #ccc;
    border-radius: 0.25rem;
    overflow: hidden;
    box-shadow: rgba(0 0 0 / 16%) 0px 2px 5px 0px,
      rgba(0 0 0 / 12%) 0px 2px 10px 0px;
  }

  .info {
    z-index: 10;
    display: none;
    position: fixed;
    flex-direction: column;
    justify-content: center;
    align-items: center;
    top: 0;
    left: 0;
    bottom: 0;
    right: 0;
    padding: 2rem 4rem;
    height: 100vh;
    background: rgb(0 0 0 / 50%);
    @media (max-width: 768px) {
      padding: 0 0.5rem;
    }
    @media (min-width: 769px) {
      margin-left: 200px;
    }

    .slider {
      position: relative;
      overflow: hidden;
      width: 100%;
      background: ${(p) => p.color};
      max-height: 394px;

      .video-blocked {
        top: 0;
        right: 0;
        bottom: 0;
        top: 0;
        position: absolute;
        display: grid;
        place-items: center;
        place-content: center;
        background: rgb(0 0 0 / 50%);
        color: white;
      }

      .view {
        display: flex;
        align-items: flex-end;
        width: 100%;
        height: 100%;
        * {
          width: 100%;
          height: 100%;
          object-fit: contain;
        }
      }
      .nav {
        display: flex;
        justify-content: space-between;
        position: absolute;
        bottom: 0;
        left: 0;
        right: 0;
        .left,
        .right {
          border-radius: 0.25rem 0 0 0.25rem;
          svg {
            transition: 0.25s ease-in-out;
          }
        }
        .left {
          transform: rotate(180deg);
        }
        .right {
          transform: rotate(0deg);
        }
        .left:hover svg {
          transform: translateX(8px);
        }
        .right:hover svg {
          transform: translateX(8px);
        }

        button {
          height: 4rem;
          width: 4rem;
          padding: 0;
          display: grid;
          place-content: center;
          place-items: center;
          border: none;
          background: rgb(0 0 0 / 50%);
          svg {
            width: 1.5rem;
            fill: white;
          }
        }
      }
    }
    .content {
      background: white;
      width: 100%;
      padding: 1rem;
      position: relative;
      display: flex;
      flex-direction: column;
      .title {
        font-weight: 900;
        margin-top: 1rem;
      }
      .subtitle {
        font-weight: 100;
        font-size: 1rem;
      }
      .sep {
        height: 1px;
        background: black;
        width: 100%;
        margin: 1rem 0;
      }
      @media (max-width: 768px) {
        ul {
          padding: 1rem;
          margin: 0;
        }
      }
      .description {
        ul {
          margin: 0;
          padding: 0 1rem;
        }
        margin-bottom: 1rem;
      }
      .description,
      .description * {
        // @media (max-width: 768px) { font-size: 0.9rem; }
        font-size: 1rem;
      }
      .nav {
        margin-top: 1rem;
        width: 100%;
        display: flex;
        justify-content: space-between;
        align-items: center;
        flex-grow: 1;
        .link {
          border-radius: 0.25rem;
          font-size: 1rem;
          background: ${(p) => p.theme.pri};
          color: white;
          text-decoration: none;
          padding: 0.5rem 1rem;
          box-shadow: 2px 2px 2px rgba(0, 0, 0, 0.25);
          svg {
            width: 1rem;
            transition: all 0.25s ease-in-out;
          }
        }
        .link:hover svg {
          transform: translate(0, -4px);
        }
        .close {
          cursor: pointer;
          background: white;
          border: none;
          height: 1.5rem;
          width: 1.5rem;
          transition: all 0.25s ease-in-out;
          padding: 0.5rem;
          box-sizing: content-box;
        }
        .close:hover {
          transform: rotate(-90deg);
        }
      }
    }
  }
`
