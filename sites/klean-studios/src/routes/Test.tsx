
import React from "react"
import gsap from "gsap"
/* import {BehaviorSubject, range, Observable} from "rxjs" */
/* import {toArray, map} from "rxjs/operators" */

import { ExpoScaleEase, RoughEase, SlowMo } from "gsap/EasePack";
gsap.registerPlugin(ExpoScaleEase, RoughEase, SlowMo);


export default () => {
  /* hooks.useEffect(() => { */
  /*   buildGrid({ */
  /*     grid: [7,15], className: "box", width: 1000, */
  /*     gutter: 15, parent: "#container" */
  /*   }) */
  /*   /1* animateBoxes("center", null, null) *1/ */
  /* }, []) */

  return <>
    <div className="absolute" id="container"> </div>
    </>
}

/* let */
/*   tl = gsap.timeline({repeat: -1, repeatDelay: 0.5}), */
/*   grid = [7,15]; */

/* function animateBoxes(from, axis, ease) { */
/*   //one stagger call does all the animation: */
/*   tl.to(".box", { */
/*       duration: 1.0, */
/*       scale: 0.1, */
/*       y: 60, */
/*       // yoyo: true, */
/*       // repeat: 1, */
/*       ease: "power1.inOut", */
/*       stagger: { */
/*         amount: 1.05, */
/*         yoyo: true, */
/*         grid: grid, */
/*         axis: axis, */
/*         ease: "slow(0.3,0.3,false)", */
/*         from: [0.5,.5], */
/*         repeat: -1, */
/*       } */
/*     } */
/*   ); */
/* } */


/* //helper function to build a grid of <div> elements */
/* function buildGrid(vars) { */
/* 	vars = vars || {}; */
/* 	var container = document.createElement("div"), */
/* 		rows = vars.grid[0] || 5, */
/* 		cols = vars.grid[1] || 5, */
/* 		width = vars.width || 100, */
/* 		gutter = vars.gutter || 1, */
/*     className = vars.className || "", */
/* 		w = (width - cols * gutter) / cols, */
/* 		parent = (typeof(vars.parent) === "string") ? document.querySelector(vars.parent) : vars.parent ? vars.parent : document.body, */
/*     css = "background: green; height: 50px; display: inline-block; margin: 0 " + (gutter / width * 100) + "% " + (gutter / width * 100) + "% 0; width: " + (w / width * 100) + "%;", */
/* 		l = rows * cols, */
/* 		i, box; */
/* 	for (i = 0; i < l; i++) { */
/* 		box = document.createElement("div"); */
/* 		box.style.cssText = css; */
/*     box.setAttribute("class", className); */
/*     box.index = i; */
/*     box.setAttribute("data-index", i); */
/* 		container.appendChild(box); */
/* 	} */
/* 	container.style.cssText = "width:" + width + "px; line-height: 0; padding:" + gutter + "px 0 0 " + gutter + "px; display:inline-block;"; */
/* 	parent.appendChild(container); */
/* 	return container; */
/* } */

/* //this just helps avoid the pixel-snapping that some browsers do. */
/* // gsap.set(".box", {rotation: 0.5, force3D: true}); */
