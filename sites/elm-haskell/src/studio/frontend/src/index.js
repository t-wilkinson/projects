import "./main.css";
import { Elm } from "./Main.elm";
import * as serviceWorker from "./serviceWorker";

// Vars
const flags = {};

// Init elm app
const app = Elm.Main.init({
  node: $("#root"),
  flags: flags
});

window.myjstilt = window.matchMedia("(pointer: fine)").matches;
// window["myjstilt"] = false;

// Document Ready
$(document).ready(() => {
  onReady();
});

/* Ports */
app.ports.logout.subscribe(_ => {});

app.ports.ready.subscribe(_ => onReady());

/* Toggles */
// Toggle login/register form

// Toggle checkbox
$("input[type=checkbox]")
  .on("blur", () => $("header aside").removeClass("js-show"))
  .on("click", () => $("header aside").toggleClass("js-show"));

/* Helpers */
// On Ready
function onReady() {
  if (window.myjstilt) mytilt();
}

$(".js-toggle-form").on("click", () => {
  $("#login").toggleClass("login register");
});

// Toggle form

// tilt
function mytilt() {
  $(".js-tilt").tilt({
    glare: true,
    maxGlare: 0.15,
    scale: 1.05
  });
}

// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
