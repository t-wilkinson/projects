import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

Elm.Main.init({
  node: document.getElementById('root')
});


const storageKey = 'cache'
const flags = JSON.stringify(
    { cache : localStorage.getItem(storageKey) }
);
const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
});


//app.ports.cache.subscribe((data) => {
//  if (data === null) {
//    localStorage.removeItem(storageKey);
//
//  } else {
//      console.log(data);
//    localStorage.setItem(storageKey, JSON.stringify(data));
//  }
//
//  setTimeout(() => app.ports.onCacheChange.send(data), 0);
//});


// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
