import './main.css';
import { Elm } from './Main.elm';
import * as serviceWorker from './serviceWorker';

const storageKey = 'cache'
const flags = JSON.stringify({ cache : localStorage.getItem(storageKey)});

const app = Elm.Main.init({
  node: document.getElementById('root'),
  flags: flags
});


app.ports.blog.subscribe((data) => {
    if (data === null) {
        setTimeout(() => app.ports.createdBlog.send(null), 0);

    } else {


    }

});


app.ports.cache.subscribe((data) => {
  if (data === null) {
    localStorage.removeItem(storageKey);

  } else {
    localStorage.setItem(storageKey, JSON.stringify(data));
    setTimeout(() => app.ports.onCacheChange.send(data), 0);

  }

});



// If you want your app to work offline and load faster, you can change
// unregister() to register() below. Note this comes with some pitfalls.
// Learn more about service workers: https://bit.ly/CRA-PWA
serviceWorker.unregister();
