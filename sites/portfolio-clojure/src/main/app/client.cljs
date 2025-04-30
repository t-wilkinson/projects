(ns app.client
  (:require
    [app.application :refer [app]]
    [app.ui :as ui]
    [com.fulcrologic.fulcro.application :as app]
    [com.fulcrologic.fulcro.data-fetch :as df]))

(defn ^:export init [this]
  (app/mount! app ui/Root "app")
  (ui/client-did-mount app)
  ; (df/load! app :root/router ui/RootRouter)
  ; (df/load! app :person/id ui/Other)
  ; (df/load! app :friends ui/PersonList)
  ; (df/load! app :enemies ui/PersonList)
  (js/console.log "Loaded"))
; (df/load this [:person/id 3] Person)

(defn ^:export refresh
  "During development, shadow-cljs will call this on every hot reload of source. See shadow-cljs.edn"
  []
  ;; re-mounting will cause forced UI refresh, update internals, etc.
  (app/mount! app ui/Root "app")
  (js/console.log "Hot reload"))

