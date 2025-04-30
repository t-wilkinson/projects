(ns app.mutations
  (:require
    [com.fulcrologic.fulcro.mutations :as m :refer [defmutation]]
    [com.fulcrologic.fulcro.algorithms.merge :as merge]))

(defmutation toggle-header [{:keys [header]}]
  (action [{:keys [state] :as env}]
          (swap! state update-in [:header] not)))
