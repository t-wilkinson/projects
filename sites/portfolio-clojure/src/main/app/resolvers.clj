(ns app.resolvers
  (:require
    [codax.core :as c]
    [com.wsscode.pathom.core :as p]
    [com.wsscode.pathom.connect :as pc]))

(def db (c/open-database! "database/database"))

(pc/defresolver img-resolver [env {:project/keys [id]}]
  {::pc/input #{:project/id}
   ::pc/output [:art/files]}
  {:art/files (remove
                nil?
                (map
                  #(if (.isFile %)
                     (clojure.string/replace (.getPath %) #"resources/public/" ""))
                  (file-seq (clojure.java.io/file
                              (str "resources/public/projects/art/" (name id))))))})

(def resolvers [img-resolver])

#_(pc/defresolver img-resolver [env {:project/keys [id]}]
  {::pc/input #{:project/id}
   ::pc/output [:art/img :art/files]}
  (merge (c/get-at! db [:projects :art id])))

#_(pc/defresolver project-resolver [env {}]
  {::pc/output [:art/ceramics]}
  (c/get-at! db [:projects :art]))

#_(pc/defresolver person-resolver [env {:person/keys [id]}]
  {::pc/input #{:person/id}
   ::pc/output [:person/id :person/name :person/test]}
   (get {1 #:person{:id 1 :name "bob" :test "otasfds"}
         2 #:person{:id 2 :name "will" :test "other"}
         3 #:person{:id 3 :name "tom" :test "this is my other name"}}
        id))


#_(c/assoc-at! db [:projects]
  {:art #:art{:description "art projects"
              :cutout {:art/img [{:src "01.jpg" :description ""}]}
              :ceramics {:art/img [{:src "01.jpg" :description "first"}
                                   {:src "02.jpg" :description "second"}
                                   {:src "03.jpg" :description "third"}
                                   {:src "04.jpg" :description "fourth"}
                                   {:src "05.jpg" :description "fifth"}
                                   {:src "06.jpg" :description "six"}
                                   {:src "07.jpg" :description "seventh"}
                                   {:src "08.jpg" :description "eight"}
                                   {:src "09.jpg" :description "ninth"}]}}})
