(ns app.ui
  (:require
    [app.mutations :as api]
    [com.fulcrologic.fulcro.components :as comp :refer [defsc]]
    [com.fulcrologic.fulcro.dom :as dom]
    [com.fulcrologic.fulcro.routing.dynamic-routing :as dr]
    [com.fulcrologic.fulcro.data-fetch :as df]
    ; [com.fulcrologic.fulcro-css.css-injection :as inj]
    ; [com.fulcrologic.fulcro-css.css :as css]
    )
  (:require-macros [app.macro :refer [create-a-function]]))

; side menu animation per letter

(create-a-function fnasdfa)

(defsc Link [this {:keys [to child]}]
  (dom/a
    :.mx-2.px-2.cursor-pointer {:onClick #(dr/change-route this to)} child))
(defn link [to child]
  ((comp/factory Link)
   {:to to :child child}))

(defsc AI [this {}]
  {:ident (fn [] [:component/id ::main])
   :query []
   :route-segment ["ai"]}
  (dom/section
    (dom/h3 "AI")))

(defsc Website [this {}]
  {:ident (fn [] [:component/id ::main])
   :query []
   :route-segment ["website"]}
  (dom/section
    (dom/h3 "Websites")
    (let [a #(dom/a :.cursor-pointer.underline.text-blue-200.mr-4.w-full.h-full
                    {:target "_blank" :href %1}
                    %2)
          tag #(dom/div :.inline-block.bg-gray.text-black.m-2.px-2.rounded-md.z-10 {:key %} %)
          card (fn [{:keys [tags href src]}]
                 (dom/div
                   :.flex.relative.w-64.h-40.rounded-md
                   (dom/a
                     :.w-full.h-full.absolute {:target "_blank" :href href}
                     (dom/img :.w-full.h-full.absolute {:src src})
                     (dom/ul
                       :.flex.flex-wrap.items-center.justify-center.w-full.h-full.z-00
                       (map tag tags)))))]
      (dom/section
        :.my-8
        {:style {:display "grid"
                 :gridTemplateColumns "repeat(auto-fill, 20rem)"
                 :justifyContent "center"
                 :justifyItems "center"}}
        (card {:tags ["fullstack" "haskell" "tailwind"]
               :href "https://youtube.com"
               :src "https://i.ytimg.com/vi/78rwHDUSpvA/maxresdefault.jpg"})
        (card {:tags ["fullstack" "clojure" "tailwind"]
               :href "https://google.com"
               :src "https://qph.fs.quoracdn.net/main-qimg-8867c8c91b510fefdd7802eccb431931"})))))

(defsc ArtProjects [this {:project/keys [id] :art/keys [files] :as props}]
  {:ident (fn [] [:project/id id])
   :query [:project/id :art/files]
   :route-segment [:project/id]
   :will-enter (fn [app {:project/keys [id]}]
                 (let [id [:project/id (keyword (str "art/" id))]]
                   (dr/route-deferred
                     id
                     #(df/load! app id ArtProjects
                                {:post-mutation `dr/target-ready
                                 :post-mutation-params {:target id}}))))}
  (dom/div {:style {:gridTemplateColumns "repeat(auto-fit, 15rem)"
                    :justifyItems "center"
                    :justifyContent "center"
                    :display "grid"
                    :gap "2rem"
                    :padding "2rem"}}
           (map #(dom/img :.inline-block.w-auto.h-auto.rounded-md.transform.rotate-90 {:src %}) files)))

(dr/defrouter ArtRouter [this {:keys [current-state pending-path-segment]}]
  {:router-targets [ArtProjects]})
(def art-router (comp/factory ArtRouter))

(defsc Art [this {:art/keys [router]}]
  {:ident (fn [] [:component/id ::main])
   :query [{:art/router (comp/get-query ArtRouter)}]
   :route-segment ["art"]
   :initial-state {:art/router {}}}
  (dom/section
    (dom/h3 "Art")
    (dom/nav
      :.flex.flex-wrap
      (link ["beginnings"] "beginnings")
      (link ["exploratory"] "exploratory")
      (link ["landscape"] "landscape")
      (link ["pillow"] "pillow")
      (link ["portrait"] "portrait")
      (link ["skull"] "skull")
      (link ["textures"] "textures")
      (link ["unnamed"] "unnamed")
      (link ["ceramics"] "ceramics"))
    (art-router router)))

(dr/defrouter HomeRouter [this {:keys [current-state pending-path-segment]}]
  {:router-targets [Website Art AI]})
(def home-router (comp/factory HomeRouter))

(defsc Home [this {:home/keys [router] :as props}]
  {:ident (fn [] [:component/id ::main])
   :query [{:home/router (comp/get-query HomeRouter)}]
   :initial-state {:home/router {}}
   :route-segment ["home"]}
  (dom/div
    (dom/section
      :.flex.flex-col.justify-center.min-h-screen.px-16
      (dom/h1
        :.font-900.inline-block.text-6xl
        (dom/span
          :.text-gradient
          "Hi, I'm Trey"))
      (dom/p
       :.text-lg
       "Organized, efficient, problem solver, attention to detail, love to learn,
       artist. Also some other stuff which I have not added yet")
      (dom/p :.text-gray-6 "Fullstack developer / machine learning enthusiast / Freelancer"))

    (let [tag #(dom/div :.bg-gray.text-white.inline-block.px-2.m-2.rounded-md {:key %} %)]
      [ (dom/section
          :.bg-white.min-h-screen.text-black.px-8.py-4
          (dom/h2 "What I do")
          (dom/ul
            (dom/li
              (dom/h3 "frontend")
              (map tag ["html" "javascript" "css" "tailwind" "sass" "postcss" "gulp"]))
            (dom/li
              (dom/h3 "backend")
              (map tag ["haskell" "clojure" "postgresql" "sql"]))
            (dom/li
              (dom/h3 "machine-learning")
              (map tag ["python" "c" "keras" "tensorflow"]))))
       (dom/section
         :.bg-black.min-h-screen.text-white.px-8.py-4
         (dom/h2 "About me")
         (dom/ul
           (dom/li
             (dom/h3 "skills")
             (map tag ["responsive" "seo" "security" "machine-learning" "accesibillity"]))
           (dom/li
             (dom/h3 "interests")
             (map tag ["category theory"]))
           (dom/li
             (dom/h3 "tools")
             (map tag ["terminal" "zsh" "github" "vim" "linux" "inkscape"]))))
       (dom/section
         :.bg-black.min-h-screen.text-white.px-8.py-4
         (dom/h2 "stuff")
         (dom/ul
           (dom/li
             (dom/p "I am dedicated to keeping your data safe. I use the latest hashing methods for protecting passwords along with salts. Api's serving sensitive data are always protected. Login sessions are remembered through cookies storing hashed tls session."))))])
    (dom/section
      :.bg-white.min-h-screen.text-black.px-8.py-4
      (dom/nav
        (dom/h2 "Projects")
        (link ["website"] "websites")
        (link ["ai"] "AI")
        (link ["art" "ceramics"] "art")
        (home-router router)))
    (dom/section
      (dom/h2 "Contact")
      (dom/p "winston.trey.wilkinson@gmail.com"))))

(dr/defrouter RootRouter [this {:keys [current-state pending-path-segment]}]
  {:router-targets [Home]})
(def root-router (comp/factory RootRouter))

(defsc Root [this {:root/keys [router]
                   :keys [header]}]
  {:query [{:root/router (comp/get-query RootRouter)}
           :header]
   :initial-state {:root/router {}
                   :header false}
   :route-segment ["main"]}
  (dom/main
    :.bg-black.text-white
    (dom/header
      :.fixed.top-0.w-screen.bg-black.flex.justify-end.pr-4
      (dom/nav
        :.transform.scale-200.cursor-pointer.z-20.p-2.mr-8.mt-8
        {:onClick #(comp/transact! this [(api/toggle-header {:header header})])}
        (dom/div :.gg-menu.text-pri))
      (dom/aside
        :.fixed.h-screen.flex.flex-col.pt-20.text-xl
        (let [li (fn [id {:keys [to txt]}]
                   (dom/li
                     (link
                       to
                       (dom/div :.text-pri.font-bold.hover:text-sec
                         (map-indexed
                           #(dom/span
                              :.transform.inline-block.duration-500.transition-transform
                              (if header
                                {:style {:transitionDelay (str (+ (* 500 id) (* 100 %)) "ms")
                                         :transform "translateX(0)"}}
                                {:style {:transform "translateX(8rem)"}})
                              %2)
                           txt)))))]
          (dom/ul
            (map-indexed
              li
              [{:to ["main"] :txt "main"}
               {:to ["ceramics"] :txt "ceramics"}
               {:to ["main"] :txt "about"}
               {:to ["main" "home"] :txt "other"}])))))
    (root-router router)
    (dom/footer
      :.w-full
      (dom/h4 "footer"))))

(defn client-did-mount [app]
  (dr/change-route app ["main" "home"]))


#_(dr/defrouter RootRouter [this {:keys [current-state pending-path-segment]}]
    {:router-targets [Main]}
    (case current-state
      :pending (dom/div "loading...")
      :failed (dom/div "failed, try another route")
      (dom/div "unknown route")))

#_(let [escape (js/document.createElement "textarea")
        escapeHtml (fn [html]
                     (set! (. escape -textContent) html)
                     (. escape -innerHTML))
        unescapeHtml (fn [html]
                       (set! (.-innerHTML escape) html)
                       (. escape -innerHTML))]
    (js/console.log (escapeHtml "<div>inner html</div>"))
    (dom/div {:dangerouslySetInnerHTML {:__html "<a href='javascript:console.log(\"hacked\")'>bobby</a> <a></a>"}} nil))
