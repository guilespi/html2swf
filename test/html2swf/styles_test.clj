(ns html2swf.styles-test
  (:require 
   [html2swf.styles :as styles]
   [net.cgrand.enlive-html :as html])

  (:use 
   midje.sweet)
  (:import 
   (java.io ByteArrayInputStream)))

(defn str->html 
  [^String content]
  (html/html-resource (ByteArrayInputStream. (.getBytes content))))

(defmacro h
  [name content]
  `(def ~name (str->html ~content)))

(h html1 "<body class=\"a b c\"><div class=\"a\"><article id=\"art1\">content</article></div></body>")

(first (html/select html1 [:body]))


(def css-example
 "
html, body, #id {
  padding  : 20px;
} 

#navbar {
 width: 80%;
 height: 23px;
 }

 #navbar, #navbar2 {
 width: 90%;
 other: 23px;
 }

 #navbar ul {
 list-style-type: none;
 }
 #navbar li {
 float: left;
 }
 #navbar li a {
 font-weight: bold;
 }
 ")


(fact "Has classes correctly tested"
      (styles/has-class? (first (html/select html1 [:body])) "a") => true
)

(fact "Correct styles selected for node"   
     ;;only tag   
     (styles/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:body {:font-size 15}]]) => {:font-size 15}
     ;;tagged class
     (styles/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:body.a {:font-size 15}]]) => {:font-size 15}

     ;;tagged with non existent class
     (styles/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:body.e {:font-size 15}]]) => {}

     ;;only class selector matching
     (styles/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:*.a {:font-size 15}]]) => {:font-size 15}

     ;;only class selector not matching
     (styles/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:*.e {:font-size 15}]]) => {}

     ;multiple selectors same rule
     (styles/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:*.a :*.e {:font-size 15}]]) => {:font-size 15}

     ;;merging multiple matching styles
     (styles/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:*.a {:font-size 15}]
                           [:*.e {:padding 10}]
                           [:body {:font-face "Arial"}]]) => {:font-face "Arial" :font-size 15}
     
     ;;merging multiple matching styles, correct order respected
     (styles/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:*.a {:font-size 15}]
                           [:body {:font-size 20}]]) => {:font-size 20}

     ;;parent hierarchy selector correctly matching
     (styles/parse-selectors [:body :div :article {:font-size 15}]) => [["body" "div" "article"]]
 
     ;;parent sequence rule basic
     (let [body (first (html/select html1 [:body]))
           div (first (html/select html1 [:div]))
           article (first (html/select html1 [:article]))]
       (styles/styles-for-node article
                            [{:node div :index 1} {:node body :index 1}] 
                          [[:body :div :article {:font-size 15}]])) => {:font-size 15}

     ;;parent sequence rule intercalated
     (let [body (first (html/select html1 [:body]))
           div (first (html/select html1 [:div]))
           article (first (html/select html1 [:article]))]
       (styles/styles-for-node article
                            [{:node div :index 1} {:node body :index 1}] 
                          [[:body :article {:font-size 15}]])) => {:font-size 15}

     ;;parent sequence rule invalid
     (let [body (first (html/select html1 [:body]))
           div (first (html/select html1 [:div]))
           article (first (html/select html1 [:article]))]
       (styles/styles-for-node article
                            [{:node div :index 1} {:node body :index 1}] 
                          [[:body :div {:font-size 15}]])) => {}

     ;;parent sequence rule invalid on loop
     (let [body (first (html/select html1 [:body]))
           div (first (html/select html1 [:div]))
           article (first (html/select html1 [:article]))]
       (styles/styles-for-node article
                            [{:node div :index 1} {:node body :index 1}] 
                          [[:ul :article {:font-size 15}]])) => {}

     ;;merging with hierarchy rules, mixed
     (let [body (first (html/select html1 [:body]))
           div (first (html/select html1 [:div]))
           article (first (html/select html1 [:article]))]
       (styles/styles-for-node article
                            [{:node div :index 1} {:node body :index 1}] 
                          [[:*.a :div :article {:font-size 15}]
                           [:article {:font-name "Arial"}]])) => {:font-size 15 :font-name "Arial"})


(fact "Styles with hierarchy operator >"

      ;;only one parent
      (let [body (first (html/select html1 [:body]))
            div (first (html/select html1 [:div]))
            article (first (html/select html1 [:article]))]
        (styles/styles-for-node article
                                [{:node div :index 1} {:node body :index 1}] 
                                [[:div :> :article {:font-size 15}]])) => {:font-size 15}

      ;;one parent and the rest normal
      (let [body (first (html/select html1 [:body]))
            div (first (html/select html1 [:div]))
            article (first (html/select html1 [:article]))]
        (styles/styles-for-node article
                                [{:node div :index 1} {:node body :index 1}] 
                                [[:body :div :> :article {:font-size 15}]])) => {:font-size 15}
                                
      ;;matching selector but not matching parent
      (let [body (first (html/select html1 [:body]))
            div (first (html/select html1 [:div]))
            article (first (html/select html1 [:article]))]
        (styles/styles-for-node article
                                [{:node div :index 1} {:node body :index 1}] 
                                [[:body.a :> :article {:font-size 15}]])) => {}

      ;;multiple parents matching
      (let [body (first (html/select html1 [:body]))
            div (first (html/select html1 [:div]))
            article (first (html/select html1 [:article]))]
        (styles/styles-for-node article
                                [{:node div :index 1} {:node body :index 1}] 
                                [[:body :> :div :> :article {:font-size 15}]])) => {:font-size 15}

     ;;multiple parents not matching
      (let [body (first (html/select html1 [:body]))
            div (first (html/select html1 [:div]))
            article (first (html/select html1 [:article]))]
        (styles/styles-for-node article
                                [{:node div :index 1} {:node body :index 1}] 
                                [[:a :> :div :> :article {:font-size 15}]])) => {}

     

)

(h html2 "<body class=\"a b c\"><div class=\"a\"><article id=\"art1\">content</article><article id=\"art2\">content2</article></div></body>")

(fact "Check selector with functions"

   ;;first of type
   (let [body (first (html/select html2 [:body]))
         div (first (html/select html2 [:div]))
         article (first (html/select html2 [:#art1]))]
     (styles/styles-for-node article
                             [{:node div :index 1} {:node body :index 1}] 
                             [[:article:first-of-type {:font-size 15}]])) => {:font-size 15}

   ;;first of type, not matching
   (let [body (first (html/select html2 [:body]))
         div (first (html/select html2 [:div]))
         article (first (html/select html2 [:#art2]))]
     (styles/styles-for-node article
                             [{:node div :index 2} {:node body :index 1}] 
                             [[:article:first-of-type {:font-size 15}]])) => {}

   ;;last of type
   (let [body (first (html/select html2 [:body]))
         div (first (html/select html2 [:div]))
         article (first (html/select html2 [:#art2]))]
     (styles/styles-for-node article
                             [{:node div :index 2} {:node body :index 1}] 
                             [[:article:last-of-type {:font-size 15}]])) => {:font-size 15}

   ;;last of type, not matching
   (let [body (first (html/select html2 [:body]))
         div (first (html/select html2 [:div]))
         article (first (html/select html2 [:#art1]))]
     (styles/styles-for-node article
                             [{:node div :index 1} {:node body :index 1}] 
                             [[:article:last-of-type {:font-size 15}]])) => {}

                             
)

;;(def hf (parser/read-html-files "/Users/guilespi/Downloads/HTML5/PVIA"))
;;(def myf (first (filter #(= (:relative-path (second %)) "/PVIA_PASSAGE6_NOUWM.html") hf)))
;;(in-ns 'html2swf.parser)
;;(def sheets (stylesheets (:html (second user/myf)) "/Users/guilespi/Downloads/HTML5/PVIA/"))

;;(require :reload-all '[html2swf.compiler :as compiler])
;;(compiler/build-file myf "/Users/guilespi/Downloads/HTML5/PVIA/")