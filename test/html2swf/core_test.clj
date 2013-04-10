(ns html2swf.html2swf-test
  (:require 
   [html2swf.html2swf :as h2s]
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
      (h2s/has-class? (first (html/select html1 [:body])) "a") => true
)

(fact "Correct styles selected for node"   
     ;;only tag   
     (h2s/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:body {:font-size 15}]]) => {:font-size 15}
     ;;tagged class
     (h2s/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:body.a {:font-size 15}]]) => {:font-size 15}

     ;;tagged with non existent class
     (h2s/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:body.e {:font-size 15}]]) => {}

     ;;only class selector matching
     (h2s/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:*.a {:font-size 15}]]) => {:font-size 15}

     ;;only class selector not matching
     (h2s/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:*.e {:font-size 15}]]) => {}

     ;multiple selectors same rule
     (h2s/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:*.a :*.e {:font-size 15}]]) => {:font-size 15}

     ;;merging multiple matching styles
     (h2s/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:*.a {:font-size 15}]
                           [:*.e {:padding 10}]
                           [:body {:font-face "Arial"}]]) => {:font-face "Arial" :font-size 15}
     
     ;;merging multiple matching styles, correct order respected
     (h2s/styles-for-node (first (html/select html1 [:body]))
                          [] 
                          [[:*.a {:font-size 15}]
                           [:body {:font-size 20}]]) => {:font-size 20}

     ;;parent hierarchy selector correctly matching
     (h2s/parse-selectors [:body :div :article {:font-size 15}]) => [["body" "div" "article"]]
 
     ;;parent sequence rule basic
     (let [body (first (html/select html1 [:body]))
           div (first (html/select html1 [:div]))
           article (first (html/select html1 [:article]))]
       (h2s/styles-for-node article
                            [div body] 
                          [[:body :div :article {:font-size 15}]])) => {:font-size 15}

     ;;parent sequence rule intercalated
     (let [body (first (html/select html1 [:body]))
           div (first (html/select html1 [:div]))
           article (first (html/select html1 [:article]))]
       (h2s/styles-for-node article
                            [div body] 
                          [[:body :article {:font-size 15}]])) => {:font-size 15}

     ;;parent sequence rule invalid
     (let [body (first (html/select html1 [:body]))
           div (first (html/select html1 [:div]))
           article (first (html/select html1 [:article]))]
       (h2s/styles-for-node article
                            [div body] 
                          [[:body :div {:font-size 15}]])) => {}

     ;;parent sequence rule invalid on loop
     (let [body (first (html/select html1 [:body]))
           div (first (html/select html1 [:div]))
           article (first (html/select html1 [:article]))]
       (h2s/styles-for-node article
                            [div body] 
                          [[:ul :article {:font-size 15}]])) => {}

     ;;merging with hierarchy rules, mixed
     (let [body (first (html/select html1 [:body]))
           div (first (html/select html1 [:div]))
           article (first (html/select html1 [:article]))]
       (h2s/styles-for-node article
                            [div body] 
                          [[:*.a :div :article {:font-size 15}]
                           [:article {:font-name "Arial"}]])) => {:font-size 15 :font-name "Arial"}

)

