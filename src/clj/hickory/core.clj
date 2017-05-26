(ns hickory.core
  (:require [hickory.utils :as utils]
            [clojure.zip :as zip])
  (:import [org.jsoup Jsoup]
           [org.jsoup.nodes Attribute Attributes Comment DataNode Document
            DocumentType Element Node TextNode XmlDeclaration]
           [org.jsoup.parser Tag Parser]))

;;
;; Protocols
;;

(defprotocol HiccupRepresentable
  "Objects that can be represented as Hiccup nodes implement this protocol in
   order to make the conversion."
  (as-hiccup [this]
    "Converts the node given into a hiccup-format data structure. The
     node must have an implementation of the HiccupRepresentable
     protocol; nodes created by parse or parse-fragment already do."))

(extend-protocol HiccupRepresentable
  Attribute
  ;; Note the attribute value is not html-escaped; see comment for Element.
  (as-hiccup [this] [(utils/lower-case-keyword (.getKey this))
                     (.getValue this)])
  Attributes
  (as-hiccup [this] (into {} (map as-hiccup this)))
  Comment
  (as-hiccup [this] (str "<!--" (.getData this) "-->"))
  DataNode
  (as-hiccup [this] (str this))
  Document
  (as-hiccup [this] (map as-hiccup (.childNodes this)))
  DocumentType
  (as-hiccup [this] (utils/render-doctype (.attr this "name")
                                          (.attr this "publicid")
                                          (.attr this "systemid")))
  Element
  (as-hiccup [this]
    ;; There is an issue with the hiccup format, which is that it
    ;; can't quite cover all the pieces of HTML, so anything it
    ;; doesn't cover is thrown into a string containing the raw
    ;; HTML. This presents a problem because it is then never the case
    ;; that a string in a hiccup form should be html-escaped (except
    ;; in an attribute value) when rendering; it should already have
    ;; any escaping. Since the HTML parser quite properly un-escapes
    ;; HTML where it should, we have to go back and un-un-escape it
    ;; wherever text would have been un-escaped. We do this by
    ;; html-escaping the parsed contents of text nodes, and not
    ;; html-escaping comments, data-nodes, and the contents of
    ;; unescapable nodes.
    (let [tag (utils/lower-case-keyword (.tagName this))]
      (into [] (concat [tag
                        (as-hiccup (.attributes this))]
                       (if (utils/unescapable-content tag)
                         (map str (.childNodes this))
                         (map as-hiccup (.childNodes this)))))))
  TextNode
  ;; See comment for Element re: html escaping.
  (as-hiccup [this] (utils/html-escape (.getWholeText this)))
  XmlDeclaration
  (as-hiccup [this] (str this)))

(defn- get-attrs [node]
  (->> (.attributes node)
       (map (fn [attr] [(utils/lower-case-keyword (.getKey attr)) (.getValue attr)]))
       (into {})
       not-empty))

(defn- jsoup-zip [jsoup-doc]
  (zip/zipper
    (comp #{Document Element} type)
    #(.childNodes %)
    (fn [node children]
      (condp = (type node)
        Document {:type :document :content (vec children)}
        Element {:type :element
                 :tag (utils/lower-case-keyword (.tagName node))
                 :attrs (get-attrs node)
                 :content (when-not (= [nil] children) (vec children))}))
    jsoup-doc))

(defn- postwalk-zip [f zip-loc]
  (loop [loc zip-loc]
    (if (zip/end? loc)
      (f (zip/node loc))
      (recur (zip/next (zip/replace loc (f (zip/node loc))))))))

(defn as-hickory [jsoup-doc]
  (->> jsoup-doc
       jsoup-zip
       (postwalk-zip
         (fn [node]
           (condp = (type node)
             Comment {:type :comment :content [(.getData node)]}
             DataNode (str node)
             DocumentType {:type :document-type :attrs (get-attrs node)}
             TextNode (.getWholeText node)
             node)))))

(defn parse
  "Parse an entire HTML document into a DOM structure that can be
   used as input to as-hiccup or as-hickory."
  [s]
  (Jsoup/parse s))

(defn parse-fragment
  "Parse an HTML fragment (some group of tags that might be at home somewhere
   in the tag hierarchy under <body>) into a list of DOM elements that can
   each be passed as input to as-hiccup or as-hickory."
  [s]
  (into [] (Parser/parseFragment s (Element. (Tag/valueOf "body") "") "")))
