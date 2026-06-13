#!/bin/bb

(ns combos
  (:refer-clojure :exclude [keys])
  (:require [clojure.string :as str]))

(def keys {:q "LT4"
           :w "LT3"
           :f "LT2"
           :p "LT1"
           :b "LT0"

           :a "LM4"
           :r "LM3"
           :s "LM2"
           :t "LM1"
           :g "LM0"

           :z "LB4"
           :x "LB3"
           :c "LB2"
           :d "LB1"
           :v "LB0"

           :j "RT0"
           :l "RT1"
           :u "RT2"
           :y "RT3"
           :quot "RT4"

           :m "RM0"
           :n "RM1"
           :e "RM2"
           :i "RM3"
           :o "RM4"

           :k "RB0"
           :h "RB1"
           :comma "RB2"
           :dot "RB3"
           :slash "RB4"})

(defn kw->key [kw]
  (if (string? kw)
    kw
    (let [default (str/upper-case (name kw))]
      (get keys kw default))))

(defn kw->binding [kw]
  (if (string? kw)
    kw
    (str "&kp " (str/upper-case (name kw)))))

(def combos
  {[:w :p] :tilde
   [:w :f] :exclamation
   [:f :p] :semicolon
   [:s :p] :at

   [:x :c] :plus
   [:c :t] :minus
   [:x :d] :asterisk
   [:s :d] :equal

   [:t :f] :backslash

   [:w :r] :left_bracket
   [:f :s] :left_brace
   [:p :t] :left_parenthesis
   [:b :g] :less_than

   [:l :y] :hash
   [:l :u] :colon
   [:u :y] :dollar
   [:e :l] :percent

   [:r :s :t] "&sk LEFT_SHIFT"

   [:n :e :i] "&sk RIGHT_SHIFT"

   [:e :h] :grave
   [:h :dot] :ampersand
   [:n :comma] :underscore
   [:comma :dot] :caret

   [:n :u] :pipe

   [:j :m] :greater_than
   [:l :n] :right_parenthesis
   [:u :e] :right_brace
   [:y :i] :right_bracket

   [:a :z] :N1
   [:r :x] :N2
   [:s :c] :N3
   [:t :d] :N4
   [:g :v] :N5
   [:m :k] :N6
   [:n :h] :N7
   [:e :comma] :N8
   [:i :dot] :N9
   [:o :slash] :N0})

(def combo-format
  "%s { timeout-ms = <%d>; key-positions = <%s>; bindings = <%s>; };")

(def combo->timeout {[:l :u] 20})

(defn combos->header-lines [combos]
  (let [with-names (map-indexed (fn [idx combo]
                                  (conj combo (str "combo_" idx)))
                                combos)]
    (-> []
        (conj "    combos {")
        (conj "       compatible = \"zmk,combos\";")
        (into (for [[keys binding name] with-names
                    :let [keystr (->> keys
                                      (map kw->key)
                                      (str/join " "))
                          bindingstr (kw->binding binding)
                          timeout (get combo->timeout keys 50)]]
                (format combo-format name timeout keystr bindingstr)))
        (conj "    };"))))

(->> combos
     combos->header-lines
     (str/join "\n")
     (spit "combos.h"))
