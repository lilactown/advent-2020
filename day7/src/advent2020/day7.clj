(ns advent2020.day7
  (:require
   [clojure.java.io :as io]
   [clojure.string :as string]))

(def example-rules
  "light red bags contain 1 bright white bag, 2 muted yellow bags.
dark orange bags contain 3 bright white bags, 4 muted yellow bags.
bright white bags contain 1 shiny gold bag.
muted yellow bags contain 2 shiny gold bags, 9 faded blue bags.
shiny gold bags contain 1 dark olive bag, 2 vibrant plum bags.
dark olive bags contain 3 faded blue bags, 4 dotted black bags.
vibrant plum bags contain 5 faded blue bags, 6 dotted black bags.
faded blue bags contain no other bags.
dotted black bags contain no other bags.")


(defn parse-color-key
  [key-spec]
  (string/replace key-spec " bags" ""))


(defn parse-rule
  [rule-spec]
  (let [split-rule (string/split rule-spec #", ")]
    (if (= ["no other bags"] split-rule)
      []
      (mapv
       (fn [s]
         (let [[quantity color] (rest (re-matches #"(\d) (.+) bag(?:s)?" s))]
           {:color color
            :quantity (Integer/parseInt quantity)}))
       split-rule))))


(defn parse-input
  [input]
  (->> input
       (re-seq #"(.+) contain (.+).")
       (map (fn [[_ color-key rule-spec]]
              [(parse-color-key color-key)
               (parse-rule rule-spec)]))))


(parse-input example-rules)
;; => (["light red" [{:color "bright white", :quantity 1} {:color "muted yellow", :quantity 2}]] ["dark orange" [{:color "bright white", :quantity 3} {:color "muted yellow", :quantity 4}]] ["bright white" [{:color "shiny gold", :quantity 1}]] ["muted yellow" [{:color "shiny gold", :quantity 2} {:color "faded blue", :quantity 9}]] ["shiny gold" [{:color "dark olive", :quantity 1} {:color "vibrant plum", :quantity 2}]] ["dark olive" [{:color "faded blue", :quantity 3} {:color "dotted black", :quantity 4}]] ["vibrant plum" [{:color "faded blue", :quantity 5} {:color "dotted black", :quantity 6}]] ["faded blue" []] ["dotted black" []])


(defn part-1
  [parsed-input]
  (let [bag->container (reduce (fn [acc [container contains]]
                                 (loop [acc acc
                                        colors contains]
                                   (if-let [color (:color (first colors))]
                                     (recur (update acc color conj container)
                                            (rest colors))
                                     acc)))
                               {}
                               parsed-input)]
    (loop [colors ["shiny gold"]
           valid-containers #{}] ;; store found containers in set to avoid dups
      (if-let [color (first colors)]
        (let [containers (get bag->container color)]
          (recur (-> colors
                     (rest) ;; drop this color
                     (into containers))
                 (into valid-containers containers)))
        (count valid-containers)))))


(part-1 parsed-example)
;; => 4


(def input (slurp (io/resource "day7-input")))


(part-1 (parse-input input))
;; => 208

(defn containers
  [parsed-input]
  (into {} parsed-input))


(defn part-2
  [parsed-input]
  (let [container->bags (containers parsed-input)]
    (letfn [(count-bags [{:keys [quantity color]}]
              (let [bags (get container->bags color)]
                (if (seq bags)
                  (apply + quantity
                         (map #(* quantity (count-bags %)) bags))
                  quantity)))]
      ;; remove 1 because we don't count the shiny gold bag
      (dec (count-bags {:color "shiny gold"
                        :quantity 1})))))


(part-2 parsed-example)
;; => 32


(def parsed-example2
  (parse-input
   "shiny gold bags contain 2 dark red bags.
dark red bags contain 2 dark orange bags.
dark orange bags contain 2 dark yellow bags.
dark yellow bags contain 2 dark green bags.
dark green bags contain 2 dark blue bags.
dark blue bags contain 2 dark violet bags.
dark violet bags contain no other bags."))


(part-2 parsed-example2)
;; => 126


(part-2 (parse-input input))
;; => 1664
