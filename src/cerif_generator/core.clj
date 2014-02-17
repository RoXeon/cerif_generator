(ns cerif-generator.core
  (:use     [clojure.tools.cli :only (cli)])
  (:use     [clojure.set :only (union)])
  (:require [clojure.string :as s])
  (:require [clojure.data.json :as json])
  (:require [clojure.data.generators :as rnd])
  (:use     [clojure.data.xml :only (element emit)])
  (:gen-class))

(def date (java.util.Date.))

(defrecord cfMultilang [cfLangCode cfTrans])

;; Language codes
(def langs (for [l1 (range (+ 32 65) (+ 32 91)) l2 (range (+ 32 65) (+ 32 91))] (apply str [(char l1) (char l2)])))

;; Countries codes
(def countries (for [l1 (range 65 91) l2 (range 65 91)] (apply str [(char l1) (char l2)])))

;; Curriencies codes
(def curr_codes (for [l1 (range 65 91) l2 (range 65 91) l3 (range 65 91)] (apply str [(char l1) (char l2) (char l3)])))

(def random (java.util.Random.))
;; define characters list to use to generate string
(def g_chars
  (map char (concat (range 65 91) (range 97 123))))


(defn random-char
  "generates 1 random character"
  ([]
   (random-char g_chars))
  ([l_chars]
   (nth l_chars (.nextInt random (count l_chars)))))


(defn random-string
  "generates random string of length characters"
  [length]
  (apply str (take length (repeatedly random-char))))


(defn in?
  "true if seq contains elm"
  [seq elm]
  (some #(= elm %) seq))


(defn uuid
  "generates random UUID"
  []
  (doall (str (java.util.UUID/randomUUID))))


(defn class_to_id
  [class_name]
  (str "id-" (.toLowerCase (.replaceAll class_name " " "-"))))


(defn map_deep_key_convert
  "Converts all map keys to :keyword. Works for all nasted maps."
  [item]
  (cond
    (map? item) (into {}
                     (for [[k v] item]
                       [(keyword k) (map_deep_key_convert v)]))
    (vector? item) (vec
                     (for [elem item]
                       (map_deep_key_convert elem)))
    (set? item) (set
                  (for [elem item]
                    (map_deep_key_convert elem)))
    (coll? item) (for [elem item]
                   (map_deep_key_convert elem))
    :else item))


(defn gen_multilang_elem
  [config state tag]
  [(doall (concat [
                    (element (keyword tag) (cfMultilang. (rand-nth langs) "o") (random-string 10))]
                  (repeatedly (rand-int 3) #(element (keyword tag) (cfMultilang. (rand-nth langs) "h") (random-string 10)))))
   state])

(defn gen_cfOrgUnit
  [config state]
  (let [OrgUnitId (uuid)]
    [(element :cfOrgUnit {}
              (element :cfOrgUnitId {}                                  OrgUnitId)
              (element :cfAcro      {}                                  (random-string 10))
              (element :cfHeadcount {}                                  (str (rand-int 50)))
              (element :cfTurn      {:cfCurrCode (rand-nth curr_codes)} (format "%3.2f" (* (rnd/float) (rand-int 100))))
              (element :cfName      (cfMultilang. (rand-nth langs) "o") (get (get config "cfOrgUnit") "name" (random-string 10)))
              (element :cfKeyw      (cfMultilang. (rand-nth langs) "o") (random-string 10))
              (element :cfResAct    (cfMultilang. (rand-nth langs) "o") (random-string 10))) ;; @todo: unknown fileld format
     (assoc (assoc state :OrgUnitId #{OrgUnitId}) :MainOrgUnitId OrgUnitId)]))

(defn gen_cfOrgUnitDep
  [config {OrgUnitId :MainOrgUnitId :as state}]
  (let [DepId (uuid)]
    [[(element :cfOrgUnit {}
              (element :cfOrgUnitId {}                                  DepId)
              (element :cfAcro      {}                                  (random-string 10))
              (element :cfHeadcount {}                                  (str (rand-int 50)))
              (element :cfTurn      {:cfCurrCode (rand-nth curr_codes)} (format "%3.2f" (* (rnd/float) (rand-int 100))))
              (element :cfName      (cfMultilang. (rand-nth langs) "o") (first (shuffle (get config "names" [(random-string 10)]))))
              (element :cfKeyw      (cfMultilang. (rand-nth langs) "o") (random-string 10))
              (element :cfResAct    (cfMultilang. (rand-nth langs) "o") (random-string 10))
              (element :cfOrgUnit_OrgUnit {}
                       (element :cfOrgUnitId1     {} OrgUnitId)
                       (element :cfClassId        {} "id-part-of")
                       (element :cfClassSchemeId  {} "id-scheme-inter-organisationa-structure"))
              (element :cfOrgUnit_Class {}
                       (element :cfClassId        {} (class_to_id (first (shuffle (:classes config [(random-string 10)])))))
                       (element :cfClassSchemeId  {} "id-scheme-departament-types")))]
     (assoc-in state [:OrgUnitId] (union (:OrgUnitId state #{}) #{DepId}))]))


(defn gen_cfCountry
  [config {OrgUnitId :OrgUnitId :as state} code]
  [(element :cfCountry {}
                (element :cfCountryCode {} code)
                (element :cfName      (cfMultilang. (rand-nth langs) "o") (random-string 10)))
       (assoc-in state [:countries] (concat (:countries state []) [code]))])

(defn gen_cfCountry_items
  [{unit_config :cfOrgUnit :as config} init_state]
  (loop [state init_state, elems [], codes (take (rand-int (* 26 26)) (shuffle countries))]
    (let [[elem new_state] (gen_cfCountry (:cfCountry config) state (first codes))]
      (if (not-empty codes)
        (recur new_state (concat elems (list elem)) (rest codes))
        [elems state]))))

(defn gen_cfPAddr
  [config {codes :countries :as state}]
  (loop [i 1, n (rand-int 5), addr_lines []]
    (if (<= i (+ 1 n))
      (recur (inc i), n
             (concat addr_lines
               [(element (keyword (apply str (concat "cfAddrline" (str i)))) {} (random-string (rand-int 80)))]))
      (let [uuid (uuid)]
        [[(element :cfPAddr {}
                  (element :cfPAddrId         {} uuid)
                  (element :cfCountryCode     {} (first (shuffle codes)))
                  addr_lines
                  (element :cfPostCode        {} (str (format "%02d" (rand-int 100)) "-" (format "%02d" (rand-int 1000))))
                  (element :cfCityTown        {} (random-string 10))
                  (element :cfStateOfCountry  {} (random-string 10)))]
         (assoc-in state [:addrs] (union (:addrs state #{}) #{uuid}))]))))

(defn gen_cfPAddr_items
  [{unit_config :cfOrgUnit :as config} init_state]
  (reduce (fn
            [[elem1 state1] [elem2 state2]]
            [(doall (concat elem1 elem2)) (assoc-in state1 [:addrs] (doall (union (:addrs state1 #{}) (:addrs state2 #{}))))])
          (repeatedly (:count (:cfPAddr unit_config) (+ 10 (rand-int 100))) #(gen_cfPAddr (:cfPAddr unit_config {}) init_state)))
  )

(defn gen_cfEquip
  [config {OrgUnitId :OrgUnitId :as state}]
  (let [ uuid           (uuid)
         [name state1]  (gen_multilang_elem config state :cfName)
         [descr state2] (gen_multilang_elem config state1 :cfDescr)
         [keyw state3]  (gen_multilang_elem config state2 :cfKeyw) ]
    [[(element :cfEquip {}
               (element :cfEquipId {} uuid)
               (element :cfAcro    {} (random-string (+ 3 (rand-int 13))))
               name
               descr
               keyw
               (element :cfEquip_Class {}
                        (element :cfClassId       {} (class_to_id (first (shuffle (:classes config [(random-string 10)])))))
                        (element :cfClassSchemeId {} "id-scheme-euipment-types")))]
     (assoc-in state3 [:equips] (union (:equips state3 #{}) #{uuid}))]))

(defn gen_cfFacil
  [config {OrgUnitId :OrgUnitId :as state}]
  (let [ uuid            (uuid)
         [name state1]   (gen_multilang_elem config state :cfName)
         [descr state2]  (gen_multilang_elem config state1 :cfDescr)
         [keyw state3]   (gen_multilang_elem config state2 :cfKeyw) ]
    [[(element :cfFacil {}
               (element :cfFacilId {} uuid)
               (element :cfAcro    {} (random-string (+ 3 (rand-int 13))))
               name
               descr
               keyw
               ;; @todo: Equipment -> Facility
               (element :cfFacil_Class {}
                        (element :cfClassId       {} (class_to_id (first (shuffle (:classes config [(random-string 10)])))))
                        (element :cfClassSchemeId {} "id-scheme-facilite-types")))]
     (assoc-in state3 [:facils] (union (:facils state3 #{}) #{uuid}))]))


(defn gen_cfPers
  [config {OrgUnits :OrgUnitId :as state}]
  (let [ uuid (uuid) ]
    [[(element :cfPers {}
               (element :cfPersId     {} uuid)
               (element :cfGender     {} (str (random-char [\f \m])))
               ;(element :cfBirthdate  {} (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") date)) ;; @todo: generate random date instead emiting current time
               (element :cfPersName_Pers  {}
                        (element :cfPersNameId    {}  (str "id-internal-person-name-" uuid))
                        (element :cfClassId       {}  "id-known-as-name")
                        (element :cfClassSchemeId {}  "id-person-name-types")
                        (element :cfFamilyNames   {}  (random-string (+ 4 (rand-int 10))))
                        (element :cfFirstNames    {}  (random-string (+ 4 (rand-int 10)))))
               (element :cfPers_OrgUnit   {}
                        (element :cfOrgUnitId     {}  (first (shuffle OrgUnits)))
                        (element :cfClassId       {}  (class_to_id (first (shuffle (:classes config [(random-string 10)])))))
                        (element :cfClassSchemeId {}  "id-person-roles")))]
           (assoc-in state [:pers] (union (:pers state #{}) #{uuid}))]))

(defn gen_cfEquip_items
  [{unit_config :cfOrgUnit :as config} init_state]
  (reduce (fn
            [[elem1 state1] [elem2 state2]]
            [(doall (concat elem1 elem2)) (assoc-in state1 [:equips] (doall (union (:equips state1 #{}) (:equips state2 #{}))))])
          (repeatedly (:count (:cfEquip unit_config) (+ 10 (rand-int 100))) #(gen_cfEquip (:cfEquip unit_config {}) init_state)))
  )

(defn gen_cfFacil_items
  [{unit_config :cfOrgUnit :as config} init_state]
  (reduce (fn
            [[elem1 state1] [elem2 state2]]
            [(doall (concat elem1 elem2)) (assoc-in state1 [:facils] (doall (union (:facils state1 #{}) (:facils state2 #{}))))])
          (repeatedly (:count (:cfFacil unit_config) (+ 10 (rand-int 100))) #(gen_cfFacil (:cfFacil unit_config {}) init_state)))
  )

(defn gen_cfOrgUnitDep_items
  [{unit_config :cfOrgUnit :as config} init_state]
  (reduce (fn
            [[elem1 state1] [elem2 state2]]
            [(doall (concat elem1 elem2)) (assoc-in state1 [:OrgUnitId] (doall (union (:OrgUnitId state1 #{}) (:OrgUnitId state2 #{}))))])
          (repeatedly (:count (:cfOrgUnit unit_config) (+ 10 (rand-int 100))) #(gen_cfOrgUnitDep (:cfOrgUnit unit_config {}) init_state))))


(defn gen_cfPers_items
  [{unit_config :cfOrgUnit :as config} init_state]
  (reduce (fn
            [[elem1 state1] [elem2 state2]]
            [(doall (concat elem1 elem2)) (assoc-in state1 [:pers] (doall (union (:pers state1 #{}) (:pers state2 #{}))))])
          (repeatedly (:count (:cfPers unit_config) (+ 10 (rand-int 100))) #(gen_cfPers (:cfPers unit_config {}) init_state))))

(defn gen_cerif
  [config]
  (let [
          [cfOrgUnitXML    cfOrgUnitState]    (gen_cfOrgUnit          config {})
          [cfOrgUnitDepXML cfOrgUnitDepState] (gen_cfOrgUnitDep_items config cfOrgUnitState)
          [cfCountryXML    cfCountryState]    (gen_cfCountry_items    config cfOrgUnitDepState)
          [cfPAddrXML      cfPAddrState]      (gen_cfPAddr_items      config cfCountryState)
          [cfFacilXML      cfFacilState]      (gen_cfFacil_items      config cfPAddrState)
          [cfEquipXML      cfEquipState]      (gen_cfEquip_items      config cfFacilState)
          [cfPersXML       cfPersState]       (gen_cfPers_items       config cfEquipState)
       ]
          (element :CERIF { :xmlns "urn:xmlns:org:eurocris:cerif-1.6-2"
                            :xmlns:xs "http://www.w3.org/2001/XMLSchema"
                            :xmlns:xsi "http://www.w3.org/2001/XMLSchema-instance"
                            :xsi:schemaLocation "urn:xmlns:org:eurocris:cerif-1.6-2 http://www.eurocris.org/Uploads/Web%20pages/CERIF-1.6/CERIF_1.6_2.xsd"
                            :date (.format (java.text.SimpleDateFormat. "yyyy-MM-dd") date) :sourceDatabase "empty"}
                 cfOrgUnitXML
                 cfOrgUnitDepXML
                 cfCountryXML
                 cfPAddrXML
                 cfFacilXML
                 cfEquipXML
                 cfPersXML)))

(defn -main
  "CERIF Generator main function"
  [& args]

  (let [[opts _ help_banner]
        (cli args
             ["-h" "--help" "Show help" :flag true :default false]
             ["-c" "--config" "Config file path (JSON format)" :default "config.json"]
             ["-o" "--output" "Output XML file" :default "cerif.xml"]
             )]

    (when (:help opts)
      (println help_banner)
      (System/exit 0))

    (let [config (map_deep_key_convert (json/read-str (slurp (:config opts))))]
      (with-open [out-file (java.io.FileWriter. (:output opts))]
        (emit (gen_cerif config) out-file))
      (println "XML output saved as:" (:output opts)))))
