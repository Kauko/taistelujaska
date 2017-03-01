(ns taistelujaska.api
  (:require [clj-http.client :as client]
            [clojure.string :as str]))

(def server-name "http://localhost:8080/api/")

(def player-name "taistelujaska")
(def password "makkaroita")

(defn build-url
  ([server endpoint] (build-url server endpoint nil))
  ([server endpoint params-map]
   (let [params-str (str/join
                      "&"
                      (keep
                        (fn [[k v]]
                          (when v
                            (str (name k)
                                 "="
                                 (if (keyword? v) (name v) v))))
                        params-map))
         params (when-not (empty? params-str) (str "?" params-str))]
     (str server endpoint params))))

(def url (partial build-url server-name))

(defn alive! []
  (client/get (url "alive")))

(defn- reset!* [password]
  (client/get (url "reset" {:pass password})))

(def reset-game! (partial reset!* "salasana"))

(defn turn-duration! []
  (client/get (url "turn-duration")))

(defn- board!* [name]
  (client/get (url "board" {:name name})))

(def board! (partial board!* player-name))

(defn- next-board!* [name]
  (client/get (url "next-board" {:name name})))

(def next-board! (partial next-board!* player-name))

(defn- add-player!* [name password]
  (client/get (url "add-player" {:name name
                                 :pass password})))

(def add-player! (partial add-player!* player-name password))

(defn- player!* [name password]
  (client/get (url "player" {:name name
                             :pass password})))

(def player! (partial player!* player-name password))

(defn- act!* [name password action target]
  (assert (#{:move :attack} action))
  (assert (#{:west :south :east :north nil} target))
  (client/get (url "act" {:name name
                          :pass password
                          :action action
                          :target target})))

(def act! (partial act!* player-name password))

(defn leaderboard! []
  (client/get (url "leaderboard")))