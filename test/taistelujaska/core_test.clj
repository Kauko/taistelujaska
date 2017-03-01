(ns taistelujaska.core-test
  (:require [clojure.test :refer :all]
            [taistelujaska.core :refer :all]))

(deftest url-test
  (is (= "server/api?nimi=teemu"
        (build-url "server" "/api" {:nimi "teemu"})))
  (is (= "server/api?nimi=teemu"
         (build-url "server" "/api" {:nimi "teemu"
                                     :id nil})))
  (is (or (= "server/api?nimi=teemu&id=5"
          (build-url "server" "/api" {:nimi "teemu"
                                :id 5}))
          (= "server/api?id=5&nimi=teemu"
             (build-url "server" "/api" {:nimi "teemu"
                                   :id 5}))))
  (is (= "server/api"
        (build-url "server" "/api" nil)))
  (is (= "server/api"
         (build-url "server" "/api")))
  (is (= "server/api"
         (build-url "server" "/api" {:id nil}))))
