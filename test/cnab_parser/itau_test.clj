(ns cnab-parser.itau-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [cnab-parser.core :refer :all]
            [cnab-parser.itau :refer :all]
            ))

(deftest parse-cnab-header-arquivo-test
  (testing "parse-cnab-header-arquivo teste"
    (let [header-arquivo "02RETORNO01COBRANCA       098765432101        ABCASDLAPKJSDKLKASJDAKLSADKSKD341BANCO ITAU S.A.11111111111BPI22222222222                                                                                                                                                                                                                                                                                   000001"]
      (is (map? (parse-cnab-header-arquivo header-arquivo :itau400 :retorno))))))


(deftest parse-cnab-trailer-arquivo-test
  (testing "parse-cnab-trailer-arquivo teste"
    (let [trailer-arquivo "9201341          000000000000000000000000000000          000000000000000000000000000000                                                  000000000000000000000000000000          0000035300000084445941  02/03S002360000007700000020467957                                                                                                                                                                000079"]
      (is (map? (parse-cnab-trailer-arquivo trailer-arquivo :itau400 :retorno))))))

(deftest parse-cnab-detalhes-test
  (testing "parse-cnab-detalhes teste"
    (let [cnab 
"12345678901234567890123456789                                 12345678            109000014585             I020203200         00001458            05032000000001252413412861308000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000410000000000000         00000000000000000000000abcdefgjhijklmopqrstuvwxyzabcd                                        000002
12345678901234567890123456789                                 00001459            109000014593             I020203200002      00001459            29022000000001088873412897708000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000360000000000000         00000000000000000000000abcdefgjhijklmopqrstuvwxyzabcd                                        000003
12345678901234567890123456789                                 00001460            109000014601             I020203200002      00001460            09032000000000625593410667608000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000200000000000000         00000000000000000000000abcdefgjhijklmopqrstuvwxyzabcd                                        000004"
          cnab-units (split-cnab cnab 400)]
      (is (every? map? (parse-cnab-detalhes cnab-units :itau400 :retorno))))))
