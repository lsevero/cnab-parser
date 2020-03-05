(ns cnab-parser.core-test
  (:require [clojure.test :refer :all]
            [clojure.java.io :as io]
            [cnab-parser.core :refer :all])
  (:import [clojure.lang ExceptionInfo]))

(deftest make-cnab-parser-test
  (testing "Testando o make-cnab-parser"
    (let [{{:keys [header_arquivo trailer_arquivo detalhes]} :retorno :as parser} (make-cnab-parser
                                                                    (-> "itau/cnab400/cobranca.yml"
                                                                        io/resource
                                                                        io/file))] 
      (is (true? (and (contains? parser :servico)
                      (contains? parser :versao)
                      (contains? parser :layout)
                      (contains? parser :remessa)
                      (contains? parser :retorno)))
          "Checando se a função make-cnab-parser consegue ler os arquivos de config dos resources")
      (is (and (some? header_arquivo)
               (some? trailer_arquivo)
               (some? detalhes))))))

(deftest parse-cnab-field-test
  (testing "Testando o parse-cnab-field"
    (is (= "00012345" (parse-cnab-field "00012345" {:pos [1 8] :picture "X(8)"})))
    (is (= 12345 (parse-cnab-field "00012345" {:pos [1 8] :picture "9(8)"})))
    (is (= 123.45 (parse-cnab-field "00012345" {:pos [1 8] :picture "9(6)V9(2)"})))))

(deftest split-cnab-test
  (testing "testando o split-cnab"
    (is (= ["123" "456" "789"] (into [] (split-cnab "123\r\n456\n789\r" 3))))))

(deftest parse-cnab-test
  (testing "testando os multimetodos default"
    (is (thrown? ExceptionInfo (parse-cnab "" :nubank400 :retorno)) "Chamando um multimetodo que nao existe")))
