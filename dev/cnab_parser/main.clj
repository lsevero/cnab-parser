(ns cnab-parser.main
  (:require [cnab-parser
             [core :refer :all]
             [bb :refer :all]
             [bradesco :refer :all]
             [itau :refer :all]]
            [clojure.pprint :as pp])
  (:gen-class))

(defn- print-help
  []
  (println "cnab-parser.main\n")
  (println "um pequeno utilitário para chamar a função parse-cnab direto da linha de comando")
  (println "cnab-parser -h | --help pra escrever esta ajuda")
  (println "cnab-parser <path> <modelo-cnab> <tipo> para executar o parse-cnab")
  (println "ou")
  (println "cnab-parser <path> <modelo-cnab> <tipo> <show-warnings> para executar o parse-cnab e mostrar todas as exceções de parse")
  (println "onde <path> é o caminho do cnab")
  (println "     <modelo-cnab> é o modelo incluindo banco e tamanho por exemplo 'bb240' ou 'itau400'")
  (println "     <tipo> deve ser ou 'remessa' ou 'retorno'")
  (println "     <show-warnings> deve ser 'true' se quiser os warnings"))

(defn -main [& args]
  (let [[path modelo tipo & _] args]
    (cond (count args)
          3 (binding [*suppress-warnings* true]
              (pp/pprint (parse-cnab (slurp path) (keyword modelo) (keyword tipo))))
          4 (pp/pprint (parse-cnab (slurp path) (keyword modelo) (keyword tipo)))
          (print-help))))
