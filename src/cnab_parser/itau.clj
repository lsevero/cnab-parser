(ns cnab-parser.itau
  (:require [cnab-parser.core :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def ^:const ^:private itau400-parser
  (make-cnab-parser (-> "itau/cnab400/cobranca.yml"
                        io/resource
                        io/file)))

(defmethod parse-cnab-header-arquivo [:itau400 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :retorno} itau400-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(comment (parse-cnab-header-arquivo "09032000000001612903414684708000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000000530000000000000         00000000000000000000000EVELIN ARRAES DA COSTA FERREIR                                        000011
10227635377000173293800421456                                 00001468            109000014684             I020203200002      00001468           " :itau400 :retorno))

(defmethod parse-cnab [:itau400 :retorno]
  [cnab padrao cnab-type]
  (let [[header & detalhes_trailer] (split-cnab (slurp cnab-file) 400)
        {:keys [retorno]} itau400-parser
        detalhes (butlast detalhes_trailer)
        trailer (last detalhes_trailer)]
    nil
    ))
