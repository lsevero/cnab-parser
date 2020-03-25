(ns cnab-parser.itau
  (:require [cnab-parser.core :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def ^:const ^:private itau400-parser
  (make-cnab-parser (-> "itau400.edn"
                        io/resource
                        slurp)))

(defmethod parse-cnab-header-arquivo [:itau400 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :retorno} itau400-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-detalhes [:itau400 :retorno]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_1 segmento_4] :as detalhes} :detalhes} :retorno} itau400-parser]
    #_(prn "seg1 " segmento_1)
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_1]
                                       [cnab-unit segmento_4]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case args-pos
                0 {:segmento_1 res}
                1 {:segmento_4 res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-arquivo [:itau400 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :retorno} itau400-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))


(defmethod parse-cnab [:itau400 :retorno]
  [cnab padrao cnab-type]
  (let [[header & detalhes_trailer] (split-cnab (if (string? cnab)
                                                  cnab
                                                  (slurp cnab)) 400)
        {:keys [retorno]} itau400-parser
        detalhes (butlast detalhes_trailer)
        trailer (last detalhes_trailer)]
    {:header_arquivo (parse-cnab-header-arquivo header padrao cnab-type)
     :detalhes (parse-cnab-detalhes detalhes padrao cnab-type)
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer padrao cnab-type)}
    ))

(comment (-> (parse-cnab (slurp "/home/severo/Documentos/cnab-exemplo/CN02030A.RET")
               ;(io/file "/home/severo/Documentos/cnab-exemplo/CN02030A.RET")
                         :itau400
                         :retorno
                         )
             :detalhes
             (nth 2)
             :segmento_1
             :nome_pagador

             ))

