(ns cnab-parser.bb
  (:require [cnab-parser.core :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def ^:const ^:private bb400-parser
  (make-cnab-parser (-> "bb400.edn"
                        io/resource
                        slurp)))

(defmethod parse-cnab-header-arquivo [:bb400 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :retorno} bb400-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-detalhes [:bb400 :retorno]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_7 segmento_2 segmento_5 segmento_3 segmento_5_01 segmento_5_04 segmento_5_06] :as detalhes} :detalhes} :retorno} bb400-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_7]
                                       [cnab-unit segmento_2]
                                       [cnab-unit segmento_5]
                                       [cnab-unit segmento_3]
                                       [cnab-unit segmento_5_01]
                                       [cnab-unit segmento_5_04]
                                       [cnab-unit segmento_5_06]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case args-pos
                0 {:segmento_7 res}
                1 {:segmento_2 res}
                2 {:segmento_5 res}
                3 {:segmento_3 res}
                4 {:segmento_5_01 res}
                5 {:segmento_5_04 res}
                6 {:segmento_5_06 res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-arquivo [:bb400 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :retorno} bb400-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))


(defmethod parse-cnab [:bb400 :retorno]
  [cnab padrao cnab-type]
  (let [[header & detalhes_trailer] (split-cnab (if (string? cnab)
                                                  cnab
                                                  (slurp cnab)) 400)
        {:keys [retorno]} bb400-parser
        detalhes (butlast detalhes_trailer)
        trailer (last detalhes_trailer)]
    {:header_arquivo (parse-cnab-header-arquivo header padrao cnab-type)
     :detalhes (parse-cnab-detalhes detalhes padrao cnab-type)
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer padrao cnab-type)
     }
    ))
(comment (-> (parse-cnab (slurp "/home/severo/Documentos/cnab-exemplo/bb0605sr.rem")
                         :bb400
                         :remessa
                         )
             :detalhes
             ))

(defmethod parse-cnab-header-arquivo [:bb400 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :remessa} bb400-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-detalhes [:bb400 :remessa]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_7 segmento_5_99 segmento_5_01 segmento_5_03] :as detalhes} :detalhes} :remessa} bb400-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_7]
                                       [cnab-unit segmento_5_99]
                                       [cnab-unit segmento_5_01]
                                       [cnab-unit segmento_5_03]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case args-pos
                0 {:segmento_7 res}
                1 {:segmento_5_99 res}
                2 {:segmento_5_01 res}
                3 {:segmento_5_03 res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-arquivo [:bb400 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :remessa} bb400-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))


(defmethod parse-cnab [:bb400 :remessa]
  [cnab padrao cnab-type]
  (let [[header & detalhes_trailer] (split-cnab (if (string? cnab)
                                                  cnab
                                                  (slurp cnab)) 400)
        {:keys [remessa]} bb400-parser
        detalhes (butlast detalhes_trailer)
        trailer (last detalhes_trailer)]
    {:header_arquivo (parse-cnab-header-arquivo header padrao cnab-type)
     :detalhes (parse-cnab-detalhes detalhes padrao cnab-type)
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer padrao cnab-type)
     }
    ))

(comment (-> (parse-cnab (slurp "/home/severo/Documentos/cnab-exemplo/bb0605sr.rem")
                         :bb400
                         :remessa
                         )
             :detalhes
             ))

