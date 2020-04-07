(ns cnab-parser.bradesco
  (:require [cnab-parser.core :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def ^:const ^:private bradesco240-parser
  (make-cnab-parser (-> "bradesco240.edn"
                        io/resource
                        slurp)))

(defmethod parse-cnab-header-arquivo [:bradesco240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :remessa} bradesco240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-header-lote [:bradesco240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [header_lote]} :remessa} bradesco240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_lote))))

(defmethod parse-cnab-detalhes [:bradesco240 :remessa]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_p segmento_q segmento_r segmento_s segmento_y01 segmento_y50] :as detalhes} :detalhes} :remessa} bradesco240-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_p]
                                       [cnab-unit segmento_q]
                                       [cnab-unit segmento_r]
                                       [cnab-unit segmento_s]
                                       [cnab-unit segmento_y01]
                                       [cnab-unit segmento_y50]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case args-pos
                0 {:segmento_p res}
                1 {:segmento_q res}
                2 {:segmento_r res}
                3 {:segmento_s res}
                4 {:segmento_y01 res}
                5 {:segmento_y50 res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-lote [:bradesco240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_lote]} :remessa} bradesco240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_lote))))

(defmethod parse-cnab-trailer-arquivo [:bradesco240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :remessa} bradesco240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))


(defmethod parse-cnab [:bradesco240 :remessa]
  [cnab padrao cnab-type]
  (let [splitted-cnab (split-cnab (if (string? cnab)
                                                  cnab
                                                  (slurp cnab)) 240)
        header-arquivo (first splitted-cnab)
        header-lote (second splitted-cnab)
        trailer-lote (-> splitted-cnab butlast last)
        trailer-arquivo (last splittled-cnab)
        ]
    {:header_arquivo (parse-cnab-header-arquivo header padrao cnab-type)
     :header_lote (parse-cnab-header-lote )
     :detalhes (parse-cnab-detalhes detalhes padrao cnab-type)
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer padrao cnab-type)
     }
    ))
(comment (-> (parse-cnab (slurp "/home/severo/Documentos/cnab-exemplo/bb0605sr.rem")
                         :bradesco240
                         :remessa
                         )
             :detalhes
             ))

