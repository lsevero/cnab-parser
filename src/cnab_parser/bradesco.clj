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
  (let [splitted-cnab (split-cnab (if (string? cnab) cnab (slurp cnab)) 240)
        header-arquivo (first splitted-cnab)
        header-lote (second splitted-cnab)
        detalhes (->> splitted-cnab
                      (drop 2)
                      (drop-last 2))
        trailer-lote (-> splitted-cnab butlast last)
        trailer-arquivo (last splitted-cnab)]
    {:header_arquivo (parse-cnab-header-arquivo header-arquivo padrao cnab-type)
     :header_lote (parse-cnab-header-lote header-lote padrao cnab-type)
     :detalhes (parse-cnab-detalhes detalhes padrao cnab-type)
     :trailer_lote (parse-cnab-trailer-lote trailer-lote padrao cnab-type)
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer-arquivo padrao cnab-type)
     }
    ))

(comment (-> (parse-cnab (slurp "/home/severo/Documentos/cnab-exemplo/bradesco240/RETORNO020301.RET")
                         :bradesco240
                         :remessa
                         )
             :detalhes
             ))

(defmethod parse-cnab-header-arquivo [:bradesco240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :retorno} bradesco240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-header-lote [:bradesco240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [header_lote]} :retorno} bradesco240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_lote))))

(defmethod parse-cnab-detalhes [:bradesco240 :retorno]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_t segmento_u segmento_y01 segmento_y50] :as detalhes} :detalhes} :retorno} bradesco240-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_t]
                                       [cnab-unit segmento_u]
                                       [cnab-unit segmento_y01]
                                       [cnab-unit segmento_y50]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case args-pos
                0 {:segmento_t res}
                1 {:segmento_u res}
                2 {:segmento_y01 res}
                3 {:segmento_y50 res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-lote [:bradesco240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_lote]} :retorno} bradesco240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_lote))))

(defmethod parse-cnab-trailer-arquivo [:bradesco240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :retorno} bradesco240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))


(defmethod parse-cnab [:bradesco240 :retorno]
  [cnab padrao cnab-type]
  (let [splitted-cnab (split-cnab (if (string? cnab) cnab (slurp cnab)) 240)
        header-arquivo (first splitted-cnab)
        header-lote (second splitted-cnab)
        detalhes (->> splitted-cnab
                      (drop 2)
                      (drop-last 2))
        trailer-lote (-> splitted-cnab butlast last)
        trailer-arquivo (last splitted-cnab)]
    {:header_arquivo (parse-cnab-header-arquivo header-arquivo padrao cnab-type)
     :header_lote (parse-cnab-header-lote header-lote padrao cnab-type)
     :detalhes (parse-cnab-detalhes detalhes padrao cnab-type)
     :trailer_lote (parse-cnab-trailer-lote trailer-lote padrao cnab-type)
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer-arquivo padrao cnab-type)
     }
    ))
(comment (-> (parse-cnab (slurp "/home/severo/Documentos/cnab-exemplo/bradesco240/RETORNO020301.RET")
                         :bradesco240
                         :retorno
                         )
             :detalhes
             ))
