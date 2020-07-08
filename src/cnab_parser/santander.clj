(ns cnab-parser.santander
  (:require [cnab-parser.core :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def ^:const ^:private santander240-parser
  (make-cnab-parser (-> "santander240.edn"
                        io/resource
                        slurp)))

(defmethod parse-cnab-header-arquivo [:santander240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :remessa} santander240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-header-lote [:santander240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [header_lote]} :remessa} santander240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_lote))))

(defmethod parse-cnab-detalhes [:santander240 :remessa]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_p segmento_q segmento_r segmento_s] :as detalhes} :detalhes} :remessa} santander240-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_p]
                                       [cnab-unit segmento_q]
                                       [cnab-unit segmento_r]
                                       [cnab-unit segmento_s]
                                       ]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case (long args-pos)
                0 {:segmento_p res}
                1 {:segmento_q res}
                2 {:segmento_r res}
                3 {:segmento_s res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-lote [:santander240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_lote]} :remessa} santander240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_lote))))

(defmethod parse-cnab-trailer-arquivo [:santander240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :remessa} santander240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))


(defmethod parse-cnab [:santander240 :remessa]
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
     }))

(defmethod parse-cnab-header-arquivo [:santander240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :retorno} santander240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-header-lote [:santander240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [header_lote]} :retorno} santander240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_lote))))

(defmethod parse-cnab-detalhes [:santander240 :retorno]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_t segmento_u] :as detalhes} :detalhes} :retorno} santander240-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_t]
                                       [cnab-unit segmento_u]
                                       ]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case (long args-pos)
                0 {:segmento_t res}
                1 {:segmento_u res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-lote [:santander240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_lote]} :retorno} santander240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_lote))))

(defmethod parse-cnab-trailer-arquivo [:santander240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :retorno} santander240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))


(defmethod parse-cnab [:santander240 :retorno]
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
     }))

