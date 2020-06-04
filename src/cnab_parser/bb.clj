(ns cnab-parser.bb
  (:require [cnab-parser.core :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def ^:const ^:private bb400-parser
  (make-cnab-parser (-> "bb400.edn"
                        io/resource
                        slurp)))

(def ^:const ^:private bb240-parser
  (make-cnab-parser (-> "bb240.edn"
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
              (case (long args-pos)
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
              (case (long args-pos)
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

(defmethod parse-cnab-header-arquivo [:bb240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :remessa} bb240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-header-lote [:bb240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [header_lote]} :remessa} bb240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_lote))))

(defmethod parse-cnab-detalhes [:bb240 :remessa]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_p segmento_q segmento_r segmento_s segmento_y01 segmento_y04
                  segmento_y05 segmento_y50 segmento_y51 segmento_y52 segmento_l] :as detalhes} :detalhes} :remessa} bb240-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_p]
                                       [cnab-unit segmento_q]
                                       [cnab-unit segmento_y52]
                                       [cnab-unit segmento_l]
                                       [cnab-unit segmento_r]
                                       [cnab-unit segmento_s]
                                       [cnab-unit segmento_y01]
                                       [cnab-unit segmento_y04]
                                       [cnab-unit segmento_y05]
                                       [cnab-unit segmento_y50]
                                       [cnab-unit segmento_y51]
                                       ]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case (long args-pos)
                0 {:segmento_p res}
                1 {:segmento_q res}
                2 {:segmento_y52 res}
                3 {:segmento_l res}
                4 {:segmento_r res}
                5 {:segmento_s res}
                6 {:segmento_y01 res}
                7 {:segmento_y04 res}
                8 {:segmento_y05 res}
                9 {:segmento_y50 res}
                10 {:segmento_y51 res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-lote [:bb240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_lote]} :remessa} bb240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_lote))))

(defmethod parse-cnab-trailer-arquivo [:bb240 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :remessa} bb240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))


(defmethod parse-cnab [:bb240 :remessa]
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
(comment (-> (parse-cnab (slurp "/home/severo/Documentos/cnab-exemplo/bb240/0955509_.rem")
                         :bb240
                         :remessa
                         )
             :detalhes))

(defmethod parse-cnab-header-arquivo [:bb240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :retorno} bb240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-header-lote [:bb240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [header_lote]} :retorno} bb240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_lote))))

(defmethod parse-cnab-detalhes [:bb240 :retorno]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_t segmento_u] :as detalhes} :detalhes} :retorno} bb240-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_t]
                                       [cnab-unit segmento_u]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case (long args-pos)
                0 {:segmento_t res}
                1 {:segmento_u res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-lote [:bb240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_lote]} :retorno} bb240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_lote))))

(defmethod parse-cnab-trailer-arquivo [:bb240 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :retorno} bb240-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))


(defmethod parse-cnab [:bb240 :retorno]
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
