(ns cnab-parser.bradesco
  (:require [cnab-parser.core :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def ^:const ^:private bradesco240-parser
  (make-cnab-parser (-> "bradesco240.edn"
                        io/resource
                        slurp)))
(def ^:const ^:private bradesco400-parser
  (make-cnab-parser (-> "bradesco400.edn"
                        io/resource
                        slurp)))
(def ^:const ^:private bradesco444-parser
  (make-cnab-parser (-> "bradesco444.edn"
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
              (case (long args-pos)
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
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer-arquivo padrao cnab-type)}))


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
              (try-args parse-detalhe [[cnab-unit segmento_u]
                                       [cnab-unit segmento_t]
                                       [cnab-unit segmento_y01]
                                       [cnab-unit segmento_y50]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case (long args-pos)
                0 {:segmento_u res}
                1 {:segmento_t res}
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
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer-arquivo padrao cnab-type)}))

(defmethod parse-cnab-header-arquivo [:bradesco400 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :remessa} bradesco400-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-detalhes [:bradesco400 :remessa]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_1 segmento_2 segmento_3 segmento_7] :as detalhes} :detalhes} :remessa} bradesco400-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_1]
                                       [cnab-unit segmento_2]
                                       [cnab-unit segmento_3]
                                       [cnab-unit segmento_7]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case (long args-pos)
                0 {:segmento_1 res}
                1 {:segmento_2 res}
                2 {:segmento_3 res}
                3 {:segmento_7 res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-arquivo [:bradesco400 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :remessa} bradesco400-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))

(defmethod parse-cnab [:bradesco400 :remessa]
  [cnab padrao cnab-type]
  (let [[header & detalhes_trailer] (split-cnab (if (string? cnab)
                                                  cnab
                                                  (slurp cnab)) 400)
        {:keys [retorno]} bradesco400-parser
        detalhes (butlast detalhes_trailer)
        trailer (last detalhes_trailer)]
    {:header_arquivo (parse-cnab-header-arquivo header padrao cnab-type)
     :detalhes (parse-cnab-detalhes detalhes padrao cnab-type)
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer padrao cnab-type)}))

(defmethod parse-cnab-header-arquivo [:bradesco400 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :retorno} bradesco400-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))


(defmethod parse-cnab-detalhes [:bradesco400 :retorno]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_1 segmento_3] :as detalhes} :detalhes} :retorno} bradesco400-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_1]
                                       [cnab-unit segmento_3]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case (long args-pos)
                0 {:segmento_1 res}
                1 {:segmento_3 res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-arquivo [:bradesco400 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :retorno} bradesco400-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))

(defmethod parse-cnab [:bradesco400 :retorno]
  [cnab padrao cnab-type]
  (let [[header & detalhes_trailer] (split-cnab (if (string? cnab)
                                                  cnab
                                                  (slurp cnab)) 400)
        {:keys [retorno]} bradesco400-parser
        detalhes (butlast detalhes_trailer)
        trailer (last detalhes_trailer)]
    {:header_arquivo (parse-cnab-header-arquivo header padrao cnab-type)
     :detalhes (parse-cnab-detalhes detalhes padrao cnab-type)
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer padrao cnab-type)}))

(defmethod parse-cnab-header-arquivo [:bradesco444 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :remessa} bradesco444-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-detalhes [:bradesco444 :remessa]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_1 segmento_2 segmento_3 segmento_7] :as detalhes} :detalhes} :remessa} bradesco444-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_1]
                                       [cnab-unit segmento_2]
                                       [cnab-unit segmento_3]
                                       [cnab-unit segmento_7]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case (long args-pos)
                0 {:segmento_1 res}
                1 {:segmento_2 res}
                2 {:segmento_3 res}
                3 {:segmento_7 res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-arquivo [:bradesco444 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :remessa} bradesco444-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))

(defmethod parse-cnab [:bradesco444 :remessa]
  [cnab padrao cnab-type]
  (let [[header & detalhes_trailer] (split-cnab (if (string? cnab)
                                                  cnab
                                                  (slurp cnab)) 444)
        {:keys [retorno]} bradesco444-parser
        detalhes (butlast detalhes_trailer)
        trailer (last detalhes_trailer)]
    {:header_arquivo (parse-cnab-header-arquivo header padrao cnab-type)
     :detalhes (parse-cnab-detalhes detalhes padrao cnab-type)
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer padrao cnab-type)}))

(defmethod parse-cnab-header-arquivo [:bradesco444 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :retorno} bradesco444-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))


(defmethod parse-cnab-detalhes [:bradesco444 :retorno]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_1 segmento_3] :as detalhes} :detalhes} :retorno} bradesco444-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_1]
                                       [cnab-unit segmento_3]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case (long args-pos)
                0 {:segmento_1 res}
                1 {:segmento_3 res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-arquivo [:bradesco444 :retorno]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :retorno} bradesco444-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))

(defmethod parse-cnab [:bradesco444 :retorno]
  [cnab padrao cnab-type]
  (let [[header & detalhes_trailer] (split-cnab (if (string? cnab)
                                                  cnab
                                                  (slurp cnab)) 444)
        {:keys [retorno]} bradesco444-parser
        detalhes (butlast detalhes_trailer)
        trailer (last detalhes_trailer)]
    {:header_arquivo (parse-cnab-header-arquivo header padrao cnab-type)
     :detalhes (parse-cnab-detalhes detalhes padrao cnab-type)
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer padrao cnab-type)}))

(comment (-> (parse-cnab (slurp "/home/littlefinger/Documentos/cnab-exemplo/bradesco444/bradesco444.txt")
                         :bradesco444
                         :retorno) :detalhes))
