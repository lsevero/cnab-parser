(ns cnab-parser.paulista
  (:require [cnab-parser.core :refer :all]
            [clojure.string :as s]
            [clojure.java.io :as io]))

(def ^:const ^:private paulista444-parser
  (make-cnab-parser (-> "paulista444.edn"
                        io/resource
                        slurp)))

(defmethod parse-cnab-header-arquivo [:paulista444 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [header_arquivo]} :remessa} paulista444-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) header_arquivo))))

(defmethod parse-cnab-detalhes [:paulista444 :remessa]
  [cnabs padrao cnab-type]
  (let [{{{:keys [segmento_1] :as detalhes} :detalhes} :remessa} paulista444-parser]
    (letfn [(parse-detalhe [cnab map-spec]
              (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) map-spec)))
            (try-parse [cnab-unit]
              (try-args parse-detalhe [[cnab-unit segmento_1]]))]
      (map #(let [{:keys [args-pos args res] :as td} (try-parse %)]
              (case (long args-pos)
                0 {:segmento_1 res}
                {:error {:cnab %}}
                )) cnabs))))

(defmethod parse-cnab-trailer-arquivo [:paulista444 :remessa]
  [cnab padrao cnab-type]
  (let [{{:keys [trailer_arquivo]} :remessa} paulista444-parser]
    (into {} (map (fn [[k spec]] [k (parse-cnab-field cnab spec)]) trailer_arquivo))))


(defmethod parse-cnab [:paulista444 :remessa]
  [cnab padrao cnab-type]
  (let [[header & detalhes_trailer] (split-cnab (if (string? cnab)
                                                  cnab
                                                  (slurp cnab)) 444)
        {:keys [retorno]} paulista444-parser
        detalhes (butlast detalhes_trailer)
        trailer (last detalhes_trailer)]
    {:header_arquivo (parse-cnab-header-arquivo header padrao cnab-type)
     :detalhes (parse-cnab-detalhes detalhes padrao cnab-type)
     :trailer_arquivo (parse-cnab-trailer-arquivo trailer padrao cnab-type)}))

(comment (-> (parse-cnab (slurp "/home/severo/Documentos/liber/CB221000.rem")
                         :paulista444
                         :remessa
                         )
             :detalhes
             ))

