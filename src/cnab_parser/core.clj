(ns cnab-parser.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]))

(defn make-cnab-parser
  "Cria um parser de cnab a partirda sua espeficicação em YAML

  Precisa seguir o formato estabelecido em https://github.com/lsevero/cnab-layouts

  path: caminho para o yaml de especificação do cnab
  retorno: um mapa contendo a espeficicação do contendo tanto o retorno e a remessa
  "
  [path]
  (letfn [(process-detalhes [detalhes]
            (apply merge (map (fn [[k v]] v) detalhes)))]
    (let [{{detalhes_remessa :detalhes} :remessa
           {detalhes_retorno :detalhes} :retorno
           :as spec} (yaml/from-file path)]
      (-> spec
          (assoc-in [:remessa :detalhes] (process-detalhes detalhes_remessa))
          (assoc-in [:retorno :detalhes] (process-detalhes detalhes_retorno))))))


(defn split-cnab
  "Divide o cnab em lote de n bytes"
  [^String cnab ^long n]
  (map (partial apply str) (partition n (s/replace cnab #"\r|\n" ""))))

(defn parse-cnab-field
  "Parseia um campo do cnab.
  Levanta uma exceção caso o intervalo definido em :pos não tenha o mesmo tamanho que o definido em picture.
  "
  [^String cnab-part {picture :picture [begin end :as pos] :pos :as spec}]
  {:pre [(and (contains? spec :picture)
              (contains? spec :pos))
         (= (- end (dec begin))
            (apply + (map #(Long/parseLong (% 1)) (re-seq #"\((\d+)\)" picture))))]}
  (let [field (subs cnab-part (dec begin) end)]
    (cond
      (s/starts-with? picture "9")
      (if (s/includes? picture "V")
        (let [[[_ size1] [_ _]] (re-seq #"\((\d+)\)" picture)]
          (Double/parseDouble (str (subs field 0 (Long/parseLong size1))
                                   "."
                                   (subs field (Long/parseLong size1)))))
        (Long/parseLong field))
      (s/starts-with? picture "X") field
      :else (throw (ex-info "Picture não está definido nem como número (9) nem como string (X)"
                            {:msg "Erro em picture"
                             :pos pos
                             :picture picture})))))

(defn- dispatch
  [cnab padrao cnab-type]
  [padrao cnab-type])

(defmulti parse-cnab dispatch)
(defmulti parse-cnab-header-arquivo dispatch)
(defmulti parse-cnab-header-lote dispatch)
(defmulti parse-cnab-detalhes dispatch)
(defmulti parse-cnab-trailer-lote dispatch)
(defmulti parse-cnab-trailer-arquivo dispatch)

(defmethod parse-cnab :default
  [cnab padrao cnab-type]
  (let [error-msg 
        (str "Não existe implementações de parse-cnab para o padrão " 
             padrao " tipo " cnab-type)]
    (throw (ex-info error-msg
                    {:cnab cnab
                     :padrao padrao
                     :cnab-type cnab-type}))))

(defmethod parse-cnab-header-arquivo :default
  [cnab padrao cnab-type]
  (let [error-msg 
        (str "Não existe implementações de parse-cnab-header-arquivo para o padrão "
             padrao " tipo " cnab-type)]
    (throw (ex-info error-msg
                    {:cnab cnab
                     :padrao padrao
                     :cnab-type cnab-type}))))

(defmethod parse-cnab-header-lote :default
  [cnab padrao cnab-type]
  nil)

(defmethod parse-cnab-detalhes :default
  [cnab padrao cnab-type]
  (let [error-msg (str "Não existe implementações de parse-cnab-detalhes para o padrão "
                       padrao " tipo " cnab-type)]
    (throw (ex-info error-msg
                    {:cnab cnab
                     :padrao padrao
                     :cnab-type cnab-type}))))

(defmethod parse-cnab-trailer-lote :default
  [cnab padrao cnab-type]
  nil)

(defmethod parse-cnab-trailer-arquivo :default
  [cnab padrao cnab-type]
  (let [error-msg 
        (str "Não existe implementações de parse-cnab-trailer-arquivo para o padrão " 
             padrao " tipo " cnab-type)]
    (throw (ex-info error-msg
                    {:cnab cnab
                     :padrao padrao
                     :cnab-type cnab-type})))) 

(comment
  (do
    (def itau (spit (io/file "/tmp/itau400.edn") (yaml/from-file "/home/severo/Documentos/cnab-layouts/config/itau/cnab400/cobranca.yml") ))
    (apply merge (:remessa itau)))
  (def detalhes (get-in  itau [:remessa :detalhes]))
  (def header (get-in itau [:remessa :header_arquivo]))
  (def spec {:header header
             :detalhes (apply merge (map (fn [[ k v]] v) detalhes))})
  (make-cnab-parser "/home/severo/Documentos/cnab-layouts/config/itau/cnab400/cobranca.yml")
  (type (make-cnab-parser (-> "itau/cnab400/cobranca.yml" io/resource io/file)))

  (->> (partition 400 (s/replace (slurp "/home/severo/Documentos/cnab-exemplo/CN14020A.RET") #"\r|\n" ""))
       (map (partial apply str))
       )

  (parse-cnab-field "00012345" {:pos [1 8] :picture "9(6)V9(2)"})

  )

