(ns cnab-parser.core
  (:require [clojure.java.io :as io]
            [clojure.string :as s]
            [clojure.edn :as edn] 
            [clojure.tools.logging :as log]
            [clojure.string :as str]
            [clojure.test :refer [is]]
            ))

(def ^:dynamic *suppress-warnings* false)

(defn make-cnab-parser
  "Cria um parser de cnab a partirda sua espeficicação em edn

  Precisa seguir o formato estabelecido em https://github.com/lsevero/cnab-layouts

  path: caminho para o edn de especificação do cnab
  retorno: um mapa contendo a espeficicação do contendo tanto o retorno e a remessa
  "
  [content]
  (edn/read-string content))


(defn split-cnab
  "Divide o cnab em lote de n bytes"
  [^String cnab ^long n]
  (map (partial apply str) (partition n (s/replace cnab #"\r|\n" ""))))

(defn parse-cnab-field
  "Parseia um campo do cnab.
  Levanta uma exceção caso o intervalo definido em :pos não tenha o mesmo tamanho que o definido em picture.
  "
  [^String cnab-part {picture :picture [begin end :as pos] :pos :as spec}]
  {:pre [(is (and (contains? spec :picture)
                  (contains? spec :pos)))
         (is (= (- end (dec begin))
                (apply + (map #(Long/parseLong (% 1)) (re-seq #"\((\d+)\)" picture)))))]}
  (try
    (let [field (subs cnab-part (dec begin) end)]
      (cond
        (s/starts-with? picture "9")
        (if (s/includes? picture "V")
          (let [[[_ size1] [_ _]] (re-seq #"\((\d+)\)" picture)]
            (Double/parseDouble (str (subs field 0 (Long/parseLong size1))
                                     "."
                                     (subs field (Long/parseLong size1)))))
          (if (<= (count field) 18)
            (Long/parseLong field)
            (BigInteger. field)))
        (s/starts-with? picture "X") field
        :else (throw (ex-info "Picture não está definido nem como número (9) nem como string (X)"
                              {:msg "Erro em picture"
                               :pos pos
                               :picture picture}))))
    (catch Exception e (if *suppress-warnings*
                         (throw e)
                         (do
                           (log/debug "parse-cnab-field fail! cnab-part: " cnab-part " spec: " spec " exception: " e)
                           (throw e)))))) 

(defn write-cnab-field
  "Escreve o campo de um cnab para um StringBuilder"
  [^StringBuilder builder content {picture :picture [^Long begin ^Long end :as pos] :pos default :default :as spec}]
  {:pre [(is (and (contains? spec :picture)
                  (contains? spec :pos)))
         (is (= (- end (dec begin))
                (apply + (map #(Long/parseLong (% 1)) (re-seq #"\((\d+)\)" picture)))))]}
  (try
    (cond
      (s/starts-with? picture "9")
      (if (s/includes? picture "V")
        (let [[[_ integer-part-size] [_ decimal-part-size]] (re-seq #"\((\d+)\)" picture)
              longnum (-> 
                        (str "%." decimal-part-size "f")
                        (format (or content default 0.0))
                        (str/replace #"\.|," "")
                        (#(Long/parseLong %)))
              total-sum (+ (Long/parseLong integer-part-size) (Long/parseLong decimal-part-size))]
          (.replace builder (long (dec begin)) end (format (str "%0" total-sum "d") longnum)))
        (let [[[_ integer-part-size]] (re-seq #"\((\d+)\)" picture)]
          (.replace builder (long (dec begin)) end (format (str "%0" integer-part-size "d") (or content default 0)))))
      (s/starts-with? picture "X") (let [[[_ size]] (re-seq #"\((\d+)\)" picture)]
                                     (.replace builder (long (dec begin)) end (format (str "%" size "s") (or content default ""))))
      :else (throw (ex-info "Picture não está definido nem como número (9) nem como string (X)"
                            {:msg "Erro"
                             :builder builder
                             :content content
                             :spec spec
                             })))
    (catch Exception e
      (throw (let [error-map {:msg "Erro"
                              :ex e
                              :builder builder
                              :content content
                              :spec spec}] (ex-info (str "Erro de formatação" error-map) error-map))))))

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

(defmulti write-cnab dispatch)
(defmulti write-cnab-header-arquivo dispatch)
(defmulti write-cnab-header-lote dispatch)
(defmulti write-cnab-detalhes dispatch)
(defmulti write-cnab-trailer-lote dispatch)
(defmulti write-cnab-trailer-arquivo dispatch)

(defmethod write-cnab :default
  [cnab padrao cnab-type]
  (let [error-msg 
        (str "Não existe implementações de write-cnab para o padrão " 
             padrao " tipo " cnab-type)]
    (throw (ex-info error-msg
                    {:cnab cnab
                     :padrao padrao
                     :cnab-type cnab-type}))))

(defmethod write-cnab-header-arquivo :default
  [cnab padrao cnab-type]
  (let [error-msg 
        (str "Não existe implementações de write-cnab-header-arquivo para o padrão "
             padrao " tipo " cnab-type)]
    (throw (ex-info error-msg
                    {:cnab cnab
                     :padrao padrao
                     :cnab-type cnab-type}))))

(defmethod write-cnab-header-lote :default
  [cnab padrao cnab-type]
  nil)

(defmethod write-cnab-detalhes :default
  [cnab padrao cnab-type]
  (let [error-msg (str "Não existe implementações de write-cnab-detalhes para o padrão "
                       padrao " tipo " cnab-type)]
    (throw (ex-info error-msg
                    {:cnab cnab
                     :padrao padrao
                     :cnab-type cnab-type}))))

(defmethod write-cnab-trailer-lote :default
  [cnab padrao cnab-type]
  nil)

(defmethod write-cnab-trailer-arquivo :default
  [cnab padrao cnab-type]
  (let [error-msg 
        (str "Não existe implementações de write-cnab-trailer-arquivo para o padrão " 
             padrao " tipo " cnab-type)]
    (throw (ex-info error-msg
                    {:cnab cnab
                     :padrao padrao
                     :cnab-type cnab-type})))) 

(defn try-fns
  "Receives a list of functions and a list of args and iterate applying each functions to args,
  if the function throws an Exception, try the next one. Returns a map with :fn-pos Long/MIN_VALUE if all functions raises a exception. "
  ([fns args ^long n]
   (let [[head & tail] fns
         random-sym (gensym)
         ans (try (apply head args)
                  (catch Exception e random-sym))]
     (if (= ans random-sym)
       (if-not (empty? tail)
         (recur tail args (inc n))
         {:error "no matching function"
          :args args
          :fn-pos Long/MIN_VALUE}
         )
       {:res ans
        :fn head
        :fn-pos n
        :args args})))
  ([fns args]
   (try-fns fns args 0)))

(defn try-args
  "Receives a function and a list of list of args and iterate applying the function to each args,
  if the function throws an Exception, try the next one. Returns a map with :args-pos Long/MIN_VALUE if all functions raises a exception."
  ([f args ^long n]
   (let [[head & tail] args
         random-sym (gensym)
         ans (try (apply f head)
                  (catch Exception e random-sym))]
     (if (= ans random-sym)
       (if-not (empty? tail)
         (recur f tail (inc n))
         {:error "no matching argument"
          :fn f
          :args-pos Long/MIN_VALUE})
       {:res ans
        :fn f
        :args-pos n
        :args head})))
  ([f args]
   (try-args f args 0)))

