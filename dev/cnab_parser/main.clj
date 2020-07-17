(ns cnab-parser.main
  (:import [java.io File])
  (:require [cnab-parser
             [core :refer :all]
             [bb :refer :all]
             [bradesco :refer :all]
             [itau :refer :all]
             [santander :refer :all]
             [paulista :refer :all]]
            [clojure.pprint :as pp]
            [cheshire.core :as json]
            [seesaw
             [core :as sc]
             [dev :as sd]
             [forms :as sf]
             [chooser :as s-file]
             ])
  (:gen-class))

(defn- print-help
  []
  (println "cnab-parser.main\n")
  (println "um pequeno utilitário para chamar a função parse-cnab direto da linha de comando")
  (println "cnab-parser sem argumentos inicia a interface grafica")
  (println "cnab-parser -h | --help pra escrever esta ajuda")
  (println "cnab-parser <path> <modelo-cnab> <tipo> para executar o parse-cnab")
  (println "ou")
  (println "cnab-parser <path> <modelo-cnab> <tipo> <show-warnings> para executar o parse-cnab e mostrar todas as exceções de parse")
  (println "onde <path> é o caminho do cnab")
  (println "     <modelo-cnab> é o modelo incluindo banco e tamanho por exemplo 'bb240' ou 'itau400'")
  (println "     <tipo> deve ser ou 'remessa' ou 'retorno'")
  (println "     <show-warnings> deve ser 'true' se quiser os warnings"))

(defn- show-frame
  []
  (sc/invoke-later
    (sc/show!
      (sc/pack!
        (sc/frame :title "Cnab Parser"
                  :minimum-size [500 :by 500]
                  :size [800 :by 800]
                  :resizable? true
                  :content (let [textarea (sc/text :text ""
                                                   :multi-line? true
                                                   :editable? false
                                                   :rows 40
                                                   :columns 80)
                                 cnab (sc/combobox :id :cnab :model ["itau400" "bb240" "bb400" "bradesco240" "paulista444" "santander240"])
                                 tipo (sc/combobox :id :tipo :model ["remessa" "retorno"])
                                 output (sc/combobox :id :output :model ["json" "edn"])
                                 arquivo (sc/text :text "Clique aqui"
                                                  :editable? false
                                                  :listen [:mouse-clicked #(sc/text! % (.getAbsolutePath ^File (s-file/choose-file :type :open
                                                                                                                                   :multi? false
                                                                                                                                   :remember-directory? true)))])]
                             (sc/scrollable
                               (sf/forms-panel 
                                 "pref"
                                 :default-dialog-border? true
                                 :items [(sf/separator "Opções do cnab")
                                         "cnab:" cnab 
                                         "tipo:" tipo 
                                         (sf/next-line)
                                         "output:" output
                                         "arquivo:" arquivo
                                         (sf/next-line)
                                         (sc/button :text "Parsear!"
                                                    :listen [:mouse-clicked (fn [e]
                                                                              (try
                                                                                (sc/text! textarea 
                                                                                          (let [parsed (into {} (parse-cnab (-> arquivo sc/value slurp)
                                                                                                                            (-> cnab sc/value keyword) 
                                                                                                                            (-> tipo sc/value keyword)))]
                                                                                            (if (= "json" (sc/value output))
                                                                                              (json/generate-string parsed {:pretty true})
                                                                                              (with-out-str (pp/pprint parsed)))))
                                                                                (catch Exception ex (sc/alert e "Cnab não válido para o formato"))))])
                                         (sf/next-line)
                                         (sf/separator)
                                         (sc/scrollable textarea)]))))))))

(defn -main [& args]
  (let [[path modelo tipo & _] args]
    (case (count args)
      0 (show-frame)
      3 (binding [*suppress-warnings* true]
          (pp/pprint (parse-cnab (slurp path) (keyword modelo) (keyword tipo))))
      4 (pp/pprint (parse-cnab (slurp path) (keyword modelo) (keyword tipo)))
      (print-help))))

