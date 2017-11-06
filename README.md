```clojure
(ns forms)

;;;;;;

(defprotocol IValidator
  (_validator-artifact [this])
  (_validate-field [this value]))

(defrecord MinValidator [limit]

  IValidator

  (_validator-artifact [_]
    :error/min-validator)

  (_validate-field [_ value]
    (>= value limit)))

#_(defrecord RangeValidator [v1 v2]

  IValidator

  (validate [this value]
    (and (>= value v1)
         (<= value v2))))

#_(defrecord RegexValidator [pattern]

  IValidator

  (validate [this value]
    (re-find pattern value)))

#_(defrecord EnumValidator [values]

  IValidator

  (validate [this value]
    (contains? (set values) value)))

;;;;;;;

(defprotocol IField
  (_field-type [this])
  (_clean-field [this value]))

(defrecord IntegerField [required nullable validators]

  IField

  (_field-type [this] :integer)

  (_clean-field
    [this value]
    (cond
      (integer? value) value
      (string? value) (Integer/parseInt value))))

#_(defn integer-field [& args]
  (apply ->IntegerField args))

(def field-defaults
  {:required true
   :nullable false})

(defn create-field [map-fn name & {:as args}]
  (let [field (map-fn (merge field-defaults args))]
    (assoc field
           :type (_field-type field)
           :name name)))

(def integer-field
  (partial create-field map->IntegerField))

(defn set-field-value
 [field value]
  (assoc field :value value))

(defn set-field-error
  [field error]
  (update field :errors concat [error]))

(defn set-field-clean-value
  [field clean-value]
  (assoc field :clean-value clean-value))

(defn get-field-clean-value
  [field]
  (get field :clean-value))

(defn field-set?
  [field]
  (contains? field :value))

(defn field-cleaned?
  [field]
  (contains? field :clean-value))

(defn get-clean-value
  [field]
  (:clean-value field))

(defmacro with-safe [& body]
  `(try
     ~@body
     (catch Throwable e#)))

(defn clean-field
  [{:keys [required nullable] :as field}]
  (if (field-set? field)
    (let [clean-value (with-safe (_clean-field field (:value field)))]
      (set-field-clean-value field clean-value))
    field))

(defn apply-field-validator
  [field validator]
  (if (_validate-field validator (get-clean-value field))
    field
    (set-field-error field (_validator-artifact field))))

(defn validate-field
  [{:keys [required nullable] :as field}]

  (cond

    (and (field-set? field)
         (not (field-cleaned? field)))
    (set-field-error field :error/clean)

    ;; required, but there is no value
    (and required (not (field-set? field)))
    (set-field-error field :error/required)

    ;; it's not nullable, but has nil value
    (and (not nullable)
         (field-cleaned? field)
         (nil? (get-field-clean-value field)))
    (set-field-error field :error/nullable)

    :default
    (loop [field field
           validators (:validators field)]
      (if (empty? validators)
        field
        (recur (apply-field-validator field (first validators))
               (rest validators))))))

;;;;;;

(defrecord Form
    [fields validators])

(defn field-exists?
  [form field-name]
  (get-in form [:fields field-name]))

(defn set-form-value
  [form field value]
  (if (field-exists? form field)
    (update-in form [:fields field] set-field-value value)
    form))

(defn set-form-data
  [form data]
  (let [pairs (vec data)]
    (loop [form form
           pairs pairs]
      (if (empty? pairs)
        form
        (let [[field value] (first pairs)
              form (set-form-value form field value)]
          (recur form (rest pairs)))))))

(defn clean-form-field
  [form field-name]
  (if-let [field (get-in form [:fields field-name])]
    (update-in form [:fields field-name] (clean-field field))
    form))

(defn clean-form
  [form]
  (let [pairs (-> form :fields vec)]
    (loop [form form
           pairs pairs]
      (if (empty? pairs)
        form
        (let [[field field-obj] (first pairs)
              form (update-in
                    form
                    [:fields field]
                    clean-field)]
          (recur form (rest pairs)))))))

(defn validate-form
  [form]
  (let [pairs (-> form :fields vec)]
    (loop [form form
           pairs pairs]
      (if (empty? pairs)
        form
        (let [[field field-obj] (first pairs)
              form (update-in
                    form
                    [:fields field]
                    validate-field)]
          (recur form (rest pairs)))))))

(defn field? [obj]
  (satisfies? IField obj))

(defn validator? [obj]
  (satisfies? IValidator obj))

(defn create-form [& args]
  (let [fields (filterv field? args)
        validators (filterv validator? args)]
    (->Form
     (into {} (for [[i field] (map vector (range) fields)]
                [(:name field) (assoc field :index i)]))
     validators)))

(def f
  (create-form
   (integer-field :age)
   (integer-field :foo)))

```
