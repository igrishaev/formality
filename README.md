```clojure
(ns forms)

;;;;;;

(defprotocol IValidator
  (_validator-artifact [this])
  (_validate-field [this value]))

(defrecord MinValidator [v]

  IValidator

  (_validator-artifact [this]
    :error/min-validator)

  (_validate-field [this value]
    (>= value v)))

(defrecord RangeValidator [v1 v2]

  IValidator

  (_validator-artifact [this]
    :error/range-validator)

  (_validate-field [this value]
    (and (>= value v1)
         (<= value v2))))

(defrecord RegexValidator [pattern]

  IValidator

  (_validator-artifact [this]
    :error/regex-validator)

  (_validate-field [this value]
    (re-find pattern value)))

(defn min-validator [v]
  (->MinValidator v))

(defn ragne-validator [v1 v2]
  (->RangeValidator v1 v2))

(defn regex-validator [pattern]
  (->RegexValidator pattern))

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

(defrecord IntegerField []

  IField

  (_field-type [this] :integer)

  (_clean-field
    [this value]
    (cond
      (integer? value) value
      (string? value) (Integer/parseInt value))))

(defrecord ListField [sample fields]

  IField

  (_field-type [this] :list)

  (_clean-field
    [this values]

    (doseq [value values]
      (-> sample
          (set-field-value value)
          clean-field
          validate-field
          )
      )
    ))

(defrecord FormField [form]

  IField

  (_field-type [this] :form)

  (_clean-field
    [this data]
    )

  )



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

#_(defn list-field [field]

  (->ListField)
  )

(defn set-field-value
 [field value]
  (assoc field :value value))

(defn get-field-value
  [field]
  (:value field))

(defn field-set?
  [field]
  (contains? field :value))

(defn set-field-error
  [field error]
  (update field :errors concat [error]))

(defmacro with-safe [& body]
  `(try
     ~@body
     (catch Throwable e#)))

(defn clean-field
  [{:keys [required nullable] :as field}]
  (let [value (get-field-value field)]
    (if (and (field-set? field)
             (not (nil? value)))
      (if-let [clean-value (with-safe (_clean-field field value))]
        (set-field-value field clean-value)
        (set-field-error field :error/clean))
      field)))

(defn field-failed
  [field]
  (-> field :errors not-empty))

(defn apply-field-validator
  [field validator]
  (if (_validate-field validator (get-field-value field))
    field
    (set-field-error field (_validator-artifact validator))))

(defn validate-field
  [{:keys [required nullable] :as field}]

  (cond

    (field-failed field)
    field

    (and required (not (field-set? field)))
    (set-field-error field :error/required)

    (and (not nullable)
         (field-set? field)
         (nil? (get-field-value field)))
    (set-field-error field :error/nullable)

    (nil? (get-field-value field))
    field

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

(defn iter-form
  [form]
  (->> form :fields vals (sort-by :index)))

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

#_(defprotocol IWidget
  (render [this obj]))

#_(defrecord InputWidget []
  (render [this field]
    [:input {:name (:name field)
             :value (:value field)}]))

#_(defrecord FormWidget []
  (render [this form]
    (for [field (iter-form form)]
      (render field))))

(def f
  (create-form
   (integer-field :age)
   (integer-field :foo)))

```
