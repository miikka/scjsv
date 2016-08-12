(ns scjsv.core-test
  (:require [midje.sweet :refer :all]
            [scjsv.core :as v]
            [clojure.java.io :as io])
  (:import [com.github.fge.jsonschema.main JsonSchemaFactory]))

(fact "Validating JSON string against JSON Schema (as string)"
  (let [schema (slurp (io/resource "scjsv/schema.json"))
        validate (v/json-validator schema)
        valid (slurp (io/resource "scjsv/valid.json"))
        invalid (slurp (io/resource "scjsv/invalid.json"))]
    (validate valid) => nil
    (validate invalid) =not=> nil))

(fact "Validating Clojure data against JSON Schema (as Clojure)"
  (let [schema {:$schema "http://json-schema.org/draft-04/schema#"
                :type "object"
                :properties {:billing_address {:$ref "#/definitions/address"}
                             :shipping_address {:$ref "#/definitions/address"}}
                :definitions {:address {:type "object"
                                        :properties {:street_address {:type "string"}
                                                     :city {:type "string"}
                                                     :state {:type "string"}}
                                        :required ["street_address", "city", "state"]}}}
        validate (v/validator schema)
        validate-with-explicit-factory (v/validator schema (JsonSchemaFactory/byDefault))
        valid {:shipping_address {:street_address "1600 Pennsylvania Avenue NW"
                                  :city "Washington"
                                  :state "DC"}
               :billing_address {:street_address "1st Street SE"
                                 :city "Washington"
                                 :state "DC"}}
        invalid (update-in valid [:shipping_address] dissoc :state)]

    (validate valid) => nil
    (validate invalid) =not=> nil

    (validate-with-explicit-factory valid) => nil
    (validate-with-explicit-factory invalid) =not=> nil

    (fact "validation errors are lovey clojure maps"
      (validate invalid)
      => [{:domain "validation"
           :instance {:pointer "/shipping_address"}
           :keyword "required"
           :level "error"
           :message "object has missing required properties ([\"state\"])"
           :missing ["state"]
           :required ["city" "state" "street_address"]
           :schema {:loadingURI "#"
                    :pointer "/definitions/address"}}])))

(fact "References to a schema with ID."
  (let [schema (slurp (io/resource "scjsv/with_id.json"))
        validate (v/validator schema)
        valid {:foo "foo"}]
    (validate valid) => nil))
