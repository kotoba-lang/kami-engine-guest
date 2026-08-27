;; host_import_test.cljs — the reference interpreter must be able to run a
;; module that calls host imports.
;;
;; Before this, `invoke` threw on every `:host-import` ("no ported test
;; exercises host calls"), so the interpreter could execute only the toy
;; modules the ported tests used. Every real game script calls into
;; `kami:engine@1.0.0`, so the interpreter could not run one — which also
;; meant there was no oracle to check a second backend against.
;;
;;   nbb --classpath src:test test/kotoba/engine_clj/host_import_test.cljs
(require '[kotoba.engine-clj.ast :as ast]
         '[kotoba.engine-clj.codegen :as cg]
         '[kotoba.engine-clj.interp :as interp]
         '[cljs.reader :as rdr])

(def failures (atom []))
(defn check [label ok] (swap! failures #(if ok % (conj % label))))

;; --- arity accounting -------------------------------------------------------
;; A :string-handle is passed as (ptr, len) and so occupies TWO stack slots.
;; Counting param-kinds instead pops one value too few, and the imbalance only
;; surfaces at the end of the enclosing function, far from the call.
(check "string-handle costs two slots" (= 2 (interp/host-arity [:string-handle])))
(check "scalars cost one slot each"    (= 4 (interp/host-arity [:i64 :f32 :f32 :f32])))
(check "mixed"                          (= 5 (interp/host-arity [:string-handle :f32 :f32 :f32])))

;; --- a module that actually calls the host ----------------------------------
(def src "(defn go [] (let [e (spawn-entity \"player\")] (set-position! e (f32 1.5) (f32 2.5) (f32 0.0)) e))")
(def m (cg/compile (ast/parse-program (rdr/read-string (str "[" src "]")))))

(def calls (atom []))
(def host
  {:host-import/scene-spawn (fn [_ ptr len] (swap! calls conj [:spawn ptr len]) 42)
   :host-import/scene-set-position (fn [_ e x y z] (swap! calls conj [:set-pos e x y z]) 0)})

(def result
  (try (interp/call-export (interp/instantiate m host) "go" [])
       (catch :default e (str "THREW: " (.-message e)))))

(check "module ran to completion" (= 42 result))
(check "spawn was called with (ptr,len)"
       (= 3 (count (first (filter #(= :spawn (first %)) @calls)))))
(check "void import received all four arguments"
       (= 5 (count (first (filter #(= :set-pos (first %)) @calls)))))
(check "f32 arguments arrive as their values, not as raw bit patterns"
       (let [[_ _ x y z] (first (filter #(= :set-pos (first %)) @calls))]
         (and (< 1.4 x 1.6) (< 2.4 y 2.6) (= 0 z))))
(check "the entity id flowed from spawn into set-position"
       (= 42 (second (first (filter #(= :set-pos (first %)) @calls)))))

;; --- an unbound import must say so, not silently do nothing -----------------
(check "unbound host import raises"
       (try (interp/call-export (interp/instantiate m {}) "go" []) false
            (catch :default e (boolean (re-find #"no host function" (.-message e))))))

(if (seq @failures)
  (do (println "FAILED:" (count @failures)) (doseq [f @failures] (println "  -" f)) (js/process.exit 1))
  (println "host_import_test: 9 checks passed"))
