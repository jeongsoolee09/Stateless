#!/Users/jslee/Library/Python/3.8/bin/hy

;; attempts to compile all .scala files
;; and checks if there are any compile errors

(import os)


(defn recursively-collect-scala []
  (setv walker (os.walk "."))
  (setv acc [])
  (for [(, directory subdirectories files) (list walker)]
    (for [file files]
      (when (in ".scala" file)
        (.append acc (os.path.join directory file)))))
  acc)


(defn compile-scala [file]
  (setv process (os.popen f"scalac {file}"))
  (.read process))


(defn error? [result]
  (in "error" result))


(defmain []
  (setv scala-files (recursively-collect-scala))
  (setv (, succeeded-files failed-files) (, [] []))
  (for [scala-file scala-files]
    (if (error? (compile-scala scala-file))
        (.append failed-files scala-file)
        (.append succeeded-files scala-file)))
  (for [succeeded-file succeeded-files]
    (print f"Compile succeeded for {succeeded-file}"))
  (for [failed-file failed-files]
    (print f"Compile failed for {failed-file}")))
