(require 'cl)
(require 's)

;; 0. collect the entire list of lines

(defun read-lines ()
  (s-split "\n" (buffer-substring-no-properties (point-min) (point-max))))


;; 1. collect the lines starting with `case class'
(defun collect-case-class (string-list)
  (cl-loop for string in string-list
           if (s-starts-with? "case class" string)
           collect string))


(defun get-class-name (case-class-string)
  (let* ((break-on-open-paren (s-split "(" case-class-string))
         (break-on-space (s-split " " (car break-on-open-paren))))
    (caddr break-on-space)))


(defun get-param-list-with-types (case-class-string)
  (let* ((open-paren-index (s-index-of "(" case-class-string))
         (closing-paren-index (s-index-of ")" case-class-string)))
    (substring case-class-string
               open-paren-index
               (+ closing-paren-index 1))))


(defun get-param-list-without-types (case-class-string)
  (let* ((param-list-with-types (get-param-list-with-types case-class-string))
         (break-on-comma (s-split "," param-list-with-types))
         (break-each-on-colon (mapcar (lambda (str) (car (s-split ":" (s-trim-left str))))
                                      break-on-comma))
         (strip-parens (mapcar (lambda (str) (->> str
                                                  (string-remove-prefix "(")
                                                  (string-remove-suffix ")")))
                               break-each-on-colon)))
    (concat "(" (cl-reduce (lambda (acc elem)
                             (concat acc elem ", ")) (butlast strip-parens) :initial-value "")
            (car (last strip-parens)) ")")))


(defun make-apply-method (class-name
                          param-list-with-types
                          param-list-wihtout-types)
  (concat "  def apply" param-list-with-types " = new " class-name param-list-without-types))


(defun assemble! (case-class-string)
  (let* ((class-name (get-class-name case-class-string))
         (param-list-with-types (get-param-list-with-types case-class-string))
         (param-list-without-types (get-param-list-without-types case-class-string)))
    (concat case-class-string " {\n" (make-apply-method class-name
                                                       param-list-with-types
                                                       param-list-without-types) "\n}")))

(defun run! (case-class-string-list)
  (mapcar #'assemble! (remove-if-not (lambda (str)
                                       (s-starts-with? "case class" str))
                                     case-class-string-list)))

(defun run! (case-class-string-list)
  (mapcar (lambda (str)
            (if (s-starts-with? "case class" str)
                (assemble! str)
              str)) case-class-string-list))


(dolist (string (run! (read-lines)))
  (insert (concat string "\n")))
