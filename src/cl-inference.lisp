;;;; cl-inference.lisp

(in-package #:cl-inference)


(defvar *operators* '())

(defun register-operator (operator-symbol)
  (unless (member operator-symbol *operators*)
    (push operator-symbol *operators*)))

(defun operator-p (expression)
  (member expression *operators*))

(defun variable-p (expression)
  (symbolp expression))

(defun operator (expression)
  (first expression))

(defun operants (expression)
  (rest expression))

(defun lhs (expression)
  (first (operants expression)))

(defun rhs (expression)
  (second (operants expression)))

(defun formula-p (expression)
  (cond
    ((variable-p expression) t)
    ((operator-p (operator expression))
     (every #'formula-p (operants expression)))
    (t (error "~a is not a formula" expression))))

(defun variables (formula)
  (cond
    ((variable-p formula) (list formula))
    ((formula-p formula) (apply #'append
				(mapcar #'variables (operants formula))))))

;; (defmacro define-operator (name operator type))

(defmacro define-operator (name operator type)
  (let ((make-f-name (symbolicate 'make- name))
	(test-f-name (symbolicate name '-p))
	(operants (cond
		    ((eq type :unary) '(lhs))
		    ((eq type :binary) '(lhs rhs))
		    (t (error "Unknows type ~a" type)))))
    `(progn
       (defun ,make-f-name ,operants
	 (list ',operator ,@operants))

       (defun ,test-f-name (formula)
	 (and (eq (operator formula) ',operator)
	      (= (list-length (operants formula)) ,(list-length operants))))

       (register-operator ',operator))))

(define-operator conjunction and :binary)
(define-operator disjunction or :binary)
(define-operator negation not :unary)
(define-operator imply => :binary)
(define-operator biconditional  <=> :binary)




(defparameter *test-and* '(and A B))
