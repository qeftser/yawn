;; ================= Yes also with No ====================

;; YawN is a interpreter for simple discrete math problems.

;; Shout out Dijkstra + Dependencies!
;; Shout out Peter Norvig + Dependencies!
;; Shout out John McCarthy + Dependencies!

(defparameter *operators* '(^ V ~ -> <-> xV) "Operations I have learned so far in Discrete Math.")

(defun collect-vars (compound &optional vars-list)
  "Returns all the variables in a equation."
  (cond ((null compound) vars-list)
	((consp (car compound)) (delete-duplicates 
				 (append (collect-vars (car compound) vars-list) 
					 (collect-vars (cdr compound)))))
	((or (member (car compound) *operators*)
	     (member (car compound) vars-list))
	 (collect-vars (cdr compound) vars-list))
	(t (collect-vars (cdr compound) (cons (car compound) vars-list)))))

(defun collect-all-vars (exprs)
  "Collect all variables from several expressions."
  (let ((o nil))
    (dolist (i exprs)
      (setq o (union o (collect-vars i))))
    o))

(defun generate-permutations (number)
  "Creates true false permutations that have a length
   of the specified number."
  (let* ((2n (expt 2 number)) (o (make-list 2n)))
    (dotimes (i number)
      (let* ((mid (/ 2n (expt 2 (+ i 1)))) (th 1) (2mid (+ mid mid)))
	(dotimes (j 2n)
	  (if (> th mid)
	      (push t (nth j o))
	      (push nil (nth j o)))
	  (if (= th 2mid)
	      (setq th 1)
	      (incf th)))))
    (sort o #'> :key #'(lambda (x) (count t x)))))

(defun match-vars (vars vals)
  "Match each variable with a truth value"
  (let ((o nil))
    (dotimes (i (length vars))
      (push (cons (nth i vars) (nth i vals)) o))
    (nreverse o)))

(defun var-val (var vals)
  "Returns the value of the given variable."
  (cond ((t-f-p var) var)
	(t (assert (member var vals :key #'car) (var) "Not a valid variable: ~s" var)
	   (cdr (assoc var vals)))))

(defun ^-op (x y) (and x y))
(defun V-op (x y) (or x y))
(defun ~-op (x) (not x))
(defun ->-op (x y) (or x (not y)))
(defun <->-op (x y) (eql x y))
(defun xV-op (x y) (not (eql x y)))

(defun ~-op-p (x) (eq '~ x))

(defun varp (x vars)
  "Is x a variable?"
  (if (member x vars) t))

(defun opp (x)
  "Is x an operator?"
  (if (member x *operators*) t))

(defun t-f-p (x)
  "Is the value true or false?"
  (if (member x '(t nil)) t))

(defun length=1 (x) (if (cdr x) t))

(defun apply-op (op x y)
  "Apply the given operation to the variables."
  (ecase op
    (^ ;;(format t "Applying ^ to ~a and ~a~&" y x) 
       (^-op x y))
    (V ;;(format t "Applying V to ~a and ~a~&" y x) 
       (V-op x y))
    (-> ;;(format t "Applying -> to ~a and ~a~&" y x) 
	(->-op x y))
    (<-> ;;(format t "Applying <-> to ~a and ~a~&" y x) 
	 (<->-op x y))
    (xV ;;(format t "Applying xV to ~a and ~a~&" y x) 
	(xV-op x y))))

(defun process-expression (expr vars vals &optional op-stack var-stack)
  "Preforms the brunt of the work for solving an expression."
  ;;(format t "Current expr: ~a | Current op-stack: ~a | Current var-stack: ~a~%" expr op-stack var-stack)
  (cond ((null expr)
	 (cond ((null op-stack) (car var-stack))
	       ((~-op-p (car op-stack))
		(process-expression expr vars vals (cdr op-stack)
				    (cons (~-op (var-val (car var-stack) vals)) (cdr var-stack))))
	       (t (process-expression expr vars vals (cdr op-stack) 
				      (cons (apply-op (car op-stack) 
						      (var-val (car var-stack) vals) 
						      (var-val (cadr var-stack) vals))
					    (cddr var-stack))))))
	((consp (car expr)) (process-expression (cdr expr) vars vals op-stack 
						(cons (process-expression (car expr) vars vals) var-stack)))
	((varp (car expr) vars) (process-expression (cdr expr) vars vals op-stack (cons (car expr) var-stack)))
	((opp (car expr)) (process-expression (cdr expr) vars vals (cons (car expr) op-stack) var-stack))))

(defun print-expressions (vars vals exprs exprs-vals)
  "Print a truth table for expressions."
  (dotimes (i (length exprs))
    (fresh-line)
    (format t "E-~d : ~a" i (nth i exprs)))
  (fresh-line)
  (dolist (i vars) (format t " ~a |" i))
  (dotimes (i (length exprs)) (format t " E-~d |" i))
  (fresh-line t)
  (dotimes (i (length vals))
    (dolist (j (nth i vals))
      (format t " ~a |" (if j 'T 'F)))
    (dolist (j (nth i exprs-vals))
      (format t "  ~a  |" (if j 'T 'F)))
    (fresh-line))
  (dotimes (i (length exprs))
    (let ((expr-v (mapcar #'(lambda (x) (nth i x)) exprs-vals)))
      (if (not (member nil expr-v))
	  (format t "E-~d is a Tautology~%" i))
      (if (not (member t expr-v))
	  (format t "E-~d is a Contradiction~%" i))))
  (if (= (length exprs) 2)
      (let ((expr-v1 (mapcar #'(lambda (x) (nth 0 x)) exprs-vals))
	    (expr-v2 (mapcar #'(lambda (x) (nth 1 x)) exprs-vals)))
	(if (equalp expr-v1 expr-v2)
	    (format t "E-1 and E-2 are Equivilant~%")))))

(defun permutate-expressions (vars vals exprs)
  "Evaluates each expression at all of it's permutations."
  (let ((o nil))
    (dolist (i vals)
      (let ((p nil) (vals2 (match-vars vars i)))
	(dolist (j exprs)
	  (setq p (push (process-expression j vars vals2) p)))
	(setq o (push (nreverse p) o))))
    (nreverse o)))

(defun YawN ()
  "Top level YawN interpreter"
  (format t "/--------------------------------------------------------\\~%")
  (format t "| Welcome to YawN, a interpreter for Discrete Mathmatics |~%")
  (format t "| Enter an expression to add it to the stack             |~%")
  (format t "| Enter 'e' to have all expressions evaluated            |~%")
  (format t "| Enter 'c' to clear the stack                           |~%")
  (format t "| Enter 'v' to view the current expression stack         |~%")
  (format t "| Enter 'x' to view YawN's abilities                     |~%")
  (format t "| Enter 'q' to quit the interpreter                      |~%")
  (format t "\\--------------------------------------------------------/~%")
  (do ((input nil) (exprs nil)) ((eq input 'q))
    (fresh-line)
    (format t "YAWN> ")
    (setq input (read))
    (cond ((consp input) (push input exprs))
	  ((eq input 'c) (setq exprs nil)
	   (format t "~%Expression Stack Cleared!"))
	  ((eq input 'x)
	   (format t "~%YawN can:~%Determine truth values for expressions~%")
	   (format t "Determine if expressions are Tautologies or Contradictions~%")
	   (format t "Determine Equivilance if two expressions are in the equation stack~%")
	   (format t "YawN cannot:~%Follow order of operations. So use parenthesis!~%"))
	  ((eq input 'v) (format t "~%Expression Stack: ~{~%~a~}" exprs))
	  ((and (eq input 'e) (consp exprs))
	   (let* ((vars (collect-all-vars exprs))
		  (vals (generate-permutations (length vars))))
	     (print-expressions vars vals exprs
				(permutate-expressions vars vals exprs))))
	  (t nil))))
		
	    