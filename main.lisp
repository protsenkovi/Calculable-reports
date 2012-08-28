(ql:quickload :cffi)
;(cffi:foreign-funcall "system" :string "echo 123" :int)
;;(cffi:load-foreign-library "msvcrt")
(cffi:defcfun "system" :int (command :string))
(setf *default-pathname-defaults* #P"D:/ProgramProjects/LatexWork/Labs/NumericalAnalysis4/")
(defparameter *host* "D:")
(load "methods.lisp")

(defconstant *programs-paths* '(:R        "\"\"E:/Program Files/R/R-2.12.2/bin/R.exe\"\""
				:pdflatex "\"\"D:/ProgramFiles/LaTeX/miktex/bin/pdflatex.exe\"\""
				:maxima   "D:/Program Files/Maxima-5.23.2/bin/maxima.bat"
				:foxit    "E:/Program Files/Foxit Reader/Foxit Reader.exe"))

(defvar clrf-s '(#\Return #\Newline))

(defun gen-tokens (str &optional (separators '(#\Space #\Newline #\Return #\Linefeed)))
  "√енерирует токены из строки. ќпциональный параметр - список разделителей."
  (let 
	((tokens (list ""))
	 (j 0)
	 (seplast-p nil))
	(progn 
	  (dotimes (i (length str)) 
			 (if (not (subsetp (list (elt str i)) separators))
			   (progn  
				 (if (and (not (string= (elt tokens j) "")) seplast-p)
				   (progn  
					 (setf tokens  (append tokens (list "")))
					 (incf j 1)))
				 (setf (elt tokens  j) (concatenate 'string (elt tokens j) (string (elt str i))))
				 (setf seplast-p nil))
			   (setf seplast-p t))) 
		   (return-from gen-tokens tokens))))

(defun append-row(table row)
  (progn (adjust-array table (list (+ (array-dimension table 0) 1) (array-dimension table 1)))
		 (do 
		   ((i (1- (array-dimension table 0)))
			(j 0 (1+ j))
			(len (min (array-dimension table 1) (length row))))
		   ((>= j len) table)
		   (setf (aref table i j) (elt row j)))
		 table))

(defun look(str &optional (start-i 0) (end-i (1-(length str))) (out t))
  "ѕросматривает строку и дл€ каждого символа выводит его название. ƒл€ нахождени€ специальных сивмолов. #\Newline"
  (loop for i from start-i to end-i do
	(cond
	  ((string= (elt str i) #\Newline) (format out "~a - newline ~a~%" (elt str i) i))
	  ((string= (elt str i) #\Linefeed) (format out "~a - linefeed ~a~%" (elt str i) i))
	  ((string= (elt str i) #\Return) (format out "~a - return ~a~%" (elt str i) i))
	  ((string= (elt str i) #\Space) (format out "~a - space ~a~%" (elt str i) i))
	  ((string= (elt str i) #\Tab) (format out "~a - tab ~a~%" (elt str i) i))
	  (t  (format out "~a - other ~a~%" (elt str i) i)))))



(defun scan(path)
  "«агружает текст из текстового файла."
 (with-open-file (in path :direction :input :if-does-not-exist nil)
    (let ((buffer (make-array (file-length in)
                    :element-type 'character
                    :adjustable t
                    :fill-pointer t)))
	  (format t "Scanning file~%" )
      (adjust-array buffer (read-sequence buffer in))
	  (string-right-trim (list (elt buffer (- (length buffer) 1))) buffer))))

(defun gen-table(str)
  "“рансормаци€ текста в таблицу."
  (let* ((table nil)
	 (rows (gen-tokens str '(#\Newline #\Return #\Linefeed)))
	 (row (gen-tokens (elt rows 0) '(#\Tab))))
	(dotimes (i (length rows))
	  (progn 
		(if (= i 0) 
			(setf table (make-array (list 1 (length row)) :adjustable t :initial-contents (list row)))
			(append-row table (gen-tokens (elt rows i) '(#\Tab))))))	  
	table))

(defun string-nconc (buffer string)
  ;;декларируем типы аргументам
  (declare (type string buffer string))
  ;;провер€ем, можно ли мен€ть buffer'у размер
  (unless (adjustable-array-p buffer)
    (error "~s is not an adjustable string" buffer))
  (let ((buffer-length (length buffer)) ;; текуща€ длина буфера
        (string-length (length string))) ;; длина строки
    (adjust-array buffer ;; первый аргумент - массив, которому мен€ем размер
                  (+ buffer-length string-length) ;; новые размерности массива
                                       ;;(вообще список, но дл€ векторов можно просто число)
                  :fill-pointer (array-has-fill-pointer-p buffer)) ;;если у буфера есть указатель заполнени€
                                                                   ;;устанавливаем его в конец
    (replace buffer string :start1 buffer-length) ;; функци€ replace замеща€ет часть элементов первой
                                                  ;; последовательности частью элементов второй.
                                                  ;; тут мы указываем что в буфере надо начать заполнение с
                                                  ;;  индекса равного старой его длине, и заполн€ть до конца
                                                  ;;  (:end1 и :end2 по дефолту равны NIL это значит что конец - минимум из длин последовательностей )
    buffer))

(defun make-adjustable-string (&optional  (length 0))
  (make-array length :adjustable t :fill-pointer t :element-type 'character))

(defun table2tex(table &key (epsilon nil) (align "c") (schema (make-list (1+ (array-dimension table 1)) :initial-element t)))
  (let 
	((result (make-adjustable-string))
	 (rows (array-dimension table 0))
	 (columns (array-dimension table 1))
	 (schema-len (length schema))
	 (indent 4)
	 (level 0))
	(string-nconc result "\\begin{tabular}{")
	(dotimes (i columns)
	  (if (and (< i schema-len) (elt schema i)) ;; дл€ вертикальных линий
		(string-nconc result (format nil " | ~a" align)) 
		(string-nconc result (format nil " ~a" align))))
	(if (and (< columns schema-len) (elt schema columns)) (string-nconc result " |") ) ;;for last column
	(string-nconc result (format nil " } ~{~a~}" clrf-s))
	(incf level)
	(dotimes (i rows)
	  (string-nconc result (format nil "~va" (* level indent) #\Space))
	  (dotimes (j columns)
		  (string-nconc result (format nil "~:[~va~;~,vf~]" (floatp (aref table i j)) epsilon (aref table i j)))
		  (if (not (= j (1- columns)))(string-nconc result "& ")))
	  (if (= i 0) (string-nconc result (format nil "\\\\ \\hline ~{~a~}" clrf-s))
				  (string-nconc result (format nil "\\\\ ~{~a~}" clrf-s))))
	(decf level)
	(string-nconc result (format nil "\\end{tabular}\\\\ ~{~a~}" clrf-s))))

(defun max+(&rest l)
  (if (setf l (remove-if #'null l))
	(list (apply #'max l))))

(defun mlength(el) 
  (if (not (atom el)) 
	(v "func result"  
	   (append 
		 (v "mapcar result" (apply #'max+ (mapcar #'mlength el)))
		 (v "length " (list (length el)))))))

(defun v(x l) (progn (format t "~a ~a~%" x l) l))

(defun ellt (spisok index) 
  (if (> index (- (length spisok) 1)) (return-from ellt 0) (elt spisok index)))
 
(defun dim (in)
  (let ((result (list (length in)))
                (temp (list ())))
        (progn
          (dolist (el in)
                (if (listp el) (nconc temp (list (dim el)))))
          (setf temp (delete nil temp))
          (if (first temp)
                (let ((ss1 (first temp))
                          (ss2 (list ())))
                  (progn
                        (dolist (el temp)
                          (progn
                                (dotimes (i (max (length el) (length ss1)))
                                  (nconc ss2 (list (max (ellt ss1 i) (ellt el i)))))
                                (setf ss1 (delete nil ss2))
                                (setf ss2 (list ()))))
                        (nconc result ss1))))
        result)))



(defun result2table(result)
  (let	((table nil)
		 (rows-count (elt (dim result) 1))
		 (columns-count (elt (dim result) 0))
		 (column nil))
	(setf table (make-array (list rows-count columns-count) :adjustable t ))
	(dotimes (j columns-count)
	  (progn
		(setf column (elt result j))
		(do ((i 0 (1+ i))
			 (list-len (length column)))
		  ((>= i rows-count) table)
		  (progn 
			(if (< i list-len)
			  (progn
				(setf (aref table i j) (elt column i)))
			  (progn
				(setf (aref table i j) (elt column (1- list-len)))))))))
	(return-from result2table table)))


(defun save (str path)
  (with-open-file (out path 
					   :direction :output
					   :if-exists :supersede
					   :external-format :utf-8)
	(format out "~a" str)))

;save (table2tex (gen-table (scan "D:/methodhord.txt")) '(t t t t t t t t t t t t t t)"D:/ProgramProjects/LatexWork/Labs/NumericalAnalysis1/2.tex")
;(save (table2tex (gen-table (scan "D:/newtonmodification.txt")) '(t t t t t t t t t t)) "D:/ProgramProjects/LatexWork/Labs/NumericalAnalysis1/3.tex")
;(save (table2tex (gen-table (scan "D:/methodStepfenson.txt")) '(t t t t t t t t t t t)) "D:/ProgramProjects/LatexWork/Labs/NumericalAnalysis1/1.tex")
;(system (format nil "pdflatex ~a -output-directory ~a" "D:/ProgramProjects/LatexWork/Labs/NumericalAnalysis1/main.tex" "D:/ProgramProjects/LatexWork/Labs/NumericalAnalysis1/"))
;(system (format nil "D:/ProgramProjects/LatexWork/Labs/NumericalAnalysis1/main.tex" "D:/ProgramProjects/LatexWork/Labs/NumericalAnalysis1/main.pdf"))


;(defun report(name path &optional authors teachers )
;  (let ((latex-str (make-adjustable-string)))))

(defun string-insert (str something &key start end)
  (let ((result (make-adjustable-string)))
	(string-nconc result (subseq str 0 start))
	(string-nconc result (format nil "~a" something))
	(string-nconc result (subseq str end (length str)))
	result))

(defun in(el li)
  (dolist(li-el li)
	(if (eq el li-el)
	  (return-from in t))))

(defun as-key (name) (values (intern (string-upcase name) :KEYWORD)))

(defun get-commands(str &optional (terminal #\@))
  (let ((script str)
		(exec-list (list))
		(exec (make-adjustable-string))
		(exec-i 0)
		(exec-p nil)
		(type-p nil)
		(stack-i 0)
		(com-type))
	(format t "Getting commands~%" )
	(dotimes (i (length script))	  
	  (progn 
		(if exec-p
			(if (char= (elt script i) terminal)
			  (progn 
				;(decf stack-i)
				(if (= stack-i 0)
				  (progn					
					(setf exec-p nil)
					(setf exec-list (append exec-list (list (list :type com-type :start exec-i :end i :command exec ))))
					(setf exec (make-adjustable-string)))))
			  (if type-p 
				(progn
				  (cond 
					((char= (elt script i) #\( ) (progn (setf com-type "lisp") (string-nconc exec (string (elt script i)))))
					((char= (elt script i) #\m) (setf com-type "maxima"))
					(t (setf com-type "")))
				  (setf type-p nil))
				  (string-nconc exec (string (elt script i)))))
		  (if (char= (elt script i) terminal) 
			(progn 
			  (setf exec-p t)
			  (setf exec-i i)
			  (setf type-p t)
			  ;(incf stack-i)
			  )))))
	exec-list))

(defun classificate-commands(commands)
  (let ((types '())
		(result '())
		(cur-type nil)
		(cur-class-list nil))
		(loop for command in commands do
			  (progn 
				(setf cur-type (getf command :type))
				(if (not (in cur-type types))
				  (progn 
					(setf types (append types (list cur-type)))
					(setf result (append result (list (as-key cur-type)) (list nil)))))
				
				(setf cur-class-list (getf result (as-key cur-type)))
				(setf (getf result (as-key cur-type)) (append cur-class-list (list command)))))
		result))

(defun get-list-classed-commands (classed-commands)
  (let ((result nil)
		(cur-list nil))
	(dolist (el classed-commands result)
	  (tagbody
		;(format t "element ~a~%" el)
		(if (keywordp el) (progn 
								  (setf cur-type el)
								  (setf cur-list nil)
								  (setf result (append result (list el))) 
								  (go next-command)))
		;(format t "~a~%~a~%" cur-list result)
		(dolist(command el)
		  ;(format t "command ~a~%~a~%~a~%" command cur-list result)
		  (setf cur-list (append cur-list  (list (getf command :command)))))
		(setf result (append result (list cur-list)))
		next-command))))

(defun eval-lisp(commands)
  (let ((result nil))
	(format t "Evaluating lisp commands~%" )
	(dolist (command commands result)
		(format t "command: ~a~%" command) 
		(setf result (append result (list (eval (read-from-string command))))))))

(defun execute-commands (commands)
  (let* ((result nil)
		(results nil)
		(classed-commands (classificate-commands commands))
		(classed-pure-commands (get-list-classed-commands classed-commands))
		(cur-type nil)
		(cur-list nil)
		(i 0))
	(format t "Executing commands~%" )
	(dolist (commands classed-commands)
	  (tagbody
		;(format t "~a~%" commands)
		(if (keywordp commands) (progn 
								  (setf cur-type commands)
								  (setf cur-list nil)
								  (setf results (append results (list commands))) 
								  (go next-command)))
		(setf results (cond
						((eq cur-type :lisp) (eval-lisp (getf classed-pure-commands :lisp)))
						((eq cur-type :maxima) (eval-maxima (getf classed-pure-commands :maxima)))
						(t (list ""))))
		;(format t "results~% ~{~a~%~}~%" results)
		(loop for command in commands
			  for value in results do
		   ;(format t "command ~a~% value ~a~%~%" command value)
		  (setf cur-list (append cur-list (list (list :start (getf command :start) :end (getf command :end) :value value)))))
		(setf result (append result cur-list)) 
		;(format t "~a~%~a~%~%" i result)
		(incf i)
		next-command))
	result))

(defun paste-results (str commands-results)
  (let ((result str)
		(last-length (length str))
		(cur-type nil)
		(dx 0)
		(i 1))
	(format t "Pasting commands~%" )
	(setf commands-results (sort commands-results #'(lambda (e1 e2) (< (getf e1 :start) (getf e2 :start)))))
	(dolist (command commands-results)
			(setf last-length (length result))
			(setf result 
				  (string-insert result 
					(getf command :value)
					 :start (+ (getf command :start) dx) 
					 :end (+ (getf command :end) dx 1)))
			(setf dx (+ dx (- (length result) last-length)))
			;(format t "~a~%~%~%" result)
			(incf i))
	 result))


;;if error occurs, it simply doesn't generate list element. (maxima-eval '("valid command" "unvalid c" "valid") -> ("result for 1", "result for 2") 
;;(1 2 3) commands
;; | |    <- it will produce error or simply wrong output
;;(1 3)   results
(defun eval-maxima(commands)
  (let* ((maxima-process (sb-ext:run-program  (getf *programs-paths* :maxima)
                                     nil ;; вс€кие опции, чтобы не печатал лишнего
                                     :wait nil ;; не ждем завершени€.
                                               ;; а то ведь зависнем, блеать
                                     :input :stream ;; stdin - возвращаетс€ поток
                                     :output :stream ;; stdout - возвращаетс€ поток
                                     :error :stream ;; stderr - тоже, но мы его не используем
                                     :search t));; ищем `' в PATH
		(out (process-input maxima-process))
		(in (process-output maxima-process)))
	(format t "Evaluating maxima commands~% ~a~%" commands)
	(loop for command in commands
		  do 
		  (format t "command: ~a~%" command) 
		  (write-line (format nil "tex(~a);" command) out))
	(write-line "quit();" out)
	(force-output out)
	(butlast (gen-tokens (parser :in in :groups `(("(%i" ")" :open) ("(%o" :close) ("$" :ignore) (,(format nil "~a" #\Newline) :ignore)) :separator #\#) '(#\#)))))


(defun parser(&key in groups (out nil) (separator nil)) 
  (with-output-to-string (out-s)
      (let ((in (if (stringp in) (make-string-input-stream in) in))
			(flags (make-list (length groups) :initial-element nil))
			(indexes-term (make-list (length groups) :initial-element 0))
			(indexes-term-ch (make-list (length groups) :initial-element 0))
			(writing-p nil)
			(terminal-p nil)
			(stoped-p nil)
			(operations '(:open :close :switch :ignore)))
		(loop for ch = (read-char in nil nil)
			  while ch do
			  ;(format t "ch = ~a~%flags = ~a~%indexes-term = ~a~%indexes-term-ch = ~a~% writing-p = ~a~%" ch flags indexes-term indexes-term-ch writing-p)
			  (format t "~a" ch)
			  (loop for group in groups
					for i from 0 by 1
					for i-term = (elt indexes-term i)
					for i-term-ch = (elt indexes-term-ch i)
					do
					;(format t "i = ~a~%i-term = ~a~%i-term-ch = ~a~%group = ~a~%~%" i i-term i-term-ch group)

					(if (char= ch (elt (elt group i-term) i-term-ch))
					  (progn 
						(incf (elt indexes-term-ch i))
						(setf terminal-p t))
					  (setf (elt indexes-term-ch i) 0))
					;(format t "indexes-term-ch = ~a~%" indexes-term-ch)

					(if (>= (elt indexes-term-ch i) (length (elt group (elt indexes-term i))))
					  (progn 
						(setf (elt indexes-term-ch i) 0)
						(incf (elt indexes-term i))))
					;(format t "indexes-term = ~a~%indexes-term-ch = ~a~%" indexes-term indexes-term-ch)

					(if (in (elt group (elt indexes-term i)) operations)
					  (progn 
						(setf (elt indexes-term i) 0)
						(setf (elt flags i) t)))
					;(format t "flags = ~a~%indexes-term = ~a~%" flags indexes-term-ch)

					(loop for flag in flags
						  for group in groups 
						  do
						  (if flag 
							(progn
							  (cond
								((eq (car (last group)) :open) (setf writing-p t))
								((eq (car (last group)) :close) (setf writing-p nil stoped-p t))
								((eq (car (last group)) :switch) (if writing-p (setf writing-p nil stoped-p t) (setf writing-p t)))
								((eq (car (last group)) :ignore) (setf stoped-p nil)))
							  (setf (elt flags i) nil)
							  (return t)))))
					;(format t "flags = ~a~%writing-p = ~a~%~%" flags writing-p)
					(if terminal-p
					  (setf terminal-p nil)
					  (if writing-p 
						(write-char ch (if (null out) out-s out))))
					(if stoped-p
					   (progn 
						 (if (not (null separator)) (format (if (null out) out-s out) "~a" separator))
						 (setf stoped-p nil)))))))

;(parser "!23!" '(("!" :switch)))

(defun plot-tex(command &optional (name "temp") (ext "pdf") (program :maxima) ) ;;bad function
  (let ((output-link (merge-pathnames name *default-pathname-defaults*))
		(input-path *default-pathname-defaults*)
		(res nil))
	(cond 
	  ((eq program :maxima)
	   (setf res (eval-maxima (list (format nil "~a,[gnuplot_term, ~a],[gnuplot_out_file, \"~a.~a\"])" 
											(subseq command 0 (- (length command) 1)) 
											ext
											output-link
											ext))))))	
	  (format nil "~a.~a" output-link ext)))

(defun process-lisp-tex(path-input path-destination)
  (let ((str (scan path-input)))
	(save (paste-results str (execute-commands (get-commands str))) path-destination)))

(defun get-file-name(filename)
  (let ((res (make-array :element-type 'base-char :adjustable t :fill-pointer 0)))
	(loop for i from 0 below (length filename)
		  while (not (char= (elt filename i) #\.))
		  do (adjust-array ch res))))

(defun report(path-input  &optional (host *host*) (path-destination (merge-pathnames (concatenate 'string "result-" (file-namestring path-input)) *default-pathname-defaults*)))
  "Major function."
  (progn 
	(process-lisp-tex path-input path-destination)
	(system (format nil "pdflatex \"~a\" -output-directory \"~a\"" path-destination 
					(concatenate 'string host (directory-namestring path-destination) "Report/" )))
	(system (format nil "\"\"~a\"\"" 
					(concatenate 'string host 
								 (directory-namestring path-destination) 
								 "Report/" 
								 (string-right-trim ".ltex" (file-namestring path-destination))
								 ".pdf")))))
