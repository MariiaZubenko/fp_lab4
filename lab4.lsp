;Реалізація exchange4 з використанням ключових параметрів

(defun left-to-right (A L R k &key (key #'identity) (test #'<))
  "Рекурсивний прохід списку зліва направо з використанням ключових параметрів :key і :test."
  (if (>= L R)
      (values k A)
      (let* ((current (funcall key (nth L A)))
             (next (funcall key (nth (1+ L) A))))
        (if (funcall test current next)
            (left-to-right A (1+ L) R k :key key :test test)
            (left-to-right
             (append (subseq A 0 L)
                     (list (nth (1+ L) A) (nth L A))
                     (nthcdr (+ 2 L) A))
             (1+ L) R L :key key :test test)))))

(defun right-to-left (A R L k &key (key #'identity) (test #'<))
  "Рекурсивний прохід списку справа наліво з використанням ключових параметрів :key і :test."
  (if (<= R L)
      (values k A)
      (let* ((prev (funcall key (nth (1- R) A)))
             (current (funcall key (nth R A))))
        (if (funcall test prev current)
            (right-to-left A (1- R) L k :key key :test test)
            (right-to-left
             (append (subseq A 0 (1- R))
                     (list (nth R A) (nth (1- R) A))
                     (nthcdr (1+ R) A))
             (1- R) L (1- R) :key key :test test)))))

(defun exchange4-rec (A L R &key (key #'identity) (test #'<))
  "Рекурсивний алгоритм сортування з використанням ключових параметрів :key і :test."
  (if (>= L R)
      A
      (multiple-value-bind (k new-A) (left-to-right A L R L :key key :test test)
        (multiple-value-bind (new-k new-A) (right-to-left new-A k L k :key key :test test)
          (exchange4-rec new-A (1+ new-k) k :key key :test test)))))

(defun exchange4-constructive (A &key (key #'identity) (test #'<))
  "Функція шейкерного сортування масива A з підтримкою ключових аргументів :key і :test."
  (exchange4-rec A 0 (1- (length A)) :key key :test test))


;Тестування першої частини
(defun check-constructive (name input expected &key (key #'identity) (test #'<))
  "Виконати `exchange4-constructive' на `input' із заданими `key' та `test',  
порівняти результат із `expected' та вивести статус порівняння."
  (let ((result (exchange4-constructive input :key key :test test)))
    (format t "~:[FAILED~;PASSED~]... ~a~%"
            (equal result expected)
            name)
    (when (not (equal result expected))
      (format t "Expected: ~a~%Got: ~a~%~%" expected result))))

(defun test-exchange4-constructive ()
  ;; Тести за замовчуванням
  (check-constructive "test 1.1" '(1 2 3 4 5) '(1 2 3 4 5))
  (check-constructive "test 1.2" '(-6 3 2 5 1 4 3) '(-6 1 2 3 3 4 5))

  ;; Тести з використанням ключових параметрів
  (check-constructive "test 1.3" '(1 2 3 4 5) '(5 4 3 2 1) :test #'>)
  (check-constructive "test 1.4" '(5 4 3 2 1) '(5 4 3 2 1) :test #'>)
  (check-constructive "test 1.5" '(5 -4 3 4 -2) '(-2 3 4 -4 5) :key #'abs)
  (check-constructive "test 1.6" '("work" "sleep" "pereat") '("pereat" "sleep" "work") :key #'length :test #'>))


;Друга частина завдання

(defun remove-each-rnth-reducer (n &optional (key nil))
  (let ((counter 0))
    (lambda (elem acc)
      (incf counter)
      (if (or (and key (funcall key elem))  
              (and (not key) (zerop (mod counter n)))) 
          acc
          (cons elem acc)))))            
     

(defun check-remove-each-rnth-reducer (name input n expected &key initial-value from-end  key)
  
  (let ((result (reduce (remove-each-rnth-reducer n key) input 
                        :from-end from-end 
                        :initial-value initial-value)))
    (format t "~:[FAILED~;PASSED~]... ~a~%"
            (equal result expected)
            name)
    (when (not (equal result expected))
      (format t "Expected: ~a~%Got: ~a~%~%" expected result))))

(defun test-remove-each-rnth-reducer ()
  ;; Тести за замовчуванням
  (check-remove-each-rnth-reducer "Test 2.1" '(1 2 3 4) 2 '(2 4) :from-end t :initial-value '())
  (check-remove-each-rnth-reducer "Test 2.2" '(1 2 3 4 5 6) 3 '(2 3 5 6) :from-end t :initial-value '())
  (check-remove-each-rnth-reducer "Test 2.3" '(1 2 3 4 5) 1 '() :from-end t :initial-value '())

  ;; Тести з використанням ключового параметру
  (check-remove-each-rnth-reducer "Test 2.4" '(1 2 3 4) 2 '(1 3) :from-end t :initial-value '() :key #'evenp)
  (check-remove-each-rnth-reducer "Test 2.5" '(1 2 3 4 5 6) 2 '(2 4 6) :from-end t :initial-value '() :key #'oddp))

(test-exchange4-constructive)
(test-remove-each-rnth-reducer)
  

