<p align="center"><b>МОНУ НТУУ КПІ ім. Ігоря Сікорського ФПМ СПіСКС</b></p>
<p align="center">
<b>Звіт до лабораторної роботи 4</b><br/>
"Функції вищого порядку та замикання"<br/>
дисципліни "Вступ до функціонального програмування"
</p>

<p align="right"> 
<b>Студент</b>: 
 Зубенко Марія Олексіївна КВ-11</p>

<p align="right"><b>Рік</b>: 2024</p>

## Загальне завдання
Завдання складається з двох частин:
1. Переписати функціональну реалізацію алгоритму сортування з лабораторної
роботи 3 з такими змінами:
- використати функції вищого порядку для роботи з послідовностями (де це
доречно);
- додати до інтерфейсу функції (та використання в реалізації) два ключових
параметра: key та test , що працюють аналогічно до того, як працюють
параметри з такими назвами в функціях, що працюють з послідовностями. При
цьому key має виконатись мінімальну кількість разів.
2. Реалізувати функцію, що створює замикання, яке працює згідно із завданням за
варіантом (див. п 4.1.2). Використання псевдо-функцій не забороняється, але, за
можливості, має бути мінімізоване.

## Варіант першої частини (варіант 8)
Алгоритм сортування обміном №4 ("шейкерне сортування") за незменшенням.
## Лістинг реалізації першої частини завдання
```lisp
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

```
### Тестові набори та утиліти першої частини
```lisp
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
  (check-constructive "test 1.4" '(5 4 3 4 2) '(2 3 4 4 5) :key #'abs)
  (check-constructive "test 1.5" '(3 2 -4 6 1 5 3 ) '(-4 1 2 3 3 5 6 ) :key #'abs :test #'>)
  (check-constructive "test 1.6" '("work" "sleep" "pereat") '("pereat" "sleep" "work") :key #'length :test #'>))
```
### Тестування першої частини
```lisp
(test-exchange4-constructive)
PASSED... test 1.1
PASSED... test 1.2
PASSED... test 1.3
PASSED... test 1.4
PASSED... test 1.4
PASSED... test 1.5
PASSED... test 1.6
NIL
```
## Варіант другої частини 8
Написати функцію **remove-each-rnth-reducer** , яка має один основний параметр n та
один ключовий параметр — функцію *key* . **remove-each-rnth-reducer** має повернути
функцію, яка при застосуванні в якості першого аргумента reduce робить наступне: при
обході списку з кінця, кожен *n* -ний елемент списку-аргумента *reduce* , для якого
функція *key* повертає значення *t* (або не *nil* ), видаляється зі списку. Якщо
користувач не передав функцію *key* у **remove-each-rnth-reducer** , тоді зі списку
видаляється просто кожен *n* -ний елемент. Обмеження, які накладаються на
використання функції-результату **remove-each-rnth-reducer** при передачі у reduce
визначаються розробником (тобто, наприклад, необхідно чітко визначити, якими мають
бути значення ключових параметрів функції *reduce from-end* та *initial-value* ).
```lisp
(reduce (remove-each-rnth-reducer 2)
                 '(1 2 3 4 5)
                 :from-end ...
                 :initial-value ...)
(1 3 5)
(reduce (remove-each-rnth-reducer 2 #'evenp)
                 '(1 2 2 2 3 4 4 4 5)
                 :from-end ...
                 :initial-value ...)
```
## Лістинг реалізованої програми
```lisp
(defun remove-each-rnth-reducer (n &optional (key nil))
  (let ((counter 0))
    (lambda (elem acc)
      (incf counter)
      (if (or (and key (funcall key elem))  
              (and (not key) (zerop (mod counter n)))) 
          acc
          (cons elem acc)))))
```
### Тестові набори та утиліти
```lisp
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
```
### Тестування
```lisp
(test-remove-each-rnth-reducer)
PASSED... Test 2.1
PASSED... Test 2.2
PASSED... Test 2.3
PASSED... Test 2.4
PASSED... Test 2.5
```



