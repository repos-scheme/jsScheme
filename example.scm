;
; Simple continuation test
;
(define retry #f)
(define factorial
  (lambda (x)
    (if (= x 0)
        (call/cc (lambda (k) (set! retry k) 1))
        (* x (factorial (- x 1))))))
(factorial 4)
(retry 1)
(retry 2)
(retry 5)
;
; Simple closure test
;
(define count
  ((lambda (next)
    (lambda ()
      (set! next (+ next 1))
      next))
   0))
(count)
(count)
(count)
;
; misc.
;
(reverse '(1 2 3 4))
(map + '(1 2 3 4) '(1000 100 10 1))
(append '(1 2 3) '(#\X #\Y #\Z ) '(a b c))
(sort < '(3 4 2 1 2 5))
(list->string (sort char-ci>? (string->list "Two Coins")))
`(a ,@(reverse '(c d e)) f ,(+ 10 4) ,@(+ 3 4))
;
; list? test
;
(let ((x (list 'a 'b 'c)))
  (set-cdr! (cddr x) x)
  (list? x))
;
; letrec test
;
(letrec ((mk (lambda (level limit start)
  (if (> start limit) '()
      (if (> level 0)
          (cons (mk (- level 1) limit start)
                (mk (- level 1) limit
                    (+ start (expt 2 (- level 1)))))
          start)))))
(mk 5 11 1))
;
; let/let*/letrec difference test
;
(let ((x 5) (y 7))
  (list
    (let ((x y) (y x))(list x y))
    (let* ((x y) (y x))(list x y))
    (letrec ((x y) (y x))(list x y))
    (list x y)))
;
; dynamic-wind test
;
(define cc #f)
(dynamic-wind
  (lambda () (display "-in-"))
  (lambda () (call/cc (lambda (x) (set! cc x))) (display "-do-") 0)
  (lambda () (display "-out-")))
(cc)
;
; Scheme <-> JS tests
;
(js-eval "2+2")
(js-invoke (js-eval "new Date()") "toString")
(define document (js-eval "document"))
(get-prop document "URL")
(js-call (js-eval "jsAlert") "Hello, World! (Called from Scheme!)")
(set-prop! (get-prop (get-prop document "all") "log") "cols" 70)
(list-props (js-eval "TopEnv"))
(str '(+ 1 2))
(parse "#f 13 (+ 1 2) #\\ ")
(define (handler this event)
  (display "Click on tag: ")
  (display (get-prop this "tagName"))
  (display ", with id: ")
  (display (get-prop this "id"))
  (display ", checked: ")
  (display (get-prop this "checked"))
  (newline))
(set-handler! (js-eval "document.getElementById('echoInp')") "onclick" handler)
(set-handler! (js-eval "document.getElementById('echoRes')") "onclick" handler)
;
; I/O test: (read) (eof-object?)
;
(define (read-all) (define obj (read))
  (if (eof-object? obj) '() (cons obj (read-all))))
(read-all)
1 2 3 4 (5 6)
