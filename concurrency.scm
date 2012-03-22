(define timeout 10)
(define current-process #f)
(define run-queue '())

(define next-pid 0)
(define (make-process thunk)
  (let ((pid next-pid))
    (set! next-pid (+ 1 next-pid))
    (let ((process (vector pid #f #f '() '())))
      (set-process-start
       process
       (lambda ()
         (call/cc
          (lambda (k)
            (set-process-abort process k)
            (set! current-process process)
            (thunk)))
         (set! current-process #f)
         (next-process)))
      process)))

(define (set-process-abort process k)
  (vector-set! process 2 k))

(define (set-process-start process thunk)
  (vector-set! process 1 thunk))

(define (get-process-pid process)
  (vector-ref process 0))

(define (get-process-start process)
  (vector-ref process 1))

(define (get-process-abort process)
  (vector-ref process 2))

(define (get-process-mailbox process)
  (vector-ref process 3))

(define (get-process-blocking process)
  (vector-ref process 4))

(define (set-process-blocking process lst)
  (vector-set! process 4 lst))

(define (mailbox-push process msg)
  (let ((this-process current-process))
    (call/cc
     (lambda (k)
       (vector-set! process 3 (reverse (cons msg (reverse (vector-ref process 3)))))
       (let ((processes (get-process-blocking process)))
         (set-process-blocking process '())
         (for-each schedule-process processes)
	 (set-process-start this-process k)
	 (schedule-process this-process)
         (next-process)
         ((get-process-abort this-process)))))
    (set! current-process this-process)))
       
   
(define (mailbox-block-if-empty process)
  (if (mailbox-empty? process)
      (let ((this-process current-process))        
        (call/cc
         (lambda (k)
           (set-process-start this-process k)
           (set-process-blocking process (cons this-process (get-process-blocking process)))
           (next-process)
           ((get-process-abort this-process))))
        (set! current-process this-process)
        (mailbox-block-if-empty process))))

           
(define (mailbox-pop process)
  (mailbox-block-if-empty process)
  (let ((msg (car (vector-ref process 3))))
    (vector-set! process 3 (cdr (vector-ref process 3)))
    msg))

(define (mailbox-empty? process)
  (= (length (vector-ref process 3)) 0))

(define (! process msg)
  (mailbox-push process msg))

(define (?)
  (mailbox-pop current-process))

(define (run-queue-empty?)
  (= (length run-queue) 0))

(define (schedule-process process)
  (let ((empty? (run-queue-empty?)))
    (set! run-queue (reverse (cons process (reverse run-queue))))
    (if empty?
        (next-process))))

(define (next-process)
  (if (run-queue-empty?)
      (error "What to do here")
      (let ((process (car run-queue)))
        (set! run-queue (cdr run-queue))
        (set-timeout! (get-process-start process) timeout))))

(define (yield)
  (let ((process current-process))
    (call/cc
     (lambda (continuation)
       (set-process-start process continuation)
       (schedule-process process)
       (next-process)
       ((get-process-abort process))))
    (set! current-process process)))

(define (sleep ms)
  (let loop ((ticks timeout))
    (yield)
    (if (< ticks ms)
        (loop (+ ticks timeout)))))
             
(define (process-msg msg)
  (display "Process: ")
  (display (get-process-pid current-process))
  (display " - ")
  (display msg)
  (newline))

(define (spawn thunk)
  (let ((process (make-process thunk)))
    (schedule-process process)
    process))

(define (test1)
  (spawn
   (lambda ()
     (process-msg " - 1")
     (yield)
     (process-msg " - 2"))))

(define (test2)
  (test1)
  (test1))

(define (test3)
  (spawn test1)
  (spawn test1)
  (spawn test2))

(define (test4)
  (spawn
   (lambda ()
     (process-msg " - a")
     (sleep 1000)
     (process-msg " - b")))
  (test1))
     
(define logger
  (spawn
   (lambda ()
     (let loop ((msg (?)))
       (if (string? msg)
           (begin
             (process-msg msg)
             (loop (?)))))
     (process-msg "Logger exiting"))))
                   

(define (logger-test)
  (spawn
   (lambda ()
     (! logger "Hello")
     (! logger "World")
     (! logger 'quit))))
  
;; A process that executes an ajax GET request and returns the result
;; as a message back to the caller. The message is expected to be
;; '(FROM URL), where 'FROM' is the process that will receive
;; the result.
(define ajax-process
  (spawn
   (lambda ()
     (let loop ((msg (?)))
       (let* ((from (car msg))
	      (url (cadr msg)))
	 (ajax-get url (lambda (result) (! from result))))
       (loop (?))))))

;; Send a request and immediately wait for the result.
;; TODO: implement tagging of messages so we receive the
;; result for the actual message sent.
(define (!? process msg)
  (! process msg)
  (?))
       
;; Remember Ajax requests only work to the domain of the
;; server hosting scheme.html.
(define (ajax-test1)
  (spawn
   (lambda ()
     (alert (!? ajax-process 
		`(,current-process "example.scm"))))))

;; Demonstrate sending multiple ajax requests and passing
;; the results to another process.
(define (ajax-test2)
  (spawn
   (lambda ()
     (! ajax-process `(,logger "example.scm"))
     (! ajax-process `(,logger "concurrency.scm")))))

