
(define *socket-path* "test.sock")

(define (display-message name msg new-line)
  (display "[")
  (display name)
  (display "] ")
  (display msg)
  (if new-line
    (newline)))

(define (read-and-report name port)
  (input-port-timeout-set! port 5
			   (lambda ()
			     (display-message name "Read timed-out" #t)
			     #f))
  (display-message name "Reading the data" #t)
  (let ((res (read port)))
    (display-message name "Receive: " #f)
    (pp res)
    res))

(define (connect-or-report name path maxloops)
  (display-message name "Connecting to the socket..." #t)
  (let loop ((t 0))
    (let ((port (domain-socket-connect path 100)))
      (if (and port (port? port))
	port
	(if (>= t maxloops)
	  (begin
	    (display-message name "Connection timed-out" #t)
	    #f)
	  (begin
	    (thread-sleep! 0.01)
	    (loop (+ t 1))))))))

(define (make-client name maxloops)
 (make-thread
   (lambda ()
     (let ((socket (connect-or-report name *socket-path* maxloops)))
       (if (and socket (port? socket))
	 (begin
	   (display-message name "Writing the data" #t)
	   (write name socket)
	   (force-output socket 1)
	   (let ((res (read-and-report name socket)))
	     (close-port socket)
	     res))
	 #f)))
   name))

(define (accept-or-report name ds maxloops)
  (display-message name "Waiting for connection..." #t)
  (let loop ((t 0))
    (let ((port (domain-socket-accept ds 0)))
      (if (and port (port? port))
	port
	(if (>= t maxloops)
	  (begin
	    (display-message name "Connection timed-out" #t)
	    #f)
	  (begin
	    (thread-yield!)
	    (loop (+ t 1))))))))

(let ((ds (make-domain-socket *socket-path* 2))
      (c1 (make-client "alpha" 50))
      (c2 (make-client "beta" 50)))
  (thread-start! c1)
  (thread-start! c2)
  (let* ((p1 (accept-or-report "server" ds 50))
	 (d1 (and p1 (read-and-report "server" p1)))
	 (p2 (accept-or-report "server" ds 50))
	 (d2 (and p2 (read-and-report "server" p2))))
    (if p1
      (begin
        (display-message "server" "Writing the data to the peer 1" #t)
	(write d1 p1)
	(force-output p1 1)
	(close-port p1)))
    (if p2
      (begin
        (display-message "server" "Writing the data to the peer 2" #t)
	(write d2 p2)
	(force-output p2 1)
	(close-port p2)))
    (delete-domain-socket ds)
    (if (and p1
	     p2
	     (equal? (thread-join! c1) (thread-name c1))
	     (equal? (thread-join! c2) (thread-name c2)))
	(begin
	  (display "Test passed")
	  (newline)
	  (exit 0))
	(begin
	  (display "Test FAILED")
	  (newline)
	  (exit 1)))))

