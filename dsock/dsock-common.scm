;; UNIX domain sockets for Gambit-C. High-level interface.
;;
;; Copyright (C) 2013 Paul Wolneykien <manowar@altlinux.org>
;;
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;;
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;;
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(define (raise-domain-socket-lasterror)
  (raise (cons 'domain-socket-exception
	       (cons (dlasterror) (dlasterror-string)))))

(define (make-domain-socket path backlog)
  (let ((socket-fd (dsocket)))
    (if (< 0 socket-fd)
      (if (= 0 (dbind socket-fd path))
	(if (= 0 (dlisten socket-fd backlog))
	  (cons socket-fd path)
	  (raise-domain-socket-lasterror))
	(raise-domain-socket-lasterror))
      (raise-domain-socket-lasterror))))

(define (domain-socket? ds)
  (and ds
       (pair? ds)
       (integer? (car ds))
       (< 0 (car ds))
       (string? (cdr ds))))

(define (assert-domain-socket ds)
  (or (domain-socket? ds)
      (raise "Instance of make-domain-socket expected")))

(define (domain-socket-fd ds)
  (and (assert-domain-socket ds) (car ds)))

(define (domain-socket-path ds)
  (and (assert-domain-socket ds) (cdr ds)))

(define (domain-socket-shutdown-read ds)
  (or (= 0 (dshutdown-read (domain-socket-fd ds)))
      (raise-domain-socket-lasterror)))

(define (domain-socket-shutdown-write ds)
  (or (= 0 (dshutdown-write (domain-socket-fd ds)))
      (raise-domain-socket-lasterror)))

(define (domain-socket-shutdown-both ds)
  (or (= 0 (dshutdown (domain-socket-fd ds)))
      (raise-domain-socket-lasterror)))

(define (delete-domain-socket ds)
  (if (domain-socket-shutdown-both ds)
      (delete-file (domain-socket-path ds))
      #t))

(define (domain-socket-accept ds . args)
  (let* ((timeout (or (and (not (null? args)) (pair? args) (car args)) -1))
	(client-fd (daccept (domain-socket-fd ds) timeout)))
    (if (< 0 client-fd)
	(##open-predefined 3 (list (domain-socket-path ds) 'server) client-fd)
	(if (and (= 0 client-fd) (<= 0 timeout))
	  #f
	  (raise-domain-socket-lasterror)))))

(define *EINPROGRESS* (dsock-EINPROGRESS-code))

(define (domain-socket-connect path . args)
  (let ((timeout (or (and (not (null? args)) (pair? args) (car args)) -1))
	(socket-fd (dsocket)))
    (if (< 0 socket-fd)
	(let ((ret (dconnect socket-fd path timeout)))
	  (if (= 0 ret)
	    (##open-predefined 3 (list path 'client) socket-fd)
	    (if (= *EINPROGRESS* ret)
	      #f
	      (raise-domain-socket-lasterror))))
	(raise-domain-socket-lasterror))))

