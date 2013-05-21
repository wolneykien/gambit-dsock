;; UNIX domain sockets for Gambit-C. Low-level interface.
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

(c-declare "
#include <sys/socket.h>
#include <sys/un.h>
#include <stddef.h>
#include <string.h>
#include <errno.h>
#include <fcntl.h>
")

(define dsocket
  (c-lambda () int
    "___result = socket (PF_LOCAL, SOCK_STREAM, 0);"))

(define dsetnonblock
  (c-lambda (int int) int
#<<c-lambda-end
int oldflags = fcntl (___arg1, F_GETFL, 0);
/* If reading the flags failed, return error indication now. */
if (oldflags == -1)
  return -1;
/* Set just the flag we want to set. */
if (___arg2 != 0)
  oldflags |= O_NONBLOCK;
else
  oldflags &= ~O_NONBLOCK;
/* Store modified flag word in the descriptor. */
___result = fcntl (___arg1, F_SETFL, oldflags);
c-lambda-end
  ))

(define dbind
  (c-lambda (int char-string) int
#<<c-lambda-end
struct sockaddr_un name;
size_t size;

name.sun_family = AF_LOCAL;
strncpy (name.sun_path, ___arg2, sizeof (name.sun_path));
name.sun_path[sizeof (name.sun_path) - 1] = '\0';
size = (offsetof (struct sockaddr_un, sun_path) + strlen (name.sun_path) + 1);
___result = bind(___arg1, (struct sockaddr *) &name, size);
c-lambda-end
  ))

(define dlisten
  (c-lambda (int int) int
    "___result = listen (___arg1, ___arg2);"))

(define daccept
  (c-lambda (int) int
#<<c-lambda-end
struct sockaddr_un clientname;
size_t size = sizeof(struct sockaddr_un);

___result = accept (___arg1, (struct sockaddr *) &clientname, &size);
c-lambda-end
  ))

(define dconnect
  (c-lambda (int char-string) int
#<<c-lambda-end
struct sockaddr_un name;
size_t size;

name.sun_family = AF_LOCAL;
strncpy (name.sun_path, ___arg2, sizeof (name.sun_path));
name.sun_path[sizeof (name.sun_path) - 1] = '\0';
size = (offsetof (struct sockaddr_un, sun_path) + strlen (name.sun_path) + 1);
___result = connect (___arg1, (struct sockaddr *) &name, size);
c-lambda-end
  ))

(define dshutdown
  (c-lambda (int) int
#<<c-lambda-end
___result = shutdown (___arg1, SHUT_RDWR);
c-lambda-end
  ))

(define dshutdown-read
  (c-lambda (int) int
#<<c-lambda-end
___result = shutdown (___arg1, SHUT_RD);
c-lambda-end
  ))

(define dshutdown-write
  (c-lambda (int) int
#<<c-lambda-end
___result = shutdown (___arg1, SHUT_WR);
c-lambda-end
  ))

(define dlasterror
  (c-lambda () char-string
    "___result = errno;"))

(define dlasterror-string
  (c-lambda () char-string
    "___result = strerror(errno);"))
