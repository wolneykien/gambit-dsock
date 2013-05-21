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
#include <poll.h>
#include <fcntl.h>
")

(define dsocket
  (c-lambda () int
    "___result = socket (PF_LOCAL, SOCK_STREAM, 0);"))

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
  (c-lambda (int int) int
#<<c-lambda-end
struct sockaddr_un clientname;
size_t size = sizeof(struct sockaddr_un);
struct pollfd fds;

fds.fd = ___arg1;
fds.events = POLLIN;
fds.revents = 0;
___result = poll (&fds, 1, ___arg2);
if (___result > 0 && (fds.revents & POLLIN)) {
    ___result = accept (___arg1, (struct sockaddr *) &clientname, &size);
}
c-lambda-end
  ))

(define dsock-EINPROGRESS-code (c-lambda () int "___result = EINPROGRESS;"))

(define dconnect
  (c-lambda (int char-string int) int
#<<c-lambda-end
struct sockaddr_un name;
size_t size;

name.sun_family = AF_LOCAL;
strncpy (name.sun_path, ___arg2, sizeof (name.sun_path));
name.sun_path[sizeof (name.sun_path) - 1] = '\0';
size = (offsetof (struct sockaddr_un, sun_path) + strlen (name.sun_path) + 1);
int flags = 0;
struct pollfd fds;
fds.fd = ___arg1;
fds.events = POLLOUT;
fds.revents = 0;
int valopt;
socklen_t optlen;
int ret;

if ((flags = fcntl (___arg1, F_GETFL, NULL)) >= 0) {
    if ((___result = fcntl (___arg1, F_SETFL, flags | O_NONBLOCK)) == 0) {
        ___result = connect (___arg1, (struct sockaddr *) &name, size);
	if (___result < 0) {
	    if (errno == EINPROGRESS || errno == EALREADY) {
		if (poll (&fds, 1, ___arg3) > 0 && (fds.revents & POLLOUT)) {
		    if ((___result = getsockopt (___arg1, SOL_SOCKET, SO_ERROR, &valopt, &optlen)) == 0) {
		        ___result = errno = valopt;
		    }
		} else {
		    ___result = EINPROGRESS;
		}
	    }
	}
	ret = fcntl (___arg1, F_SETFL, flags & ~O_NONBLOCK);
	if (___result == 0) {
	    ___result = ret;
	}
    }
} else {
    ___result = (int) flags;
}
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
  (c-lambda () int
    "___result = errno;"))

(define dlasterror-string
  (c-lambda () char-string
    "___result = strerror(errno);"))
