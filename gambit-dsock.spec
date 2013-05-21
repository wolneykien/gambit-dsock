Name: gambit-signal
Version: 1.0
Release: alt1
Summary: UNIX domain sockets for Gambit-C Scheme programming system
License: GPLv3+
Group: Development/Scheme

Packager: Paul Wolneykien <manowar@altlinux.ru>

BuildPreReq: gambit glibc-devel

Source: %name-%version.tar

%description
UNIX domain sockets for Gambit-C Scheme programming system

%package devel
Summary: UNIX domain sockets for Gambit-C Scheme programming system
Group: Development/Scheme
Requires: %name = %version-%release
BuildArch: noarch

%description devel
UNIX domain sockets for Gambit-C Scheme programming system

This package contains the library link file

%prep
%setup -q

%build
%make_build

%install
%makeinstall

%check
%make check

%files
%{_libdir}/gambit/libgambc-dsock.so

%files devel
%{_includedir}/gambit/libgambc-dsock.c

%changelog
* Tue May 21 2013 Paul Wolneykien <manowar@altlinux.org> 1.0-alt1
- Initial release for ALT Linux.
