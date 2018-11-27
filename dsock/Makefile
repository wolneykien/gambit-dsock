
libdir=/usr/lib
includedir=/usr/include

BASENAME=dsock
LIBS=
#LIBS=-l$(BASENAME)

all: libgambit-$(BASENAME).so

libgambit-$(BASENAME).c: $(BASENAME).scm $(BASENAME)-common.scm $(BASENAME)-ffi.scm
	gsc -:daq- -link -flat -o libgambit-$(BASENAME).c $(BASENAME).scm

$(BASENAME).c: libgambit-$(BASENAME).c
	[ -f $< ]

libgambit-$(BASENAME).o: $(BASENAME).c libgambit-$(BASENAME).c
	gsc -:daq- -obj -cc-options "-D___LIBRARY -D___SHARED -D___PRIMAL" $(BASENAME).c libgambit-$(BASENAME).c

$(BASENAME).o: libgambit-$(BASENAME).o
	[ -f $< ]

libgambit-$(BASENAME).so: $(BASENAME).o libgambit-$(BASENAME).o
	gcc -shared $(BASENAME).o libgambit-$(BASENAME).o -lgambit $(LIBS) -o libgambit-$(BASENAME).so

install: libgambit-$(BASENAME).so libgambit-$(BASENAME).c
	install -Dp -m0644 libgambit-$(BASENAME).so $(libdir)/gambit/libgambit-$(BASENAME).so
	install -Dp -m0644 libgambit-$(BASENAME).c $(includedir)/gambit/libgambit-$(BASENAME).c

$(BASENAME)-test: $(BASENAME)-test.scm libgambit-$(BASENAME).so
	echo "Run $(BASENAME)-test.scm to verify the library"
	gsc -:daq- -link ./libgambit-$(BASENAME).c $(BASENAME)-test.scm
	gsc -:daq- -obj $(BASENAME)-test.c $(BASENAME)-test_.c
	gcc $(BASENAME)-test.o $(BASENAME)-test_.o -lgambit -L. -lgambit-$(BASENAME) -o $(BASENAME)-test

check: $(BASENAME)-test
	LD_LIBRARY_PATH=. ./$(BASENAME)-test -:daq-

clean:
	rm -fv libgambit-$(BASENAME).c libgambit-$(BASENAME).o libgambit-$(BASENAME).so
	rm -fv $(BASENAME)-test $(BASENAME)-test.c $(BASENAME)-test.o $(BASENAME)-test_.c $(BASENAME)-test_.o $(BASENAME).c $(BASENAME).o
