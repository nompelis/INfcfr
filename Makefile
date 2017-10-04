### Setting compilation mode selects from multiple builds in Makefile.comm
MODE = zero

include Makefile.comm

LIB_NAME = libINfluent
COPTS += -DNO_USE_HDF
LINK_OPTS = -shared -Wl,-soname,$(LIB_NAME).so -o $(LIB_NAME).so 
AR_OPTS = rsc $(LIB_NAME).a

COPTS += -D_DEBUG_

all: lib demo

lib:
	$(CC) $(COPTS) $(HDF_INCLUDE) -c  fluent.c
	$(CC) $(COPTS) $(HDF_INCLUDE) -c  fluent_fortran.c
	ar $(AR_OPTS) fluent.o fluent_fortran.o
	$(LD) $(LINK_OPTS) fluent.o fluent_fortran.o $(HDF_LIB)

test:
	$(CC) $(COPTS) -Wl,-rpath=. test.c  -L. -lINfluent

demo:
	$(FC) $(FOPTS) -Wl,-rpath=. test.f  -L. -lINfluent

clean:
	rm -f *.o *.so *.a a.out 

