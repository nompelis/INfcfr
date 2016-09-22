### Setting compilation mode selects from multiple builds in Mkaefile.comm
MODE = zero

include Makefile.comm

LIB_NAME = libINfluent
COPTS += -D_USE_HDF
LINK_OPTS = -shared -Wl,-soname,$(LIB_NAME).so -o $(LIB_NAME).so 
AR_OPTS = rsc $(LIB_NAME).a

all: lib test

lib:
	$(CC) $(COPTS) $(HDF_INCLUDE) -c  fluent.c
	$(CC) $(COPTS) $(HDF_INCLUDE) -c  hdf5.c
	ar $(AR_OPTS) hdf5.o fluent.o
	$(LD) $(LINK_OPTS) hdf5.o fluent.o $(HDF_LIB)

test:
	$(CC) $(COPTS) -Wl,-rpath=. test.c  -L. -lINfluent

clean:
	rm -f *.o *.so *.a a.out 

