### Setting compilation mode selects from multiple builds in Makefile.comm
MODE = zero

include Makefile.comm

LIB_NAME = libINfluent
COPTS += -DNO_USE_HDF
COPTS += -D  _TEST_PLOT3D_
LINK_OPTS = -shared -Wl,-soname,$(LIB_NAME).so -o $(LIB_NAME).so 
AR_OPTS = rsc $(LIB_NAME).a

COPTS += -D_DEBUG_
FOPTS += -D_DEBUG_ -fbounds-check

all: lib demo3

lib:
	$(CC) $(COPTS) $(HDF_INCLUDE) -c  fluent.c
	$(CC) $(COPTS) $(HDF_INCLUDE) -c  fluent_fortran.c
	ar $(AR_OPTS) fluent.o fluent_fortran.o
	$(LD) $(LINK_OPTS) fluent.o fluent_fortran.o $(HDF_LIB)

test: lib
	$(CC) $(COPTS) -Wl,-rpath=. test.c  -L. -lINfluent

demo:
	$(FC) $(FOPTS) -c inMesh_Elements.F
	$(FC) $(FOPTS) -Wl,-rpath=. test.f inMesh_Elements.o -L. -lINfluent

demo2:
	$(FC) $(FOPTS) -U_DEBUG_ -U_DEBUG2_ -c inMesh_Elements.F
	$(FC) $(FOPTS) -Wl,-rpath=. test2.F inMesh_Elements.o -L. -lINfluent

demo3:
	$(FC) $(FOPTS) -U_DEBUG_ -U_DEBUG2_ -c inMesh_Elements.F
	$(FC) $(FOPTS) -Wl,-rpath=. test3.F inMesh_Elements.o -L. -lINfluent

clean:
	rm -f *.o *.so *.a a.out *.mod

