# INfcfr v1.9

 A simple library for parsing and handling Fluent "case" files intended for
 computational fluid dynamics (CFD) simulations written in C.
 Copyright 2011-2018 Ioannis Nompelis

This is a very simple library that can be used to parse "case" files prepared
for use by the Fluent software package. It is meant to be used for loading an
unstructured grid that has been assigned boundary conditions in the form of face
groups. The existing minimal structures can be used to build additional data
structures (as needed) in order to perform simulations directly. Parsing
provides verbose output, including line-numbers of key points in the file,
and echos comments.

The library can be compiled against the HDF5 libraries whereby acting as a
converter from a typical ASCII file to a cross-platform binary with the same
structure. Using this functionality, the converter can be used to build binary
files, which in turn can be used for parallel reading of very large grids.
The memory foot-print of the library when it acts as a converter is extremely
small.

An example Makefile has been included, as well as a Makefile.comm which
provides a template for compile-time options for different compilers. A
driver (test.c) has also been included in this distribution. The driver
demonstrates how to initialize, read (parse) into memory, handle and clean
the case-file object; both grid loading and converting to HDF5 examples are
found in the driver. A routine is also included to plot the different face
groups with AMTEC's TecPlot. This routine collects the vertices associated
with each 2D (face) element in the same way that they were going to be
visualized with plain OpenGL calls or creating OpenGL display lists, or
creating vertex arrays. This acts as an example for visualizing the grid
with organic software as needed.

http://nobelware.com/

IN 2016/09/22


A Fortran interface to the library and a driver code to illustrate how to use
this library API from a Fortran code have been added. Doing 'make lib' will
build the library. Doing 'make test' will build a C driver code to read a file
named "grid.cas" and make TecPlot style files of all face-groups in the case
file. Doing 'make' will build the library and compile the Fortran driver
called "test.f" to an executable. The executable opens a file called "grid.cas"
and makes a TecPlot-style file of all cells (volume elements). This demo uses
an excerpt of a separate library that constructs volume elements; this code is
included in this bundle.

The Fortran demonstration driver along with the element library code can be used
to directly build a serial or shared-memory parallel (OpenMP/POSIX threaded)
numerical solver. To do this, one needs to be build on top of the "make_cells()"
subroutine and by forming additional arrays as needed.

IN 2017/10/24


A second and more useful example of a Fortran demonstration has been added in
which derived structures from a loaded mesh are built, such as the element/cell
and face centroids.

IN 2017/11/06


A third useful example of a Fortran demonstration has been added in which the
spatial gradient of an imposed field-variable is computed using a Weighted
Least-Squares method. This demonstration is built on top of the previous (demo2)
code as it uses the element centroid structures.

This demonstration is useful for evaluating the accuracy of the weighted least
squares approach when different weights are used. In the present demonstration,
the common form of inverse-distance is used as the weight for each of the cells
that contribute to a given cell's gradient estimation. The Fortran routine
"test_gradient()" can be modified to alter the trial-field, compute the field's
analytical spatial derivatives, change the form of the weights used, and be
modified to compute a normed error. The linear system formed by the WLS method
is assembled in this routine without regard for arithmetic efficiency as it is
only for instruction purposes.

(NOTE FOR PERSONS WHO ARE INTERESTED IN BUILDING A SOLVER ON TOP OF THIS CODE:
This gradient estimation can be used for evaluating any term in a PDE involving
first derivatives, and they are expected to be second order accurate. In CFD
performed using the finite volume method, this form is used to compute viscous
fluxes at faces and in some cases higher-order inviscid fluxes. Also, when
threading, the gradient calculation should be completely parallelizable.)

IN 2017/11/08


Separated methods for testing gradient reconstructions. The following methods
are presently implemented:
    
1. My Weighted Least-Squares (WLS) method based on a hyper-plane regression
    
2. The WLSQ(N) as described in:
    
Eiji Shima, Keiichi Kitamura, and Takamori Haga, ``Green-Gauss/Weighted-Leas
Squares hybrid Gradient Reconstruction for Arbitrary Polyhedra Unstructured
Grids,'' Technical Notes, AIAA Journal, Vol. 51, No. 11, November 2013.

The code can very easily be modified to test additional methods.

IN 2017/11/10


Added functionality to allow for plot3d exporting of multi-block structured
grids. This works for case-files that have been built by such grids.

IN 2018/01/29

