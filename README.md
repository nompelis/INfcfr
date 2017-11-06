# INfcfr v1.9

 A simple library for parsing and handling Fluent "case" files intended for
 computational fluid dynamics (CFD) simulations written in C.
 Copyright 2011-2016 Ioannis Nompelis

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

