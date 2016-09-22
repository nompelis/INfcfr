<<<<<<< HEAD
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

=======
# INfcfr
A library for parsing and handling Fluent "case" files intended for computational fluid dynamics (CFD) simulations.
>>>>>>> 1df39bcb3bd64a04f32811eb6ec5378714b4a849
