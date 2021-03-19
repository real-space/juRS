# juRS
Jülich Real-space grid Density Functional Theory for large systems

juRS is a massively-parallel Density Functional Theory (DFT) application. It is based on the direct representation of the Kohn-Sham (KS) single-electron wave function on uniform Cartesian grids, sparse implicit operators and iterative solver methods. A distributed memory parallelisation (message-passing, MPI) of non-local operations like the finite-difference approximated kinetic energy operator allows for the efficient usage of large numbers of compute nodes. Additional parallelisation levels tackle the memory requirements that scale quadratically with system size (number of atoms or simulated volume). Atomic ions in in juRS are treated with a frozen-core Projector Augmented Wave (PAW) method, the latest and most accurate version of the atom treatment originating from the pseudopotential approach.
juRS equally targets large supercell geometries with (potentially magnetic) impurities and/or lattice distortions, open systems like surfaces or wire structures or isolated molecules. It has been used to investigate Phase Change Materials (PCMs) like GeSbTe.

# Installation
1) Choose a system.make from systems/ and soft link it as
      ln -s systems/system.make.INTEL ./system.make
2) make deps

3) make
or
3) make EXTENDED=TRUE
    

