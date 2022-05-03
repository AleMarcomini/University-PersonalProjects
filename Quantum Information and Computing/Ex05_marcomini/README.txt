
Here I report some useful information regarding compilation 
and update of code. 

## Ex 1
I created a FORTRAN code to generate a tri-diagonal Hamiltonian for the Time Indep 
Schrodinger equation of the harmonic oscillator, find the ground state and make it evolve 
via a time-dependent potential. You can change the parameter in the specified sections of 
the file ```main.f90``` and then compile it 

```! gfortran main.f90 DFT.f90 DEBUGGER.f90 HAMILTONIAN.f90 -I/usr/local/include -L/usr/local/lib -lfftw3 -lm -llapack -o main.out```

Using LAPACK & fftw libraries and my libraries HAMILTONIAN, DFT and DEBUGGER for creation, save-to-file, transform and debug.

The program outputs some files containing the modulus squared of psi and the potential at each 
time iteration, to be fed to the .ipynb file for python analysis.

The plots and results and present in the notebook and in the Plots folder. 
I strongly suggest to have a look at the .gif files (really cool! =)
A little analysis is provided in my pptx presentation. 

Have fun! :D

Alessandro