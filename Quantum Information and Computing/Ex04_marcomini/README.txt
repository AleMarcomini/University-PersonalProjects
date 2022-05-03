
Here I report some useful information regarding compilation 
and update of code. 

## Ex 1
I created a FORTRAN code to generate a tri-diagonal Hamiltonian for the Time Indep 
Schrodinger equation of the harmonic oscillator. You can change the parameter in the
section "tunable parameters" of the file ```SchrodingerTI.f90``` and then compile it 

```gfortran SchrodingerTI.f90 HAMILTONIAN.f90 -o SchrodingerTI.out -llapack```

Using LAPACK library and my library HAMILTONIAN for creation and save-to-file.

The program outputs some files containing the eigenvalues and eigenvectors. 
A further analysis of the behaviour of such program while changing variable has been done:
In the folder ```data``` you will find all the results with given labels, which are loaded 
and plotted in the .ipynb file. 

The plots and results and present in the notebook and in the Figures folder. A little 
analysis is provided in my pptx presentation. 

Have fun! :D

Alessandro