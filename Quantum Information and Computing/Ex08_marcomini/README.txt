
Here I report some useful information regarding compilation 
and update of code. 

## Ex 1
I created a FORTRAN code to run and test the performances of the real-field renormalization group algorithm.
You can explore all of them and change the parameters in the specified sections of the file 
```main.f90```, and then compile it with

```gfortran main.f90 HAMILTONIAN.f90 TFI.f90 -llapack -o main.out```

Using my libraries HAMILTONIAN and TFI for all the functions and model creation, as well as LAPACK 
for matrix diagonalization.

A little analysis of my results is provided in my pptx presentation. 

Have fun! :D

Alessandro