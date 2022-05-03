
Here I report some useful information regarding compilation 
and update of code. 

## Ex 1
I created a FORTRAN code to create density matrices for generating the Hamiltonian of the 
Trasverse Field Ising model for arbitrary size. I checked the correct output on some known 
small systems, before proceeding to diagonalize H for investigating the spectrum. 
You can explore the code and change the parameters in the specified sections of the file 
```main.f90```, and then compile it with

```gfortran main.f90 HAMILTONIAN.f90 TFI.f90 -llapack -o main.out```

Using my libraries TFI and HAMILTONIAN for all the functions, visualization and generative code.

A little analysis of my results is provided in my pptx presentation. 

Have fun! :D

Alessandro