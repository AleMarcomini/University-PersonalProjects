
Here I report some useful information regarding compilation 
and update of code. 

## Ex 1
I created a FORTRAN code to create density matrices for representation of many-body quantum system
states, with all the necessary function for generation and management. The exercise is divided in 
three parts, therefore I created three sections in the main program, investigating different scenarios
You can explore all of them and change the parameters in the specified sections of the file 
```main.f90```, and then compile it with

```gfortran main.f90 DENSITYMATRICES.f90 DEBUGGER.f90 -o main.out```

Using my libraries DENSITYMATRICES and DEBUGGER for all the functions and debug.

A little analysis of my results is provided in my pptx presentation. 

Have fun! :D

Alessandro