
Here I report some useful information regarding compilation 
and update of code. 

## Ex 1
The subroutine is prepared in the DEBUGGER module, while a 
test program is writte in the "Debug_TestProgram.f90" file.

You can edit the parameters in the TUNABLE area and then 
compile the full program via the following command in bash: 

```gfortran Debug_TestProgram.f90  DEBUGGER.f90 -o DebugTest.out```

And then run the code via: 

```./DebugTest.out```


## Ex 2
I rewrote the exercise by creating an apposite module 
MATMAT_MUL_METHODS with all useful subroutines. I also
exploited the DEBUGGER module of Ex 1 and added some operational
modalities so to have a better management of the program goal.

Again, you can play with parameters in the "MatMat_Mult_TestProgram.f90"
placed in the TUNABLE area. You can also see what happens while 
using a negative size for a matrix ;)

To compute after saving changes:

```gfortran MatMat_Mult_TestProgram.f90 DEBUGGER.f90 MATMAT_MUL_METHODS.f90 -o MM_MultTest.out```

To execute:

```./MM_MultTest.out```

## Ex 3
I created the COMPLEX_MATRIX method (in the omonym .f90 file)
which contatins the new type "c_matrix" with all the proper 
components and some useful methods. 

The test program for it is in the "C_Matrix_TestProgram.f90"
file: as before, feel free to change all the details in the 
TUNABLE section. 

To compute after saving changes:

```./gfortran C_Matrix_TestProgram.f90 COMPLEX_MATRIX.f90 DEBUGGER.f90 -o C_MatrixTest.out```./

To execute:

```././C_MatrixTest.out```./



Have fun! :D

Alessandro