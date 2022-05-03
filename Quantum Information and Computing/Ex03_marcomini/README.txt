
Here I report some useful information regarding compilation 
and update of code. 

## Ex 1
For the first exercise I created a Python interface to run the FORTRAN code I wrote
for matrix-matrix multiplication several times, saving the CPU usage to plot it.
In order to perform also a cubit fit on such data, I decided to exploit F2PY function
of numpy which allows me to build a library out of a .f90 file. This to be said,
I reorganized the code I wrote past week (ex 02) for this task and created a library 
called "matmat_mult" by giving the following bash command to create a .so file:

```Python3 -m numpy.f2py -c -m matmat_mult MATMAT_MUL_METHODS.f90```

Then, I could import my FORTRAN code as any library in Python: ```import matmat_mult```

Finally, I wrote a program in Python for data visualization. To run it, simply do

```python3 Display_CPU_time.py ```

And look at the result in the .png picture.


## Ex 2
I created some code to perform eigendecomposition via LAPACK in FORTRAN, where I generate 
a certain number of random hermitian matrices of given size as well as some diagonal real
ones of the same size, saving all eigenvalues in .csv files for further analysis. You can 
play with the number of matrices for statistics and their size in the tunable parameters 
section of the file "Eigenproblem.f90", and change the output names as well.

To compute after saving changes:

```gfortran Eigenproblem.f90 COMPLEX_MATRIX.f90 DEBUGGER.f90 -o Eigenproblem.out -llapack```

To execute:

```./Eigenproblem.out```

The output of this file is then loaded by the Jupyter notebook with Python3 kernel I wrote 
to visualize data and perform fit on histogrammed distributions. The plots and results and 
present in the notebook, together with some inputs. A little analysis is provided in my pptx
presentation in the THEORY folder. 

Have fun! :D

Alessandro