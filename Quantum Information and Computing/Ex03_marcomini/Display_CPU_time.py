# Code to plot the computational time of MM-multiplication

# Import linraries and FORTRAN code in the form of library (via f2py)
import numpy as np
import matplotlib.pyplot as plt
import matmat_mult

# Define variables for loop and subroutine
output_time = [0,0,0,0]
maxsize = 1000
data = []

#Loop over data size
for N in range(1,maxsize,50):
    print(N)
    output_time = matmat_mult.random_mat_mult([N,N,N])
    output_time[0] = N
    data.append(output_time)  


# Define variables and perform cubic fits to show trend
data = np.array(data)
size = np.array(data[:,0].astype(int))
method_1 = np.array(data[:,1])
fit_1_coef = np.polyfit(size, method_1, 3)
fit_1 = fit_1_coef[3] + fit_1_coef[2]*size + fit_1_coef[1]*size**2 + fit_1_coef[0]*size**3
method_2 = np.array(data[:,2])
fit_2_coef = np.polyfit(size, method_2, 3)
fit_2 = fit_2_coef[3] + fit_2_coef[2]*size + fit_2_coef[1]*size**2 + fit_2_coef[0]*size**3
method_builtin = np.array(data[:,3])
fit_bi_coef = np.polyfit(size, method_builtin, 3)
fit_bi = fit_bi_coef[3] + fit_bi_coef[2]*size + fit_bi_coef[1]*size**2 + fit_bi_coef[0]*size**3

# Plot settings
plt.plot(size, method_1, color = 'blue', lw = .8, label = 'Method 1')
plt.plot(size, fit_1, color = 'blue', ls = ':')
plt.plot(size, method_2, color = 'red', lw = .8, label = 'Method 2')
plt.plot(size, fit_2, color = 'red', ls = ':')
plt.plot(size, method_builtin, color = 'green', lw = .8, label = 'Built-in method')
plt.plot(size, fit_bi, color = 'green', ls = ':')
plt.xlabel('Input size')
plt.ylabel('Time [s]')
plt.title('Computational time for mat-mat multiplication')
plt.legend()
plt.grid()
plt.show
plt.savefig('CPU_TIME_MATMULT.png')