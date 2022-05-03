# Code to create three plots (one for method) where to show 
# the CPU usage time while changing optimization flags

import numpy as np
import matplotlib.pyplot as plt

# Import data
O0 = np.genfromtxt('CPU_TIME_MATMULT_O0.csv')
O1 = np.genfromtxt('CPU_TIME_MATMULT_O1.csv')
O2 = np.genfromtxt('CPU_TIME_MATMULT_O2.csv')
O3 = np.genfromtxt('CPU_TIME_MATMULT_O3.csv')
Os = np.genfromtxt('CPU_TIME_MATMULT_Os.csv')
all_data = [O0,O1,O2,O3,Os]
all_data_names = ['None','-O1','-O2','-O3','-Os']

size = np.array(O0[:,0].astype(int))


# First method
method = 1
for idx,data in enumerate(all_data):
    plt.plot(size, data[:,method], label = all_data_names[idx])

plt.xlabel('Input size')
plt.ylabel('Time [s]')
plt.title('CPU time for mat-mat mult (optimization) - method 1')
plt.legend(title="Flag")
plt.grid()
plt.show
plt.savefig('OPT_TIME_MATMULT_method1.png')


# Second method
method = 2
for idx,data in enumerate(all_data):
    plt.plot(size, data[:,method], label = all_data_names[idx])

plt.xlabel('Input size')
plt.ylabel('Time [s]')
plt.title('CPU time for mat-mat mult (optimization) - method 2')
plt.legend(title="Flag")
plt.grid()
plt.show
plt.savefig('OPT_TIME_MATMULT_method2.png')


# Built-in method
method = 3
for idx,data in enumerate(all_data):
    plt.plot(size, data[:,method], label = all_data_names[idx])

plt.xlabel('Input size')
plt.ylabel('Time [s]')
plt.title('CPU time for mat-mat mult (optimization) - method built-in')
plt.legend(title="Flag")
plt.grid()
plt.show
plt.savefig('OPT_TIME_MATMULT_method_builtin.png')