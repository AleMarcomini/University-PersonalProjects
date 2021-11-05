import serial

def anticomp2(x):
  if x <= 127: return x
  else: return x-256
ser = serial.Serial('/dev/ttyUSB7', baudrate=115200) # open serial port
print(ser.name) # check which port was really used
signal = []
output = [] 
file = open("input_vectors.txt","r")
for line in file:
  signal.append(line)
for sig in signal:
    ser.write(chr(sig))
    d = ser.read()
    output.append(ord(d))
output = [anticomp2(output[i]) for i in range(0,len(output))]
file2 = open("output_vectors.txt", "w")
for o in output:
  file2.write(str(o))
  file2.write("\n")
file2.close()
