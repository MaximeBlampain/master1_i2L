import matplotlib.pyplot as plt

file = open('train.txt', "r")
lines = file.readlines()
file.close()

blue1 = []
blue2 = []
red1 = []
red2 = []

for line in lines:
    v = line.split(' ')
    if int(v[2]) == 1:
        blue1.append(float(v[0]))
        blue2.append(float(v[1]))
    else:
        red1.append(float(v[0]))
        red2.append(float(v[1]))

plt.scatter(blue1, blue2, color="blue")
plt.scatter(red1, red2, color="red")
plt.show()
