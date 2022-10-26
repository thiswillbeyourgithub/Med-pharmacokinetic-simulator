import numpy as np
import datetime
import matplotlib.pyplot as plt

# SETTINGS ###################################################
cp = 10  # mg inside a pill
hour_taken = [8, 11, 12, 13, 13]  # when were the pills taken
step = 0.1  # subdivision

dose = cp * 0.4  # bioabailability=0.4
half_life = 2.5  # in hours

starting_hour_plot = 7
max_y_plot = 10

##############################################################

total_amount = len(hour_taken) * cp

# init the time axis
x = np.fromiter((round(i*step, 1) for i in range(0, 240 + 1)), dtype=float)

curve_template = (
        dose * (-np.exp(-np.log(2)/0.4 * x
                ) + np.exp(-(np.log(2) / half_life * x))))
# why /0.4 ? found empirically while tinkering with the curve to comeup
# within 1h13 (about the average between 1h (during meal) and 1h30 (on
# empty stomach))

# make a long string of zero that will be replaced with the curve values
y = np.fromiter((0 for i in x), dtype=float)
for i, h in enumerate(hour_taken):
    limit = len(y) - int(h * 1 / step)
    y[int(h * 1/step):] += curve_template[:limit]

# compute AUC elapsed
now = datetime.datetime.now()
hour = int(now.hour)
minute = int(now.minute)
time = hour+minute/60
elapsed = round(sum(y[:int(time * 1 / step)]) / sum(y) * 100, 2)

# plot ####################################################
fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(x, y, color='lightblue', linewidth=1)
ax.set(xlim=[starting_hour_plot, 24],
       ylim=[0, max_y_plot])

# axis
ax.set(xlabel='Time over the day (hour)',
       ylabel='Amount of Med (arbitrary unit)')
xticks = []
for i in range(0, 10):
    xticks.append(i)
xticks.append(max(y))
xticks.append(y[-1])

for i in range(0, len(xticks)):
    xticks[i] = round(xticks[i], 3)
ax.yaxis.set(ticks=xticks)

# title
plotTitle = (f"Methylphenidate Pharmacokinetic Simulator \n({total_amount}mg "
             f"in {len(hour_taken)} pills)\nElapsed: {elapsed}%")
plotTitle = str(''.join(plotTitle))
fig.suptitle(plotTitle)

# vertical lines
plt.axhline(y=y[240], color="red", linestyle='--', linewidth=0.5)
plt.axhline(y=max(y), color="red", linestyle='--', linewidth=0.5)

for i in range(0, len(hour_taken)):
    plt.axvline(x=hour_taken[i], linestyle=':', linewidth=0.5)

# display vertical line for time
plt.axvline(x=time, color="purple", linewidth=1, linestyle=":", label="Now")


ax.legend(loc='best')  # no overlapping elements
plt.show()
