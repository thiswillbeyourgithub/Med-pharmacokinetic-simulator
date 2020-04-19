import math
import matplotlib.pyplot as plt

##### NOTES
# This code is really similar to the R version but really more ugly
# It has more chances of being buggy as I spent way less time on this one

#SETUP
cp = 10 # mg inside a pill
hour_taken = [8, 10, 15] # when aree pills taken


step = 0.1 #e subdivision
#setup x, the time axis
x=[]
for i in range(0,240+1):
    x.append(round(i*step,1))


dose = cp*0.4 # bioabailability=0.4
half_life = 2.5 # in hours

# time at which the dose is taken
total_amount = len(hour_taken)*cp


curve_template=[]
for i in x:
    curve_template.append(dose*(-math.exp(-math.log(2)/0.4*i)+math.exp(-(math.log(2)/half_life*i))))
# why /0.4 ? found empirically while tinkering with the curve to comeup within 1h13 (about the average between 1h (during meal) and 1h30 (on empty stomach))

y = list([])
for i in range(0,len(x)+1) :
    y.append(0)

#for(i in seq(1,length(hour_taken))) {
#  y <- y+ c(rep(0,hour_taken[i]*(1/step)-1),curve_template)[1:length(x)]
#}


y=[0]*(2*len(x)) # make a long string of zero that will be replaced with the curve values
for i in range(0,len(hour_taken)) :
        beginningOfCurve = [0]*(int(hour_taken[i]*1/step))
        endOfCurve=curve_template
        completeCurve=beginningOfCurve+endOfCurve
        for a in range(0,len(completeCurve)) :
            y[a] = y[a]+completeCurve[a]

y=y[0:len(x)] # remove unecessary 0 as the plot will stop at the end of x

##### plot
#setup
starting_hour_plot = 7
max_y_plot = 8

fig = plt.figure()
ax = fig.add_subplot(111)
ax.plot(x,y,color='lightblue',linewidth=1)
ax.set(xlim=[starting_hour_plot,24],
        ylim=[0,max_y_plot])
#axis
ax.set(xlabel='Time over the day (hour)',
        ylabel='Amount of MPH (arbitrary unit)')
xticks=[]
for i in range(0,10):xticks.append(i)
xticks.append(max(y))
xticks.append(y[-1])
for i in range(0,len(xticks)) : xticks[i]=round(xticks[i],3)
ax.yaxis.set(ticks=xticks)
#title
plotTitle = ["Methylphenidate Pharmacokinetic Simulator \n(",str(total_amount), "mg in ", str(len(hour_taken)), " pills)"]
plotTitle = str(''.join(plotTitle))
fig.suptitle(plotTitle)
#vertical lines
plt.axhline(y=y[240], color="red",linestyle='--',linewidth=0.5)
plt.axhline(y=max(y), color="red",linestyle='--',linewidth=0.5)
for i in range(0,len(hour_taken)) : plt.axvline(x=hour_taken[i], linestyle=':', linewidth=0.5)
#display vertical line for time
import datetime  
now=datetime.datetime.now()
hour=int(now.hour)
minute=int(now.minute)
time=hour+minute/60
plt.axvline(x=time,color="purple",linewidth=1,linestyle=":",label="Now")

ax.legend(loc='best') # no overlapping elements
plt.show()
