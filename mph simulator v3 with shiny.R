##### NOTES
# 0.3 at midnight seemed fine to sleep
# I made a python version but it was not as good, if you want it open an issue
# the Shiny code is at the bottom

mph <- function(cp=10, hour_taken=c(8,10),half_life=2.5, sleep_threshold=0.4,max_y_plot=8) {

step <- 0.1 # time subdivision
x <- seq(0,24,step) # time 
# cp <- 10 # mg inside a pill
dose <- cp*0.4 # bioabailability=0.4 source : https://escholarship.org/uc/item/9gv7p39v
# half_life <- 2.5 # en heure source : https://escholarship.org/uc/item/9gv7p39v
# time at which the dose is taken
# hour_taken <- c(8,9,11)
#      ajoute 1h30 si a jeun ou 1h si pendant le repas ; source : https://escholarship.org/uc/item/9gv7p39v
total_amount <- length(hour_taken)*cp


#curve_template <- dose*exp(-(log(2)/half_life*(x)))
curve_template <- dose*(-exp(-log(2)/0.4*x)+exp(-(log(2)/half_life*x)))
# why /0.4 ? found empirically while tinkering with the curve to comeup within 1h13 (about the average between 1h (during meal) and 1h30 (on empty stomach))
# to fit with the data in the study
# IF YOU FIND A BETTER IDEA OPEN AN ISSUE
y <- rep(0,length(x))
for(i in seq(1,length(hour_taken))) {
  y <- y+ c(rep(0,hour_taken[i]*(1/step)-1),curve_template)[1:length(x)]
}



########## plot
starting_hour_plot <- 7
# max_y_plot <- 8
plot(seq(starting_hour_plot,max(x),step),y[seq(starting_hour_plot,max(x),step)*10],
     xlab=("Time over the day (hour)"),ylab="Amount of MPH (arbitrary unit)",
     type="l",ylim=c(0,max_y_plot),xaxt='n',yaxt='n', frame.plot = TRUE)
#plot details
plot_title=paste(c("Methylphenidate Simulator\n(", total_amount, "mg in ",length(hour_taken), " pills)"),collapse='')
title(plot_title)
abline(h=c(y[length(y)],max(y)),col=3,lty=2)
library(pracma)
mtext(text = paste("t1/2=", half_life, "h ; AUC=",round(trapz(x,y),0)),side=1) # area under curve, but it's obvious that it's going to be =total amount
legend("topright",legend=c("Now"),col=c("purple"),lty=1:2, cex=0.8)
abline(v=hour_taken,lty=2,col="red")
#axis
axis(1,at=c(starting_hour_plot,hour_taken,24)) #horizontal
axis(1,at=seq(starting_hour_plot,24,4))
axis(2,las=1) # left
axis(4,at=round(c(y[hour_taken*1/step],y[length(y)]),2),las=1) # right, duplicated to make reading easier

#display time
hour <- as.numeric(format(Sys.time(), "%H"))
minute <- as.numeric(format(Sys.time(), "%M"))/60
time <- hour+minute
arrows(time,y[time*1/step]+3,time,y[time*1/step]+1,col="purple",lty=5)
arrows(time,-1.01,time,y[time*1/step]-1,col="purple",lty=5) # fleche du haut
axis(1,at=round(time,2),col="purple")
axis(4,at=round(y[time*1/step],2),las=1,col="purple") # unit at current time

#display polygons
#grey before waking up
xx <- c(starting_hour_plot-1,min(hour_taken),min(hour_taken),starting_hour_plot-1)
yy <- c(-2,-2,11,11)
polygon(xx,yy,density=10,col="grey")
#grey when under a certain threshold for sleep
# sleep_threshold <- 0.4
last_time_focused <- (length(y) - which(rev(y)>sleep_threshold)[1])*step
ltf <- last_time_focused
xx <- c(ltf, 26, 26, ltf)
yy <- c(-2,-2,11,11)
polygon(xx,yy,density=10,col="grey")
# display under the curve
# xx <- c(seq(hour_taken[1],ltf,0.0001),ltf,hour_taken[1])
# yy <- c(y[seq(hour_taken[1],ltf,0.0001)/step],y[ltf],0)
# polygon(xx,yy,density=50,col="blue")
}


library(shiny)
ui <- fluidPage(
  titlePanel("MPH Simulator !"),
  fluidRow(column(2,
      textInput(inputId = "hours",value = "8.5 13",label="Times of ingestion, separated with a space"),
      em("example : \"8.5 9 13\" if taken at 8h30 and 9AM and 1PM."),br() ),
      
      column(2, sliderInput(inputId = "cp",
                  label = "Amount of methylphenidate inside a pill, in mg.",
                  min = 1,
                  max = 40,
                  value = 10),br(), ),
      column(2, sliderInput(inputId = "sleep_threshold",
                  label = "Threshold under which you think you can sleep, in mg.",
                  min = 0,
                  max = 1,
                  value = 0.4), ),
      column(2, sliderInput(inputId = "half_life",br(),
                  label = "Half-life of the drug in your system, in hours.",
                  min = 0,
                  max = 4,
                  value=2.5,step = 0.25),br(), ),
      column(2, sliderInput(inputId = "max_y_plot",
                  label = "Maximum height of the plot",
                  min = 5,
                  max = 10,
                  value = 8),)
  
    ),
        h1("This program was made by a bored student with average grades, it comes with no warranty whatsoever."),
        br(),strong(em("Stick to default values as much as you can.")),
        br(),strong(em("This script obviously only works with Immediate Release version of the drug")),
        br(),strong(em("This script could be adapted to other drugs but the constants would have to be adjusted.")),
        br(),strong(em("Most pharmacological figures are directly from the following study :  https://escholarship.org/uc/item/10gv7p39v which is of poor quality.")),
        br(),br(),em("The grey areas indicate the sleeping times, notice that it starts right after the sleeping threshold has been reached."),
        br(),em("The purple arrows indicate the current time."),
        br(),em("The red lines indicate the hour of consumption."),
        br(),em("The green dotted lines indicates the maximum concentration reached as well as the concentration at midnight, this is the value you want to keep attention to if you have trouble sleeping."),
        br(),em("You can change all values in the source code, including the height of the plot and the default values. If you run into issues I'll be glad to take a look so open an issue on github."),

  fluidRow(column(12,
      plotOutput(outputId = "distPlot")
    )
  )
)

server <- function(input, output) {
  output$distPlot <- renderPlot({
    mph(input$cp, as.numeric(c(strsplit(input$hours, split=" "),recursive=T)), input$half_life, input$sleep_threshold, input$max_y_plot)
  },height = 900)
}
shinyApp(ui = ui, server = server)













