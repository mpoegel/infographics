library("ggplot2")

# Read in the first data set
wp.data <- read.csv("./data/wp-police-shootings.csv")

#
# Munge the data
#
N = length(wp.data$id)


#
# Plotting themes
#
# color theme: http://paletton.com/#uid=53n0u0kllllaFw0g0qFqFg0w0aF
#

blue.green <- "#255C69"
blue       <- "#303E73"
dark.blue  <- "#07123A"
green      <- "#2A7F41"
gray       <- "#E2E2E3"

blue_theme <- function() {
  theme(
    legend.position = "bottom", legend.title = element_text(family = "Segoe UI", 
                                                            colour = dark.blue, 
                                                            size = 10),
    legend.background = element_rect(fill = gray),
    legend.key = element_rect(fill = gray, colour = gray),
    legend.text = element_text(family = "Segoe UI", colour = dark.blue, size = 10),
    legend.title = element_text(face = "bold", size = 10),
    plot.background = element_rect(fill = gray, colour = gray),
    panel.background = element_rect(fill = gray),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(colour = dark.blue, family = "Segoe UI"),
    plot.title = element_text(colour = dark.blue, face = "bold", size = 18, vjust = 1, 
                              family = "Segoe UI"),
    axis.title = element_text(colour = dark.blue, face = "bold", size = 13, family = "Segoe UI"),
    panel.grid.major.y = element_line(colour = blue),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Segoe UI", colour = "white"),
    strip.background = element_rect(fill = blue),
    axis.ticks = element_line(colour = blue)
  )
}


#
# Plot the number of incidents per month
#
getMonth <- function(d) { as.numeric(substr(as.character(d),6,7)) }
getYear  <- function(d) { as.numeric(substr(as.character(d),1,4)) }

first.date = wp.data$date[1]
last.date = wp.data$date[N-1]
num.months = (getMonth(last.date) - getMonth(first.date) + 1) +
             (getYear(last.date) - getYear(first.date)) * 12

count = replicate(num.months, 0)
for (d in wp.data$date) {
  index = (getMonth(d) - getMonth(first.date) + 1) +
          (getYear(d) - getYear(first.date)) * 12
  count[index] <- count[index] + 1
}

date = replicate(num.months, "")
year = replicate(num.months, 0)
month = replicate(num.month, 0)
start.year = getYear(first.date)
start.month = getMonth(first.date)
for (i in start.month:num.months + start.month - 1) {
  year[i] = start.year + floor((i-1)/12)
  month[i] = ((i-1) %% 12) + 1
  date[i] = paste(month.abb[month[i]], year[i])
}

incidents.by.month = data.frame(date, month, year, count)

p1 <- ggplot(incidents.by.month,
             aes(x=month, y=count, group=factor(year))) +
      geom_point(aes(linetype=factor(year))) +
      geom_line(stat = "identity", aes(linetype=factor(year)), color=green) +
      scale_x_continuous(breaks=1:12, labels=month.abb) +
      scale_linetype_discrete(name="Year") +
      xlab("Month") +
      ylab("Number of Incidents") + 
      ggtitle("Number of Police Shootings by Month")
p1


