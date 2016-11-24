library(extrafont)
library(ggplot2)
library(grid)
library(waffle)

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
dark.gray  <- "#D2D2D3"

blue_theme <- function() {
  theme(
    legend.position = "bottom", legend.title = element_text(family = "Segoe UI", 
                                                            color = dark.blue, 
                                                            size = 10),
    legend.background = element_rect(fill = gray),
    legend.key = element_rect(fill = gray, color = gray),
    legend.text = element_text(family = "Segoe UI", color = dark.blue, size = 10),
    legend.title = element_text(face = "bold", size = 10),
    plot.background = element_rect(fill = gray, color = gray),
    panel.background = element_rect(fill = gray),
    panel.background = element_rect(fill = "white"),
    axis.text = element_text(color = dark.blue, family = "Segoe UI"),
    plot.title = element_text(color = dark.blue, face = "bold", size = 18, vjust = 1, 
                              family = "Segoe UI"),
    axis.title = element_text(color = dark.blue, face = "bold", size = 13, family = "Segoe UI"),
    panel.grid.major.y = element_line(color = blue),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = "Segoe UI", color = "white"),
    strip.background = element_rect(fill = blue),
    axis.ticks = element_line(color = blue)
  )
}

waffle_theme <- function() {
  theme(
    legend.position = "bottom", legend.title = element_text(family = "Segoe UI", 
                                                            color = dark.blue, 
                                                            size = 10),
    legend.background = element_rect(fill = gray),
    legend.key = element_rect(fill = gray, color = gray),
    legend.text = element_text(family = "Segoe UI", color = dark.blue, size = 10),
    legend.title = element_text(face = "bold", size = 10),
    plot.background = element_rect(fill = gray, color = gray),
    plot.title = element_text(color = dark.blue, face = "bold", size = 18, vjust = 1,
                              family = "Segoe UI", margin=margin(1, 0, 0.7, 0, unit="cm")),
    axis.title = element_text(color = dark.blue, face = "bold", size = 13, family = "Segoe UI"),
    strip.text = element_text(family = "Segoe UI", color = "white"),
    strip.background = element_rect(fill = blue)
  )
}

single_bar_theme <- function() {
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = gray, color = gray),
    panel.background = element_rect(fill = gray),
    panel.background = element_rect(fill = "white"),
    axis.text = element_blank(),
    plot.title = element_text(color = dark.blue, face = "bold", size = 18, vjust = 1, 
                              family = "Segoe UI"),
    axis.ticks = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
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
month = replicate(num.months, 0)
start.year = getYear(first.date)
start.month = getMonth(first.date)
for (i in start.month:num.months + start.month - 1) {
  year[i] = start.year + floor((i-1)/12)
  month[i] = ((i-1) %% 12) + 1
  date[i] = paste(month.abb[month[i]], year[i])
}

incidents.by.month = data.frame(date, month, year, count)

png("./reports/figures/incidents_by_month.png")
p1 <- ggplot(incidents.by.month,
             aes(x=month, y=count, group=factor(year))) +
      geom_point(aes(linetype=factor(year)), color=green) +
      geom_line(stat = "identity", aes(linetype=factor(year)), color=green) +
      scale_x_continuous(breaks=1:12, labels=month.abb) +
      scale_linetype_discrete(name="Year") +
      xlab("Month") +
      ylab("Number of Incidents") + 
      ggtitle("Number of Americans Killed by Police by Month")
p1 <- p1 + blue_theme()
p1
dev.off()


#
# Plot cumulative incidents over the months
#

cumsum.counts <- c()
year <- getYear(first.date)
while (year <= getYear(last.date)) {
  x <- cumsum(incidents.by.month[ incidents.by.month$year == year, "count" ])
  cumsum.counts <- append(cumsum.counts, x )
  year <- year + 1
}

incidents.by.month$annual.cumsum <- cumsum.counts

png("./reports/figures/cumulative_by_month.png")
p2 <- ggplot(incidents.by.month,
             aes(x=month, y=annual.cumsum, group=factor(year))) +
  geom_point(aes(linetype=factor(year)), color=green) +
  geom_line(stat = "identity", aes(linetype=factor(year)), color=green) +
  scale_x_continuous(breaks=1:12, labels=month.abb) +
  scale_linetype_discrete(name="Year") +
  xlab("Month") +
  ylab("Cumulative Number of Incidents") + 
  ggtitle("Cumulative Number of Americans Killed by Police")
p2 <- p2 + blue_theme()
p2
dev.off()

#
# Plot the race of the victims using a waffle chart
#

wp.data$race <- as.character(wp.data$race)
wp.data[wp.data$race == "", ]$race <- "?"
races <- list("A"="Asian", "B"="Black", "H"="Hispanic", "N"="Native American", "O"="Other", "W"="White",
              "?"="Unknown")
wp.data$race <- unlist(races[wp.data$race], use.names=FALSE)
wp.data$race <- as.factor(wp.data$race)

race.counts <- c()
for (race in races) {
  x <- length(wp.data[ wp.data$race == race, ]$race)
  race.counts[race] = x
}

p3 <- waffle(race.counts / 10,
             rows=7,
             size=0,
             title="Americans Killed by Police",
             xlab="1 square = 10 Americans")
p3 <- p3 + waffle_theme()
p3

#
# Plot unarmed or undetermined victims by race
#

wp.data.unarmed <- wp.data[ wp.data$armed == "unarmed" | wp.data$armed == "undetermined", ]
unarmed.race.counts <- c()
for (race in races) {
  x <- length(wp.data.unarmed[ wp.data.unarmed$race == race, ]$race)
  unarmed.race.counts[race] = x
}

p4 <- waffle(unarmed.race.counts,
             rows=7,
             size=0,
             title="Unarmed Americans Killed by Police",
             xlab="1 square = 1 American")
p4 <- p4 + waffle_theme()
p4

png("./reports/figures/incidents_by_race.png")
p5 <- iron(p3 + theme(legend.position = "none",
                      plot.title = element_text(margin=margin(0.5,0,0.2,0, unit="cm"))),
           p4 + theme(plot.title = element_text(margin=margin(1,0,0.2,0, unit="cm"))))
p5
dev.off()

# From the US Census
# https://www.census.gov/quickfacts/table/PST045215/00
us.pop.race <- c("Asian"=5.6, "Black"=13.3, "Hispanic"=15.5, "Native American"=1.4, "Other"=2.6,
                 "White"=61.6)
us.pop.black <- us.pop.race["Black"]
black.deaths <- unarmed.race.counts["Black"] / sum(unarmed.race.counts) * 100

black.df <- data.frame("Count"=c(us.pop.black, black.deaths, 100 - us.pop.black, 100 - black.deaths),
                       "Race"=c("Black", "Black", "Other", "Other"),
                       "Label"=c("US Population", "Killed By Police"))

png("./reports/figures/black_american_comparison.png")
p7 <- ggplot(black.df,
             aes(x=Label, y=Count, fill=Race)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c(blue, dark.gray)) +
  geom_text(label=c(paste(us.pop.black, "%", sep=""),
                    paste(round(black.deaths, digits=1), "%", sep=""), 
                    "", ""),
            size=10,
            vjust=-0.5,
            color=blue) +
  xlab("") +
  ylab("") +
  ggtitle("Unarmed Black Americans vs. Everyone Else") +
  blue_theme() +
  theme(
    legend.position = "bottom",
    panel.grid.major.y = element_blank(),
    axis.text.y = element_blank(),
    axis.ticks.y = element_blank(),
    plot.title = element_text(size = 16)
  )
p7
dev.off()


#
# Unarmed victims' threat level
#

unarmed.threat <- summary(wp.data.unarmed$threat_level) / length(wp.data.unarmed$id)


#
# Plot the mental state of the victims
#

mental.percent <- c()
for (val in levels(wp.data$signs_of_mental_illness)) {
  mental.percent <- append(mental.percent,
                           length(wp.data[wp.data$signs_of_mental_illness == val, "id"]))
}
mental.percent <- mental.percent / length(wp.data$signs_of_mental_illness) * 100

attach(wp.data)
p6 <- ggplot(wp.data[order(signs_of_mental_illness, decreasing=TRUE), ],
             aes(x = 1, y = id, fill = signs_of_mental_illness)) +
  geom_bar(stat="identity") +
  scale_fill_manual(values=c(dark.gray, blue)) +
  xlab("") +
  ylab("") +
  single_bar_theme()
p6


#
# Combine the plots together into an infographic
#
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)

png("./reports/figures/black_and_blue_infographic.png", width = 10, height = 20, res = 300,
    units = "in")
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=5, ncol=4)))
grid.rect(gp = gpar(fill = gray, col = gray))

# top banner
grid.rect(gp = gpar(fill = dark.blue, color = dark.blue),
          x = unit(0.5, "npc"), y = unit(0.95, "npc"),
          width = unit(1, "npc"), height = unit(0.15, "npc"))

print(p7 + theme(plot.margin=margin(3, 0.5, 2, 0, unit="cm")),
      vp = vplayout(4:5, 3:4))

# bottom banner
grid.rect(gp = gpar(fill = dark.blue, color = dark.blue),
          x = unit(0.5, "npc"), y = unit(0.0, "npc"),
          width = unit(1, "npc"), height = unit(0.06, "npc"))
grid.text("Black and Blue", 
          vjust = 0, hjust = 0,
          x = unit(0.01, "npc"), y = unit(0.92, "npc"), 
          gp = gpar(fontfamily = "Segoe UI", col = "white", cex = 8))
grid.text("An examination of deadly police encounters in America", 
          vjust = 0, hjust = 0,
          x = unit(0.02, "npc"), y = unit(0.895, "npc"), 
          gp = gpar(fontfamily = "Segoe UI", col = "white", cex = 2))
grid.text("By Matt Poegel", 
          vjust = 0, hjust = 0,
          x = unit(0.01, "npc"), y = unit(0.86, "npc"),
          gp = gpar(fontfamily = "Segoe UI", col = blue, cex = 1))
grid.text(paste("Updated:", Sys.Date()), 
          vjust = 0, hjust = 0,
          x = unit(0.825, "npc"), y = unit(0.86, "npc"),
          gp = gpar(fontfamily = "Segoe UI", col = blue, cex = 1))
grid.text("Sources: The Washington Post, 2015 US Census",
          vjust = 0, hjust = 0, 
          x = unit(0.03, "npc"), y = unit(0.012, "npc"), 
          gp = gpar(fontfamily = "Segoe UI", col = "white", cex = 1))


print(p2 + theme(legend.position = "none",
                 plot.margin=margin(2, 1, -2, 1, unit="cm")),
      vp = vplayout(3, 1:3))
print(p1 + theme(plot.margin=margin(2, 1, -2, 1, unit="cm")),
      vp = vplayout(2, 1:3))
print(p3 + theme(legend.position = "none",
                 plot.margin=margin(0, 1, 0, 1, unit="cm")),
      vp = vplayout(4, 1:2))
print(p4 + theme(plot.margin=margin(-8, 1, 0, 1, unit="cm")), 
      vp = vplayout(5, 1:2))
print(p6 + theme(plot.margin=margin(3.5, 1.5, -2, 0.5, unit="cm")),
      vp = vplayout(2:3, 4))

grid.text(length(wp.data$id), 
          vjust = 0, hjust = 0,
          x = unit(0.17, "npc"), y = unit(0.79, "npc"), 
          gp = gpar(fontfamily = "Segoe UI", col = blue, cex = 10))
grid.text("Americans killed by police\nsince January 1, 2015", 
          vjust = 0, hjust = 0,
          x = unit(0.55, "npc"), y = unit(0.80, "npc"), 
          gp = gpar(fontfamily = "Segoe UI", col = blue, cex = 2))

grid.text(paste(round(mental.percent[2]), "%", sep=""), 
          vjust = 0, hjust = 0,
          x = unit(0.81, "npc"), y = unit(0.58, "npc"), 
          gp = gpar(fontfamily = "Segoe UI", col = blue, cex = 3.8))
grid.text(paste("of the victims",
                "showed signs of",
                "mental illness", sep="\n"), 
          vjust = 0, hjust = 0,
          x = unit(0.81, "npc"), y = unit(0.54, "npc"), 
          gp = gpar(fontfamily = "Segoe UI", col = blue, cex = 1))

grid.text(length(wp.data.unarmed$id), 
          vjust = 0, hjust = 0,
          x = unit(0.02, "npc"), y = unit(0.05, "npc"), 
          gp = gpar(fontfamily = "Segoe UI", col = blue, cex = 6))
grid.text(paste("unarmed Americans have been killed",
                "by law enforcement since January 1, 2015", sep="\n"), 
          vjust = 0, hjust = 0,
          x = unit(0.20, "npc"), y = unit(0.06, "npc"), 
          gp = gpar(fontfamily = "Segoe UI", col = blue, cex = 1))

dev.off()

