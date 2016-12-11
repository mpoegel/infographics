library(extrafont)
library(ggplot2)
library(ggrepel)
library(grid)
library(jpeg)
library(maps)
library(mapdata)
library(ggmap)
library(waffle)

#
# Load the compiled data
#
prison.data <- read.csv("./data/jd-private-prison-data.csv")


#
# Make sure the data is stored as the correct types
#
prison.data$private <- as.numeric(as.character(prison.data$private))
prison.data$local <- as.numeric(as.character(prison.data$local))
prison.data$other <- as.numeric(as.character(prison.data$other))
prison.data$total <- as.numeric(as.character(prison.data$total))


#
# Color themes
#
black      <- "#000000"
off.black  <- "#121212"
dark.gray  <- "#D2D2D3"
light.gray <- "#F3F3F3"
white      <- "#FFFFFF"

# Pallet: http://paletton.com/#uid=50q0u0kllllaFw0g0qFqFg0w0aF
orange      <- "#AA6839"
dark.orange <- "#804115"
red         <- "#A43741"
dark.red    <- "#520008"

main.font <- "Bookman Old Style"

orangeLineTheme <- function() {
  theme(
    legend.position = "none",
    plot.background = element_rect(fill = light.gray, color = light.gray),
    panel.background = element_rect(fill = light.gray),
    axis.text = element_text(color = dark.orange, family = main.font),
    plot.title = element_text(color = dark.orange, face = "bold", size = 24, vjust = 1, 
                              family = main.font),
    axis.title = element_text(color = dark.orange, face = "bold", size = 18, family = main.font),
    axis.title.x = element_text(margin = margin(0.5, 0, 0, 0, unit = "cm")),
    axis.title.y = element_text(margin = margin(0, 0.5, 0, 0, unit = "cm")),
    panel.grid.major.y = element_line(color = orange),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_text(family = main.font, color = "white"),
    strip.background = element_rect(fill = orange),
    axis.ticks = element_line(color = orange)
  )
}

mapTheme <- function() {
  theme(
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background =  element_rect(fill = "transparent", color = "transparent"),
    axis.title = element_blank(),
    axis.ticks = element_blank(),
    axis.text = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank()
  )
}

waffleTheme <- function() {
  theme(
    legend.position = "none",
    plot.title = element_text(color = dark.orange, face = "bold", size = 24, vjust = 1, 
                              family = main.font),
    axis.title = element_text(color = dark.orange, face = "bold", size = 18, family = main.font),
    strip.text = element_text(family = main.font, color = "white"),
    strip.background = element_rect(fill = orange)
  )
}

bullet <- intToUtf8(0x2022)
bulletize <- function(lis, width) {
  res <- ""
  for (text in lis) {
    res <- paste(res, bullet, paste(strwrap(text, width=width), collapse = "\n    "), "\n")
  }
  res
}


#
# Plot the U.S. Total Population of Private Prisons over time
#
percision <- 10000
min.pop <- round(min(prison.data[prison.data$jurisdiction == "U.S. Total", ]$private)/ percision) *
  percision
max.pop <- round(max(prison.data[prison.data$jurisdiction == "U.S. Total", ]$private)/ percision) *
  percision

png("./reports/figures/total_us_private_prison_pop.png")
p1 <- ggplot(prison.data[prison.data$jurisdiction == "U.S. Total", ], 
             aes(x = year, y = private, group = jurisdiction)) +
  geom_line(stat="identity", color=red, size=1.5) +
  scale_x_continuous(breaks=c(seq(2000, 2014, 2))) +
  scale_y_continuous(breaks=c(seq(min.pop, max.pop + percision, percision))) +
  expand_limits(x = 2000, y = max.pop) +
  xlab("Year") +
  ylab("Population of Private Prisons") +
  ggtitle("Rapid Growth in Total Population of Private Prisons\nfrom 2000-2014") +
  orangeLineTheme()
p1
dev.off()


#
# Plot all of the states' private prison populations over time
#
not.a.state <- c("U.S. Total", "Federal", "State", "Midwest", "Northeast", "South", "West")

state.prison.data <- prison.data[ !prison.data$jurisdiction %in% not.a.state, ]

png("./reports/figures/private_prison_pop_by_state.png")
p2 <- ggplot(state.prison.data,
             aes(x = year, y = private, group = jurisdiction)) +
  geom_line(stat = "identity", aes(color=jurisdiction)) + 
  xlab("Year") + 
  ylab("Population in Private Prisons") +
  ggtitle("Population in Private Prisons by State (2000-2014)")
p2
dev.off()

labeled.states = c("Texas", "Florida")

png("./reports/figures/private_prison_pop_by_state_highlighted.png")
p3 <- ggplot(state.prison.data,
             aes(x = year, y = private, group = jurisdiction)) +
  geom_line(stat = "identity", color = dark.gray) +
  geom_line(stat = "identity", data = subset(state.prison.data, jurisdiction == "Texas"),
            color = red, size = 1) +
  geom_line(stat = "identity", data = subset(state.prison.data, jurisdiction == "Florida"),
            color = red, size = 1) +
  geom_label_repel(
    data = subset(state.prison.data, jurisdiction %in% labeled.states & year == 2014),
    aes(2014, private, label = jurisdiction),
    color = "black",
    fill = light.gray,
    box.padding = unit(0.3, "cm"),
    point.padding = unit(0.1, "cm"),
    segment.color = 'grey50',
    label.size = 0
  ) +
  scale_x_continuous(breaks=c(seq(2000, 2014, 2))) +
  # scale_y_continuous(breaks=c(seq(min.pop, max.pop + percision, percision))) +
  # expand_limits(x = 2000, y = max.pop) +
  xlab("Year") +
  ylab("Population in Private Prisons") +
  ggtitle("Texas and Florida Have Highest Populations\nin Private Prisons (2000-2014)") +
  orangeLineTheme() +
  theme(
    legend.position = "none"
  )
p3
dev.off()


#
# Plot the proportion of prisoners held in private prisons
#

state.prison.data$fraction.private = state.prison.data$private / state.prison.data$total

png("./reports/figures/private_prison_fraction_pop_by_state.png")
p4 <-ggplot(state.prison.data,
            aes(x = year, y = fraction.private, group = jurisdiction)) +
  geom_line(stat = "identity", aes(color=jurisdiction)) + 
  xlab("Year") + 
  ylab("Fraction in Private Prisons") +
  ggtitle("Fraction of Prison Population in Private Prisons by State (2000-2014)")
p4
dev.off()

labeled.states = c("New Mexico", "Alaska", "New Hampshire")

png("./reports/figures/private_prison_fraction_pop_by_state_highlighted.png")
p5 <- ggplot(state.prison.data,
             aes(x = year, y = fraction.private, group = jurisdiction)) +
  geom_line(stat = "identity", color = dark.gray) +
  geom_line(stat = "identity", data = subset(state.prison.data, jurisdiction == "New Mexico"),
            color = "red") +
  geom_line(stat = "identity", data = subset(state.prison.data, jurisdiction == "Alaska"),
            color = "blue") +
  geom_line(stat = "identity", data = subset(state.prison.data, jurisdiction == "New Hampshire"),
            color = "green") +
  geom_label_repel(
    data = subset(state.prison.data, jurisdiction %in% labeled.states & year == 2014),
    aes(2014, fraction.private, label = jurisdiction),
    color = "black",
    fill = light.gray,
    box.padding = unit(0.3, "cm"),
    point.padding = unit(0.1, "cm"),
    segment.color = 'grey50',
    label.size = 0
  ) +
  xlab("Year") + 
  ylab("Fraction in Private Prisons") +
  ggtitle("Fraction of Prison Population in Private Prisons by State (2000-2014)")
  theme(
    legend.position = "none"
  )
p5
dev.off()


#
# Plot prison population vs total population
#

world.prison.pop <- read.csv("./data/prison-population-by-country.csv")
world.prison.pop$Prison.Population <- world.prison.pop$Prison.Population / 100000
world.pop <- read.csv("./data/population-by-country.csv")
world.pop.data <- merge(x = world.prison.pop, y = world.pop, by = "Country")

labeled.countries = c("United States", "China", "Russia", "United Kingdom", "India")

png("./reports/figures/total_population_vs_prison_population_by_country.png")
p6 <- ggplot(world.pop.data,
       aes(x = Population, y = Prison.Population, group = Country)) +
  geom_point(data = world.pop.data, color = dark.gray) +
  geom_label_repel(
    data = subset(world.pop.data, Country %in% labeled.countries),
    aes(x = Population, y = Prison.Population, label = Country),
    color = dark.gray,
    fill = white,
    box.padding = unit(0.15, "cm"),
    point.padding = unit(0.1, "cm"),
    segment.color = 'grey50',
    label.size = 0,
    size = 3
  ) +
  theme(
    panel.background = element_rect(fill = "transparent", color = "transparent"),
    plot.background =  element_rect(fill = "transparent", color = "transparent"),
    axis.title = element_blank(),
    # axis.ticks = element_blank(),
    # axis.text = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    panel.margin.y = unit(0, "cm")
  )
p6
dev.off()


#
# Map cruel and inhuame incidents in private prisons across the country
#

state.map.data <- map_data("state")
incidents.data <- read.csv("./data/cruel_and_inhumane_incidents.csv")
incidents.locs <- read.csv("./data/cruel_and_inhumane_incidents_locs.csv")

p7 <- ggplot() +
  geom_map(data = state.map.data, map = state.map.data,
           aes(x=long, y=lat, map_id=region), fill = dark.gray, color = off.black) +
  geom_point(data = incidents.locs, aes(x = lon, y = lat), size = 9, color = orange) +
  geom_text(data = incidents.locs, aes(x = lon, y = lat, label = id), color = white,
            fontface = "bold", vjust = 0.35, hjust = 0.40) +
  mapTheme()
p7


#
# Graph 34,000 beds
#

p8 <- waffle(c(340), rows = 10, size = 0, use_glyph = "bed", glyph_size = 4,
             legend_pos = "none", color = red,
             title ="Congress Forces ICE to Maintain 34,000\nPrison Cells for Immigration",
             xlab = "1 Bed = 100 ICE Beds") +
  waffleTheme()
p8


#
# Create the poster
#

vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
prison.compare.img <- readJPEG("./data/norway-vs-alcatraz.jpg")


cairo_pdf("./reports/figures/private_prisons.pdf", width = 30, height = 40, family = "Arial Unicode MS")
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=8, ncol=6)))
grid.rect(gp = gpar(fill = light.gray, color = light.gray))

print(p6 + theme(plot.margin=margin(7, 0.5, 6, 0.5, unit="cm")),
      vp = vplayout(1:8,1:6))

# header
grid.rect(gp = gpar(fill = orange, color = orange),
          x = unit(0.5, "npc"), y = unit(0.95, "npc"),
          width = unit(1, "npc"), height = unit(0.10, "npc"))
grid.text("Monetizing Prisons",
          x = unit(0.5, "npc"), y = unit(0.97, "npc"), 
          gp = gpar(fontfamily = main.font, color = dark.gray, cex = 10))
grid.text("HOW MONETIZING THE U.S. PRISON SYSTEM HAS AFFECTED SOCIETY",
          x = unit(0.5, "npc"), y = unit(0.93, "npc"),
          gp = gpar(fontfamily = main.font, color = light.gray, cex = 3))
grid.text("Andrew Batbouta and Matt Poegel",
          x = unit(0.5, "npc"), y = unit(0.91, "npc"),
          gp = gpar(fontfamily = main.font, color = light.gray, cex = 2))

header.gpar <- gpar(fontfamily = main.font, col = off.black, cex = 4)
paragraph.gpar <- gpar(fontfamily = main.font, col = off.black, cex = 2)
splash.gpar <- gpar(fontfamily = main.font, col = dark.orange, cex = 9)


print(p1 + theme(plot.margin=margin(2.5, -1, -2.5, 3.5, unit="cm")),
      vp = vplayout(3, 1:2))
print(p7 + theme(plot.margin=margin(1, 0, 0, 3, unit="cm")),
      vp = vplayout(7, 1:2))
print(p3 + theme(plot.margin=margin(-6, 0, 4, 4, unit="cm")),
      vp = vplayout(5, 3:4))

# first column
grid.text("Prisons for Profit", x = unit(0.12, "npc"), y = unit(0.88, "npc"), gp = header.gpar)

pp.intro <- c(
  "There are 2.2 Million people in prisons in the US.",
  "We have the most in the world, 600K more than China a country that has 1 billion more citizens than the US.",
  "Why do we have so many? We don't take care of our prisoners, 77% recidivism in State prisons, 44% in federal. Norway has 20% recidivism.",
  "We punish rather than rehabilitate.",
  "Private Prisons bring out the worst in prisons by having no motivations to help prisoners and rather and can make money by not rewarding good behavior and having high recidivism."
)

grid.text(bulletize(pp.intro, 55),
          x = unit(0.03, "npc"), y = unit(0.795, "npc"), gp = paragraph.gpar, just = "left")

grid.text("Cruel and Inhumane", x = unit(0.02, "npc"), y = unit(0.58, "npc"), gp = header.gpar,
          just = "left")

i <- 0
for (d in levels(incidents.data$description)) {
  ss <- incidents.data[ incidents.data$description == d, ]
  lines <- strwrap(ss$description[1], width = 60 - (length(ss$id) * 3))
  i <- i + max(0, length(lines) / 4)
  k <- 0
  while (k < length(ss$id)) {
    grid.circle(x = 0.03 + (k * 0.015), y = 0.57 - (i * 0.03), r = 0.007,
                gp = gpar(col = orange, fill = orange))
    grid.text(ss$id[k + 1], x = 0.03 + (k * 0.015), y = 0.57 - (i * 0.03),
              gp = gpar(col = white, cex = 1.5))
    k <- k + 1
  }
  grid.text(paste(lines, collapse = "\n"), 
            x = 0.03 + (k * 0.015), y = 0.57 - (i * 0.03), gp = paragraph.gpar, just = "left")
  i <- i + max(0.5, length(lines) / 4)
}

# second column

grid.text("High Recidivism", x = unit(0.38, "npc"), y = unit(0.88, "npc"), gp = header.gpar,
          just = "left")
pp.high.recid1 <- c(
  "There is a lack of reason to rehabilitate prisoners, because companies can make profits instead",
  "Labor includes making Licence plates to office furniture to staffing call centers, run by UNICOR, a government run corporation",
  "Inmates get paid in a range from $0.23 to $1.15 an hour working in prisons. Prisoners do not get health insurance or benefits."
)
grid.text(bulletize(pp.high.recid1, 55),
          x = unit(0.38, "npc"), y = unit(0.815, "npc"), gp = paragraph.gpar, just = "left")

grid.text("$2.4 Billion", x = unit(0.53, "npc"), y = unit(0.73, "npc"),
          gp = splash.gpar)
grid.text("the amount of money prison labor generates each year", x = unit(0.535, "npc"), 
          y = unit(0.705, "npc"), gp = paragraph.gpar, just = "center")

pp.high.recid2 <- c(
  "Private prisons do not reward good behaviour often because it would lose inmates, which are the source of profits.",
  "Prisoners often lack resources found at state or Federal prisons",
  "To increase profits, private prisons cut back on spending related to improving prisoner conditions",
  "These prisons often employ less guards to save money, which leads to more violence"
)
grid.text(bulletize(pp.high.recid2, 55),
          x = unit(0.38, "npc"), y = unit(0.62, "npc"), gp = paragraph.gpar, just = "left")

grid.text("Prison is Punishment", x = unit(0.38, "npc"), y = unit(0.40, "npc"),
          gp = header.gpar, just = "left")



# third column

grid.text("The Solutions", x = unit(0.70, "npc"), y = unit(0.58, "npc"), gp = header.gpar,
          just = "left")
pp.solutions.1 <- c(
  "Improve living conditions to make inmates feel more like people.",
  "Restore voting rights, should be able to act as citizens if they will be citizens after finishing sentence."
)
grid.text("Treat Prisoners Like People", x = unit(0.705, "npc"), y = unit(0.56, "npc"),
          gp = gpar(fontfamily = main.font, col = off.black, cex = 2, fontface = "italic"), just = "left")
grid.text(bulletize(pp.solutions.1, 50),
          x = unit(0.70, "npc"), y = unit(0.52, "npc"), gp = paragraph.gpar, just = "left")
pp.solutions.2 <- c(
  "Non violent crimes, drug offenses should not result in serious jail time.",
  "Those not yet convicted of their crime should not be in jail, many pretrial detainees who, in some cases, had spent years detained in county jails awaiting trial.",
  "Improve free legal aid so that people aren't forced to make a deal instead of a fair trial."
)
grid.text("Keep Less People in Prison", x = unit(0.70, "npc"), y = unit(0.48, "npc"),
          gp = gpar(fontfamily = main.font, col = off.black, cex = 2, fontface = "italic"), just = "left")
grid.text(bulletize(pp.solutions.2, 50),
          x = unit(0.70, "npc"), y = unit(0.42, "npc"), gp = paragraph.gpar, just = "left")
pp.solutions.3 <- c(
  "Spend less money on prisons and re-invest in health care programs for those with mental health or drug addiction programs.",
  "Have more programs to provide job training, english lessons, parenting, and drug abuse before prisoners will receive these programs in jail."
)
grid.text("Spend More on Health and Welfare Programs", x = unit(0.70, "npc"), y = unit(0.36, "npc"),
          gp = gpar(fontfamily = main.font, col = off.black, cex = 2, fontface = "italic"), just = "left")
grid.text(bulletize(pp.solutions.3, 50),
          x = unit(0.70, "npc"), y = unit(0.31, "npc"), gp = paragraph.gpar, just = "left")


grid.raster(prison.compare.img, x = unit(0.85, "npc"), y = unit(0.18, "npc"), width = unit(0.27, "npc"))



# footer
grid.text(paste(intToUtf8(0x0266B),
                "\"It's a fact that needs to be spoken: America's prisons are broken!\"",
                intToUtf8(0x0266B)),
          x = unit(0.5, "npc"), y = unit(0.08, "npc"),
          gp = gpar(fontfamily = main.font, fontface = "italic", col = off.black, cex = 3.5))
grid.text("- John Oliver",
          x = unit(0.5, "npc"), y = unit(0.06, "npc"),
          gp = gpar(fontfamily = main.font, col = off.black, cex = 2))

grid.rect(gp = gpar(fill = orange, color = orange),
          x = unit(0.5, "npc"), y = unit(0, "npc"),
          width = unit(1, "npc"), height = unit(0.1, "npc"))
grid.text("American Politics in Crisis, Fall 2016",
          x = unit(0.89, "npc"), y = unit(0.01, "npc"),
          gp = gpar(fontfamily = main.font, col = off.black, cex = 2))

dev.off()


