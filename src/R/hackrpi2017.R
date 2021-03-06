library(ggplot2)
library(grid)
library(waffle)

#
# Color theme
#
red <- '#C03328'
dark.red <- '#C1261A'
dark.red2 <- '#8c0000'
foreground <- gray(0.2)
background <- gray(0.95)
load.fontawesome()


#
# The data
#
num.unique.schools <- 258
num.projects <- 60
percent.no.prior.hackathons <- 32.15
slices.of.pizza <- 130 * 10
farthest.distance.traveled <- 8188

interests <- data.frame(percentage = c(43.6, 92.5, 81.26, 75.65),
                        label = c('To find a job', 'To make a cool project',
                                      'To explore new technologies', 'To meet new people'))

interests.plot <- ggplot(interests, aes(x = label, y = percentage)) +
  geom_bar(stat = 'identity', fill = gray(0.7)) +
  geom_text(aes(label = paste(' ', label), x = label, y = 0), hjust = 0, color = red, size = 5) +
  geom_text(aes(label = paste(' ', percentage, '%', sep = ''), x = label, y = percentage), hjust = 0,
            color = red, size = 5) +
  ylim(0, 100) +
  coord_flip() +
  theme(
    plot.background = element_rect(fill = background, color = background),
    panel.background = element_rect(fill = background),
    axis.text = element_blank(),
    plot.title = element_text(color = red, face = 'bold', size = 18, vjust = 1, 
                              family = 'Segoe UI'),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_blank()
  )
interests.plot

interests.areas <- data.frame(percentage = c(66.28, 52.96, 55.04, 60.77, 38.91, 40.58),
                              label = c('Backend', 'Data Science', 'Frontend (UI/UX)',
                                        'Mobile', 'Game Development', 'Hardware'))

ggplot(interests.areas, aes(x = label, y = percentage)) +
  geom_bar(stat = 'identity', fill = gray(0.7)) +
  geom_text(aes(label = paste(' ', label), x = label, y = 0), hjust = 0, color = red, size = 5) +
  geom_text(aes(label = paste(' ', percentage, '%', sep = ''), x = label, y = percentage), hjust = 0,
            color = red, size = 5) +
  ylim(0, 100) +
  coord_flip() +
  theme(
    plot.background = element_rect(fill = background, color = background),
    panel.background = element_rect(fill = background),
    axis.text = element_blank(),
    plot.title = element_text(color = red, face = 'bold', size = 18, vjust = 1, 
                              family = 'Segoe UI'),
    axis.title = element_blank(),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_blank()
  )

backend.waffle <- waffle(c('Backend'=66, '-'=34),
                         rows=5,
                         size=0,
                         colors = c(red, gray(0.7)),
                         #use_glyph = 'database',
                         legend_pos = 'non') +
  theme(
    plot.background = element_rect(fill = background, color = background),
    panel.background = element_rect(fill = background),
    panel.grid.major.y = element_blank(),
    panel.grid.minor.y = element_blank(),
    panel.grid.major.x = element_blank(),
    panel.grid.minor.x = element_blank(),
    strip.text = element_blank(),
    strip.background = element_blank(),
    axis.ticks = element_blank()
  )
backend.waffle


#
# Make the infographic!
#
vplayout <- function(x, y) viewport(layout.pos.row = x, layout.pos.col = y)
windowsFonts('Segoe UI'=windowsFont('Segoe UI'))

png('./reports/figures/hackrpi2017.png', width = 8.5, height = 11, units = "in", res = 300,
    family = 'Segoe UI')
#cairo_pdf('./reports/figures/hackrpi2017.pdf', width = 8.5, height = 11,
#    family = 'Segoe UI')
grid.newpage()
pushViewport(viewport(layout = grid.layout(nrow=5, ncol=4)))
grid.rect(gp = gpar(fill = background, col = background))

# title
grid.text('&HackRPI = 0x2016',
          x = unit(0.5, 'npc'), y = unit(0.90, 'npc'),
          gp = gpar(fontfamily = 'Segoe UI', col = red, fontsize = 56))
grid.text('BY THE NUMBERS',
          x = unit(0.1, 'npc'), y = unit(0.83, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = red, fontsize = 24))
grid.rect(
  x = unit(0.1, 'npc'), y = unit(0.81, 'npc'), just = 'left',
  width = unit(0.8, 'npc'), height = unit(0.005, 'npc'),
  gp = gpar(fill = red, col = red)
)

# numbers
number.size <- 48
label.size <- 12
# first row
grid.text(num.unique.schools,
          x = unit(0.16, 'npc'), y = unit(0.74, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = number.size))
grid.text("unique schools registered",
          x = unit(0.11, 'npc'), y = unit(0.69, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = label.size))
grid.text("2    3",
          x = unit(0.56, 'npc'), y = unit(0.73, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = number.size))
grid.text("in\nevery",
          x = unit(0.645, 'npc'), y = unit(0.73, 'npc'), just = 'center',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = label.size))
print(backend.waffle + theme(plot.margin=margin(-3.2, 2, 3.2, -2, unit="cm")),
      vp = vplayout(3, 3:4))
grid.text("students were interested in backend development",
          x = unit(0.44, 'npc'), y = unit(0.68, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = label.size))

# second row
grid.text(slices.of.pizza,
          x = unit(0.14, 'npc'), y = unit(0.61, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = number.size))
grid.text("slices of pizza consumed",
          x = unit(0.12, 'npc'), y = unit(0.56, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = label.size))

# figures
grid.text("Why do students hack at HackRPI?",
          x = unit(0.15, 'npc'), y = unit(0.49, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = 24))
print(interests.plot + theme(plot.margin=margin(-2, 0.5, 2, 2.5, unit="cm")),
      vp = vplayout(4, 1:3))

# final rows of numbers
grid.text(farthest.distance.traveled,
          x = unit(0.14, 'npc'), y = unit(0.21, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = number.size))
grid.text("most miles traveled to\nHackRPI",
          x = unit(0.13, 'npc'), y = unit(0.16, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = label.size))
grid.text(num.projects,
          x = unit(0.46, 'npc'), y = unit(0.21, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = number.size))
grid.text("projects created",
          x = unit(0.43, 'npc'), y = unit(0.16, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = label.size))
grid.text(percent.no.prior.hackathons,
          x = unit(0.67, 'npc'), y = unit(0.21, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = number.size))
grid.text("percent of students with no\nprior hackathon experience",
          x = unit(0.645, 'npc'), y = unit(0.16, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = foreground, fontsize = label.size))

# footer
grid.rect(
  x = unit(0.1, 'npc'), y = unit(0.1, 'npc'), just = 'left',
  width = unit(0.8, 'npc'), height = unit(0.005, 'npc'),
  gp = gpar(fill = red, col = red)
)
grid.text(paste(intToUtf8(0x000A9), 'HackRPI 2014-2017'),
          x = unit(0.1, 'npc'), y = unit(0.08, 'npc'), just = 'left',
          gp = gpar(fontfamily = 'Segoe UI', col = red, fontsize = 12))
grid.text('Join our team!',
          x = unit(0.5, 'npc'), y = unit(0.08, 'npc'), just = 'center',
          gp = gpar(fontfamily = 'Segoe UI', col = red, fontsize = 24))
grid.text('team@hackrpi.com',
          x = unit(0.9, 'npc'), y = unit(0.08, 'npc'), just = 'right',
          gp = gpar(fontfamily = 'Segoe UI', col = red, fontsize = 12))

dev.off()

