library(tidyverse)
library(reshape2)
library(ggtext)
library(ggview)


# dados
df <- read.csv("~/owid-covid-data.csv")


# fontes
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Open Sans","opensans")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)

# cores
pal_continents = c('#ffb3c1', '#ff758f', '#ff4d6d')


# titulo, subtitulo e caption
title = paste0("<span style='font-family:opensans;color:#242526;'><br>COVID-19 VACCINATION PROGRESS BY CONTINENT IN 2022<br><br> </span>")
caption <- paste0(
  "<span style='font-family:opensans;color:#242526;font-size:23pt;'>Source: Our World In Data </span><br>",
  "<span style='font-family:fb;color:#242526;'>&#xf099;</span>",
  "<span style='font-family:opensans;color:#242526;'> @taferreiraua  | </span>",
  "<span style='font-family:fb;color:#242526;'>&#xf09b;</span>",
  "<span style='font-family:opensans;color:#242526;'> taferreiraua </span>"
  )


# manipulação de dados
continent_data = df |>
  filter(date=='2023-01-01' & 
           location %in% c('Europe', 'Asia', 'South America', 'North America', 'Africa', 'Oceania') & 
           !is.na(people_fully_vaccinated) & !is.na(people_vaccinated)) |>
  select(location, population, people_fully_vaccinated, people_vaccinated) |>
  mutate(people_not_vaccinated = population - people_vaccinated,
         perc.vac = (people_vaccinated - people_fully_vaccinated)/population,
         perc.fully.vac = people_fully_vaccinated/population,
         perc.not.vac = people_not_vaccinated/population) |>
  select(location, perc.not.vac, perc.vac, perc.fully.vac) |>
  melt(id.var='location')


# plot - vacinação por continentes
ggplot(continent_data) +
  geom_bar(aes(x=value, y=location, fill=variable), stat='identity') +
  scale_fill_manual(values=pal_continents, labels=c("% Not vaccineted", 
                                                    "% Vaccineted (at least one dose)", 
                                                    "% Vaccineted (all doses prescribed)")) +
  scale_x_continuous(breaks=c(0, .1, .2, .3, .4, .5, .6, .7, .8, .9, 1), 
                     labels = c('0%', '10%', '20%', '30%', '40%', '50%', 
                                '60%', '70%', '80%', '90%', '100%'),
                     expand = c(0, 0),
                     sec.axis = dup_axis()) +
  theme_minimal() +
  labs(title=title, caption=caption) +
  theme(
    plot.title.position = 'plot',
    plot.title = element_markdown(size=40, hjust=0),
    plot.caption = element_markdown(size=17),
    plot.background = element_rect(fill='#fff0f3', color='#fff0f3'),
    plot.margin = margin(t=50, r=190, b=100, l=100),
    panel.grid = element_blank(),
    legend.margin = margin(t=10, r=0, b=50, l=0),
    legend.position = 'bottom',
    legend.justification = "center",
    legend.title = element_blank(),
    legend.key.size = unit(.5, 'cm'),
    legend.text = element_text(size=24),
    axis.title = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.text.x.top = element_text(size=25),
    axis.text.y = element_text(size=30)
  ) +
  guides(fill = guide_legend(reverse = T))


ggview(units='px', height=4000, width=4000)
ggsave(units='px', height=4000, width=4000, filename="Vaccination-by-continent.png")
