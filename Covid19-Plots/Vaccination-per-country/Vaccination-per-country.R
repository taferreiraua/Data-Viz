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

# plot - vacinação por país por continente

# titulo, subtitulo e caption
title = paste0("<span style='font-family:opensans;color:#242526;'><br>COVID-19 VACCINATION PROGRESS BY COUNTRY IN 2022</span>",
               "<span style='font-family:opensans;color:#242526;font-size:6pt;'><br>Percent of people who received at least one vaccine dose per country.</span>")
caption <- paste0(
  "<span style='font-family:opensans;color:#242526;font-size:5.7pt;'>Source: Our World In Data </span><br>",
  "<span style='font-family:fb;color:#242526;font-size:4pt;'>&#xf099;</span>",
  "<span style='font-family:opensans;color:#242526;font-size:4pt;'> @taferreiraua  | </span>",
  "<span style='font-family:fb;color:#242526;font-size:4pt;'>&#xf09b;</span>",
  "<span style='font-family:opensans;color:#242526;font-size:4pt;'> taferreiraua </span>"
)

ggplot(country_data) +
  geom_point(aes(x=location, y=1, size=1.8), colour='#ff4d6d', show.legend = T) +
  geom_point(aes(x=location, y=1, size=percent.people.vaccinated), colour='#ffb3c1', show.legend = T) +
  geom_text(aes(x=location, y=1, label=paste0(round(percent.people.vaccinated*100,1), '%')),
            size=2.3, fontface='bold', family='headland one') +
  facet_wrap(~location, scales = "free", labeller = labeller(.rows = label_wrap_gen(1))) +
  theme_minimal() +
  labs(title=title, caption=caption) +
  theme(
    plot.title.position = 'plot',
    plot.title = element_markdown(size=7.5, hjust = .5, lineheight=.7),
    plot.caption = element_markdown(size=5, lineheight=.7),
    plot.background = element_rect(fill='#fff0f3', color='#fff0f3'),
    axis.title = element_blank(),
    axis.text = element_blank(),
    legend.position = 'top',
    panel.grid = element_blank(),
    strip.background = element_blank(),
    strip.text = element_text(size=6, family = 'opensans', lineheight=.5),
    strip.clip = 'off'
  ) +
  guides(size="none")

ggview(units = 'px', width = 600, height = 1100)
ggsave(units = 'px', width = 600, height = 1100, filename='Vaccination-per-country.png')
