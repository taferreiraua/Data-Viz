library(tidyverse)
library(reshape2)
library(ggtext)
library(ggview)



# dados
df <- read.csv("~/owid-covid-data.csv")


# fontes
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Headland One", "headlandOne")
sysfonts::font_add_google("Open Sans","opensans")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)


# titulo, subtitulo e caption
title = paste0("<span style='font-family:headlandOne;font-size:27pt;color:black;'><br> **Covid-19 vaccination across the world** </span>")
subtitle = paste0("<span style='font-family:headlandOne;font-size:12pt;color:#242526;'>Vaccination progress by country. January, 2023.</span>")
caption <- paste0(
  "<span style='font-family:fb;color:#242526;'>&#xf099;</span>",
  "<span style='font-family:opensans;color:#242526;'> @taferreiraua |</span>",
  "<span style='font-family:fb;color:#242526;'>&#xf09b;</span>",
  "<span style='font-family:opensans;color:#242526;'> taferreiraua </span>",
  "<span style='font-family:opensans;color:#242526;'>| Dados: Our World In Data </span>")


# cores
col_bg = '#FFFFFF'
palette = c(
  '#55A6CB',
  '#F7C45F',
  '#D65353'
)
col_nv = '#D65353'
col_v = '#F7C45F'
col_fv = '#55A6CB'


# manipulaçao de dados
vac_data = df |>
  filter(date=='2023-01-01' & !grepl("OWID", iso_code) & !is.na(people_vaccinated) & !is.na(people_fully_vaccinated)) |>
  arrange(desc(population)) |>
  mutate(percent_fully_vac = people_fully_vaccinated/population,
         percent_vac = (people_vaccinated/population) - percent_fully_vac,
         percent_not_vac = 1 - percent_vac - percent_fully_vac,
         percent_total = percent_vac + percent_fully_vac + percent_not_vac,
         percent_fully_vac_label = paste0(round(percent_fully_vac*100, 1), '%'),
         percent_vac_label = paste0(round(percent_vac*100, 1), '%'),
         percent_not_vac_label = paste0(round(percent_not_vac*100, 1), '%')) |>
  select(location, percent_fully_vac, percent_vac, percent_not_vac,
         percent_fully_vac_label, percent_vac_label, percent_not_vac_label) 

chart_data = vac_data |>
  select(location, percent_fully_vac, percent_vac, percent_not_vac) |>
  melt(id.var = "location") |>
  left_join(vac_data, by=c('location'='location'))
  

# plot
ggplot(chart_data) + 
  geom_bar(aes(x=value, y=reorder(location, percent_fully_vac), fill=variable),
           stat='identity', position='fill', width=.7) +
  geom_text(aes(x=1.09, y=reorder(location, percent_fully_vac), label=percent_not_vac_label),
            size=3.5, fontface = "bold", color=col_nv) +
  geom_text(aes(x=1.16, y=reorder(location, percent_fully_vac), label=percent_vac_label),
            size=3.5, fontface = "bold", color=col_v) +
  geom_text(aes(x=1.23, y=reorder(location, percent_fully_vac), label=percent_fully_vac_label),
            size=3.5, fontface = "bold", color=col_fv) +
  scale_fill_manual(values = palette, 
                    labels = c("% Vaccineted (all doses prescribed)", 
                               "% Vaccineted (at least one dose)", 
                               "% Not vaccineted")) +
  theme_minimal() +
  labs(title=title, subtitle=subtitle, caption=caption) +
  theme(
    plot.title.position = 'plot',
    plot.title = element_markdown(hjust=.5),
    plot.subtitle = element_markdown(hjust=.5),
    plot.caption = element_markdown(size=10),
    plot.margin = margin(t=2, r=10, b=8, l=8),
    plot.background = element_rect(fill=col_bg, color=col_bg),
    panel.grid = element_blank(),
    legend.position = "top",
    legend.margin = margin(t=5, r=61, b=-5, l=0),
    legend.title = element_blank(),
    legend.key.size = unit(.1, 'cm'),
    legend.text = element_text(size=11),
    axis.text.y = element_text(size=15),
    axis.text.x = element_blank(),
    axis.title = element_blank()
    ) +
  guides(fill = guide_legend(label.position = "bottom", reverse = T))

ggview(units="px", height=2200, width=1700)
ggsave(units="px", filename = "World-Vaccinatin-Covid19.png", height=2200, width=1700)
