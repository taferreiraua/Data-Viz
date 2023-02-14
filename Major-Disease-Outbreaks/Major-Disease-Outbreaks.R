library(tidyverse)
library(sysfonts)
library(ggtext)
library(ggview)


df <- read.csv("https://raw.githubusercontent.com/taferreiraua/Estudos-de-Exploracao-e-Visualizacao-de-Dados/main/Major-Disease-Outbreaks/Dados/MajorDeseaseOutbreaks.csv")


# fontes
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Open Sans", "opensans")
showtext::showtext_auto()
showtext::showtext_opts(dpi=176)


# caption
caption <- paste0(
  "<span style='font-family:fb;color:#212529;'>&#xf099;</span>",
  "<span style='font-family:opensans;color:#212529;'> @taferreiraua | </span>",
  "<span style='font-family:fb;color:#212529;'>&#xf09b;</span>",
  "<span style='font-family:opensans;color:#212529;'> taferreiraua </span>",
  "<span style='font-family:opensans;color:#212529;'> | Dados: Centers for Disease Control and Prevention (CDC) </span>"
)

# cores
col_background = "#F8F9FA"
col_bars = "#212529"
    
  
# plot
top7 <- df |> 
  filter(Location=="Worldwide") |>
  arrange(Rank) |>
  mutate(Hanking = paste0(c(1:7), "º"),
         Lost.Popul = case_when(Death.toll=="17–100 million"~"17-100 milhões",
                                Death.toll=="40.1 million (as of 2021)"~"40.1 milhões",
                                Death.toll=="7–28 million (as of November 2022)"~"7-28 milhões",
                                Death.toll=="12–15 million"~"12-15 milhões",
                                Death.toll=="1–4 million"~"1-4 milhões",
                                Death.toll=="1–4 million"~"1-4 milhões",
                                Death.toll=="1 million+"~"1 milhão",
                                TRUE ~ Death.toll
                                ),
         Date = case_when(Date=="2019[c]–present"~"2019-presente",
                          Date=="1981–present"~"1981-presente",
                          TRUE ~ Date
                          ),
         Disease = case_when(Disease=="Influenza A/H1N1"~"H1N1",
                             Disease=="Influenza A/H2N2"~"H2N2",
                             Disease=="Influenza A/H3N2"~"H3N2",
                             Disease=="Cholera"~"Cólera",
                             Disease=="COVID-19"~"Covid-19",
                             Disease=="Bubonic plague"~"Peste Bubônica",
                             TRUE ~ Disease)
         ) |>
  select(Hanking, Date, Disease, Lost.Popul) |>
  mutate(n = -4:2) |>
  rowwise() |>
  mutate(
    x = list(c(-6, 0, 0, -6)),
    y = list(c(n*4 - 1.4, n*2 - 0.7, n*2 + 0.7, n*4 + 1.4))
  ) |>
  unnest(cols = c(x, y)) 
  

ggplot(top7) +
  geom_rect(aes(xmin = -18.5, ymin = n*4 - 1.4,
                xmax = -6, ymax = n*4 + 1.4), fill = col_bars, color = NA) +
  geom_polygon(aes(x, y, group = n), fill = col_bars, color = NA) +
  geom_rect(aes(xmin = 0, ymin = n*2 - 0.7,
                xmax = 7, ymax = n*2 + 0.7), fill = col_bars, color = NA) +
  geom_text(aes(-17.5, n*4, label = Disease), family = 'opensans', fontface = "bold",
            color = col_background, hjust = 0, size = 12.5, check_overlap = T) +
  geom_text(aes(7.5, n*2, label = Date), family = "opensans", fontface = "bold",
            color = col_bars, hjust = 0, size = 5.9, check_overlap = T) +
  geom_text(aes(6.5, n*2, label = Lost.Popul), family = "opensans", fontface = "bold",
            color = col_background, hjust = 1, size = 5, check_overlap = TRUE) +
  geom_text(aes(x=-20, y=n*4, label = Hanking), family = "opensans", fontface = "bold",
            color = col_bars, hjust = .5, size=12, check_overlap = T) +
  geom_text(aes(x=5.3, y=-16.5, label = str_wrap("As 7 epidemias/pandemias mais mortais da história", 25)),
            color=col_bars, hjust = .5, size = 10, check_overlap = T, lineheight = 0.5,
            family="opensans", fontface="bold") +
  geom_text(aes(x=4, y=-11.5, label="Número estimado de mortos"), 
            family="opensans", size=4.5, color=col_bars) +
  geom_text(aes(x=9, y=7.5, label="Período de tempo"), 
            family="opensans", size=4.5, color=col_bars) +
  geom_segment(aes(x=4, xend=4, y=-11, yend=-9)) +
  geom_segment(aes(x=9, xend=9, y=5, yend=7)) +
  scale_y_reverse() +
  scale_x_continuous(limits = c(-20, 12)) +
  theme_minimal() +
  labs(caption=caption) +
  theme(
    plot.background = element_rect(fill=col_background, color=col_background),
    plot.caption = element_markdown(size=12),
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank()
  )


ggview(units='px', height=1900, width=2400)  
ggsave("Maiores-Pand-Epid.png", units='px', width=2400, height=1900)
