library(tidyverse)
library(sysfonts)
library(showtext)
library(ggimage)
library(ggtext)


df <- read.csv("C:/Users/Thays Ferreira/Downloads/df_1.csv")


# fontes
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Open Sans", "opensans")
sysfonts::font_add_google("Girassol", "girassol")
showtext::showtext_auto()
showtext::showtext_opts(dpi=176)


# titulo
titulo <- paste0(
  "<br><span style='font-family:girassol;font-size:44pt;color:white;'> **Epidemias/Pandemias** <br> **mais mortais da historia** </span>"
)

caption <- paste0(
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style='font-family:opensans;'> @taferreiraua | </span>",
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:opensans;'> taferreiraua </span>",
  "<span style='font-family:opensans;'> | Dados: Centers for Disease Control and Prevention (CDC) </span>"
)


# paleta
color_bg <- "black"
col_low <- "red"
col_high <- "yellow"
col_tit <- "white"
col_nom <- "gold"


# plot
top7 <- df %>% 
  filter(Location=="Worldwide") %>%
  arrange(Rank) %>%
  mutate(Hanking = 1:7,
         Lost.Popul = case_when(Death.toll=="17–100 million"~"17-100 milhões",
                                Death.toll=="40.1 million (as of 2021)"~"40.1 milhões",
                                Death.toll=="7–28 million (as of November 2022)"~"7-28 milhões",
                                Death.toll=="12–15 million"~"12-15 milhões",
                                Death.toll=="1–4 million"~"1-4 milhões",
                                Death.toll=="1–4 million"~"1-4 milhões",
                                Death.toll=="1 million+"~"1 milhão",
                                TRUE ~ Death.toll
                                ),
         Date = case_when(Date=="2019[c]–present"~"2019–presente",
                          Date=="1981–present"~"1981–presente",
                          TRUE ~ Date
                          ),
         Disease = case_when(Disease=="Influenza A/H1N1"~"H1N1",
                             Disease=="Influenza A/H2N2"~"H2N2",
                             Disease=="Influenza A/H3N2"~"H3N2",
                             Disease=="Cholera"~"Cólera",
                             Disease=="Bubonic plague"~"Peste Bubônica",
                             TRUE ~ Disease)
         ) %>%
  select(Hanking, Hanking_label, Date, Disease, Lost.Popul)
  
# -Hanking
top7 %>% ggplot(aes(-Hanking, rev(Hanking))) +
  geom_col(aes(fill=Hanking)) +
  scale_fill_gradient(low=col_low, high=col_high) +
  geom_text(aes(label=Disease), nudge_y = 0.8, size=6, 
            fontface="bold", family="opensans", color=col_nom) +
  geom_text(aes(label=paste0('(', Date, ')')), nudge_y = 0.5, size=5.7,
            fontface="bold", family="opensans", color=col_nom) +
  geom_text(aes(label=Lost.Popul), nudge_y = 0.2, size=5.15, 
            fontface="bold", family="opensans", color=col_tit) +
  labs(title=titulo, caption=caption) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill=color_bg, color=color_bg),
    plot.title = element_textbox_simple(halign=0.5, maxwidth = unit(7, "in")),
    plot.caption = element_markdown(size = 11.5, color="white"),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )

  ggsave("Maiores-Pand-Epid.png", width=10, height=6)
