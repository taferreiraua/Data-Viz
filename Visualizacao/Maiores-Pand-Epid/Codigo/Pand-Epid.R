library(tidyverse)
library(sysfonts)
library(showtext)
library(ggimage)
library(ggtext)


df <- read.csv("C:/Users/Thays Ferreira/Downloads/df_1.csv")


# fontes
sysfonts::font_add("Highrise", "fontes/HighriseFont-Bold-Demo.otf")
sysfonts::font_add_google("Open Sans", "opensans")

showtext::showtext_auto()
showtext::showtext_opts(dpi=250)


# titulo
titulo <- paste0(
  "<span style='font-family:Highrise;font-size:16pt;color:white;'> **Maiores Epidemias/Pandemias da historia** </span>"
)


# pallete
color_bg <- "black"
color_bars <- c("#B3E0DC",
                "#81CDC6",
                "#4FB9AF",
                "#28A99E",
                "#05998C",
                "#048C7F",
                "#037C6E")


# manipulação de dados
top7 <- df %>% 
  filter(Location=="Worldwide") %>%
  arrange(Rank) %>%
  mutate(Hanking = 1:7, Hanking_label = paste0(Hanking, "°"),
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
                             TRUE ~ Disease)
         ) %>%
  select(Hanking, Hanking_label, Date, Disease, Lost.Popul)
  
# Hanking
top7 %>% ggplot(aes(-Hanking, rev(Hanking))) +
  geom_col(aes(fill=Hanking_label)) +
  scale_fill_manual(values = rev(color_bars)) +
  geom_text(aes(label=Disease), nudge_y = 1.1, size=1.6, 
            fontface="bold", family="opensans", color="gold") +
  geom_text(aes(label=Lost.Popul), nudge_y = 0.5, size=1.15, 
            fontface="bold", family="opensans", color="white") +
  labs(title=titulo) +
  theme_minimal() +
  theme(
    plot.background = element_rect(fill=color_bg, color=color_bg),
    plot.title = element_textbox_simple(halign=0.5, maxwidth = unit(2, "in")),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.title = element_blank(),
    axis.text = element_blank()
  )
