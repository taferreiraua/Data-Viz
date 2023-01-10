library(tidyverse)
library(sysfonts)
library(showtext)
library(ggimage)
library(ggtext)
library(geomtextpath)


# dados
df <- read.csv("~/WorldCupMatches.csv")


# titulo do grafico
sysfonts::font_add('fs', 'fontes/Font Awesome 6 Free-Solid-900.otf')
titulo <- paste0(
  "<span style='font-family:fs;font-size:16pt;color:white;'> **Gols em titulos da selecao brasileira** </span>"
)
showtext::showtext_auto()
showtext::showtext_opts(dpi=130)




# paleta de cores
pal <- c(
  "yellow",
  "#fdd301",
  "#abcb2d",
  "springgreen4",
  "#49bce2"
)


# adicionais
Year.Cup.Br <- c("1958", "1962", "1970", "1994", "2002")
links <- c("~/1958.png",
           "~/1962.png",
           "~/1970.png",
           "~/1994.png",
           "~/2002.png")

imagens <- data.frame(Year = as.integer(Year.Cup.Br), links)


# manipulação de dados
brasil <- df %>%
  left_join(imagens, by=c("Year"="Year")) %>%
  filter(Year %in% Year.Cup.Br) %>%
  mutate(Gols.Match = Home.Team.Goals + Away.Team.Goals,
         Match.Label = paste0(Home.Team.Initials, "(", Home.Team.Goals, ")", " x ", 
                              "(", Away.Team.Goals, ")", Away.Team.Initials),
         image = links) %>% 
  group_by(Year) %>% 
  mutate(Gols.Cup.Avg = mean(Gols.Match),
         Round.Gols.Cup.Avg = round(mean(Gols.Match))) %>%
  ungroup()


# media de gols por partida
overall_avg = mean(brasil$Gols.Match)


# grafico
ggplot(brasil, aes(group=Year)) + 
  geom_jitter(mapping = aes(x=Gols.Match, y=reorder(Year, Year), color=as.character(Year)),
              show.legend = FALSE, size=4, alpha=0.6, height=0.2
  ) +
  scale_color_manual(values=pal) +
  scale_x_continuous(limits=c(0,10), expand=c(0,0), breaks=c(2,4,6,8)) +
  theme_minimal() + 
  labs(title=titulo) +
  geomtextpath::geom_textvline(mapping=aes(xintercept=overall_avg, 
                                           label=paste0("Media de gols: ",round(overall_avg,0))), 
                               size=3.9, color="red", hjust=0.04, vjust=0.5, 
                               family="Open Sans") +
  geom_image(data=brasil, 
             mapping=aes(x=Gols.Cup.Avg, y=reorder(Year, Year), image=image),
             size = 0.08, asp=1.5) +
  geom_text(data=brasil, 
            mapping=aes(x=Gols.Cup.Avg, y=reorder(Year, Year), label=round(Gols.Cup.Avg, 2)),
            size=3, color="white", vjust=7, family="Open Sans") + 
  theme(
    plot.background = element_rect(fill="black", color="black"),
    plot.title = element_textbox_simple(halign=0.5, maxwidth = unit(25, "in")),
    legend.position = "none",
    panel.grid = element_blank(),
    plot.margin = margin(l=20, r=40, b=10, t=20),
    axis.title = element_blank(),
    axis.text.x = element_text(color="white"),
    axis.text.y = element_text(color="white")
  )
