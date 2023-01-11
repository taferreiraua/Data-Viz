library(tidyverse)
library(sysfonts)
library(dplyr)
library(showtext)
library(ggimage)
library(ggtext)
library(geomtextpath)



# ------ Leitura dos dados

df <- read.csv("~/WorldCupMatches.csv") # dados de 1930-2014
df2018 <- read.csv("~/FIFA - 2018.csv") # dados de 2018
df2022 <- read.csv("~/FIFA - 2022.csv") # dados de 2022


# ------ Aesthetic - por algum motivo as fontes não funcionaram :(

sysfonts::font_add_google("Literata", "Literata")
sysfonts::font_add_google("Open Sans","opensans")
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)

# titulo
titulo <- paste0(
  "<span style='font-family:Literata;font-size:40.5pt;color:white;'> **QUANTOS GOLS FAZ A SELEÇÃO BRASILEIRA?** </span>",
  "<br><span style='font-family:Literata;font-size:30.6pt;color:grey;'> Média de gols da seleção brasileira por copa do mundo. </span></br>"
)

caption <- paste0(
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style='font-family:opensans;'> @taferreiraua |</span>",
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:opensans;'> taferreiraua </span>",
  "<span style='font-family:opensans;'>| Dados: FIFA World Cup Archive </span>")


# cores
col_background <- "black"
col_line <- "springgreen4"
col_years <- "gold"
col_avg_line <- "white"
col_min <- "red"
col_max <- "#49bce2"
  

# ------ Manipulação de dados

# Media de gols por titulo brasileiro
brasil_tit_gols_avg <- brasil %>% 
  filter(Year==1958|Year==1962|Year==1970|Year==1994|Year==2002) %>%
  select(Year, Gols.Avg) %>%
  distinct(Year, Gols.Avg)

# guardando os valores em diferentes variaveis pra facilitar o mapeamento das imagens
m58 <- 3.67
m62 <- 3
m70 <- 4.33
m94 <- 1.86
m02 <- 3.57

# Numero de gols por partida, media de gols por ano, numero de jogos e total de gols por copa
brasil = df %>% 
  filter(Away.Team.Name=="Brazil"|Home.Team.Name=="Brazil") %>%
  mutate(Num.Gols = case_when(Away.Team.Name=="Brazil"~Away.Team.Goals+Half.time.Away.Goals,
                              Home.Team.Name=="Brazil"~Home.Team.Goals+Half.time.Home.Goals)) %>%
  group_by(Year) %>%
  mutate(Gols.Avg = mean(Num.Gols), 
         Games.Played=n(),
         Total.Num.Gols.Cup=sum(Num.Gols)) %>%
  ungroup()

# info de 2018
df18 <- df2018 %>%
  filter(Team=="Brazil") %>%
  mutate(Gols.Avg = Goals.For/Games.Played,
         Year = 2018) %>%
  select(Year, Gols.Avg, Goals.For, Games.Played)

# info de 2019
df22 <- df2022 %>%
  filter(Team=="Brazil") %>%
  mutate(Gols.Avg = Goals.For/Games.Played,
         Year = 2022) %>%
  select(Year, Gols.Avg, Goals.For, Games.Played)

# adicionando informações sobre os anos de 2018 e 2022
brasil = add_row(brasil, Year=c(df18$Year, df22$Year), 
            Gols.Avg=c(df18$Gols.Avg, df22$Gols.Avg),
            Games.Played=c(df18$Games.Played, df22$Games.Played),
            Total.Num.Gols.Cup=c(df18$Goals.For, df22$Goals.For))

# media de gols
Total.Gols.Avg = sum(brasil$Total.Num.Gols.Cup)/sum(brasil$Games.Played)



# ------ plot

## Mapeamento dos itens
map_cup <- data.frame(x=c(1958, 1962, 1970, 1994, 2002),
                  y=c(m58+1.8, m62-2, m70+2, m94-1.8, m02+2),
                  imagem=c("~/1958.png", "~/1962.png",
                           "~/1970.png", "~/1994.png",
                           "~/2002.png"))

map_years <- data.frame(x=c(1958, 1962, 1970, 1994, 2002),
                        y=c(m58+2.65, m62-2.85, m70+2.86,
                            m94-2.65, m02+2.85))

map_avg <- data.frame(x=c(1960, 1963, 1972, 1996, 2004),
                      y=c(m58+0.2, m62+0.22, m70+0.22,
                          m94-0.2, m02+0.22))

map_stars <- data.frame(x=c(1958, 1962, 1970, 1994, 2002),
                        y=c(m58+3, m62-3.23, m70+3.23,
                            m94-3, m02+3.23),
                        imagem=c("~/star.png",
                                 "~/star.png",
                                 "~/star.png",
                                 "~/star.png",
                                 "~/star.png"))

map_segm <- data.frame(x=c(1958, 1962, 1970, 1994, 2002),
                       xend=c(1958, 1962, 1970, 1994, 2002),
                       y=c(m58, m62, m70, m94, m02),
                       yend=c(m58+1.2, m62-1.4, m70+1.3,
                              m94-1.2, m02+1.4))

map_point <- data.frame(x=c(1958, 1962, 1970, 1994, 2002),
                         y=c(m58, m62, m70, m94, m02))

# Maximos e Minimos - Media de gols
max_gols <- brasil %>% group_by(Year) %>% 
  select(Year, Gols.Avg) %>%
  arrange(desc(Gols.Avg)) %>% 
  head(1)

min_gols <- brasil %>% group_by(Year) %>% 
  select(Year, Gols.Avg) %>%
  arrange(Gols.Avg) %>% 
  head(1)
  
## grafico
ggplot(brasil) +
  geom_line(aes(Year, Gols.Avg), color=col_line, linewidth=0.97) +
  geom_point(data=map_point, mapping=aes(x=x, y=y), color=col_line, size=2.5) +
  geom_point(data=max_gols, mapping=aes(x=Year, y=Gols.Avg), color=col_max, size=2.5) +
  geom_point(data=min_gols, mapping=aes(x=Year, y=Gols.Avg), color=col_min, size=2.5) +
  geom_text(data=max_gols, mapping=aes(x=Year, y=Gols.Avg+0.3, 
            label=round(Gols.Avg,2)), color=col_max, size=7.5) +
  geom_text(data=min_gols, mapping=aes(x=Year, y=Gols.Avg-0.3, 
            label=round(Gols.Avg,2)), color=col_min, size=7.5) +
  geom_text(data=max_gols, mapping = aes(x=Year-10, y=Gols.Avg-0.3,
            label="Maior média de gols"), color=col_avg_line, size=7.5) +
  geom_text(data=min_gols, mapping = aes(x=Year+10, y=Gols.Avg-0.3,
            label="Menor média de gols"), color=col_avg_line, size=7.5) +
  geom_text(data=max_gols, mapping=aes(x=Year-10, y=Gols.Avg,
            label=Year), color=col_max, size=8) +
  geom_text(data=min_gols, mapping=aes(x=Year+10, y=Gols.Avg,
            label=Year), color=col_min, size=8) +
  geom_segment(data=max_gols, mapping=aes(x=Year, xend=Year-8.2,
            y=Gols.Avg, yend=Gols.Avg), color=col_max, linetype="dotted", linewidth=0.59) +
  geom_segment(data=min_gols, mapping=aes(x=Year, xend=Year+8.2,
            y=Gols.Avg, yend=Gols.Avg), color=col_min, linetype="dotted", linewidth=0.59) +
  geom_segment(mapping = aes(x=min(Year), xend=max(Year), y=Total.Gols.Avg, 
                             yend=Total.Gols.Avg), color=col_avg_line, linewidth=0.61) +
  geom_curve(mapping=aes(x=1940, xend=1943, y=Total.Gols.Avg-0.05, 
                         yend=Total.Gols.Avg-0.3), color="white", curvature=0.2, 
             linewidth=0.3, arrow=arrow(length=unit(0.08,"in"))) +
  scale_y_continuous(limits=c(-1.5,8)) +
  geom_text(label=paste0("Media geral: ", round(Total.Gols.Avg,2)),
            mapping = aes(x=1949, y=Total.Gols.Avg-0.3), 
            size=7.5, color=col_avg_line) +
  geom_image(data=map_cup, mapping=aes(x=x, y=y, image=imagem), 
             size = 0.08, asp=1.5) +
  geom_image(data=map_stars, mapping=aes(x=x, y=y, image=imagem), 
             size=0.025, asp=1.5) +
  geom_text(data=map_years, mapping=aes(x=x, y=y), 
            label=c("1958", "1962", "1970", "1994", "2002"), 
            color=col_years, size=8) +
  geom_text(data=map_avg, mapping=aes(x=x, y=y), 
            label=c(m58, m62, m70, m94, m02), 
            color=col_avg_line, size=7.5) +
  geom_segment(data=map_segm, mapping=aes(x=x, xend=xend, y=y, yend=yend), 
               color=col_line, linetype="dotted", linewidth=0.59) +
  theme_minimal() +
  labs(title=titulo, caption=caption) +
  theme(plot.background = element_rect(fill=col_background, color=col_background),
        plot.title = element_textbox_simple(halign=0.5, maxwidth = unit(7, "in")),
        plot.caption = element_textbox_simple(halign=0.97, 
                                              hjust=1, 
                                              color="white", 
                                              size=17, 
                                              margin=margin(t=8)),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.text = element_blank())

  ggsave("copa.png", width=11, height=7)
