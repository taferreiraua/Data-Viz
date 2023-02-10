library(tidyverse)
library(sysfonts)
library(dplyr)
library(showtext)
library(ggimage)
library(ggtext)
library(geomtextpath)
library(ggview)



# ------ Leitura dos dados

df <- read.csv("C:/Users/Thays Ferreira/Documents/Visualização de dados/Brasil WorldCup/Data/WorldCupMatches.csv") # dados de 1930-2014
df2018 <- read.csv("C:/Users/Thays Ferreira/Documents/Visualização de dados/Brasil WorldCup/Data/FIFA - 2018.csv") # dados de 2018
df2022 <- read.csv("C:/Users/Thays Ferreira/Documents/Visualização de dados/Brasil WorldCup/Data/FIFA - 2022.csv") # dados de 2022


# ------ Aesthetic

sysfonts::font_add_google("Literata", "Literata")
sysfonts::font_add_google("Open Sans","opensans")
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)

# titulo
titulo <- paste0(
  "<span style='font-family:Literata;font-size:50pt;color:#DFE667;'><br> **QUANTOS GOLS FAZ A SELEÇÃO BRASILEIRA?** <br></span>",
  "<span style='font-family:Literata;font-size:24pt;color:white;'><br> O Brasil é o país com mais gols na história das Copas do Mundo. Única seleção a disputar todas as edições, a equipe<br> comandada por Tite marcou oito gols na edição do Qatar e chegou a 237 gols na história da competição. </span>"
    )

caption <- paste0(
  "<span style='font-family:fb;color:#DFE667;'>&#xf099;</span>",
  "<span style='font-family:opensans;color:white;'> @taferreiraua |</span>",
  "<span style='font-family:fb;color:#DFE667;'>&#xf09b;</span>",
  "<span style='font-family:opensans;color:white;'> taferreiraua </span>",
  "<span style='font-family:opensans;color:white;'>| Dados: FIFA World Cup Archive </span>")


# cores
col_bg <- '#213502'
col_line <- '#7ea310'
col_years <- 'gold'
col_avg_line <- 'white'
col_min <- '#d94b5b'
col_max <- "#54c2cc"
  

# ------ Manipulação de dados

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

# adicionando informa??es sobre os anos de 2018 e 2022
brasil = add_row(brasil, Year=c(df18$Year, df22$Year), 
            Gols.Avg=c(df18$Gols.Avg, df22$Gols.Avg),
            Games.Played=c(df18$Games.Played, df22$Games.Played),
            Total.Num.Gols.Cup=c(df18$Goals.For, df22$Goals.For))

# media de gols
Total.Gols.Avg = sum(brasil$Total.Num.Gols.Cup)/sum(brasil$Games.Played)

# Media de gols por titulo brasileiro
brasil_tit_gols_avg <- brasil %>% 
  filter(Year==1958|Year==1962|Year==1970|Year==1994|Year==2002) %>%
  select(Year, Gols.Avg) %>%
  distinct(Year, Gols.Avg)


## Mapeamento dos itens

anos = brasil_tit_gols_avg$Year
gols = brasil_tit_gols_avg$Gols.Avg
imagens = paste0("C:/Users/Thays Ferreira/Documents/Visualização de dados/Brasil WorldCup/Images/", anos, ".png")

map_cup <- data.frame(x=anos, y=gols + c(1.8, -2, 2, -1.8, 2), imagem=imagens)

map_years <- data.frame(x=anos, y=gols + c(2.65, -2.85, 2.86, -2.65, 2.85))

map_avg <- data.frame(x=anos + c(2, 1, 2, 2, 2), y=gols + c(0.2, 0.22, 0.22, -0.2, 0.22), label=round(gols, 2))

map_stars <- data.frame(x=anos, y=gols + c(3, -3.23, 3.23, -3, 3.23), 
                        imagem=c(rep("C:/Users/Thays Ferreira/Documents/Visualização de dados/Brasil WorldCup/Images/star.png", 5)))

map_segm <- data.frame(x=anos, xend=anos, y=gols, yend=gols + c(1.2, -1.4, 1.3, -1.2, 1.4))

map_point <- data.frame(x=anos, y=gols)

# Maximos e Minimos - Media de gols
max_gols <- brasil %>% group_by(Year) %>% 
  select(Year, Gols.Avg) %>%
  arrange(desc(Gols.Avg)) %>% 
  head(1)

min_gols <- brasil %>% group_by(Year) %>% 
  select(Year, Gols.Avg) %>%
  arrange(Gols.Avg) %>% 
  head(1)
  

## plot
ggplot(brasil) +
  geom_segment(mapping = aes(x=min(Year), xend=max(Year), y=Total.Gols.Avg, 
                             yend=Total.Gols.Avg), color=col_avg_line, linewidth=0.7) +
  geom_line(aes(Year, Gols.Avg), color=col_line, linewidth=1.4) +
  geom_point(data=map_point, mapping=aes(x=x, y=y), color=col_years, size=3.5) +
  geom_point(data=max_gols, mapping=aes(x=Year, y=Gols.Avg), color=col_max, size=3.5) +
  geom_point(data=min_gols, mapping=aes(x=Year, y=Gols.Avg), color=col_min, size=3.5) +
  geom_text(mapping=aes(x=1986, y=5.5, label=str_wrap("Média de gols por partida em copas do mundo conquistadas",25)),
            size=7.5, color=col_avg_line, lineheight=.5) +
  geom_text(label=paste0("Média geral: ", round(Total.Gols.Avg,2)), mapping = aes(x=1951, y=Total.Gols.Avg-0.4), 
            size=7.5, color=col_avg_line, fontface = "bold") +
  geom_text(data=map_years, mapping=aes(x=x, y=y, label=x), color=col_years, size=8, fontface = "bold") +
  geom_text(data=map_avg, mapping=aes(x=x, y=y, label=label), color=col_years, size=7.5, fontface = "bold") +
  geom_text(data=max_gols, mapping=aes(x=Year, y=Gols.Avg+0.3, label=round(Gols.Avg,2)), 
            color=col_max, size=8, fontface = "bold") +
  geom_text(data=min_gols, mapping=aes(x=Year, y=Gols.Avg-0.3, label=round(Gols.Avg,2)), 
            color=col_min, size=8, fontface = "bold") +
  geom_text(data=max_gols, mapping = aes(x=Year-10, y=Gols.Avg-0.3, label="Maior média de gols"), 
            color=col_max, size=7.5) +
  geom_text(data=min_gols, mapping = aes(x=Year+10, y=Gols.Avg-0.3, label="Menor média de gols"), 
            color=col_min, size=7.5) +
  geom_text(data=max_gols, mapping=aes(x=Year-10, y=Gols.Avg, label=Year), color=col_max, size=8, fontface = "bold") +
  geom_text(data=min_gols, mapping=aes(x=Year+10, y=Gols.Avg, label=Year), color=col_min, size=8, fontface = "bold") +
  geom_segment(data=max_gols, mapping=aes(x=Year, xend=Year-8.2,
            y=Gols.Avg, yend=Gols.Avg), color=col_max, linetype="dotted", linewidth=0.59) +
  geom_segment(data=min_gols, mapping=aes(x=Year, xend=Year+8.2,
            y=Gols.Avg, yend=Gols.Avg), color=col_min, linetype="dotted", linewidth=0.59) +
  geom_curve(mapping=aes(x=1940, xend=1944, y=Total.Gols.Avg-0.15, yend=Total.Gols.Avg-0.4), 
             color=col_avg_line, curvature=0.2, linewidth=0.3, arrow=arrow(length=unit(0.08,"in"))) +
  geom_curve(mapping=aes(x=1976, xend=1972, y=5.5, yend=4.8),
             color=col_avg_line, curvature=0.3, linewidth=0.3, arrow=arrow(length=unit(0.08,"in"))) +
  scale_y_continuous(limits=c(-1.5,8)) +
  geom_image(data=map_stars, mapping=aes(x=x, y=y, image=imagem), size=0.025, asp=1.5) +
  geom_image(data=map_cup, mapping=aes(x=x, y=y, image=imagem), size=0.085, asp=1.5, color=col_years) +
  geom_image(data=map_cup, mapping=aes(x=x, y=y, image=imagem), size = 0.08, asp=1.5) +
  geom_segment(data=map_segm, mapping=aes(x=x, xend=xend, y=y, yend=yend), color=col_years, linetype="dotted", linewidth=0.61) +
  theme_minimal() +
  labs(title=titulo, caption=caption) +
  theme(plot.background = element_rect(fill=col_bg, color=col_bg),
        plot.title = element_markdown(hjust=.5, lineheight = .5),
        plot.caption = element_markdown(size=15),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.title = element_blank(),
        axis.text = element_blank())
  

ggview(units="px", height=2150, width=3000)
ggsave(units="px", height=2150, width=3000, "C:/Users/Thays Ferreira/Documents/Visualização de dados/Brasil WorldCup/Brasil-WorldCup.png")
