# packages
pacman::p_load(tidyverse,
               ggtext,
               ggimage,
               ggview)





# dados
df = read.csv('https://raw.githubusercontent.com/taferreiraua/Data-Viz/main/Brasil-WorldCup/Dados/WorldCupMatches.csv') # dados de 1930-2014
df2018 = read.csv('https://raw.githubusercontent.com/taferreiraua/Data-Viz/main/Brasil-WorldCup/Dados/FIFA%20-%202018.csv') # dados de 2018
df2022 = read.csv('https://raw.githubusercontent.com/taferreiraua/Data-Viz/main/Brasil-WorldCup/Dados/FIFA%20-%202022.csv') # dados de 2022




# texto
sysfonts::font_add_google("Raleway", "raleway")
sysfonts::font_add_google("Open Sans","opensans")
sysfonts::font_add('aw6', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)

title = "<span style='font-family:raleway;font-size:55pt;color:#DFE667;'><br><i>De **gol** em **gol** ..."
  
subtitle = "<span style='font-family:opensans;font-size:25pt;color:white;'>Número de gols feitos pela seleção brasileira por edição da Copa do Mundo."

caption = paste0(
  "<span style='font-family:opensans;color:white;font-size:19pt;'>**Dados:** _FIFA World Cup Archive_<br></span>",
  "<span style='font-family:aw6;color:#DFE667;font-size:15pt;'>&#xf099;</span>",
  "<span style='font-family:opensans;color:white;font-size:17pt;'> @taferreiraua </span>",
  "<span style='font-family:aw6;color:#DFE667;font-size:15pt;'>&#xf09b;</span>",
  "<span style='font-family:opensans;color:white;font-size:17pt;'> taferreiraua")

  


# manipulação de dados
brasil = df |>
  select(Year, Home.Team.Name, Away.Team.Name, Home.Team.Goals, Away.Team.Goals) |>
  filter(Home.Team.Name=='Brazil' | Away.Team.Name=='Brazil') |>
  mutate(Goals = case_when(Home.Team.Name=='Brazil' ~ Home.Team.Goals,
                           Away.Team.Name=='Brazil' ~ Away.Team.Goals)) |>
  group_by(Year) |>
  mutate(Goals.year = sum(Goals)) |>
  ungroup() |>
  distinct(Year, Goals.year)

# adicionando informações sobre 2018 e 2022
brasil = add_row(brasil, Year = c(2018, 2022),
                         Goals.year = c(df2018 |> filter(Team=='Brazil') |> select(Goals.For) |> unlist(),
                                        df2022 |> filter(Team=='Brazil') |> select(Goals.For) |> unlist()))

# anos de cada titulo da seleção:
titulos = c(1958, 1962, 1970, 1994, 2002)

# dataset p/ mapear as linhas, texto e imagens
map_itens = data.frame(
  xyear = titulos,
  yimg = c(26.2, -3.7, 29.2, -5.7, 29),
  img = paste0('https://raw.githubusercontent.com/taferreiraua/Data-Viz/main/Brasil-WorldCup/Imagens/', 
               titulos, '.png'),
  brasil |> filter(Year %in% titulos) |> select(Goals.year),
  yseg = c(22, 0, 25, -2, 25),
  yyear = c(23, -.5, 26, -2.5, 25.8)
)





# plot
ggplot(brasil) +
  geom_segment(aes(x=1930, xend=2022, y=mean(Goals.year), yend=mean(Goals.year)),
               color='white',
               linewidth=.8) +
  geom_text(aes(x=1938, y=9.7, label=paste0('Média de gols por copa: ', round(mean(Goals.year)))),
            color='white',
            size=5,
            hjust=0) +
  geom_line(aes(x=Year, y=Goals.year),
            color='#7ea310',
            linewidth=1.6) +
  geom_segment(data = map_itens,
               aes(x=xyear, xend=xyear, y=Goals.year, yend=yseg),
               color='white',
               linetype='dotted',
               linewidth=.75) +
  geom_text(data = brasil |> filter(Year %in% titulos),
                aes(x=Year, y=Goals.year, label='★'),
                color='gold',
                size=14) +
  geom_text(data = map_itens,
            aes(x=xyear, y=yyear, label=xyear),
            color='gold',
            family='opensans',
            fontface='bold',
            size=6) +
  geom_image(data = map_itens,
             aes(x=xyear, y=yimg, image=img),
             color='gold',
             size=0.054,
             asp=1.6) +
  geom_image(data = map_itens,
             aes(x=xyear, y=yimg, image=img),
             size=0.05,
             asp=1.6) +
  scale_y_continuous(limits=c(-15,33), breaks=seq(0, 22, 2), sec.axis = dup_axis()) +
  scale_x_continuous(breaks=seq(1930, 2022, 4), sec.axis = dup_axis()) +
  labs(title=title, subtitle=subtitle, caption=caption, x='★ = _título_') +
  theme_minimal() +
  theme(plot.background = element_rect(fill='#213502', color='#213502'),
        plot.title = element_markdown(hjust=.5),
        plot.subtitle = element_markdown(hjust=.5),
        plot.caption = element_markdown(hjust=.98, lineheight=1.5),
        panel.grid = element_blank(),
        axis.title.y = element_blank(),
        axis.title.x.top = element_blank(),
        axis.title.x.bottom = element_markdown(size=28, color='gold', hjust=.1),
        axis.text.x.bottom = element_blank(),
        axis.text.x.top = element_text(size=14, color='white', margin=margin(t=15)),
        axis.text.y.left = element_text(size=14, hjust=.5, color='white'),
        axis.text.y.right = element_text(size=14, hjust=.5, color='white'))




ggview(units="px", height=2150, width=3000)
ggsave(units="px", height=2150, width=3000, "Brasil-Copa.png")
