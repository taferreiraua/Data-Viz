library(tidyverse)
library(sysfonts)
library(showtext)
library(ggimage)
library(ggtext)
library(geomtextpath)
#remotes::install_github("idmn/ggview")
library(ggview)


# lendo os dados
df <- read.csv("~/Tesla Deaths - Deaths (3).csv")


# aesthetic
col_bars=c("#efa1a1",
           "#e97f7f",
           "#e56565",
           "#e04c4c",
           "red3",
           "#bc2020",
           "#9a1a1a"
)

sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Headland One", "headland one")
sysfonts::font_add_google("Open Sans","opensans")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)

caption <- paste0(
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style='font-family:opensans;'> @taferreiraua |</span>",
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:opensans;'> taferreiraua </span>",
  "<span style='font-family:opensans;'>| Dados: tesladeaths.com </span>")


# manipulação de dados
tesla <- df %>%
  filter(Year>=2016) %>%
  mutate(Date = as.Date(Date, format='%m/%d/%Y'),
         Date = format(as.Date(Date), "%Y-%m")) %>%
  group_by(Date) %>%
  mutate(n=n(), sum.n = sum(n)) %>%
  ungroup() %>%
  distinct(Year, Date, n, sum.n)

infos <- df %>%
  group_by(Year) %>%
  slice(which.max(Deaths)) %>%
  select(Date, Country, Description, Deaths, Model)

mortes <- df %>%
  filter(Year>=2016) %>%
  mutate(total=sum(Deaths)) %>%
  distinct(total)


# mapeando os textos
map_infos = data.frame(
  xleg = c("2016-06", "2016-09", "2017-07", "2018-03", "2018-11", "2019-05", "2020-02", "2020-08", "2021-06", "2021-10"),
  yleg = c(91, 50, 81, 36, 120, 80, 150, 60, 127, 192),
  legend = c("Elon Musk afirma que Teslas ser?o capazes de dirigir melhor que humanos dentro de 2 a 3 anos.",
            "Joshua Brown morre enquanto usava o piloto autom?tico depois que seu Modelo S bateu na lateral de um trailer que atravessava uma estrada perto de Williston, Fl?rida.",
            "Tesla ignora recomenda??o do NTSB de melhorar o monitoramento de motoristas enquanto eles est?o usando os sistemas.",
            "Walter Huang, um funcion?rio da Apple, morre quando seu Model X bate em uma barreira em Mountain View, Calif?rnia, enquanto o piloto autom?tico est? em uso.",
            "Elon Musk deixa o cargo de presidente da Tesla.",
            "NTSB novamente critica o piloto autom?tico, dizendo que o design do Tesla torna muito f?cil para os motoristas se desligarem mentalmente da tarefa de dirigir.",
            "A Consumer Reports testa o recurso Smart Summon da Tesla, que a montadora afirma poder ?convocar? um ve?culo Tesla para dirigir sozinho em um estacionamento sem nenhum ocupante dentro do ve?culo. Descobrimos que ele tem dificuldade em estacionar, cruza as linhas da pista e vagueia erraticamente ?como um motorista b?bado ou distra?do?.",
            "David e Sheila Brown, que estavam casados h? 52 anos, s?o mortos em Saratoga, Calif?rnia, depois que seu Tesla saiu de uma rodovia. Documentos judiciais mostram que o piloto autom?tico estava ativo no momento do acidente.",
            "A NHTSA abre uma investiga??o preliminar de defeito de seguran?a no piloto autom?tico. Membros do senado norte-americano pedem que a Federal Trade Commission investigue o que eles chamam de pr?ticas de marketing potencialmente enganosas da Tesla em torno do Autopilot e do FSD, incluindo o uso da frase ?full self-conduction? para um recurso que n?o torna um ve?culo totalmente aut?nomo.",
            "S?o registrados 196 vitimas fatais de acidentes envolvendo Teslas em apenas um m?s no mundo todo."),
  xyea = c("2016-06", "2016-09", "2017-07", "2018-03", "2018-11", "2019-05", "2020-02", "2020-08", "2021-06", "2021-10"),
  yyea = c(79, 33, 69.6, 18.3, 113.7, 64.7, 120, 38.7, 89.45, 180.5),
  years = c("Janeiro, 2016", "Maio, 2016", "Setembro, 2017", "Mar?o, 2018", "Setembro, 2018", 
            "Agosto, 2019", "Outubro, 2019", "Agosto, 2020", "Agosto, 2021", "Maio, 2022")
  )

map_segm <- data.frame(
  x=c("2016-01", "2016-01", "2016-05", "2016-05", "2016-09", "2017-09", "2017-09", "2017-06", 
      "2018-03", "2018-09", "2018-09", "2018-11", "2019-08", "2019-08", "2019-05", "2019-10",
      "2019-10", "2020-08", "2021-08", "2021-08", "2021-06", "2022-04", "2021-10"),
  xend=c("2016-01", "2016-02", "2016-05", "2016-09", "2016-09", "2017-09", "2017-06", "2017-06",
      "2018-03", "2018-09", "2018-11", "2018-11", "2019-08", "2019-05", "2019-05", "2019-10",
      "2019-11", "2020-08", "2021-08", "2021-06", "2021-06", "2021-10", "2021-10"),
  y=c(2, 79, 2, 16, 16, 2, 12, 12, 10, 5, 16, 16, 5, 36, 36, 10, 120, 17, 17, 75, 75,
      168.5, 168.5),
  yend=c(79, 79, 16, 16, 29, 12, 12, 65, 15, 16, 16, 109.9, 36, 36, 61, 120, 120, 35, 
      75, 75, 86, 168.5, 176.5)
)



# plot 
ggplot(tesla) +
  geom_col(aes(Date, sum.n, fill=as.character(Year))) +
  scale_y_reverse(limits=c(200,-5), breaks=c(1,4,9,16,25,36,81,100,196)) +
  scale_fill_manual(values=col_bars) +
  geom_text(mapping=aes(x="2017-09", y=166, 
                        label=str_wrap("Morte no piloto automático", width=25)), 
    color="white", family="headland one", fontface="bold", size=20.8, lineheight = .5) +
  geom_text(mapping=aes(x="2017-09", y=188, 
                        label=str_wrap("Acidentes fatais envolvendo veículos autônomos da Tesla.", width=65)), 
            color="grey", family="headland one", size=6.8, lineheight = .5) +
  geom_text(data=map_infos, mapping=aes(x=xleg, y=yleg, label=str_wrap(legend, width=25)),
            color="white", family="opensans", size=4.5, lineheight = .5) +
  geom_text(data=map_infos, mapping=aes(x=xyea, y=yyea, label=years), fontface="bold",
            color="white", family="opensans", size=4.55, lineheight = .5) +
  geom_segment(data=map_segm, mapping = aes(x=x, xend=xend, y=y, yend=yend), color="white") +
  geom_textsegment(inherit.aes = FALSE,
                   data = map_years,
                   mapping=aes(x=start, xend=end, y=-5, yend=-5, label=label), 
                   family = "headland one", fontface="bold", color="white", size=5.9, 
                   linecolor="white", vjust=0.5) +
  theme_minimal() +
  labs(caption=caption) +
  theme(plot.background = element_rect(fill=col_background, color=col_background),
        plot.caption = element_markdown(size = 17, color="white"),
        legend.position = "none",
        panel.grid = element_blank(),
        axis.text.y = element_text(size=14, color="grey49"),
        axis.text.x = element_blank()
        )


  ggview(units="px", height=2100, width=3000)
  ggsave("Tesla-Deaths.png", width=11, height=7)
  
