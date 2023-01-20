library(tidyverse)
library(ggview)
library(sysfonts)
library(lubridate)
library(ggtext)



# dados
df <- readRDS("~/planecrashinfo_accident_data.rds")


# fonte e texto
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Oranienbaum", "oranienbaum")
sysfonts::font_add_google("Open Sans","opensans")
showtext::showtext_opts(dpi=150)
showtext::showtext_auto()


caption <- paste0(
  "<span style='font-family:fb;'>&#xf099;</span>",
  "<span style='font-family:opensans;'> @taferreiraua |</span>",
  "<span style='font-family:fb;'>&#xf09b;</span>",
  "<span style='font-family:opensans;'> taferreiraua </span>",
  "<span style='font-family:opensans;'>| Dados: planecrashinfo.com </span>")



## Manipula??o de dados

# transformando a variavel fatalidades em numerico
count=1
for(str in df$fatalities){
  df$fatalities[count] = str_split_i(str, " ", 1)
  count = count + 1
}

lct <- Sys.getlocale("LC_TIME"); Sys.setlocale("LC_TIME", "C")

nepal = df |>
  filter(grepl("Nepal", location) | grepl("nepal", location) | grepl("Napal", location) | grepl("Near Lete Pass", location)) |>
  mutate(fatalities = as.numeric(fatalities),
         date = strptime(date, format="%B %d, %Y"), 
         year = format(date, "%Y"),
         day = format(date, "%d"),
         month = format(date, "%m", lang="pt_BR"),
         month = month(as.numeric(month), 
                       label = T,
                       locale = "pt_BR"),
         year = as.numeric(year),
         date.label = paste0(day, '. ', month, '. ', year)) |>
  filter(year>2001)

# adicionando informa??es sobre o acidente de janeiro de 2023 (assumindo que o numero de vitimas atual ? 71)
nepal = add_row(nepal, 
                date=as.POSIXct("2023-01-15"),
                date.label="15. jan. 2023",
                fatalities = 71,
                location = "Pokhara, Nepal",
                year=2023)


num.total.vit = sum(nepal$fatalities)

# mapeando os elementos de texto
map_labels = data.frame(
  label=c(
    "Quatro pessoas foram consideradas mortas depois que um avi?o bimotor caiu em uma montanha no oeste do Nepal minutos antes de pousar.",
    "Um avi?o de passageiros Twin Otter, transportando turistas estrangeiros, colidiu com uma montanha devido ao mau tempo no Nepal , matando todas as 18 pessoas a bordo. Era operado pela Shangrila Air.",
    "Um avi?o de carga Twin Otter caiu na regi?o do Monte Everest, matando seus tr?s tripulantes. Era operado pela Yeti Airlines.",
    "Um avi?o de passageiros Twin Otter operado pela Yeti Airlines caiu minutos antes de pousar no oeste do pa?s, matando todas as nove pessoas a bordo.",
    "Quatro monitores de armas da ONU estavam entre as pelo menos 10 pessoas mortas em um acidente de helic?ptero no Nepal.",
    "Um pequeno avi?o Twin Otter caiu nas remotas montanhas do nordeste do Nepal, matando pelo menos 18 pessoas, a maioria estrangeiros.",
    "14 pessoas morreram quando seu pequeno avi?o caiu devido ao mau tempo no Nepal. Era operado pela empresa privada Agni Air.",
    "Um pequeno avi?o caiu no sop? do Himalaia, no remoto leste do Nepal , matando todas as 22 pessoas a bordo. A aeronave Twin Otter era operada pela Tara Air.",
    "Um pequeno avi?o que transportava turistas estrangeiros para ver o Monte Everest caiu devido ao mau tempo perto de Katmandu, matando todas as 19 pessoas a bordo. A aeronave Beech era operada pela companhia a?rea privada Buddha Air.",
    "Um pequeno avi?o com 21 pessoas a bordo caiu, matando 15 pessoas, incluindo o piloto e o co-piloto. O acidente, envolvendo uma aeronave Dornier 228 operada pela Agni Air, ocorreu perto do Aeroporto Jomsom, cerca de 125 milhas a noroeste de Kathmandu.",
    "Um pequeno avi?o Dornier movido a h?lice atingiu um p?ssaro e caiu logo ap?s a decolagem de Katmandu, matando 19 pessoas.",
    "Todas as 18 pessoas em um pequeno avi?o que caiu devido ao mau tempo morreram. A aeronave Twin Otter era operada pela estatal Nepal Airlines Corp.",
    "Um pequeno avi?o caiu devido ao mau tempo, matando todas as 23 pessoas a bordo. A aeronave Twin Otter, operada pela Tara Air, estava em um voo de Pokhara.",
    "Duas pessoas foram consideradas mortas depois que um pequeno avi?o caiu no distrito de Kalikot, no oeste do Nepal. Era operado pela Kasthamandap Airlines.",
    "Um Let L-410 Turbolet operando como Summit Air Flight 409 caiu perto da pista enquanto tentava pousar no Aeroporto Tenzing-Hillary, no Nepal. O capit?o e o copiloto morreram em decorr?ncia do acidente.",
    "51 das 71 pessoas em um avi?o de Bangladesh operado pela US-Bangla Airlines morreram quando ele caiu em tempo nublado ao pousar no aeroporto da capital do Nepal.",
    "Dezesseis nepalenses, quatro indianos e dois alem?es morreram em uma aeronave De Havilland Canada DHC-6-300 Twin Otter que caiu 15 minutos depois de decolar de Pokhara, 125 km a oeste de Katmandu.",
    "Uma aeronave bimotor ATR 72 transportando 72 pessoas, operada pela Yeti Airlines do Nepal, caiu em Pokhara, matando pelo menos 71."
  ),
  x=nepal$date.label,
  y=c(21, 43, 21, 38, 23, 42, 27.3, 44, 67, 35, 61, 35, 59, 23.5, 48, 86, 60, 84)
)

map_segm = data.frame(
  x=nepal$date.label,
  xend=nepal$date.label,
  y=nepal$fatalities+1.5,
  yend=c(14, 31, 13, 29, 16, 34, 20.5, 35.3, 54.5,
         21.8, 54, 27, 50, 14.8, 36, 76.6, 49, 77)
)



## Plot

ggplot(nepal, aes(as.factor(reorder(date.label, date)), fatalities)) + 
  geom_area(aes(group=1), fill="#d55c5a", alpha=0.95) +
  geom_line(aes(group=1), color="#894242", linewidth=1.9) +
  geom_point(color = "#894242", size=4.9) +
  scale_y_reverse(limits=c(100,-15)) +
  geom_text(mapping=aes(x="25. set. 2011", y=-12.5, label="Uma timeline dos acidentes a?reos no Nepal"),
            family="oranienbaum", fontface="bold", size=61) +
  geom_text(mapping=aes(x=date.label, y=-2.5, label=date.label), 
            family="oranienbaum", lineheight = .5, size=17) +
  geom_segment(data=map_segm, mapping=aes(x=x, xend=xend, y=y, yend=yend), 
            linetype="dashed", linewidth=0.9) +
  geom_text(data=map_labels, mapping=aes(x=x, y=y, label=str_wrap(label, width=20)),
            family="opensans", lineheight = .5, size=12) +
  geom_text(mapping=aes(x="21. jun. 2006", y=82, 
    label=str_wrap(paste0("Ao longo de 20 anos, ", num.total.vit, " pessoas perderam a vida devido a acidentes a?reos no Nepal."), 
                   width = 50)), size=20, family="opensans", lineheight = .5, color="gray26") +
  theme_minimal() +
  labs(caption=caption) +
  theme(
    plot.background = element_rect(fill="#edf2f4", color="#edf2f4"),
    plot.caption = element_markdown(size = 40.5, color="gray18"),
    legend.position = "none",
    panel.grid = element_blank(),
    axis.text = element_blank(),
    axis.title = element_blank(),
    plot.margin = margin(l=50, r=70, b=10, t=20)
  )

ggview(units="px", height=6050, width=12000)
ggsave("Nepal-Airplane-Crashes.png", height=6050, width=12000, units="px")
