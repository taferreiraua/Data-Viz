library(tidyverse) 
library(ggtext) 
library(ggview)
library(cowplot)


# dados
df = read.csv('https://raw.githubusercontent.com/taferreiraua/Estudos-de-Exploracao-e-Visualizacao-de-Dados/main/Brasil-ONS/Dados/energy_demand_hourly_brazil.csv')

# fontes e elementos de texto
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Spectral","spectral")
sysfonts::font_add_google("Open Sans","opensans")
sysfonts::font_add_google("Quicksand","quicksand")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)

title.plot = paste0(
  "<span style='font-family:spectral;font-size:112pt;colour:#212529;'><br>2022</span>",
  "<span style='font-family:quicksand;font-size:40pt;colour:#343A40;'><br>O ANO COM A MAIOR DEMANDA DE<br><span>",
  "<span style='font-family:quicksand;font-size:40pt;colour:#343A40;'> ENERGIA ELÉTRICA NO BRASIL EM 23 ANOS<br></span>"
)

subtitle.plot = paste0(
  "<span style='font-family:spectral;font-size:23pt;colour:#212529;'>Dados: Operador Nacional do Sistema Elétrico</span>"
)

title = paste0(
  "<span style='font-family:spectral;font-size:18pt;colour:#343A40;'><br> **Demanda por dia no ano de 2022** <br></span>",
  "<span style='font-family:quicksand;font-size:13pt;colour:#343A40;'>Nota-se que datas comemorativas como<br></span>",
  "<span style='font-family:quicksand;font-size:13pt;colour:#343A40;'>Natal(DEZ/25) e Ano Novo(JAN/01) apresentam<br></span>",
  "<span style='font-family:quicksand;font-size:13pt;colour:#343A40;'>menor demanda diária. Também, os meses de<br></span>",
  "<span style='font-family:quicksand;font-size:13pt;colour:#343A40;'>Janeiro, Fevereiro e Março possuem maior índice<br></span>",
  "<span style='font-family:quicksand;font-size:13pt;colour:#343A40;'>de demanda de energia elétrica.<br></span>"
)

caption = paste0(
  "<span style='font-family:fb;color:#B22222;'>&#xf099;</span>",
  "<span style='font-family:opensans;'> @taferreiraua |</span>",
  "<span style='font-family:fb;color:#B22222;'>&#xf09b; </span>",
  "<span style='font-family:opensans;'> taferreiraua </span>")


# cores
col_bg = '#E5E5E5'
col_line = '#118AB2'
col_low = '#F7CE38'
col_high = '#FD210D'


# manipulação de dados
colnames(df) = c('data', 'demanda')

# subplot 1 - Demanda por ano
brasil = df |>
  mutate(data = as.Date(data),
         ano = format(data, '%Y')) |>
  filter(ano != '2023') |>
  group_by(ano) |>
  mutate(Demanda = sum(demanda)) |>
  ungroup() |>
  distinct(ano, Demanda)

# subplot 2 - Demanda 2022
brasil2022 = df |>
  filter(grepl("2022", data)) |>
  mutate(data = as.Date(data),
         mes = as.numeric(format(data, '%m')),
         mes_label = toupper(format(data, "%b", lang="pt_BR")),
         dia = as.numeric(format(data,"%d"))) |>
  group_by(mes, dia) |>
  mutate(valor = sum(demanda)) |>
  ungroup() |>
  select(mes, mes_label, dia, valor) 

# plots

# Demanda por ano
ggplot(brasil) +
  geom_bar(mapping=aes(x=ano, y=Demanda), alpha=.15, stat='identity', width=.7) +
  geom_line(mapping=aes(x=ano, y=Demanda, group=1), colour=col_line, linewidth=2.2) +
  scale_y_continuous(breaks = c(150000000, 300000000, 450000000, 600000000),
                     labels = c('150mi', '300mi', '450mi', '600mi')) +
  theme_classic() +
  theme(
    plot.background = element_rect(fill=col_bg, color=col_bg),
    panel.grid = element_blank(),
    panel.background = element_rect(fill=col_bg, color=col_bg),
    axis.text.x = element_text(family='quicksand', size=45, angle=90),
    axis.text.y = element_text(family='quicksand', size=45),
    axis.title.y = element_text(family='quicksand', size=50),
    axis.title.x = element_blank()
  )

ggview(units='px', width=3500, height=1500)
ggsave(filename = 'C:/Users/Thays Ferreira/Documents/Visualização de dados/Brasil ONS/Images/Brasil-Demanda-Ano.png',
       units='px', width=3500, height=1500)


# Demanda 2022
ggplot(brasil2022) +
  geom_tile(aes(y=dia, x=mes, fill=valor), color=col_bg, linewidth=.5) +
  coord_fixed() +
  scale_y_reverse(breaks=c(1:31)) +
  scale_x_continuous(sec.axis = dup_axis(), breaks = 1:12, labels=unique(brasil2022$mes_label)) +
  scale_fill_gradient(low=col_low, high=col_high, labels=scales::label_comma()) +
  theme_minimal() +
  labs(title=title, fill='Demanda') +
  theme(
    plot.background = element_rect(fill=col_bg, color=col_bg),
    plot.title = element_markdown(hjust=.5, lineheight=.6),
    plot.margin = margin(t=10, l=15, r=10, b=6),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(family='quicksand', size=11),
    legend.position = 'bottom',
    legend.margin = margin(t=-25, b=-5),
    axis.title = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(size=13, family='quicksand', margin=margin(r=-3)),
    axis.text.x.top = element_text(size=13, family='quicksand', margin=margin(b=-20))) +
  guides(fill = guide_legend(label.position = 'bottom',
                             title.vjust = .8,
                             keyheight = unit(.09, 'in'),
                             nrow=1)) 

ggview(units='px', width=1500, height=2500)
ggsave(filename = 'C:/Users/Thays Ferreira/Documents/Visualização de dados/Brasil ONS/Images/Brasil-Demanda-2022.png',
       units='px', width=1500, height=2500)


# Plot 

# "pano de fundo"
data = data.frame(x=seq(-20,20), y=seq(-20,20))

plot = ggplot(data, aes(x,y)) +
  labs(caption=caption, title=title.plot, subtitle=subtitle.plot) +
  theme_minimal() +
  theme(plot.background = element_rect(fill=col_bg, color=col_bg),
        plot.title.position = "plot",
        plot.title = element_textbox_simple(halign=.5),
        plot.subtitle = element_markdown(hjust=.5),
        plot.caption = element_markdown(hjust=1, size=22),
        axis.text = element_blank(),
        axis.title = element_blank(),
        panel.background = element_rect(fill=col_bg, color=col_bg),
        panel.grid = element_blank())

# posicionando os plots com o ggdraw
ggdraw(plot) +
  draw_image(magick::image_read('https://raw.githubusercontent.com/taferreiraua/Estudos-de-Exploracao-e-Visualizacao-de-Dados/main/Brasil-ONS/Plots/Brasil-Demanda-Ano.png'),
             x=-.011, y=.34, scale=.43) +
  draw_image(magick::image_read('https://raw.githubusercontent.com/taferreiraua/Estudos-de-Exploracao-e-Visualizacao-de-Dados/main/Brasil-ONS/Plots/Brasil-Demanda-2022.png'),
             x=-.01, y=-.093, scale=.91)

ggview(units='px', width=3500, height=7000, dpi=150)
ggsave(filename = 'C:/Users/Thays Ferreira/Documents/Visualização de dados/Brasil ONS/Brasil-ONS.png', units='px', width=3500, height=7000, dpi=150)