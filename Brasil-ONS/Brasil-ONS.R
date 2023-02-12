library(tidyverse)
library(ggview)


# dados
df = read.csv('https://raw.githubusercontent.com/taferreiraua/Estudos-de-Exploracao-e-Visualizacao-de-Dados/main/Brasil-ONS/Dados/energy_demand_hourly_brazil.csv')

# fontes e elementos de texto
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add_google("Montserrat Alternates","montserrat")
sysfonts::font_add_google("Open Sans","opensans")
sysfonts::font_add_google("Quicksand","quicksand")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)

title = "<span style='font-family:montserrat;font-size:25pt;colour:#242423;'><br> Demanda de energia elétrica no Brasil em 2022 <br></span>"
caption = paste0(
  "<span style='font-family:fb;color:#B22222;'><br><br><br><br>&#xf099;</span>",
  "<span style='font-family:opensans;'> @taferreiraua |</span>",
  "<span style='font-family:fb;color:#B22222;'>&#xf09b;</span>",
  "<span style='font-family:opensans;'> taferreiraua </span>",
  "<span style='font-family:opensans;'>| Dados: Operador Nacional do Sistema Elétrico </span>")

# manipulação de dados
colnames(df) = c('data', 'demanda')

brasil = df |>
  filter(grepl("2022", data)) |>
  mutate(data = as.Date(data),
         mes = as.numeric(format(data, "%m")),
         dia = as.numeric(format(data,"%d"))) |>
  group_by(mes, dia) |>
  mutate(valor = sum(demanda)) |>
  ungroup() |>
  select(mes, dia, valor)

# plot
ggplot(brasil) +
  geom_tile(aes(x=dia, y=mes, fill=valor), color='white', size=.5) +
  coord_fixed() +
  scale_x_continuous(sec.axis = dup_axis(), breaks = 1:31) +
  scale_y_reverse(breaks = 1:12, 
                  labels=c('JAN', 'FEV', 'MAR', 'ABR', 'MAI', 'JUN',
                           'JUL', 'AGO', 'SET', 'OUT', 'NOV', 'DEZ')) +
  scale_fill_gradient(low='skyblue', high='red', labels=scales::label_comma()) +
  theme_minimal() +
  labs(title=title, caption=caption, fill='Demanda') +
  theme(
    plot.background = element_rect(fill='white', color='white'),
    plot.title = element_markdown(hjust=.5),
    plot.caption = element_markdown(size=9.5),
    plot.margin = margin(t=2, l=15, r=10, b=-10),
    panel.grid = element_blank(),
    legend.title = element_blank(),
    legend.text = element_text(family='quicksand'),
    legend.position = 'bottom',
    legend.margin = margin(t=-15, b=6),
    axis.title = element_blank(),
    axis.text.x.bottom = element_blank(),
    axis.text.y = element_text(size=12, family='quicksand', margin=margin(r=-22)),
    axis.text.x.top = element_text(size=12, family='quicksand', margin=margin(b=-6.5))) +
  guides(fill = guide_legend(label.position = 'bottom',
                             title.vjust = .8,
                             keyheight = unit(.09, 'in'),
                             nrow=1))


ggview(units='px', width=2500, height=1500)
ggsave(filename = 'C:/Users/Thays Ferreira/Documents/Visualização de dados/Brasil ONS/Brasil-ONS.png',
       units='px', width=2500, height=1500)
