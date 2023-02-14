library(tidyverse)
library(sysfonts)
library(ggview)
library(ggpubr)
library(png)
library(ggimage)
library(ggtext)


# dados
df <- read.csv("https://raw.githubusercontent.com/taferreiraua/Estudos-de-Exploracao-e-Visualizacao-de-Dados/main/Mandalorian-Directors/Data/mandalorian.csv")

# fontes e caption
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('skate sans', 'fontes/Skate Sans W00 Regular.otf')
sysfonts::font_add_google("Open Sans","opensans")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)

caption <- paste0(
  "<span style='font-family:fb;color:#B22222'>&#xf099;</span>",
  "<span style='font-family:opensans;'> @taferreiraua |</span>",
  "<span style='font-family:fb;color:#B22222'>&#xf09b;</span>",
  "<span style='font-family:opensans;'> taferreiraua </span>",
  "<span style='font-family:opensans;'>| Source: Rotten Tomatoes <br></span>")

# cores e imagens
col_bars <- "firebrick1"
  img <- readPNG("https://raw.githubusercontent.com/taferreiraua/Estudos-de-Exploracao-e-Visualizacao-de-Dados/main/Mandalorian-Directors/Images/poster.png")
  imagens <- c("~/filoni.png", "~/rick.png", "~/chow.png", "~/bryce.png", "~/filoni.png",
               "~/rick.png", "~/chow.png", "~/taika.png")
  names <- c("Dave Filoni", "Rick Famuyiwa", "Deborah Chow", "Bryce Dallas Howard", 
             "Dave Filoni", "Rick Famuyiwa", "Deborah Chow", "Taika Waititi")
  
  # plot
  df |>
    filter(season==1) |>
    mutate(imagens = paste0("https://raw.githubusercontent.com/taferreiraua/Estudos-de-Exploracao-e-Visualizacao-de-Dados/main/Mandalorian-Directors/Images/",
                            str_replace(str_replace_all(director, " ", "-"), "\\.", ""), ".png")) |>
    ggplot() +
    ggpubr::background_image(img) +
    geom_bar(aes(episode, tomatometer, fill=col_bars, color=col_bars), 
             stat="identity", width=0.75) +
    scale_y_continuous(lim=c(-35, 280)) +
    geom_image(aes(x=episode, y=-8, image=imagens), 
               size=0.103, asp=1, color="black") +
    geom_image(aes(x=episode, y=-8, image=imagens), 
               size=0.1, asp=1) +
    geom_text(aes(x=4.5, y=280, label="Which director is the highest rated?"),
              size=35, family="skate sans", fontface="bold", color="white") +
    geom_text(aes(x=4.5, y=263.5, label=str_wrap("Ratings per episode from The Mandalorian's season 1, and their respectives directors. Score based on 'tomatometer' from Rotten Tomatoes.", 75)),
              size=15, family="opensans", fontface="bold", color="white", lineheight=.5) +
    geom_text(aes(x=episode, y=tomatometer+11.15, label=paste0("Chapter ", chapter, ":")),
              size=13, family="skate sans", fontface="bold", color="white") +
    geom_text(aes(x=episode, y=tomatometer+5.15, label=title), 
              size=13, family="skate sans", fontface="bold", color="white") +
    geom_text(aes(x=episode, y=tomatometer-3.5, label="score:"),
              size=9.5, family="opensans", fontface="bold", color="black") +
    geom_text(aes(x=episode, y=tomatometer-10, label=paste0(tomatometer, "%")),
              size=23, family="skate sans", fontface="bold") +
    geom_text(aes(x=episode, y=-30.5, label="Directed by"),
              size=9.6, color="white") +
    geom_text(aes(x=episode, y=-35, label=str_wrap(names, width=13)), 
              size=12, vjust=1, lineheight=.5, fontface="bold", color="white", family="skate sans") +
    theme_minimal() +
    labs(caption=caption) +
    theme(
      plot.background = element_rect(fill="#0E0000"),
      plot.caption = element_markdown(color="white", size=23, vjust=1.5, hjust=0.98, lineheight = .05),
      plot.margin = margin(t=-4, r=-4, l=-8),
      legend.position = "none",
      panel.grid = element_blank(),
      axis.text = element_blank(),
      axis.title = element_blank()
    )
  
  ggview(units="px", height=4500, width=4500)
  ggsave("C:/Users/Thays Ferreira/Documents/Visualização de dados/Mandalorian Directors/Mandalorian-Directors-Plot.png", height=4500, width=4500, units="px")
 
