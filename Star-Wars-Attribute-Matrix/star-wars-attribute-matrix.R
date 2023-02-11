library(tidyverse)
library(ggtext)
library(ggview)
library(geomtextpath)
library(ggimage)
library(sysfonts)
library(showtext)


# função 'coord_radar()'
#https://stackoverflow.com/questions/36579767/add-unit-labels-to-radar-plot-and-remove-outer-ring-ggplot2-spider-web-plot-co
coord_radar <- function (theta = "x", start = 0, direction = 1) 
{
  theta <- match.arg(theta, c("x", "y"))
  r <- if (theta == "x") 
    "y"
  else "x"
  
  #dirty
  rename_data <- function(coord, data) {
    if (coord$theta == "y") {
      plyr::rename(data, c("y" = "theta", "x" = "r"), warn_missing = FALSE)
    } else {
      plyr::rename(data, c("y" = "r", "x" = "theta"), warn_missing = FALSE)
    }
  }
  theta_rescale <- function(coord, x, scale_details) {
    rotate <- function(x) (x + coord$start) %% (2 * pi) * coord$direction
    rotate(scales::rescale(x, c(0, 2 * pi), scale_details$theta.range))
  }
  
  r_rescale <- function(coord, x, scale_details) {
    scales::rescale(x, c(0, 0.4), scale_details$r.range)
  }
  
  ggproto("CordRadar", CoordPolar, theta = theta, r = r, start = start, 
          direction = sign(direction),
          is_linear = function(coord) TRUE,
          render_bg = function(self, scale_details, theme) {
            scale_details <- rename_data(self, scale_details)
            
            theta <- if (length(scale_details$theta.major) > 0)
              theta_rescale(self, scale_details$theta.major, scale_details)
            thetamin <- if (length(scale_details$theta.minor) > 0)
              theta_rescale(self, scale_details$theta.minor, scale_details)
            thetafine <- seq(0, 2 * pi, length.out = 100)
            
            rfine <- c(r_rescale(self, scale_details$r.major, scale_details))
            
            # This gets the proper theme element for theta and r grid lines:
            #   panel.grid.major.x or .y
            majortheta <- paste("panel.grid.major.", self$theta, sep = "")
            minortheta <- paste("panel.grid.minor.", self$theta, sep = "")
            majorr     <- paste("panel.grid.major.", self$r,     sep = "")
            
            ggplot2:::ggname("grill", grid::grobTree(
              ggplot2:::element_render(theme, "panel.background"),
              if (length(theta) > 0) ggplot2:::element_render(
                theme, majortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(theta))) + 0.5,
                y = c(rbind(0, 0.45 * cos(theta))) + 0.5,
                id.lengths = rep(2, length(theta)),
                default.units = "native"
              ),
              if (length(thetamin) > 0) ggplot2:::element_render(
                theme, minortheta, name = "angle",
                x = c(rbind(0, 0.45 * sin(thetamin))) + 0.5,
                y = c(rbind(0, 0.45 * cos(thetamin))) + 0.5,
                id.lengths = rep(2, length(thetamin)),
                default.units = "native"
              ),
              
              ggplot2:::element_render(
                theme, majorr, name = "radius",
                x = rep(rfine, each = length(thetafine)) * sin(thetafine) + 0.5,
                y = rep(rfine, each = length(thetafine)) * cos(thetafine) + 0.5,
                id.lengths = rep(length(thetafine), length(rfine)),
                default.units = "native"
              )
            ))
          })
}

 


# dados
df = read.csv("https://raw.githubusercontent.com/taferreiraua/Estudos-de-Exploracao-e-Visualizacao-de-Dados/main/Star-Wars-Attribute-Matrix/Data/starwars.csv")

# fontes
sysfonts::font_add('starwars', 'fontes/Starjedi.ttf')
sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf')
sysfonts::font_add('skate sans', 'fontes/Skate Sans W00 Regular.otf')
sysfonts::font_add_google("Open Sans","opensans")
showtext::showtext_auto()
showtext::showtext_opts(dpi=150)

# texto
title = paste0(
  "<span style='font-family:starwars;color:yellow;'>**STAR WARS**</span>",
  "<span style='font-family:opensans;color:white;'> **Attribute Matrix** </span>"
)
subtitle = "<span style='font-family:opensans;color:white;'> Characters personality traits scaled from 0 to 100.<br></span>"
caption <- paste0(
  "<span style='font-family:fb;color:yellow;'>&#xf099;</span>",
  "<span style='font-family:opensans;color:white;'> @taferreiraua |</span>",
  "<span style='font-family:fb;color:yellow;'>&#xf09b;</span>",
  "<span style='font-family:opensans;color:white;'> taferreiraua </span>",
  "<span style='font-family:opensans;color:white;'>| Data: Open-Source Psychometrics Project </span>"
  )


# manipulação de dados
starwars = df |>
  select(character, item, avg_rating) |>
  filter(item %in% c('loyal (not traitorous)', 'heroic (not villainous)', 'emotional (not unemotional)', 
                     'empath (not psychopath)', 'devoted (not unfaithful)', 'persistent (not quitter)', 
                     'idealist (not realist)', 'emotional (not logical)', 'competent (not incompetent)', 
                     'impulsive (not cautious)', 'traitorous (not loyal)', 'cautious (not impulsive)',
                     'realist (not idealist)', 'psychopath (not empath)', 'villainous (not heroic)')) |>
  mutate(
    avg_rating = case_when(
      item=='traitorous (not loyal)'~abs(avg_rating-100),
      item=='cautious (not impulsive)'~abs(avg_rating-100),
      item=='realist (not idealist)'~abs(avg_rating-100),
      item=='psychopath (not empath)'~abs(avg_rating-100),
      item=='villainous (not heroic)'~abs(avg_rating-100),
      TRUE ~ avg_rating),
    item = case_when(
      item=='loyal (not traitorous)'~'loyal',
      item=='heroic (not villainous)'~'heroic',
      item=='emotional (not unemotional)'~'emotional',
      item=='empath (not psychopath)'~'empath',
      item=='devoted (not unfaithful)'~'devoted',
      item=='persistent (not quitter)'~'persistent',
      item=='idealist (not realist)'~'idealist',
      item=='emotional (not logical)'~'emotional',
      item=='competent (not incompetent)'~'competent',
      item=='impulsive (not cautious)'~'impulsive',
      item=='traitorous (not loyal)'~'loyal',
      item=='cautious (not impulsive)'~'impulsive',
      item=='realist (not idealist)'~'idealist',
      item=='psychopath (not empath)'~'empath',
      item=='villainous (not heroic)'~'heroic')) |>
  group_by(character, item) |>
  mutate(avg_rating = mean(avg_rating)) |>
  ungroup() |>
  distinct(item, character, avg_rating) |>
  arrange(character, item) |>
  mutate(
    images = paste0("https://raw.githubusercontent.com/taferreiraua/Estudos-de-Exploracao-e-Visualizacao-de-Dados/main/Star-Wars-Attribute-Matrix/Images/", str_replace(str_replace_all(character, " ", "-"), "\\.", ""), ".png"),
    item = toupper(item)
  )

item = unique(starwars$item)
line = data.frame(x=rep(item,2),y=c(rep(0, length(item)),rep(100, length(item))))

# plot
plot = ggplot(starwars, aes(x=item, y=avg_rating, group=character)) +
  coord_radar() +
  geom_polygon(fill="#44C7EA", color="#44C7EA", alpha=0.35) +
  facet_wrap(~character) +
  geom_line(data=line, mapping=aes(x=x,y=y, group=x), color="white", alpha=0.5) +
  geom_point(inherit.aes=FALSE, data=data.frame(x=item, y=rep(100,length(item))),
             mapping=aes(x=x, y=y), shape=21, fill='#000112', color='white', size=5) +
  geom_point(colour='skyblue', size=4.5) +
  geom_image(mapping=aes(x=1, y=-80, image=images), size=0.27) +
  geom_textpath(inherit.aes=FALSE, mapping=aes(x=item, label=item, y=150), family='opensans',
                fontface="bold", upright=TRUE, text_only=TRUE, size=6.5, color='white') + 
  geom_textpath(inherit.aes=FALSE, mapping=aes(x=item, label=avg_rating, y=124), family='opensans',
                fontface="bold", upright=TRUE, text_only=F, size=9, color='skyblue') + 
  scale_y_continuous(limits=c(-80,150), breaks=c(0, 25, 50, 75, 100)) +
  theme_minimal() +
  labs(title=title, subtitle=subtitle, caption=caption) +
  theme(
    plot.background = element_rect(fill='#000112', color='#000112'),
    plot.caption = element_markdown(size=25),
    plot.title = element_markdown(size=100, hjust=.05),
    plot.margin = margin(t=5, r=27, l=10),
    plot.subtitle = element_markdown(size=48, hjust=.04),
    strip.background = element_blank(),
    strip.text = element_text(size=50, family = 'skate sans', color='white'),
    legend.position = 'none',
    panel.background = element_blank(),
    axis.text.x = element_blank(),
    axis.text.y = element_blank(),
    panel.grid.major.x=element_blank()
  )

ggview(units='px', height=5000, width=6000)
ggsave(units='px', height=5000, width=6000, filename = "C:/Users/Thays Ferreira/Documents/Visualização de dados/Star Wars Attribute Matrix/star-wars-attribute-matrix.png") 
