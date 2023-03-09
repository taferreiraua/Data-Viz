# packages
pacman::p_load(tidyverse,
               ggtext,
               ggview)

# Data from WikiArt by Konstantinos Katserelis.

# dados
df <- read.csv("C:/Users/Thays Ferreira/Downloads/df.csv")

c(sysfonts::font_add('fb', 'fontes/Font Awesome 6 Brands-Regular-400.otf'),
  sysfonts::font_add_google("Spectral","spectral"),
  sysfonts::font_add_google("Open Sans","opensans"),
  sysfonts::font_add_google("Quicksand","quicksand"),
  showtext::showtext_auto(),
  showtext::showtext_opts(dpi=150))

title = paste0("<span style='font-family:spectral;font-size:95pt;color:#111111;'>**VAN GOGH'S**<br></span>",
               "<span style='font-family:spectral;font-size:47pt;color:#111111;'>Style & Subjects</span>")
subtitle = paste0("<span style='font-family:quicksand;font-size:21pt;color:#111111;'>**Vincent Van Gogh is one of the most influential painters in the history of Western art.**<br></span>",
                  "<span style='font-family:quicksand;font-size:21pt;color:#111111;'>Passing through Impressionism, Japonism, and Realism, the artist became a key figure in the world<br></span>",
                  "<span style='font-family:quicksand;font-size:21pt;color:#111111;'>of Post-Impressionism. He produced over 2000 works, with subjects including cloudscapes, still lifes,<br></span>",
                  "<span style='font-family:quicksand;font-size:21pt;color:#111111;'>portraits and self-portraits. The chart below shows the distribution of subjects from 1931 works of his<br></span>",
                  "<span style='font-family:quicksand;font-size:21pt;color:#111111;'>authorship for each artistic movement that he was part of, as well as the number of works produced.<br></span>",
                  "<span style='font-family:quicksand;font-size:18pt;color:#111111;'>Chart by Thays Ferreira, data from WikiArt by Konstantinos Katserelis.</span>")
caption = paste0("<span style='font-size:17pt;font-family:fb;color:#111111;'>&#xf099;</span>",
                 "<span style='font-size:18pt;font-family:quicksand;color:#111111;'> @taferreiraua </span>",
                 "<span style='font-size:17pt;font-family:fb;color:#111111;'> &#xf09b;</span>",
                 "<span style='font-size:18pt;font-family:quicksand;color:#111111;'> taferreiraua</span>")

# manipulação de dados
vangogh = df |>
  mutate(Genre = case_when(Genre%in%c('figurative', 'panorama', 'religious painting',
                                      'marina', 'vanitas', 'interior')~'other',
                           Genre=='nude painting (nu)'~'nude',
                           Genre=='animal painting'~'animals',
                           Genre=='flower painting'~'flowers',
                           TRUE ~ Genre),
         total.Paintings = n()) |>
  group_by(Style, Genre) |>
  mutate(Count = n(),
         Style = toupper(Style),
         Genre = toupper(Genre)) |>
  ungroup() |>
  group_by(Style) |>
  mutate(Num.Paintings = n(),
         Style = paste0(Style, ': <b>', Num.Paintings, '</b>')) |>
  distinct(Style, Genre, Num.Paintings, Count)


# palette
pal = c('ANIMALS'='#FFF702',
        'CITYSCAPE'='#FF8700',
        'CLOUDSCAPE'='#FFD300',
        'FLOWERS'='#FF0000',
        'GENRE PAINTING'='#DEFF0A',
        'LANDSCAPE'='#f5db37',
        'NUDE'='#0AFF99',
        'OTHER'='#241E26',
        'PORTRAIT'='#37cae5',
        'SELF-PORTRAIT'='#0f86b6',
        'SKETCH AND STUDY'='#123f77',
        'STILL LIFE'='#BE0AFF'
        )

#Distribution of genres per style. Number of paintings per style.
# plot
ggplot(vangogh) +
  geom_bar(aes(x=reorder(Style, Num.Paintings), y=Count, fill=Genre), 
           stat='identity', position='fill', width=.3) +
  scale_fill_manual(values=pal) +
  labs(title=title, subtitle=subtitle, caption=caption) +
  theme_minimal() +
  theme(plot.background = element_rect(fill='white', color='white'),
        plot.title = element_markdown(hjust=.5, margin=margin(b=10)),
        plot.subtitle = element_markdown(hjust=.5, lineheight=.6),
        plot.caption = element_markdown(hjust=.5, margin=margin(t=70, b=37, l=460)),
        plot.margin = margin(l=200, r=200, t=45, b=-20),
        legend.position = 'top',
        legend.key.size = unit(.1, 'in'),
        legend.text = element_text(size=13, color='#111111'),
        legend.title = element_blank(),
        panel.grid=element_blank(),
        axis.title = element_blank(),
        axis.text.y = element_blank(),
        axis.text.x = element_markdown(family='quicksand', size=22, angle=90, hjust=1, vjust=.6, margin=margin(t=-13))) +
  guides(fill=guide_legend(title.position="top", nrow=2, byrow=TRUE))


ggview(units='px', height=4200, width=2700)
ggsave(units='px', height=4000, width=2700, filename='van-gogh-colors.png')

