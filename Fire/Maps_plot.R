library(tidyverse)
library(brazilmaps)
library(ggplot2)
library(cowplot)
get_brmap("City", geo.filter = list(State = 33)) %>% 
  left_join(pop2017, c("City" = "mun")) %>% 
  ggplot() +
  geom_sf(aes(fill = pop2017),
  colour = "black", size = 0.1) +
  # muda escala de cores
  scale_fill_viridis_c(option = "C")

mapa <- get_brmap("Region")
glimpse(mapa)

pnud_minima <- abjData::pnud_muni %>% 
  filter(ano == 2010) %>% 
  select(cod_uf = uf, 
         cod_muni = codmun7, 
         starts_with("idh"))

glimpse(pnud_minima)

pnud_minima_estado <- pnud_minima %>% 
  group_by(cod_uf) %>% 
  summarise(idh = mean(idhm))

glimpse(pnud_minima_estado)

uf_map <- get_brmap("State") %>% 
  inner_join(pnud_minima_estado, c("State" = "cod_uf"))

glimpse(uf_map)

uf_map %>% 
  ggplot() +
  geom_sf()+
  ggtitle("Brazil") +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "none",
    axis.text=element_text(size=19),
    legend.text=element_text(size=rel(1)),
    plot.title = element_text(hjust=0.5,size=28),
    text=element_text(family="Times New Roman"),
    axis.title=element_text(size=28))+
  xlab("Longitude") +  ylab("Latitude")


SP<-get_brmap("State", geo.filter = list(State = 35)) %>% 
  ggplot() +
  geom_sf()+
  ggtitle("Sao Paulo") +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    legend.text=element_text(size=rel(1)),
    plot.title = element_text(hjust=0.5,size=28),
    text=element_text(family="Times New Roman"),
    axis.title=element_text(size=12))+
  xlab("Longitude") +  ylab("Latitude")

PR<-get_brmap("State", geo.filter = list(State = 41)) %>% 
  ggplot() +
  geom_sf()+
  ggtitle("Parana") +
  theme_bw(base_size = 18) +
    theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    legend.text=element_text(size=rel(1)),
    plot.title = element_text(hjust=0.5,size=28),
    text=element_text(family="Times New Roman"),
    axis.title=element_text(size=12))+
  xlab("Longitude") +  ylab("Latitude")

MG<-get_brmap("State", geo.filter = list(State = 31)) %>% 
  ggplot() +
  geom_sf()+
  ggtitle("Minas Gerais") +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    legend.text=element_text(size=rel(1)),
    plot.title = element_text(hjust=0.5,size=28),
    text=element_text(family="Times New Roman"),
    axis.title=element_text(size=12))+
  xlab("Longitude") +  ylab("Latitude")

RJ<-get_brmap("State", geo.filter = list(State = 33)) %>% 
  ggplot() +
  geom_sf()+
  ggtitle("Rio de Janeiro") +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "none",
    axis.text=element_text(size=12),
    legend.text=element_text(size=rel(1)),
    plot.title = element_text(hjust=0.5,size=28),
    text=element_text(family="Times New Roman"),
    axis.title=element_text(size=12))+
  xlab("Longitude") +  ylab("Latitude")

x11()
plot_grid(MG,PR,RJ,SP,labels=c("A","B","C","D",nrow=2))


########################################################
library(abjData)

uf_map$Scale<-c(62, 29,157, 62,520,157,61,145,155,
                426,106,39,847,117,23,453,1036,147,
                1066,2689,1631,887,130,166,233,99,
                1192)
uf_map$Scale1<-c(0, 0,0, 0,0,0,0,0,0,
                0,0,0,0,0,0,0,500,0,
                1001,2001,1501,0,0,0,0,0,
                0)
  uf_map%>%
  ggplot() +
  geom_sf(aes(fill = Scale1))+
    scale_colour_viridis_d(option = "inferno") +
  ggtitle("Brazil") +
  theme_bw(base_size = 18) +
  theme(
    legend.position = "none",
    axis.text=element_text(size=19),
    legend.text=element_text(size=rel(1)),
    plot.title = element_text(hjust=0.5,size=28),
    text=element_text(family="Times New Roman"),
    axis.title=element_text(size=28))+
  xlab("Longitude") +  ylab("Latitude")
  
  muni_map <- get_brmap("City") %>% 
    left_join(pnud_minima, c("City" = "cod_muni"))
  
  muni_map %>% 
    mutate(idhm = cut(idhm, c(0, 0.5, 0.6, 0.75, 1.0))) %>% 
    ggplot() +
    geom_sf(aes(fill = idhm), 
            # ajusta tamanho das linhas
            colour = "black", size = 0.1) +
    geom_sf(data = get_brmap("State"),
            fill = "transparent",
            colour = "black", size = 0.5) +
    # muda escala de cores
    scale_fill_viridis_d(option = 2, begin = 0.2, end = 0.8) +
    # tira sistema cartesiano
    theme(panel.grid = element_line(colour = "transparent"),
          panel.background = element_blank(),
          axis.text = element_blank(),
          axis.ticks = element_blank())