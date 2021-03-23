library(tidyverse)
library(ggplot2)
library(sf)
library(brazilmaps)
# library(Cairo)
library(extrafont)

amazon <- c(
  "Acre", "Amapá", "Amazonas", "Mato Grosso", "Pará", "Rondônia",
  "Roraima", "Tocantins", "Maranhão"
)

atlantic <- c(
  "Rio Grande do Sul", "Santa Catarina", "Paraná", "São Paulo",
  "Goiás", "Rio de Janeiro", "Minas Gerais",
  "Espírito Santo", "Bahia", "Alagoas", "Sergipe", "Paraíba",
  "Pernambuco", "Rio Grande do Norte", "Ceará", "Piauí",
  "Distrito Federal"
)

pantanal <- c("Mato Grosso do Sul")

estados <- data.frame(
  "nome" = c(amazon, atlantic, pantanal),
  "Biome" = c(
    rep("Amazon Forest", length(amazon)),
    rep("Atlantic Forest", length(atlantic)),
    rep("Pantanal", length(pantanal))
  )
) %>% mutate(
  nome = as.character(nome),
  nome = toupper(nome)
)

bioma_map <-
  get_brmap("State") %>%
  full_join(estados)

map <- bioma_map %>%
  ggplot() +
  geom_sf(aes(fill = Biome), color = "black") +
  theme_bw() +
  scale_fill_brewer(palette = "Set1") +
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.justification = "left",
    legend.direction = "vertical",
    legend.position = c(0, 0.1),
    text = element_text(size = 20, family = "CM Roman")
  )

for (device in c("png", "pdf")) {
  map %>%
    ggsave(
      filename = here::here("figures", paste0("biome_map.", device)),
      device = device,
      width = 6,
      height = 6,
      units = "in",
      dpi = 1200
    )
}
