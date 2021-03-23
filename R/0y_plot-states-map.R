library(magrittr)
library(ggplot2)
library(sf)
library(extrafont)

amazon_states <-
  data.frame(
    "nome" = c(
      "Acre", "Amapá", "Amazonas", "Mato Grosso", "Pará", "Rondônia",
      "Roraima", "Tocantins", "Maranhão"
    ),
    "abb" = c("AC", "AP", "AM", "MT", "PA", "RO", "RR", "TO", "MA"),
    "Biome" = "Amazon Forest"
  ) %>%
  dplyr::mutate(
    nome = as.character(nome),
    nome = toupper(nome)
  )

amazon_data <-
  brazilmaps::get_brmap("State") %>%
  dplyr::full_join(amazon_states)

set1_palette <- RColorBrewer::brewer.pal(9, "Set1")

amazon_map <-
  amazon_data %>%
  ggplot() +
  geom_sf(aes(fill = Biome), color = "black") +
  geom_sf_text(aes(label = abb), color = "black", family = "CM Roman") +
  theme_bw() +
  scale_fill_manual(values = set1_palette[3], na.translate = FALSE) +
  theme(
    legend.title = element_blank(),
    legend.background = element_blank(),
    legend.justification = "left",
    legend.direction = "vertical",
    legend.position = c(0, 0.1),
    text = element_text(size = 20, family = "CM Roman"),
    axis.title = element_blank()
  )

amazon_map

amazon_map %>%
  ggsave(
    filename = here::here("figures", "states_map.pdf"),
    device = "pdf",
    width = 6,
    height = 6,
    units = "in",
    dpi = 1200
  )
