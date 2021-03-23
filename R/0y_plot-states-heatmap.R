library(magrittr)
library(ggplot2)
library(sf)
library(extrafont)

states_data <-
  data.table::fread(here::here("data", "states_data.csv")) %>%
  tidyr::gather(key = "states", value = "fires", -date) %>%
  dplyr::group_by(states) %>%
  dplyr::summarise(
    fires_sum = sum(fires),
    fires_mean = mean(fires),
    .groups = "drop"
  ) %>%
  as.data.frame()

states_data$states <-
  c(
    "Acre", "Amapá", "Amazonas", "Mato Grosso", "Pará",
    "Rondônia", "Roraima", "Tocantins", "Maranhão"
  ) %>%
  sort() %>%
  toupper()

states_data$abb <-
  c("AC", "AP", "AM", "MA", "MT", "PA", "RO", "RR", "TO")

amazon_data <-
  brazilmaps::get_brmap("State") %>%
  dplyr::full_join(states_data, by = c("nome" = "states"))

heatmap_sum <- amazon_data %>%
  ggplot() +
  geom_sf(aes(fill = fires_sum), colour = "black") +
  geom_sf_label(aes(label = abb), family = "CM Roman") +
  theme_bw() +
  theme(
    axis.title = element_blank(),
    text = element_text(family = "CM Roman", size = 15),
    legend.background = element_blank(),
    legend.justification = "left",
    legend.direction = "vertical",
    legend.position = c(0.05, 0.2),
  ) +
  scale_fill_gradientn(
    colours = heat.colors(4, rev = TRUE), na.value = "grey90",
    "No. of\nFire Spots", labels = scales::comma
  )

heatmap_sum %>%
  ggsave(
    filename = here::here("figures", "heatmap_sum.pdf"),
    device = "pdf",
    width = 6,
    height = 6,
    units = "in",
    dpi = 1200
  )
