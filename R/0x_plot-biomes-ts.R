library(ggplot2)
library(magrittr)
library(extrafont)

biomes_data <-
  data.table::fread(here::here("data", "biomes_data.csv"))

biomes_gather <-
  biomes_data %>% tidyr::gather(key = "biome", value = "fires", -date)

biomes_gather$biome <-
  biomes_gather$biome %>%
  factor(labels = c("Amazon Forest", "Atlantic Forest", "Pantanal"))

biomes_plot <- biomes_gather %>%
  ggplot(aes(x = date, y = fires, colour = biome)) +
  geom_line(size = 1) +
  scale_color_brewer(palette = "Set1") +
  facet_wrap(
    ~biome,
    ncol = 1,
    scales = "free",
    strip.position = "right"
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    text = element_text(size = 15, family = "CM Roman")
  ) +
  labs(x = "Month", y = "Fire spots")

biomes_plot %>%
  ggsave(
    filename = here::here("figures", "biomes.pdf"),
    device = "pdf",
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 300
  )
