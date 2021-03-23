library(ggplot2)
library(magrittr)
library(extrafont)

name_states <- c(
  "Acre", "Amapá", "Amazonas", "Mato Grosso", "Pará", "Rondônia",
  "Roraima", "Tocantins", "Maranhão"
) %>% sort()

states_data <-
  data.table::fread(here::here("data", "states_data.csv"))

states_gather <-
  states_data %>% tidyr::gather(key = "state", value = "fires", -date)

states_gather$state <-
  states_gather$state %>%
  factor(labels = name_states)

set1_palette <- RColorBrewer::brewer.pal(9, "Set1")

states_plot <- states_gather %>%
  ggplot(aes(x = date, y = fires)) +
  geom_line(size = 0.7, colour = set1_palette[1]) +
  facet_wrap(
    ~state,
    ncol = 3,
    scales = "free",
  ) +
  theme_bw() +
  theme(
    legend.position = "none",
    strip.background = element_blank(),
    text = element_text(size = 15, family = "CM Roman")
  ) +
  labs(x = "Month", y = "Number of fire spots") +
  scale_y_continuous(labels = scales::comma)

states_plot %>%
  ggsave(
    filename = here::here("figures", "states_ts.pdf"),
    device = "pdf",
    width = 12,
    height = 6.75,
    units = "in",
    dpi = 1200
  )
