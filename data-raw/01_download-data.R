## code to prepare `01_download-data` dataset goes here
library(magrittr)

biomes <- c("amazonia", "pantanal", "mata_atlantica")

biomes_data <- data.frame("date" = NA)

for (biome in biomes) {
  aux <-
    data.table::fread(
      input = sprintf(
        "http://queimadas.dgi.inpe.br/queimadas/portal-static//bioma/csv_estatisticas/historico_bioma_%s.csv",
        biome
      ),
      encoding = "UTF-8"
    ) %>%
    dplyr::select(-c("Total", "V1")) %>%
    head(-3) %>%
    t() %>%
    as.data.frame() %>%
    tidyr::gather() %>%
    dplyr::select(-"key") %>%
    dplyr::filter(value != "-") %>%
    dplyr::mutate(dplyr::across(everything(), as.numeric)) %>%
    head(-1)

  biomes_data <- cbind(biomes_data, aux)

  rm(aux)
}

biomes_data$date <- seq.Date(
  from = as.Date("1998-06-01"),
  by = "month",
  length.out = nrow(biomes_data)
)

colnames(biomes_data) <- c("date", biomes)

biomes_data %>%
  write.csv(
    file = here::here("data", "biomes_data.csv"),
    row.names = FALSE
  )
