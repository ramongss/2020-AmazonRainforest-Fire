## code to prepare `02_download-data-state` dataset goes here
library(magrittr)

states <- c(
  "acre", "amapa", "amazonas", "mato_grosso", "para", "rondonia",
  "roraima", "tocantins", "maranhao"
) %>% sort()

states_list <- list()

for (state in states) {
  states_list[[state]] <-
    data.table::fread(
      input = sprintf(
        "http://queimadas.dgi.inpe.br/queimadas/portal-static//estado/csv_estatisticas/historico_estado_%s.csv",
        state
      ),
      encoding = "UTF-8"
    ) %>%
    dplyr::select(-c("Total", "V1")) %>%
    head(-3) %>%
    t() %>%
    as.data.frame() %>%
    tidyr::gather() %>%
    dplyr::select(-"key") %>%
    # dplyr::filter(value != "-") %>%
    dplyr::mutate(dplyr::across(everything(), as.numeric))
}

states_data <- do.call(cbind.data.frame, states_list)

states_data <-
  cbind(
    date = seq.Date(as.Date("1998-01-01"),
      by = "month",
      length.out = nrow(states_data)
    ),
    states_data
  )

colnames(states_data) <- c("date", states)

states_data <- states_data %>%
  dplyr::filter(date >= ("1998-06-01") & date < format(Sys.Date(), "%Y-%m-01"))

states_data[is.na(states_data)] <- 0

states_data %>%
  write.csv(
    file = here::here("data", "states_data.csv"),
    row.names = FALSE
  )
