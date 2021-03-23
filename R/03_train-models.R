library(caret)
library(magrittr)
library(Metrics)

# Data treatment ----------------------------------------------------------
## load data
raw_data <-
  data.table::fread(here::here("data", "states_data.csv")) %>%
  tidyr::gather(key = "state", value = "fires", -date) %>%
  split(.$state)

## call prediction functions
source(here::here("R", "01_stl-stack-fit_function.R"))
source(here::here("R", "02_stack-fit_function.R"))

## list of models
model_list <- c(
  "ridge",
  "svmLinear2",
  "knn",
  "pls"
) %>% sort()
meta_model <- "cubist"
horizon <- seq(12)

stl_stack_results <- list()
stack_results <- list()
results <- list()

tictoc::tic.clearlog()
for (state in names(raw_data)) {
  tictoc::tic(state)

  ## stl stack prediction
  stl_stack_results[[state]] <-
    stl_stack_pred(
      data = raw_data[[state]],
      model_list = model_list,
      meta_model = meta_model,
      horizon = horizon
    )

  ## stack prediction
  stack_results[[state]] <-
    stack_pred(
      data = raw_data[[state]],
      model_list = model_list,
      meta_model = meta_model,
      horizon = horizon
    )

  ## gather results
  results[[state]] <-
    list(
      "STL-STACK" = stl_stack_results[[state]],
      "STACK" = stack_results[[state]]
    )

  tictoc::toc(log = TRUE)
}

## save results
saveRDS(
  object = results,
  file = here::here("data", "results.rds")
)

tic_log <- unlist(tictoc::tic.log(format = TRUE))

beepr::beep(8)
