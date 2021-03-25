results <- readRDS(here::here("data", "results.rds"))

sheets <- list()

for (state in names(results)) {

  sheets[[state]] <- data.frame()

  for (step in seq(results[[state]]$`STL-STACK`[[2]])) {
    aux <-
      data.frame(
        "state" = state,
        "step" = paste0(step, "-step"),
        "criteria" = c("MAE", "MAPE", "RMSE"),
        cbind(
          # STL-STACK
          data.frame("STL-STACK" = results[[state]]$`STL-STACK`$STACK_Metrics[[step]]),
          # STL
          t(results[[state]]$`STL-STACK`$Decomp_Metrics[[step]][,-1]),
          # STACK
          data.frame("STACK" = results[[state]]$STACK$STACK_Metrics[[step]]),
          # SIGLE
          t(results[[state]]$STACK$Metrics[[step]][,-1])
        )
      )

    sheets[[state]] <- rbind(sheets[[state]], aux)

    rownames(sheets[[state]]) <- NULL
  }
}

results_sheets <- do.call(rbind, sheets)

rownames(results_sheets) <- NULL

write.csv(
  x = results_sheets,
  file = here::here("data", "metrics.csv"),
  row.names = FALSE
)
