library(DT)

print_table_bootstrapped_summary <- function(table, decimal_places){
  datatable(
    table,
    rownames = FALSE,
    extensions = list("FixedColumns" = NULL),
    options = list(
      dom = "tlp",
      scrollX = TRUE,
      fixedColumns = list(leftColumns = 2)
    ) 
  ) %>%
    formatRound(
      names(table)[-c(1, 2)],
      digits = decimal_places
    ) %>%
    formatStyle(
      names(table),
      lineHeight = "80%"
    )
}


print_table_summary <- function(table, decimal_places){
  datatable(
    table,
    rownames = FALSE,
    extensions = list("FixedColumns" = NULL),
    options = list(
      dom = "tlp",
      scrollX = TRUE,
      fixedColumns = TRUE
    ) 
  ) %>%
    formatRound(
      names(table)[-1],
      digits = decimal_places
    ) %>%
    formatStyle(
      names(table),
      lineHeight = "80%"
    )
}


print_table_distribution <- function(table, decimal_places){
  datatable(
    table,
    rownames = FALSE,
    extensions = list("FixedColumns" = NULL),
    options = list(
      dom = "t",
      paging = FALSE,
      scrollY = "200px",
      fixedColumns = list(leftColumns = 1)
    ) 
  ) %>%
    formatRound(
      names(table)[-1],
      digits = decimal_places
    ) %>%
    formatStyle(
      names(table),
      lineHeight = "80%"
    )
}