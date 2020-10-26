library(tercen)
library(dplyr)

ctx <- tercenCtx()

type <- "numeric"
if(!is.null(ctx$op.value("type"))) type <-ctx$op.value("type")
comparison <- "greater"
if(!is.null(ctx$op.value("comparison"))) type <-ctx$op.value("comparison")
value <- "100"
if(!is.null(ctx$op.value("comparison"))) type <-ctx$op.value("value")

comp.num <- c("equals", "greater", "less", "less_or_equal", "greater_or_equal")
comp.char <- c("equals", "contains", "is_in")

if(type == "numeric") {
  if(!comparison %in% comp.num) stop("Wrong comparison for numeric values.")
  value <- as.numeric(value)
  df <- ctx %>% 
    select(.y)
  if(comparison == "equals") df <- df %>% transmute(flag = ifelse(.y == value, "pass", "fail"))
  if(comparison == "greater") df <- df %>% transmute(flag = ifelse(.y > value, "pass", "fail"))
  if(comparison == "less") df <- df %>% transmute(flag = ifelse(.y < value, "pass", "fail"))
  if(comparison == "greater_or_equal") df <- df %>% transmute(flag >= ifelse(.y < value, "pass", "fail"))
  if(comparison == "less_or_equal") df <- df %>% transmute(flag <= ifelse(.y < value, "pass", "fail"))
  df %>%
    ctx$addNamespace() %>%
    ctx$save()
} else if(type == "character") {
  if(!comparison %in% comp.char) stop("Wrong comparison for character values.")
  df <- ctx %>% 
    rselect()
  if(comparison == "equals") flag <- ifelse(df[[1]] == value, "pass", "fail")
  if(comparison == "contains") flag <- ifelse(grepl(value, df[[1]]), "pass", "fail")
  if(comparison == "is_in") flag <- ifelse(df[[1]] %in% value, "pass", "fail")
  tibble(flag) %>% mutate(.ri = seq_len(nrow(.)) - 1) %>%
    ctx$addNamespace() %>%
    ctx$save()
}

