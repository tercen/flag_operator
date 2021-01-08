library(tercen)
library(dplyr)

options("tercen.workflowId" = "01cb95cd7b746443ed9f40625200ef4f")
options("tercen.stepId"     = "635b15c9-55d2-466d-bda9-6e9469b67532")

getOption("tercen.workflowId")
getOption("tercen.stepId")

ctx <- tercenCtx()

type <- "numeric"
if(!is.null(ctx$op.value("type"))) type <- ctx$op.value("type")
comparison <- "top"
if(!is.null(ctx$op.value("comparison"))) comparison <- ctx$op.value("comparison")
value <- "0.06"
if(!is.null(ctx$op.value("value"))) value <- ctx$op.value("value")

comp.num <- c("equals", "greater", "less", "less_or_equal", "greater_or_equal", "top", "bottom")
comp.char <- c("equals", "contains", "is_in")

do.compare <- function(df, comparison, value) {
  if(comparison == "equals") df <- df %>% transmute(flag = ifelse(.y == value, "pass", "fail"))
  if(comparison == "greater") df <- df %>% transmute(flag = ifelse(.y > value, "pass", "fail"))
  if(comparison == "less") df <- df %>% transmute(flag = ifelse(.y < value, "pass", "fail"))
  if(comparison == "greater_or_equal") df <- df %>% transmute(flag = ifelse(.y >= value, "pass", "fail"))
  if(comparison == "less_or_equal") df <- df %>% transmute(flag = ifelse(.y <= value, "pass", "fail"))
  if(comparison == "top") {
    if(0 < value & value < 1) {
      tr <- quantile(df$.y, probs = 1 - value, na.rm = TRUE)
      df <- df %>% transmute(flag = ifelse(.y >= tr, "pass", "fail"))
    } else {
      tr <- sort(df$.y, decreasing = TRUE)[value]
      df <- df %>% transmute(flag = ifelse(.y >= tr, "pass", "fail"))
    }
  }
  if(comparison == "bottom") {
    if(0 < value & value < 1) {
      tr <- quantile(df$.y, probs = value, na.rm = TRUE)
      df <- df %>% transmute(flag = ifelse(.y <= tr, "pass", "fail"))
    } else {
      tr <- sort(df$.y, decreasing = FALSE)[value]
      df <- df %>% transmute(flag = ifelse(.y <= tr, "pass", "fail"))
    }
  }
  return(df)
}

if(type == "numeric") {
  
  if(!comparison %in% comp.num) stop("Wrong comparison for numeric values.")
  value <- as.numeric(value)
  df <- ctx %>% 
    select(.y, .ri, .ci) %>%
    group_by(.ri, .ci) %>%
    do(do.compare(., comparison, value)) %>%
    ungroup() %>%
    select(-.ri, -.ci)

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

