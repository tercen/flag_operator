library(tercen)
library(dplyr)

ctx <- tercenCtx()

type <- "numeric"
if(!is.null(ctx$op.value("type"))) type <- ctx$op.value("type")
comparison <- "top"
if(!is.null(ctx$op.value("comparison"))) comparison <- ctx$op.value("comparison")
value <- "0.06"
if(!is.null(ctx$op.value("value"))) value <- ctx$op.value("value")

pass.flag <- "pass"
if(!is.null(ctx$op.value("pass.flag"))) pass.flag <- ctx$op.value("pass.flag")
fail.flag <- "fail"
if(!is.null(ctx$op.value("fail.flag"))) fail.flag <- ctx$op.value("fail.flag")


comp.num <- c("equals", "greater", "less", "less_or_equal", "greater_or_equal", "top", "bottom")
comp.char <- c("equals", "contains", "is_in")

do.compare <- function(df, comparison, value) {
  if(comparison == "equals") df <- df %>% transmute(flag = ifelse(.y == value, pass.flag, fail.flag))
  if(comparison == "greater") df <- df %>% transmute(flag = ifelse(.y > value, pass.flag, fail.flag))
  if(comparison == "less") df <- df %>% transmute(flag = ifelse(.y < value, pass.flag, fail.flag))
  if(comparison == "greater_or_equal") df <- df %>% transmute(flag = ifelse(.y >= value, pass.flag, fail.flag))
  if(comparison == "less_or_equal") df <- df %>% transmute(flag = ifelse(.y <= value, pass.flag, fail.flag))
  if(comparison == "top") {
    if(0 < value & value < 1) {
      tr <- quantile(df$.y, probs = 1 - value, na.rm = TRUE)
      df <- df %>% transmute(flag = ifelse(.y >= tr, pass.flag, fail.flag))
    } else {
      tr <- sort(df$.y, decreasing = TRUE)[value]
      df <- df %>% transmute(flag = ifelse(.y >= tr, pass.flag, fail.flag))
    }
  }
  if(comparison == "bottom") {
    if(0 < value & value < 1) {
      tr <- quantile(df$.y, probs = value, na.rm = TRUE)
      df <- df %>% transmute(flag = ifelse(.y <= tr, pass.flag, fail.flag))
    } else {
      tr <- sort(df$.y, decreasing = FALSE)[value]
      df <- df %>% transmute(flag = ifelse(.y <= tr, pass.flag, fail.flag))
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
  if(comparison == "equals") flag <- ifelse(df[[1]] == value, pass.flag, fail.flag)
  if(comparison == "contains") flag <- ifelse(grepl(value, df[[1]]), pass.flag, fail.flag)
  if(comparison == "is_in") flag <- ifelse(df[[1]] %in% value, pass.flag, fail.flag)
  tibble(flag) %>% mutate(.ri = seq_len(nrow(.)) - 1) %>%
    ctx$addNamespace() %>%
    ctx$save()
  
}