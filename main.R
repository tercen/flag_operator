library(tercen)
library(dplyr)

ctx <- tercenCtx()

type <- ctx$op.value("type", as.character, "numeric")
comparison <- ctx$op.value("comparison", as.character, "equals")
value <- ctx$op.value("value", as.character, "0.5")
flag.name <- ctx$op.value("flag.name", as.character, "flag")
pass.flag <- ctx$op.value("pass.flag", as.character, "pass")
fail.flag <- ctx$op.value("fail.flag", as.character, "fail")

comp.num <- c("equals", "greater", "less", "less_or_equal", "greater_or_equal", "top", "bottom")
comp.char <- c("equals", "contains", "is_in")

if(type == "numeric") {
  
  if(!comparison %in% comp.num) {
    stop("Wrong comparison for numeric values.")
  } 

  value <- as.numeric(value)
  df <- ctx %>%
    select(.y, .ri, .ci) %>%
    group_by(.ri, .ci)
  
  if(comparison == "top") {
    if(0 < value & value < 1) {
      tr <- quantile(df$.y, probs = 1 - value, na.rm = TRUE)
      df <- df %>% transmute(!!flag.name := ifelse(.y >= tr, pass.flag, fail.flag))
    } else {
      tr <- sort(df$.y, decreasing = TRUE)[value]
      df <- df %>% transmute(!!flag.name := ifelse(.y >= tr, pass.flag, fail.flag))
    }
  } else if(comparison == "bottom") {
    if(0 < value & value < 1) {
      tr <- quantile(df$.y, probs = value, na.rm = TRUE)
      df <- df %>% transmute(!!flag.name := ifelse(.y <= tr, pass.flag, fail.flag))
    } else {
      tr <- sort(df$.y, decreasing = FALSE)[value]
      df <- df %>% transmute(!!flag.name := ifelse(.y <= tr, pass.flag, fail.flag))
    }
  } else {
    df %>% transmute(!!flag.name := if_else(
      do.call(case_when(
        comparison == "equals" ~ "==",
        comparison == "greater" ~ ">",
        comparison == "less" ~ "<",
        comparison == "greater_or_equal" ~ ">=",
        comparison == "less_or_equal" ~ "<=",
      ), list(.y, value)),
      pass.flag,
      fail.flag
    )) %>%
      ctx$addNamespace() %>%
      ctx$save()
  }  
} else if(type == "character") {
  
  if(!comparison %in% comp.char) stop("Wrong comparison for character values.")
  df <- ctx %>% 
    rselect()
  if(comparison == "equals") flag <- ifelse(df[[1]] == value, pass.flag, fail.flag)
  if(comparison == "contains") flag <- ifelse(grepl(value, df[[1]]), pass.flag, fail.flag)
  if(comparison == "is_in") flag <- ifelse(df[[1]] %in% value, pass.flag, fail.flag)
  tibble(flag) %>% mutate(.ri = seq_len(nrow(.)) - 1L) %>%
    rename(!!flag.name := flag) %>%
    ctx$addNamespace() %>%
    ctx$save()
  
}
