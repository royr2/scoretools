#
# Bunch of utility functions
#

prettify_ks_table <- function(tab, accr = 0.01){
  tab$Pop.Pct <- scales::percent(tab$Pop.Pct, accuracy = accr)
  tab$Event.Rate <- scales::percent(tab$Event.Rate, accuracy = accr)
  tab$KS <- scales::number(tab$KS * 100, accuracy = accr)
  tab$Capture.Rate <- scales::percent(tab$Capture.Rate, accuracy = accr)
  tab$Cum.Event.Rate <- scales::percent(tab$Cum.Event.Rate, accuracy = accr)
  tab$Cum.Events.Dist <- scales::percent(tab$Cum.Events.Dist, accuracy = accr)
  tab$Cum.N.Events.Dist <- scales::percent(tab$Cum.N.Events.Dist, accuracy = accr)

  return(tab)
}

# Function to print something to screen whita white background utilising the full console width
print_bg <- function(msg = "", collap = " "){
  msg <- paste("\n", msg, paste0(rep("", c_width - nchar(msg)), collapse = collap), "\n\n")
  cat(crayon::bgWhite(crayon::black(crayon::bold(msg))))
}

ROC <- function(act, pred){
  pred <- ROCR::prediction(pred, act)
  return(ROCR::performance(pred,"auc")@y.values[[1]])
}

ipsum_theme <- function(p, font_size = 14){
  p +
    theme_ipsum(plot_title_size = font_size,
                subtitle_size = font_size * 0.8,
                axis_title_size = font_size * 0.7,
                axis_text_size = font_size * 0.6,
                caption_size = font_size * 0.7,
                plot_margin = unit(c(0.2,0.2,0.2,0.2), "cm")) +
    theme(title = element_blank(), legend.position = "top")
}

tpr <- function(act, pred, cutoff){
  pred_right <- as.numeric(pred >= cutoff)
  sum(act * pred_right)/sum(act)
}

fpr <- function(act, pred, cutoff){
  pred_right <- as.numeric(pred >= cutoff)
  sum((1 - act) * pred_right)/sum((1 - act))
}

accuracy <- function(act, pred, cutoff){
  pred_right <- as.numeric(pred >= cutoff)

  num <- sum(act * pred_right) + sum((1 - act) * (1 - pred_right))
  den <- length(act)

  num/den
}

precision <- function(act, pred, cutoff){
  pred_right <- as.numeric(pred >= cutoff)
  tp <- sum(act * pred_right)
  fp <- sum((1 - act) * pred_right)

  tp/(tp + fp)
}

recall <- function(act, pred, cutoff){
  pred_right <- as.numeric(pred >= cutoff)
  tp <- sum(act * pred_right)
  fn <- sum(act * (1 - pred_right))

  tp/(tp + fn)
}

sensitivity <- function(act, pred, cutoff){
  pred_right <- as.numeric(pred >= cutoff)
  tp <- sum(act * pred_right)
  fn <- sum(act * (1 - pred_right))

  tp/(tp + fn)
}

specificity <- function(act, pred, cutoff){
  pred_right <- as.numeric(pred >= cutoff)
  tn <- sum((1 - act) * (1 - pred_right))
  fp <- sum((1 - act) * pred_right)

  tn/(tn + fp)
}

f1_score <- function(act, pred, cutoff){
  pred_right <- as.numeric(pred >= cutoff)

  rec <- recall(act, pred, cutoff)
  prec <- precision(act, pred, cutoff)

  2 * (prec * rec)/(prec + rec)
}

cum_bad_rate <- function(act, pred, cutoff, bad = 0){
  df <- data.frame(act, pred, cutoff)
  df %>%
    arrange(desc(pred)) %>%
    mutate(check = pred >= cutoff) %>%
    filter(check == TRUE) %>%
    summarise(event_rate = sum(act == bad)/n(),
              pop_rate = n()/nrow(df))
}

 capture_rate <- function(act, pred, cutoff, bad = 0){
  df <- data.frame(act, pred, cutoff)
  total_bads <- sum(df$act == bad)

  df %>%
    arrange(desc(pred)) %>%
    mutate(check = pred <= cutoff) %>%
    filter(check == TRUE) %>%
    summarise(capture_rate = sum(act == bad)/total_bads)
}


check_if_binary <- function(vec){
  if(sum(unique(vec) %in% c(0, 1)) != 2)
    cli_abort("Only binary outcomes (0/1) are supported!")
}

IV <- function(act, var, method = "quantile", nBins = 10){

  # Check if binary
  check_if_binary(act)

  # Bin as per chosen method
  if(method == "quantile"){
    qtile <- unique(quantile(var, probs = seq(0, 1, length.out = nBins, type = 3)))

    # Warn if quantiling didn't work as expected
    if(length(qtile) != nBins) cli_warn("Quantiles are not unique!")

    bins <- cut(var, breaks = qtile, include.lowest = T, right = T, ordered_result = T)

    dt <- data.table(bins, act)
    dt <- dt[,.(.N), by = .(bins, act)]
    dt <- dcast(data = dt, formula = bins ~ act, value.var = "N")

    dt <- dt[order(bins), a := cumsum(`0`) / sum(`0`)]
    dt <- dt[,b := cumsum(`1`) / sum(`1`)]

    dt[,iv := (a-b)*(log(a/b))]
    print(dt)
    sum(dt$iv)

  }

}
