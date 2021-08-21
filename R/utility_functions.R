# Function to make KS tables pretty
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
  c_width <- cli::console_width()
  msg <- paste("\n", msg, paste0(rep("", c_width - nchar(msg)), collapse = collap), "\n\n")
  cat(crayon::bgWhite(crayon::black(crayon::bold(msg))))
}

ROC <- function(act, pred){
  pred = ROCR::prediction(pred, act)
  # perf = ROCR::performance(pred,"tpr","fpr")
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

