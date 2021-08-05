# Function to make KS tables pretty
prettify_ks_table <- function(tab, accr = 0.01){
  tab$Pop_Pct <- scales::percent(tab$Pop_Pct, accuracy = accr)
  tab$Event.Rate <- scales::percent(tab$Event.Rate, accuracy = accr)
  tab$KS <- scales::number(tab$KS * 100, accuracy = accr)
  tab$Capture.Rate <- scales::percent(tab$Capture.Rate, accuracy = accr)
  tab$Cum.Event.Rate <- scales::percent(tab$Cum.Event.Rate, accuracy = accr)
  tab$Cum.Events.Dist <- scales::percent(tab$Cum.Events.Dist, accuracy = accr)
  tab$Cum.N.Events.Dist <- scales::percent(tab$Cum.N.Events.Dist, accuracy = accr)

  return(tab)
}

# Function to print something to screen whita white background utilising the full console width
# print_bg <- function(msg = "", collap = " "){
#   c_width <- cli::console_width()
#   msg <- paste("\n", msg, paste0(rep("", c_width - nchar(msg)), collapse = collap), "\n\n")
#   cat(crayon::bgWhite(crayon::black(crayon::bold(msg))))
# }

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
    theme(legend.position = "top-right")
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

precision_chart <- function(act, pred){
  cutoffs <- seq(min(pred), max(pred), length.out = 1000)
  y <- sapply(cutoffs, function(x){precision(act, pred, x)})

  plot(cutoffs, y, type = 'l')
}

recall_chart <- function(act, pred){
  cutoffs <- seq(min(pred), max(pred), length.out = 1000)
  y <- sapply(cutoffs, function(x){recall(act, pred, x)})

  plot(cutoffs, y, type = 'l')
}

precision_recall_chart <- function(act, pred, base_size = 10){
  cutoffs <- seq(min(pred), max(pred), length.out = 1000)

  prec <- sapply(cutoffs, function(x){precision(act, pred, x)})
  rec <- sapply(cutoffs, function(x){recall(act, pred, x)})

  plot(cutoffs, prec, type = 'l', ylim = c(0, 1))
  lines(cutoffs, rec, type = 'l')
}

gains_chart <- function(obj, font_size = 14){

  df <- attr(obj, "data")
  # max_ks <- attr(obj, "max_ks")
  # auroc <- attr(obj, "roc")
  # gini <- attr(tab, "gini")

  # if(max(df$Pop_Pct) > df$Event.Rate){
  #   fact = 1/(max(df$Pop_Pct)/max(df$Event.Rate)) / 1.2
  # }else{
  #   fact = max(df$Pop_Pct)/max(df$Event.Rate) / 1.2
  # }

  fact <- max(df$Pop_Pct) / max(df$Event.Rate)


  p <- df %>%
    mutate(x = row_number()) %>%
    ggplot(., aes(x = x)) +
    geom_col(aes(y = Pop_Pct), fill = "#E1E5EA", color = "black") +
    geom_line(aes(y = Event.Rate * fact), size = 2, color = "#393E46", alpha = 0.8) +
    geom_point(aes(y = Event.Rate * fact), shape = 21, size = 3, color = "black", fill = "#E1E5EA") +
    geom_text(aes(y = Event.Rate * fact, label = scales::percent(Event.Rate)),
              position = position_nudge(x = 0.005, y = 0.005),
              color = "black") +
    scale_y_continuous(labels = scales::percent,
                       sec.axis = sec_axis(~ . / fact,
                                           name="Event Rate",
                                           labels = scales::percent)) +
    scale_x_continuous(breaks = 1:nrow(df), labels = as.character(df$Bins)) +

    labs(title = "Gains Chart",
         subtitle = "Population distribution and event rate trend",
         caption = "Pop. dist. should be consistent and event rate trend should be monotonic",
         x = "Bins",
         y = "Population Distribution")
  ipsum_theme(p, font_size)
}

roc_chart <- function(act, pred, font_size = 14){

  cutoffs <- seq(min(pred), max(pred), length.out = 1000)
  y <- sapply(cutoffs, function(x){tpr(act, pred, x)})
  x <- sapply(cutoffs, function(x){fpr(act, pred, x)})
  area <- ROCR::performance(ROCR::prediction(pred, act),"auc")@y.values[[1]]

  y_coord <- max(y[near(x, 0.5, tol = 0.01)])

  p <- data.frame(c = round(cutoffs, 3), x = x, y = y) %>%
    mutate(c = ifelse(row_number() %% 100 == 0, c, "")) %>%

    ggplot(aes(x = x)) +
    geom_line(aes(y = y), size = 1.2, color = "#393E46", alpha = 0.8) +
    geom_line(aes(y = x), size = 1.2, color = "#393E46", alpha = 0.8, linetype = 2) +

    geom_text(aes(x = 0.55, y = 0.5, label = "Random Model")) +
    geom_text(aes(x = 0.5, y = y_coord, label = "Actual Model")) +
    geom_text(aes(y = y, label = c)) +

    labs(title = "ROC Curve",
         subtitle = paste("Area:", round(area, 3)),
         caption = "Solid line should be above the dotted line. If below, event definition might be inverted",
         x = "False Positive Rate (1 - Specificity)",
         y = "True Positive Rate (Sensitivity)")

  ipsum_theme(p, font_size)
}

ks_chart <- function(act, pred, font_size = 14){
  cutoffs <- seq(min(pred), max(pred), length.out = 1000)
  y <- sapply(cutoffs, function(x){tpr(act, pred, x)})
  x <- sapply(cutoffs, function(x){fpr(act, pred, x)})
  max_ks <- round(max(abs(y - x)), 3)
  max_c <- cutoffs[which.max(abs(y - x))]


  colors <- c("TPR" = "#3fbf5f", "FPR" = "#d65867")

  p <- data.frame(x = x, y = y, c = cutoffs) %>%

    ggplot(aes(x = c)) +

    geom_line(aes(y = y), linetype = 1, size = 1.2) +
    geom_line(aes(y = x), linetype = 1, size = 1.2) +

    geom_vline(xintercept = max_c, linetype = 2, size = 1) +

    labs(title = "TPR / FPR Curve",
         subtitle = paste("Max KS:", max_ks, "(Cutoff:", round(max_c, 3), ")"),
         caption = "The maximum difference beetween the TPR and the FPR curves is reported as the value of the KS statistic",
         x = "Cutoff",
         y = "TPR + FPR") +
    scale_color_manual(values = colors)

  ipsum_theme(p, font_size)
}

accuracy_recall_chart <- function(act, pred, font_size = 14){
  cutoffs <- seq(min(pred), max(pred), length.out = 1000)
  y1 <- sapply(cutoffs, function(x){accuracy(act, pred, x)})
  y2 <- sapply(cutoffs, function(x){recall(act, pred, x)})
  y3 <- sapply(cutoffs, function(x){precision(act, pred, x)})

  d_diff <- abs(y1 - y2)
  min_c <- round(cutoffs[which.min(d_diff)],3)

  acc_x <- cutoffs[which.max(y1)]
  acc_y <- max(y1)

  rec_x <- cutoffs[which.min(y2)]
  rec_y <- min(y2)


  p <- data.frame(x = cutoffs, acc = y1, rec = y2, prec = y3) %>%

    ggplot(aes(x = x)) +

    geom_line(aes(y = acc), size = 1.2, color = "#393E46", alpha = 0.8) +
    geom_line(aes(y = rec), size = 1.2, color = "#393E46", alpha = 0.8) +
    geom_line(aes(y = prec), size = 1.2, color = "#393E46", alpha = 0.8) +

    geom_text(aes(x = acc_x, y = acc_y, label = "Accuracy"), nudge_y = 0.1) +
    geom_text(aes(x = rec_x, y = rec_y, label = "Recall"), nudge_y = 0.1) +

  labs(title = "Accuracy vs Recall",
       subtitle = paste0("Minimum abs. diff at (Cutoff:", min_c, ")"),
       caption = "The minimum absolute difference between accuracy and recall curves is reported",
       x = "Cutoff",
       y = "Accuracy + Recall")

  ipsum_theme(p, font_size)
}

