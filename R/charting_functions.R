gains_chart <- function(obj, font_size = 14){

  df <- attr(obj, "data")
  fact <- max(df$Pop.Pct) / max(df$Event.Rate)


  p <- df %>%
    mutate(x = row_number()) %>%
    ggplot(., aes(x = x)) +
    geom_col(aes(y = Pop.Pct, fill = "Population%"), color = "black") +
    geom_line(aes(y = Event.Rate * fact, color = "Event Rate"), size = 1.5) +
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
         y = "Population Distribution") +

    scale_color_manual(values = c("Event Rate" = "#D72323")) +
    scale_fill_manual(values = c("Population%" = "#EDEDED"))

  ipsum_theme(p, font_size)
}

roc_chart <- function(act, pred, font_size = 14){

  cutoffs <- seq(min(pred), max(pred), length.out = 100)
  y <- sapply(cutoffs, function(x){tpr(act, pred, x)})
  x <- sapply(cutoffs, function(x){fpr(act, pred, x)})
  area <- ROCR::performance(ROCR::prediction(pred, act),"auc")@y.values[[1]]

  p <- data.frame(c = round(cutoffs, 3), x = x, y = y) %>%
    mutate(c = ifelse(row_number() %% 10 == 0, c, "")) %>%

    ggplot(aes(x = x)) +
    geom_line(aes(y = y, color = "Model"), size = 1.5) +
    geom_line(aes(y = x, color = "Random"), size = 1.5, linetype = 2) +

    geom_text(aes(y = y, label = c), position = position_nudge(x = 0, y = 0.05)) +

    labs(title = "ROC Curve",
         subtitle = paste("AUROC:", round(area, 3)),
         caption = "Solid line should be above the dotted line",
         x = "False Positive Rate (1 - Specificity)",
         y = "True Positive Rate (Sensitivity)") +

    scale_color_manual(values = c("Model" = "#D72323", "Random" = "black"))

  ipsum_theme(p, font_size)
}

ks_chart <- function(act, pred, font_size = 14){
  cutoffs <- seq(min(pred), max(pred), length.out = 100)
  y <- sapply(cutoffs, function(x){tpr(act, pred, x)})
  x <- sapply(cutoffs, function(x){fpr(act, pred, x)})
  max_ks <- round(max(abs(y - x)), 3)
  max_c <- cutoffs[which.max(abs(y - x))]


  p <- data.frame(x = x, y = y, c = cutoffs) %>%

    ggplot(aes(x = c)) +

    geom_line(aes(y = y, color = "TPR"), size = 1.5) +
    geom_line(aes(y = x, color = "FPR"), size = 1.5) +

    geom_vline(xintercept = max_c, linetype = 2, size = 1) +

    labs(title = "TPR / FPR Curve",
         subtitle = paste("Max KS:", max_ks, "(Cutoff:", round(max_c, 3), ")"),
         caption = "The maximum difference beetween the TPR and the FPR curves is reported as the value of the KS statistic",
         x = "Cutoff",
         y = "TPR + FPR") +

    scale_color_manual(values = c("FPR" = "#D72323", "TPR" = "#297F87"))

  ipsum_theme(p, font_size)
}

accuracy_recall_chart <- function(act, pred, font_size = 14){

  cutoffs <- seq(min(pred), max(pred), length.out = 100)
  y1 <- sapply(cutoffs, function(x){accuracy(act, pred, x)})
  y2 <- sapply(cutoffs, function(x){recall(act, pred, x)})

  d_diff <- abs(y1 - y2)
  min_c <- round(cutoffs[which.min(d_diff)],3)

  acc_x <- cutoffs[which.max(y1)]
  acc_y <- max(y1)

  rec_x <- cutoffs[which.min(y2)]
  rec_y <- min(y2)


  p <- data.frame(x = cutoffs, acc = y1, rec = y2) %>%

    ggplot(aes(x = x)) +

    geom_line(aes(y = acc, color = "Accuracy"), size = 1.5) +
    geom_line(aes(y = rec, color = "Recall"), size = 1.5) +

    labs(title = "Accuracy vs Recall",
         x = "Cutoff",
         y = "Accuracy + Recall") +
    scale_color_manual(values = c("Accuracy" = "#297F87", "Recall" = "#D72323"))

  ipsum_theme(p, font_size)
}

precision_recall_chart <- function(act, pred, font_size = 14){
  cutoffs <- seq(min(pred), max(pred), length.out = 100)

  prec <- sapply(cutoffs, function(x){precision(act, pred, x)})
  rec <- sapply(cutoffs, function(x){recall(act, pred, x)})

  p <- data.frame(x = cutoffs, prec = prec, rec = rec) %>%

    ggplot(aes(x = x)) +

    geom_line(aes(y = prec, color = "Precision"), size = 1.5) +
    geom_line(aes(y = rec, color = "Recall"), size = 1.5) +

    labs(title = "Precision vs Recall",
         x = "Cutoff",
         y = "Precision + Recall") +

    scale_color_manual(values = c("Precision" = "#297F87", "Recall" = "#D72323"))

  ipsum_theme(p, font_size)
}

accuracy_precision_chart <- function(act, pred, font_size = 14){
  cutoffs <- seq(min(pred), max(pred), length.out = 100)

  prec <- sapply(cutoffs, function(x){precision(act, pred, x)})
  acc <- sapply(cutoffs, function(x){accuracy(act, pred, x)})

  p <- data.frame(x = cutoffs, prec = prec, acc = acc) %>%

    ggplot(aes(x = x)) +

    geom_line(aes(y = prec, color = "Precision"), size = 1.5) +
    geom_line(aes(y = acc, color = "Accuracy"), size = 1.5) +

    labs(title = "Accuracy vs Precision",
         x = "Cutoff",
         y = "Accuracy + Precision") +

    scale_color_manual(values = c("Accuracy" = "#297F87", "Precision" = "#D72323"))

  ipsum_theme(p, font_size)
}

sensitivity_specificity_chart <- function(act, pred, font_size = 14){
  cutoffs <- seq(min(pred), max(pred), length.out = 100)

  sens <- sapply(cutoffs, function(x){sensitivity(act, pred, x)})
  spec <- sapply(cutoffs, function(x){specificity(act, pred, x)})

  p <- data.frame(x = cutoffs, sens = sens, spec = spec) %>%

    ggplot(aes(x = x)) +

    geom_line(aes(y = sens, color = "Sensitivity"), size = 1.5) +
    geom_line(aes(y = spec, color = "Specificity"), size = 1.5) +

    labs(title = "Sensitivity vs Specificity",
         x = "Cutoff",
         y = "Sensitivity + Specificity") +

    scale_color_manual(values = c("Sensitivity" = "#297F87", "Specificity" = "#D72323"))

  ipsum_theme(p, font_size)
}

f1_score_chart <- function(act, pred, font_size = 14){
  cutoffs <- seq(min(pred), max(pred), length.out = 100)

  f1 <- sapply(cutoffs, function(x){f1_score(act, pred, x)})

  p <- data.frame(x = cutoffs, f1 = f1) %>%

    ggplot(aes(x = x)) +

    geom_line(aes(y = f1, color = "F1 Score"), size = 1.5) +

    labs(title = "F1 Score",
         x = "Cutoff",
         y = "F1 Score") +

    scale_color_manual(values = c("F1 Score" = "#297F87"))

  ipsum_theme(p, font_size)
}
