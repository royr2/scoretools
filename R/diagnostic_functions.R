#' Generate a Gains table
#'
#' @description
#' Function generates a gains table that is typically used to assess the performance of scorecards.
#' The primary outputs to look at would be:
#'
#'\enumerate{
#' \item Event rate trend
#' \item Value of the KS statistic
#' \item Capture rates
#' \item Cumulative Event Rates for threshold selection
#' }
#'
#' @details
#' The function relies on provided vectors for observed classes and predictions to create
#' a bin wise summary. Typically, this is done by dividing the population into deciles, or
#' 10 separate bins. While the function supports any number of bins but going beyond 20 bins
#' is not recommended since individual computations like event rates become unreliable.
#'
#' The KS statistic is the primary performance measure when using a gains table. The function
#' also computes the AUROC and the Gini coefficient as \code{2*AUROC - 1}.
#'
#' @param act
#' A numeric vector of actual observed classes
#'
#' @param pred
#' A numeric vector of predictions (probabilities/log-odds/scores)
#'
#' @param nBins
#' An integer specifying the number of bins to use (10 = deciles, 20 = semi deciles etc)
#' @param prettify
#' A boolean specifying if the function should return a pretty table (huxtable will be used)
#'
#' @param type
#' A string specifying if event = goods or bads. Output will be inverted accordingly.
#'
#' @return
#' A tibble if \code{prettify = F} else an object of type \code{ks}
#'
#' @export gains_table
#'
#' @examples
#' data("accepted_base")
#' accepted_base[is.na(accepted_base)] <- -999
#'
#'with(accepted_base, {
#'  bad_flag <- ifelse(loan_status == "Charged Off", 1, 0)
#'  mdl <- glm(factor(bad_flag) ~ revol_util + last_pymnt_amnt,
#'             family = "binomial")
#'  gains_table(bad_flag, predict(mdl), nBins = 10, type = "bads")
#'})

gains_table <- function(act, pred, nBins = 10, prettify = T, type = "goods"){

  q <- quantile(pred, probs = seq(0, 1, length.out = nBins + 1))
  q <- unique(q)

  # Check if binary or not
  check_if_binary(act)

  # Warn if quantiles are not unique
  if(length(q) < nBins){
    cli::cli_alert_warning("Quantiles are not unique. Using only unique quantiles.")
  }

  # Cut pred based on quantiles
  pred_binned <- cut(pred, breaks = q, include.lowest = T, right = T, ordered_result = T)

  # Generate gains table
  tab <- tibble(actuals = act,
                predicted = pred,
                Bins = pred_binned) %>%
    group_by(Bins) %>%

    summarise(Total = n(),
              Events = sum(actuals == 1),
              N.Events = sum(actuals == 0),
              Event.Rate = Events / Total) %>%

    {if(type == "goods") arrange(., Bins) else .} %>%
    {if(type == "bads") arrange(., desc(Bins)) else .} %>%

    mutate(Pop.Pct = Total / sum(Total),
           Cum.Events = cumsum(Events),
           Cum.N.Events = cumsum(N.Events),
           Cum.Events.Dist = Cum.Events / sum(Events),
           Cum.N.Events.Dist = Cum.N.Events / sum(N.Events),
           KS = abs(Cum.Events / sum(Events) - Cum.N.Events / sum(N.Events)),
           Capture.Rate = Cum.Events / sum(Events),
           Cum.Event.Rate = (sum(Events) - cumsum(Events))/(sum(Total) - cumsum(Total))) %>%

    select(Bins, Total, Pop.Pct, Events, N.Events, Event.Rate, Cum.Events, Cum.N.Events,
           Cum.Events.Dist, Cum.N.Events.Dist, KS, Capture.Rate, Cum.Event.Rate)

  # Get AUROC
  roc_val <- ROC(act = act, pred = pred)
  gini_val <- 2 * roc_val - 1

  # Set some attributes
  attr(tab, "max_ks") <- scales::number(100 * max(tab$KS), accuracy = 0.01)
  attr(tab, "decile") <- which.max(tab$KS)
  attr(tab, "roc") <- roc_val
  attr(tab, "gini") <- gini_val
  attr(tab, "type") <- type
  attr(tab, "data") <- tab

  # Prettify table if required
  # This also sets a different class with its own print method
  if(prettify == T){
    class(tab) <- c("ks", "data.frame")
  }

  return(tab)
}

#' Generate a diagnostic plot
#'
#' @description
#' Function generates a set of diagnostic plots to help evaluate the performance of a predictive model.
#'
#' @details
#'
#' @param act
#' A numeric vector of actual observed classes
#'
#' @param pred
#' A numeric vector of predictions (probabilities/log-odds/scores)
#'
#' @param font_size
#' A number specifying the general font size to be used in all the charts
#'
#' @export diag_charts
#'
#' @examples
#' data("accepted_base")
#' accepted_base[is.na(accepted_base)] <- -999
#'
#' with(accepted_base, {
#' bad_flag <- ifelse(loan_status == "Charged Off", 1, 0)
#' mdl <- glm(factor(bad_flag) ~ revol_util + last_pymnt_amnt,
#'           family = "binomial")
#' diag_charts(bad_flag, predict(mdl, type = "response"))
#'})
diag_charts <- function(act, pred, font_size = 14){

  p1 <- ks_chart(act, pred, font_size = font_size)
  p2 <- precision_recall_chart(act, pred, font_size = font_size)
  p3 <- sensitivity_specificity_chart(act, pred, font_size = font_size)
  p4 <- f1_score_chart(act, pred, font_size = font_size)

  gridExtra::grid.arrange(p1, p2, p3, p4)

}
