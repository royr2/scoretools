#' Generate scores using a linear transformation
#'
#' @description
#' Function to convert log odds to an easy to interpret numeric score.
#'
#' @details
#' Scores are generated as linear trnasformation of log odds such that \eqn{score = \alpha + \beta \times LogOdds}.
#' Where,
#' \deqn{\alpha = PDO/log(scale)}
#' \deqn{\beta - Anchor - \alpha * OddsAtAnchor}
#'
#' @param logodds
#' A numeric vector of LogOdds
#'
#' @param points_to_scale
#' An integer specifying the number of points(score points i.e.) which would trigger a 'x' times change in log odds
#'
#'@param scale
#' An integer specifying the factor why which \code{points_to_scale} will change log odds. (Ex: 2 would mean doubling of odds)
#'
#' @param anchor
#' An integer used to anchor the score output. See \code{details}
#'
#' @param odds_at_anchor
#' A numeric value specifying the log odds at the anchor point
#'
#' @param plot
#' A True/False value specifying if the function should plot a histogram of the generated scores
#'
#' @export linear_scale
linear_scale <- function(logodds, points_to_scale = 20, scale = 2, anchor = 700, odds_at_anchor = 0.1, plot = T){
  a <- points_to_scale / log(scale)
  b <- anchor - (points_to_scale/log(scale) * odds_at_anchor)

  scores <- a * logodds + b

  if(plot == TRUE){
    hist(scores)
  }

  return(scores)
}
