#' qbr
#' 
#' @description Function to calculate the qbr for a team in a single game. 
#'
#' @param completions The number of completed passes for a team in a single game.
#' @param passing_yards The number of total passing yards for a team in a single game.
#' @param pass_touchdowns The number of total passing touchdowns for a team in a single game.
#' @param interceptions The number of interceptions thrown by a team in a single game.
#' @param passes_attempted The number of passes attempted by a team in a single game.
#'
#' @return The value returned is the qbr for a team based off the inputted statistics.
#' @export qbr
#'
#' @examples
#' qbr(completions = 23,
#'     passing_yards = 245,
#'     pass_touchdowns = 2,
#'     interceptions = 1,
#'     passes_attempted = 28)
qbr <- function(completions, passing_yards , pass_touchdowns, interceptions, 
                passes_attempted){
  
  a = ((completions/passes_attempted - .3) * 5)
  b = ((passing_yards/passes_attempted - 3) * .25)
  c = ((pass_touchdowns/passes_attempted) * 20)
  d = (2.375 - (interceptions/passes_attempted * 25))
  
  a = dplyr::case_when(
    a >= 2.375 ~ 2.375,
    a >= 0 ~ a,
    a < 0 ~ 0
  )
  b = dplyr::case_when(
    b >= 2.375 ~ 2.375,
    b >= 0 ~ b,
    b < 0 ~ 0
  ) 
  c = dplyr::case_when(
    c >= 2.375 ~ 2.375,
    c >= 0 ~ c,
    c < 0 ~ 0
  )
  d = dplyr::case_when(
    d >= 2.375 ~ 2.375,
    d >= 0 ~ d,
    d < 0 ~ 0
  )
  qbr = signif((a + b + c +d)/6 * 100,4)
  
  return(qbr)
}

