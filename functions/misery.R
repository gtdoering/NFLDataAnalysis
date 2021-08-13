#' misery
#' @description This function is used to produce a measurement for 
#' 
#' @param wins Total number of wins for a team in a single season
#' @param total_yards Total amount of yards for a team in a single season
#'
#' @return The returned value is a weighted misery value 0-10 for how hard it was
#' as a fan to watch the team in that season. 10 is the most miserable.
#' 
#' @export misery
#'
#' @examples
#' misery(wins = 7, total_yards = 5765)
misery <- function(wins, total_yards){
  misery <- vector()
  for (i in 1:length(wins)) {
    
    if(total_yards[i] > 6653){
      misery[i] = 0
    }else{
      misery[i] = 3 - (total_yards[i]-4400)/2653 * 3
    }
    
    if(wins[i] > 16){
      misery[i] = misery[i] 
    }else{
      misery[i] = misery[i] + (7 - (wins[i] / 16)*7)
    }
  }
  
  signif(misery, 3)
}
