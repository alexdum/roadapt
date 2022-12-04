#https://stackoverflow.com/questions/6461209/how-to-round-up-to-the-nearest-10-or-100-or-x
round_even <- function(x, roundTo, dir = 1) {
  if(dir == 1) {  ##ROUND UP
    x + (roundTo - x %% roundTo)
  } else {
    if(dir == 0) {  ##ROUND DOWN
      x - (x %% roundTo)
    }
  }
}

# round to the next even naumber

round_any <- function(x, accuracy, f=round){
  f(x/ accuracy) * accuracy
}

