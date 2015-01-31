countinv <- function(x) {
  if (length(x) == 1) {
    return (0)
  } else {
    h <- length(x) %/% 2
    g <- h + 1
    a <- x[1:h]
    b <- x[g : length(x)]
    sa <- sort(a)
    sb <-sort(b)
    isa <- 1
    isb <- 1
    lsa <- length(sa)
    lsb <- length(sb)
    count <-0
    while (isa <= lsa && isb <= lsb){
      if (sa[isa] > sb[isb]) {
        count <- count + lsa - isa + 1
        isb <- isb + 1
      } else {
        isa <- isa + 1
      }
    }
    return (count + countinv(a) + countinv(b))
  } 
  
}
