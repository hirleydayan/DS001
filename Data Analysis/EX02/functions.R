mymin <- function(...){
    values <- c(...)
    if (length(values) > 0){
        m <- values[1]
        for (i in values){
            m <- ifelse(m > i, i, m)
        }
        return(m)
    }
}

mysubset <- function(set1, set2){
    all(set1 %in% set2)
}

myindex <- function(e, set){
    range <- c(1:length(set))
    idx <- set == e
    r <- idx * range
    r[r!=0]
}
