
#' @export
recode <- function(x, fromto) {
  UseMethod("recode", x)
}

#' @export
recode.haven_labelled <- function(x, fromto) {
  x_new <- x
  mostattributes(x_new) <- attributes(x)
  
  l <- attr(x_new, "labels", exact = T)[]
  
  attr(x_new, "labels")[] <- recode(l, fromto)
  
  x_new[] <- structure(.Data = recode(unclass(x_new), fromto), class = class(x_new))
  
  x_new
}

#' @export
recode.numeric <- function(x, fromto) {
  
  x_new <- x
  mostattributes(x_new) <- attributes(x)
  
  if(is.list(fromto)) {
    for(entry in fromto) {
      x_new[x == entry$from] <- entry$to
    }  
  } else if(is.vector(fromto)) {
    keys <- names(fromto)
    values <- fromto
    
    for(key in keys) {
      x_new[x == key] <- fromto[[key]]
    }
  } else {
    stop("Unknown type fromto: ", class(fromto), call. = T)
  }
  
  
  x_new
}