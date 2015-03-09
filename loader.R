
pgn.parserCreate <- function() {
  list(
    matches=list(),
    properties=list(),
    moves=list(),
    states = factor(c(
      "init",
      "open.bracket",
      "close.bracket",
      "open.quote",
      "close.quote",
      "move.index",
      "move.desc",
      "move.final",
      "property.name",
      "property.value")),
    
    handle.line = function(l) {
      message(l)
    }
  )
}

pgn.load <- function(filename) {
  con <- file(filename, open="r")
  lines <- readLines(con)
  
  
  parser <- pgn.parserCreate()
  sapply(lines, parser$handle.line)
  
  close(con, type="rw")
  flush(con)
  
  parser
}

init <- function() {
  
  # load in all the potential PGN file names
  files <- list.files("championships", full.names=TRUE)
  files2 <- files[grepl(".*pgn", files)]
  lapply(files2, pgn.load)
}
