
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

predict <- function(rating.a, rating.b) {
  1 / (1 + 10 ^ ((rating.b - rating.a) / 400))
}

collect.players <- function(matches) {
  unique(c(matches$WhiteNameHash, matches$BlackNameHash))
}

update.ratings <- function(rating.a, rating.b, score.a, score.b, k) {
  irating.a <- 10 ^ (rating.a / 400)
  irating.b <- 10 ^ (rating.b / 400)
  
  expected.a <- irating.a / (irating.a + irating.b)
  expected.b <- irating.b / (irating.a + irating.b)
  
  newrating.a <- rating.a + (k * (score.a - expected.a))
  newrating.b <- rating.b + (k * (score.b - expected.b))
  
  c(newrating.a, newrating.b)
}

rating.boltzman <- function(r) {
  p <- 1 - (1 / ( 1 + exp((0.00583 * r)  - 0.0505)))
  return(p)
}

rating.linear <- function(r) {
  p <- 0.5 + (0.001 * r)
}