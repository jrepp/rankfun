## OBS
# ba3397ab is not as popular, why?
# db7f7d9c is more pouplar than 7d8ca22e, why?
# mean rank centered at .835 +- .00016

# head(samples, )

compare <- function(sample) {
  # 10: p1_u
  # 11: p2_u
  # 12: p1_s
  # 13: p2_s
  # 14: p1_o
  # 15: p2_o
  absdelta <- as.numeric(sample[[10]]) - as.numeric(sample[[11]])
  if (absdelta > 0) {
    p1_o_expected <- 1
    p2_o_expected <- 0
  } else {
    p1_o_expected <- 0
    p2_o_expected <- 1
  }
  
  pred <- p1_o_expected == as.numeric(sample[[14]])
  
  list(sample=sample, pred=pred, absdelta=absdelta)
}