#load("EmailsDist.rda")

predNeighbor = function(k, distMat, indSpam) {
  ordering = t(apply(distMat, 1, FUN = order))
  votes = apply(ordering, 1, FUN = function(orderedDist, indSpam, k) {
                                      sum(indSpam[orderedDist][1:k]) / k
                                   }, 
                indSpam = indSpam, k = k)
  return (votes)
}

cvKnn = function(distances, isSpam, k, v = 5) {
  n = nrow(distances)
  permRows = sample(1:n, floor(n/v) * v)
  folds = matrix(permRows, byrow = TRUE, nrow = v)
  
  vs = matrix(0, nrow = n, ncol = length(k))
  for (j in 1:length(k)) {
    for (i in 1:v) {
      vs[folds[i,], j] = predNeighbor(k = k[j], 
                                      distMat = distances[folds[i,], folds[-i,]], 
                                      indSpam = isSpam[folds[-i,]]) 
    } 
  }
  return(vs) 
}

#spamPred = cvKnn(distances = distEmails, isSpam = isSpam, k = 1:20, v = 10)

plotErrorRates = function(cutoff, spamTruth, spamPred) {
  errors = apply(spamPred, 2, function(spamPred, spamTruth) { 
                      spamPred = as.numeric(spamPred > cutoff)
                      assess = table(spamPred, spamTruth)
                      errs1 = assess[2, 1] / sum(assess[, 1])
                      errs2 = assess[1, 2] / sum(assess[, 2])
                      errors = data.frame(errs1, errs2)
                    },
                 spamTruth = spamTruth)
  errs1 = sapply(errors, function(el) el[[1]])
  errs2 = sapply(errors, function(el) el[[2]])
  errors = data.frame(errs1, errs2)
  
  plot(x = 1:ncol(spamPred), y = errs1, type = "l", 
       main = "Type 1 and Type 2 Error Rates \nfor K-nearest neighbor Predictions", 
       xlab = "k", ylab = "error rates", ylim = c(0, 0.5), col = "red")
  lines(errs2, col = "green")
  legend("topleft", legend = c("type 1 error", "type 2 error"), fill = c("red", "green"))
  return (data.frame(errs1, errs2))
}

#plotErrorRates(cutoff = 0.6, isSpam, spamPred)

#Type 2 error (Spam that is falsely predicted as Ham) increases with higher values of k. 
#Meanwhile type 1 error remains relatively low across all values of k, stablizing around .025 for higher k.
#In this case, type 1 error, although less common, is much more serious as we want to avoid missing any Ham
#emails that may be important. Thus an optimal k is around 4 (type 1 error begins to stablize, type 2 error
#still barely above .3) where we only miss 2.5% of Ham while filtering out nearly 70% of Spam.
  