# run this script from the R/ folder in this project

rm(list = ls())

library(dplyr)
source("./cd_methods.R")
path = "../data/SIM_pairs/"
ext = "SIM-G/" # SIM/ ; SIM-ln/; SIM-G/; SIM-c/;
pairmeta <- read.table(paste0(path, ext,"pairmeta.txt"),quote = "\"")
file.names <- dir(paste0(path,ext), pattern = ".txt")

ce_gt <- data.frame(pairmeta[, 2], pairmeta[, 5]) # the ground truth
ce_gt = ce_gt[,1]
n_pairs = 100
ce_gt[ce_gt == 2] = 0



runSIM <- function(method){
  count_corr = 0
  na = 0
  
  pair_idx = rep("", n_pairs)
  eps = rep(0, n_pairs)
  cds = rep("--", n_pairs)
  time = rep(0, n_pairs)
  eps = rep(0, n_pairs)
  Correct = rep(0, n_pairs)
  n_corr = 0
  results = matrix(0,nrow = n_pairs, ncol = 2)
  
  method_to_run = switch(method,
                         "QCCD" = QCCD)  
  
  for (i in 1:100)
  {
    correct = 0
    gt = ce_gt[i]
    print(paste("Pair #",i))
    t1 = Sys.time()
    X <- read.table(paste0(path, ext, file.names[i]),quote = "\"")
    plot(X)
  
    
    res = method_to_run(X)
    t2 = Sys.time()
    elapsed = as.numeric(difftime(t2, t1), units = "secs")
    pair_idx[i] = i
    eps[i] = res$epsilon
    cds[i] = res$cd
    time[i] = elapsed
    
    # dummy prints for debug
    print(res$cd)
    print(gt)
    print("-----")
    results[i,1] = res$cd
    
    if(!is.na(res$cd)){
      correct = (res$cd == gt )
      n_corr = n_corr + correct
    }
    print(paste("correct =", n_corr))
  }
  
  ncorrect = sum(results[,1]  == ce_gt )
  
  print(paste("correct =", n_corr, "mean =", ncorrect))
  
  corr = rep(0, n_pairs)
  corr[ce_gt == cds] = 1
  resDF = data.frame(Correct = corr, Eps = eps, Cds = cds,T = time)
  write.table(resDF,file = paste0("../results/",method,"_",substr(ext,1,nchar(ext)-1),".tab"),row.names = F, quote = F, sep = "\t")
  acc = ncorrect/100
  acc
}

methods <- list("QCCD")
resultsAll <- lapply(methods, function (m) runSIM(m))
unlist(resultsAll)