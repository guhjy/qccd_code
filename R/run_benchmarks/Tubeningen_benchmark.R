#!/usr/bin/Rscript

rm(list = ls())
library(rvinecopulib)
library(Hmisc)
library(statmod)
source("./others/Slope/utilities.R")
source("./cd_methods.R")
library(acepack)


# This code for reading in data is borrowed from 
# Marx, A. and Vreeken, J. Telling Cause from Effect using MDL-based Local and Global Regression.
# In ICDM, 2017
uv = c(1:51,56:70,72:104,106)
uvnd = c(1:46, 48:51,56:69,72:104,106)

my.file.rename <- function(from, to) {
  todir <- dirname(to)
  if (!isTRUE(file.info(todir)$isdir)) dir.create(todir, recursive=TRUE)
  file.copy(from = from,  to = to)
}

ref = read.table("../data/tuebingen_benchmark/README_polished.tab", sep="\t", header=F, stringsAsFactors = F)
meta = read.table("../data/tuebingen_benchmark/pairmeta.txt")
ref.uv = ref[uv, ]
ref.uvnd = ref[uvnd, ]

readI = function(i, r=ref){
  f = paste(c("../data/tuebingen_benchmark/", r$V1[i], ".txt"), collapse="")
  t = read.table(f, sep=" ", header=F, stringsAsFactors = F)
  return(t)
}




QCCDwrap_tu <- function(X, Y){
  
  res = QCCDWrapper(cbind(X,Y))
  if(!is.na(res$cd)) {
  cd = ifelse(res$cd == 1, "->", "<-")
  } else{
    cd = "--"
  }
  
  list(cd = cd, eps = res$eps)
}



  cnt = 0
  pair = rep("", length(uvnd))
  eps = rep(0, length(uvnd))
  cds = rep("--", length(uvnd))
  time = rep(0, length(uvnd))
for(i in 1:length(uv)){
 
  t1 = Sys.time()
 
  
  t = readI(uv[i])[,1:2]
  
    res = QCCDwrap_tu(t[,1], t[,2])
  
  t2 = Sys.time()
  elapsed = as.numeric(difftime(t2,t1), units="secs")
  pair[i] = uv[i]
  eps[i] = res$eps
  cds[i] = res$cd
  time[i] = elapsed
  
  }
corr = rep(0,length(uv))
corr[ref.uv$V6 == cds] = 1
print(sum(corr))
print(sum(cds == "--"))




resCopula = data.frame(Correct = corr, Eps = eps, Cds = cds, T = time)
write.table(resCopula, file = "../results/QCCD_Tueb.tab", row.names = F, quote = F, sep="\t")

print ("done vinecopula")

