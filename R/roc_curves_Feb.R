# ROC plot for Causal Copula
# 16.12.2017

rm(list = ls())
library(ROCR)
library(PRROC)
library(readr)
library(naglr)
source("./cd_utils.R")

pdf(width = 9, height=9, file="roc_curves_TEST.pdf")
par(pty = "s")

CausalCopula_ANMs <- read_delim("./../results/CausalCopula_ANM-s_21012018_m3.tab",
                                "\t", escape_double = FALSE, trim_ws = TRUE)
pairs_gt_s <- read_csv("./../data/ANM_pairs/ANM-s/pairs_gt.txt", 
                       col_names = FALSE)
pred <- prediction(CausalCopula_ANMs$Eps, pairs_gt_s$X1)
perf <- performance(pred,"tpr","fpr")
auc_anms <- performance(pred, "auc")@y.values[[1]] # this is from ROCR
pr.curve(scores.class0 = CausalCopula_ANMs$Eps, weights.class0 = pairs_gt_s$X1, curve = T); # PROC
plot(perf, col = "blue", lwd = 2, cex.lab = 1.5, cey.lab = 1.5, type="l", lty=2)


# i need these for the ggplot graph
auc_anms_x = unlist(perf@x.values)
auc_anms_y = unlist(perf@y.values)


CausalCopula_ANM <- read_delim("./../results/CausalCopula_ANM-Gauss_21012018_m3.tab",
                               "\t", escape_double = FALSE, trim_ws = TRUE)
pairs_gt_anm <- read_csv("./../data/ANM_pairs/ANM-Gauss/pairs_gt.txt", 
                         col_names = FALSE)
pred_anm <- prediction(CausalCopula_ANM$Eps, pairs_gt_anm)
perf_anm <- performance(pred_anm,"tpr","fpr")
auc_anm <- performance(pred_anm, "auc")@y.values[[1]]
pr.curve(scores.class0 = CausalCopula_ANM$Eps, weights.class0 = pairs_gt_anm$X1, curve = T);
par(new = TRUE)
plot(perf_anm, col = "steelblue", lwd = 2, cex.lab = 1.5, cey.lab = 1.5, type="l", lty=3)

auc_anm_x = unlist(perf_anm@x.values)
auc_anm_y = unlist(perf_anm@y.values)

CausalCopula_H <- read_delim("./../results/CausalCopula_AN-het-v3_23012018_m7.tab",
                             "\t", escape_double = FALSE, trim_ws = TRUE)
pairs_gt_h <- read_csv("./../data/ANM_pairs/AN-het-v3/pairs_gt.txt", 
                       col_names = FALSE)
pred_h <- prediction(CausalCopula_H$Eps, pairs_gt_h)
perf_h <- performance(pred_h,"tpr","fpr")
auc_h <- performance(pred_h, "auc")@y.values[[1]]
pr.curve(scores.class0 = CausalCopula_H$Eps, weights.class0 = pairs_gt_h$X1, curve = T);
par(new = TRUE)
plot(perf_h, col = "mediumblue", lwd = 2, cex.lab = 1.5, cey.lab = 1.5, type="l", lty=4)

auc_h_x = unlist(perf_h@x.values)
auc_h_y = unlist(perf_h@y.values)

CausalCopula_HS <- read_delim("./../results/CausalCopula_ANs-het_23012018_m7.tab",
                              "\t", escape_double = FALSE, trim_ws = TRUE)
pairs_gt_hs <- read_csv("./../data/ANM_pairs/ANs-het/pairs_gt.txt", 
                        col_names = FALSE)
pred_hs <- prediction(CausalCopula_HS$Eps, pairs_gt_hs)
perf_hs <- performance(pred_hs,"tpr","fpr")
auc_hs <- performance(pred_hs, "auc")@y.values[[1]]
pr.curve(scores.class0 = CausalCopula_HS$Eps, weights.class0 = pairs_gt_hs$X1, curve = T);
par(new = TRUE)
plot(perf_hs, col = "red", lwd = 2, cex.lab = 1.5, cey.lab = 1.5, type="l", lty=4)

auc_hs_x = unlist(perf_hs@x.values)
auc_hs_y = unlist(perf_hs@y.values)


CausalCopula_HSs <- read_delim("./../results/CausalCopula_ANs-het_23012018_m7.tab",
                               "\t", escape_double = FALSE, trim_ws = TRUE)
pairs_gt_hs <- read_csv("./../data/ANM_pairs/ANs-het/pairs_gt.txt", 
                        col_names = FALSE)
pred_hss <- prediction(CausalCopula_HSs$Eps, pairs_gt_hs)
perf_hss <- performance(pred_hss,"tpr","fpr")
auc_hss <- performance(pred_hss, "auc")@y.values[[1]]
pr.curve(scores.class0 = CausalCopula_HSs$Eps, weights.class0 = pairs_gt_hs$X1, curve = T);
par(new = TRUE)
plot(perf_hs, col = "green", lwd = 2, cex.lab = 1.5, cey.lab = 1.5, type="l", lty=4)

auc_hss_x = unlist(perf_hss@x.values)
auc_hss_y = unlist(perf_hss@y.values)



CausalCopula_sim <- read_delim("./../results/CausalCopula_SIM_20012018_m3.tab", 
                               "\t", escape_double = FALSE, trim_ws = TRUE)
path = "../data/SIM_pairs/"
ext = "SIM/" # SIM/ ; SIM-ln/; SIM-G/; SIM-c/;
pairmeta <- read.table(paste0(path, ext,"pairmeta.txt"),quote = "\"")
file.names <- dir(paste0(path,ext), pattern = ".txt")

ce_gt <- data.frame(pairmeta[, 2], pairmeta[, 5]) # the ground truth
ce_gt = ce_gt[,1]
n_pairs = 100
ce_gt[ce_gt == 2] = 0

pairs_gt_sim <- ce_gt

pred_sim <- prediction(CausalCopula_sim$Eps, pairs_gt_sim)
perf_sim <- performance(pred_sim,"tpr","fpr")
auc_sim <- performance(pred_sim, "auc")@y.values[[1]]
pr.curve(scores.class0 = CausalCopula_sim$Eps, weights.class0 = pairs_gt_sim, curve = T);
par(new = TRUE)
plot(perf_sim, col = "lightsalmon4", lwd = 2, cex.lab = 1.5, cey.lab = 1.5, type = "l", lty=1)

auc_sim_x = unlist(perf_sim@x.values)
auc_sim_y = unlist(perf_sim@y.values)

CausalCopula_simln <- read_delim("./../results/CausalCopula_SIM-ln_21012018_m3.tab", 
                                 "\t", escape_double = FALSE, trim_ws = TRUE)
path = "../data/SIM_pairs/"
ext = "SIM-ln/" # SIM/ ; SIM-ln/; SIM-G/; SIM-c/;
pairmeta <- read.table(paste0(path, ext,"pairmeta.txt"),quote = "\"")
file.names <- dir(paste0(path,ext), pattern = ".txt")

ce_gt <- data.frame(pairmeta[, 2], pairmeta[, 5]) # the ground truth
ce_gt = ce_gt[,1]
n_pairs = 100
ce_gt[ce_gt == 2] = 0

pairs_gt_simln <- ce_gt
CausalCopula_simln$Eps[is.na(CausalCopula_simln$Eps)]= 0.5
pred_simln <- prediction(CausalCopula_simln$Eps, pairs_gt_simln)
perf_simln <- performance(pred_simln,"tpr","fpr")
auc_simln <- performance(pred_simln, "auc")@y.values[[1]]
pr.curve(scores.class0 = CausalCopula_simln$Eps, weights.class0 = pairs_gt_simln, curve = T);
par(new = TRUE)
plot(perf_simln, col = "orange", lwd = 2, cex.lab = 1.5, cey.lab = 1.5, type = "l")

auc_simln_x = unlist(perf_simln@x.values)
auc_simln_y = unlist(perf_simln@y.values)

CausalCopula_simg <- read_delim("./../results/CausalCopula_SIM-G_21012018_m3.tab", 
                                "\t", escape_double = FALSE, trim_ws = TRUE)
path = "../data/SIM_pairs/"
ext = "SIM-G/" # SIM/ ; SIM-ln/; SIM-G/; SIM-c/;
pairmeta <- read.table(paste0(path, ext,"pairmeta.txt"),quote = "\"")
file.names <- dir(paste0(path,ext), pattern = ".txt")

ce_gt <- data.frame(pairmeta[, 2], pairmeta[, 5]) # the ground truth
ce_gt = ce_gt[,1]
n_pairs = 100
ce_gt[ce_gt == 2] = 0

pairs_gt_simg <- ce_gt
CausalCopula_simg$Eps[is.na(CausalCopula_simg$Eps)]= 0.5
pred_simg <- prediction(CausalCopula_simg$Eps, pairs_gt_simg)
perf_simg <- performance(pred_simg,"tpr","fpr")
auc_simg <- performance(pred_simg, "auc")@y.values[[1]]
pr.curve(scores.class0 = CausalCopula_simg$Eps, weights.class0 = pairs_gt_simg, curve = T);
par(new = TRUE)
plot(perf_simg, col = "lightsalmon1", lwd = 2, cex.lab = 1.5, cey.lab = 1.5, type = "l")


auc_simg_x = unlist(perf_simg@x.values)
auc_simg_y = unlist(perf_simg@y.values)





CausalCopula_ANMs_un <- read_delim("./../results/CausalCopula_ANMs-Unif-multi_-11_27012018.tab",
                                   "\t", escape_double = FALSE, trim_ws = TRUE)
pairs_gt_umn <- read_csv("./../data/ANM_pairs/ANMs-Unif-multi_-11/pairs_gt.txt", 
                         col_names = FALSE)
pred <- prediction(CausalCopula_ANMs_un$Eps, pairs_gt_umn$X1)
perf <- performance(pred,"tpr","fpr")
auc_umn <- performance(pred, "auc")@y.values[[1]]
pr.curve(scores.class0 = CausalCopula_ANMs_un$Eps, weights.class0 = pairs_gt_umn$X1, curve = T);
par(new = TRUE)
plot(perf, col = "indianred1", lwd = 2, cex.lab = 1.5, cey.lab = 1.5, type="l", lty=2)

auc_unm_x = unlist(perf@x.values)
auc_unm_y = unlist(perf@y.values)




CausalCopula_ANMs_gmn <- read_delim("./../results/CausalCopula_NM-Gauss-multi_23012018_m3.tab",
                                    "\t", escape_double = FALSE, trim_ws = TRUE)
pairs_gt_gmn <- read_csv("./../data/ANM_pairs/NM-Gauss-multi/pairs_gt.txt", 
                         col_names = FALSE)
pred <- prediction(CausalCopula_ANMs_gmn$Eps, pairs_gt_gmn)
perf <- performance(pred,"tpr","fpr")
auc_gmn <- performance(pred, "auc")@y.values[[1]]
par(new = TRUE)
pr.curve(scores.class0 = CausalCopula_ANMs_gmn$Eps, weights.class0 = pairs_gt_gmn$X1, curve = T);
plot(perf, col = "indianred3", lwd = 2, cex.lab = 1.5, cey.lab = 1.5, type="l", lty=2)


auc_gnm_x = unlist(perf@x.values)
auc_gnm_y = unlist(perf@y.values)


ref.uv$V6_01[ref.uv$V6=="->"]=1
ref.uv$V6_01[ref.uv$V6=="<-"]=0
tuebcop <- read.table("../results/copulaQuant_relm=3_21012018.tab", as.is = TRUE, header = TRUE, sep = "\t")

#tuebcop$Eps[tuebcop$Cds=="<-"] = -tuebcop$Eps[tuebcop$Cds=="<-"]
pred <- prediction(tuebcop$Eps ,ref.uv$V6_01)
perf <- performance(pred,"tpr","fpr")
par(new = TRUE)
plot(perf,col = "red", lwd = 2, cex.lab = 1.5, cey.lab = 1.5, cex.axis = 4, cey.axis = 2, type = "l")
auc_tueb = performance(pred, "auc")@y.values[[1]]

pr.curve(scores.class0 = tuebcop$Eps , weights.class0 = ref.uv$V6_01, curve = T);

roc.curve(scores.class0 = tuebcop$Eps, weights.class0 = ref.uv$V6_01, curve = T);
#pr_tub <- pr.curve(scores.class0 = tuebcop$Eps, weights.class0 = ref.uvnd$V6_01, curve = T);

auc_tueb_x = unlist(perf@x.values)
auc_tueb_y = unlist(perf@y.values)

# random classifier line
abline(0,1 , lty = 2)


legend("bottomright", c("AN-s","AN-G", "AN-t", "AN-n", "SIM", "SIM-ln", "SIM-G", "MN-U", "MN-G", "Tueb-CE" ,"rand. cls"), col = c("blue", "steelblue", "mediumblue", "slateblue2", "lightsalmon4", "orange", "lightsalmon1", "indianred1", "indianred3","red", "black"),lty = 1,  lwd = 2, cex=1.5)

dev.off()


df1 <- data.frame(x = auc_anm_x, y = auc_anm_y, Type = as.factor("AN"))
df2 <- data.frame(x = auc_anms_x, y = auc_anms_y, Type = as.factor("AN-s"))
df3 <- data.frame(x = auc_h_x, y = auc_h_y, Type = as.factor("HS"))
df4 <- data.frame(x = auc_hs_x, y = auc_hs_y, Type = as.factor("HS-s"))
df5 <- data.frame(x = auc_sim_x, y = auc_sim_y, Type = as.factor("SIM"))
df6 <- data.frame(x = auc_simln_x, y = auc_simln_y, Type = as.factor("SIM-ln"))
df7 <- data.frame(x = auc_simg_x, y = auc_simg_y, Type = as.factor("SIM-G"))
df8 <- data.frame(x = auc_unm_x, y = auc_unm_y, Type = as.factor("MN-U"))
df9 <- data.frame(x = auc_gnm_x, y = auc_gnm_y, Type = as.factor("MN-G"))
df10 <- data.frame(x = auc_tueb_x, y = auc_tueb_y, Type = as.factor("Tueb"))

df.merged <- rbind(df1, df2, df3, df4,df5, df6, df7, df8, df9, df10)

ggplot(df.merged, aes(x, y, colour = Type, linetype = Type, shape = Type)) + geom_line() + geom_point() +
  geom_abline(intercept = 0, aes(color = "rnd.class"),linetype="dotted")+
  xlab("False Positive Rate") + ylab("True Positive Rate") +
  theme_naglr() +
  theme(plot.margin=grid::unit(rep(1,4), "mm"),axis.title.x=element_text(size=10),axis.title.y=element_text(size=10),axis.text.x =element_text(size=8),axis.text.y =element_text(size=8),legend.title=element_blank())


