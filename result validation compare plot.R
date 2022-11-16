# result validation compare

library(readr)
library(dplyr)
library(tidytext)
library(tidyr)
library(stats)
library(NLP)
library(tm)
library(ggplot2)
library(stringr)

plot(0,xaxt = "n", xlab='',pch=22,cex=1.5,ylim=c(0.1,1),xlim=c(0,8),ylab="percentage of the same words")

SDG <- c("SDG1")
for (sdg in SDG) {
  setwd(paste("C:/Users/wwan484/OneDrive - The University of Auckland/SDGs/paper/code/",sdg,sep="",collapse=NULL))
  getwd()
  
  full_dir <- "out"
  sample_dir <- "result validation"
  
  # read full dataset results
  kw_f <- read.csv(paste(full_dir,"/sdg_kw_f.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=200)
  distinct_bigrams <- read.csv(paste(full_dir,"/distinct_bigrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=200)
  distinct_trigrams <- read.csv(paste(full_dir,"/distinct_trigrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=50)
  distinct_quadgrams <- read.csv(paste(full_dir,"/distinct_quadgrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=50)
  
  header=c("X","keyword")
  l2keep <- 200
  tmp <- read.csv(paste(full_dir,"/tfidf_rank_bigrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T)
  nL <- nrow(tmp)
  rm(tmp)
  #tfidf_rank_bigrams <- read.csv(paste(full_dir,"/tfidf_rank_bigrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, skip=nL-l2keep) # for kk's outputs
  tfidf_rank_bigrams <- read.csv(paste(full_dir,"/tfidf_rank_bigrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=200) # for ww's outputs
  colnames(tfidf_rank_bigrams)=header
  
  l2keep <- 50
  tmp <- read.csv(paste(full_dir,"/tfidf_rank_trigrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T)
  nL <- nrow(tmp)
  rm(tmp)
  #tfidf_rank_trigrams <- read.csv(paste(full_dir,"/tfidf_rank_trigrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, skip=nL-l2keep)
  tfidf_rank_trigrams <- read.csv(paste(full_dir,"/tfidf_rank_trigrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=50)
  colnames(tfidf_rank_trigrams)=header
  
  tmp <- read.csv(paste(full_dir,"./tfidf_rank_quadgrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T)
  nL <- nrow(tmp)
  rm(tmp)
  #tfidf_rank_quadgrams <- read.csv(paste(full_dir,"/tfidf_rank_quadgrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, skip=nL-l2keep)
  tfidf_rank_quadgrams <- read.csv(paste(full_dir,"/tfidf_rank_quadgrams.csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=50)
  colnames(tfidf_rank_quadgrams)=header
  
  # read sampled dataset results
  valis <- c(1,2,3,4,5,6,7,8,9,10)
  for (sample in valis) {
    kw_f1 <- read.csv(paste(sample_dir,"/sdg_kw_f",sample,".csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=200)
    kw_f_merg <- merge(kw_f, kw_f1, by="temp_kw_full")
    if (sample == 1) {
      per_kw <- nrow(kw_f_merg)/200
    } else {
      per_kw <- append(per_kw,nrow(kw_f_merg)/200)
    }
    rm(kw_f1)
    rm(kw_f_merg)
  }
  
  for (sample in valis) {
    distinct_bigrams1 <- read.csv(paste(sample_dir,"/distinct_bigrams",sample,".csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=200)
    dis_big_merg <- merge(distinct_bigrams, distinct_bigrams1, by="keyword")
    if (sample == 1) {
      per_dis_big <- nrow(dis_big_merg)/200
    } else {
      per_dis_big <- append(per_dis_big,nrow(dis_big_merg)/200)
    }
    rm(distinct_bigrams1)
    rm(dis_big_merg)
  }
  
  for (sample in valis) {
    distinct_trigrams1 <- read.csv(paste(sample_dir,"/distinct_trigrams",sample,".csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=50)
    dis_tri_merg <- merge(distinct_trigrams, distinct_trigrams1, by="keyword")
    if (sample == 1) {
      per_dis_tri <- nrow(dis_tri_merg)/50
    } else {
      per_dis_tri <- append(per_dis_tri,nrow(dis_tri_merg)/50)
    }
    rm(distinct_trigrams1)
    rm(dis_tri_merg)
  }
  
  for (sample in valis) {
    distinct_quadgrams1 <- read.csv(paste(sample_dir,"/distinct_quadgrams",sample,".csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=50)
    dis_qua_merg <- merge(distinct_quadgrams, distinct_quadgrams1, by="keyword")
    if (sample == 1) {
      per_dis_qua <- nrow(dis_qua_merg)/50
    } else {
      per_dis_qua <- append(per_dis_qua,nrow(dis_qua_merg)/50)
    }
    rm(distinct_quadgrams1)
    rm(dis_qua_merg)
  }
  
  for (sample in valis) {
    tfidf_rank_bigrams1 <- read.csv(paste(sample_dir,"/tfidf_rank_bigrams",sample,".csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=200)
    tfidf_big_merg <- merge(tfidf_rank_bigrams, tfidf_rank_bigrams1, by="keyword")
    if (sample == 1) {
      per_tfidf_big <- nrow(tfidf_big_merg)/200
    } else {
      per_tfidf_big <- append(per_tfidf_big,nrow(tfidf_big_merg)/200)
    }
    rm(tfidf_rank_bigrams1)
    rm(tfidf_big_merg)
  }
  
  for (sample in valis) {
    tfidf_rank_trigrams1 <- read.csv(paste(sample_dir,"/tfidf_rank_trigrams",sample,".csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=50)
    tfidf_tri_merg <- merge(tfidf_rank_trigrams, tfidf_rank_trigrams1, by="keyword")
    if (sample == 1) {
      per_tfidf_tri <- nrow(tfidf_tri_merg)/50
    } else {
      per_tfidf_tri <- append(per_tfidf_tri,nrow(tfidf_tri_merg)/50)
    }
    rm(tfidf_rank_trigrams1)
    rm(tfidf_tri_merg)
  }
  
  for (sample in valis) {
    tfidf_rank_quadgrams1 <- read.csv(paste(sample_dir,"/tfidf_rank_quadgrams",sample,".csv",sep="",collapse=NULL), stringsAsFactors = F, header = T, nrows=50)
    tfidf_qua_merg <- merge(tfidf_rank_quadgrams, tfidf_rank_quadgrams1, by="keyword")
    if (sample == 1) {
      per_tfidf_qua <- nrow(tfidf_qua_merg)/50
    } else {
      per_tfidf_qua <- append(per_tfidf_qua,nrow(tfidf_qua_merg)/50)
    }
    rm(tfidf_rank_quadgrams1)
    rm(tfidf_qua_merg)
  }
  
  if (sdg == "SDG1") {
    colo <- "#E5243B"
  } else if (sdg == "SDG2") {
    colo <- "#DDA63A"
  } else if (sdg == "SDG3") {
    colo <- "#4C9F38"
  } else if (sdg == "SDG4") {
    colo <- "#C5192D"
  } else if (sdg == "SDG5") {
    colo <- "#FF3A21"
  } else if (sdg == "SDG6") {
    colo <- "#26BDE2"
  } else if (sdg == "SDG7") {
    colo <- "#FCC30B"
  } else if (sdg == "SDG8") {
    colo <- "#A21942"
  } else if (sdg == "SDG9") {
    colo <- "#FD6925"
  } else if (sdg == "SDG10") {
    colo <- "#DD1367"
  } else if (sdg == "SDG11") {
    colo <- "#FD9D24"
  } else if (sdg == "SDG12") {
    colo <- "#BF8B2E"
  } else if (sdg == "SDG13") {
    colo <- "#3F7E44"
  } else if (sdg == "SDG14") {
    colo <- "#0A97D9"
  } else if (sdg == "SDG15") {
    colo <- "#56C02B"
  } else if (sdg == "SDG16") {
    colo <- "#00689D"
  }
  
  #plot(1,ave(per_kw)[1],xaxt = "n", xlab='',col=colo,pch=22,cex=1.5,ylim=c(0.1,1),xlim=c(0,8),ylab="percentage of the same words")
  points(1,ave(per_kw)[1],col=colo,pch=22,cex=1.5)
  points(2,ave(per_dis_big)[1],col=colo,pch=22,cex=1.5)
  points(3,ave(per_dis_tri)[1],col=colo,pch=22,cex=1.5)
  points(4,ave(per_dis_qua)[1],col=colo,pch=22,cex=1.5)
  points(5,ave(per_tfidf_big)[1],col=colo,pch=22,cex=1.5)
  points(6,ave(per_tfidf_tri)[1],col=colo,pch=22,cex=1.5)
  points(7,ave(per_tfidf_qua)[1],col=colo,pch=22,cex=1.5)
  
  axis(1, at=1, labels="keyword", cex.axis=0.9)
  axis(1, at=2, labels="fre_bia", cex.axis=0.9)
  axis(1, at=3, labels="fre_tri", cex.axis=0.9)
  axis(1, at=4, labels="fre_quad", cex.axis=0.9)
  axis(1, at=5, labels="tfidf_bia", cex.axis=0.9)
  axis(1, at=6, labels="tfidf_tri", cex.axis=0.9)
  axis(1, at=7, labels="tfidf_quad", cex.axis=0.9)
}

legend(0.05, 0.74, legend=c("SDG1"),col=c("#E5243B"), cex=1, pch=22)
#legend(0.05, 0.74, legend=c("SDG1","SDG2","SDG3","SDG4","SDG5","SDG6","SDG7","SDG8","SDG9","SDG10","SDG11","SDG12","SDG13","SDG14","SDG15","SDG16"),
#       col=c("#E5243B", "#DDA63A", "#4C9F38", "#C5192D", "#FF3A21", "#26BDE2", "#FCC30B", "#A21942", "#FD6925", "#DD1367", "#FD9D24", "#BF8B2E", "#3F7E44", "#0A97D9", "#56C02B", "#00689D"), cex=1,
#       , pch=22)
