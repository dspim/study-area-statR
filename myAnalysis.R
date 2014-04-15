library(XML)
library(RCurl)
library(tm)
library(tmcn)
library(Rwordseg)
library(wordcloud)
library(topicmodels)
library(RColorBrewer)
library(vegan)
library(jsonlite)
#-----------------------------------------
# 處理字詞庫
#-----------------------------------------

# 匯入sogou字庫
words1 <- toTrad(readLines("http://wubi.sogou.com/dict/download_txt.php?id=9182")) # ptt字庫
words2 <- toTrad(readLines("http://wubi.sogou.com/dict/download_txt.php?id=9912")) # 繁體字庫
words <- c(words1,words2)
insertWords(words)

# 自建字庫 (需要簡體字)
strwords <-  c("服贸", "马英九", "江宜桦", "立法院", "国会", "行政院", "魏扬", "林飞帆", "陈为廷", "台湾",
               "警察", "暴力", "镇暴警察", "学运", "黑色岛国", "清大", "台大", "镇压", "后退", "张庆忠", "王金平",
               "苹果", "陪审团", "粉丝团", "苹论", "阵线", "最新", "评论", "独立", "媒体", "每日", "总览", "有话",
               "要说" ,"即时", "论坛", "反服贸", "反反服贸")
insertWords(strwords, strtype=rep("n", length(strwords)), numfreq=rep(1000, length(strwords)))

# 定義停詞
myStopWords <- c(toTrad(stopwordsCN()), 
                 "編輯", "時間", "標題", "發信", "實業", "作者", "要聞", "即時新聞", "聯合新聞網", "全文網址",
                 "全文", "網址", "大家", "今天", "知道", "非常", "很多", "現在", "希望", "不要", "已經",
                 "看到", "謝謝", "其實", "事情", "蘋果", "陪審團", "粉絲團", "蘋論", "陣線", "最新", "評論",
                 "獨立", "媒體", "每日", "總覽", "有話", "要說" ,"即時", "論壇", "投稿", "報導", "新聞",
                 "表示", "粉絲", "沒有", "青島", "院內", "濟南", "現場", "主持人", "場內", "一起", "出來",
                 "一下", "裡面", "可能", "需要", "應該", "覺得", "繼續", "告訴", "不能", "剛剛", "接下來",
                 "下去", "廣播", "訊息", "可能","問題", "文章", "社會", "政治", "朋友", "心得", "代表",
                 "方式", "事件", "地方", "內容", "產業", "行為", "運動", "電視", "意見", "高調", "部分",
                 "感覺", "重點", "小時", "狀況", "聲音", "言論", "原則", "意義", "理由", "意思", "物資",
                 "口號", "後退", "掌聲", "台大", "情況", "通道", "小姐", "部分", "身體", "廁所", "內容",
                 "位置", "媽媽", "手機", "圖文", "影音"
)

# main function
myDocTerMat <- function(docs, method="vec"){
  if(method=="vec"){
    corpus <- Corpus(VectorSource(docs)) # 建立語料庫    
    corpus <- tm_map(corpus, removePunctuation) #清除標點符號
    corpus <- tm_map(corpus, function(word) {gsub("[A-Za-z0-9]", "", word)}) #清除英數符號
    
    corpus <- tm_map(corpus, segmentCN, nature = TRUE)
    corpus <- lapply(corpus, function(sent) sent[names(sent)=="n"])
    
  }else if(method=="dir"){
    corpus <- Corpus(DirSource(docs)) # 建立語料庫
    corpus <- tm_map(corpus, removePunctuation) #清除標點符號
    corpus <- tm_map(corpus, function(word) {gsub("[A-Za-z0-9]", "", word)}) #清除英數符號
    
    corpus <- tm_map(corpus, segmentCN, nature = TRUE)    
    corpus <- tm_map(corpus, function(sent) {
      noun <- lapply(sent, function(w) w[names(w) == "n"])
      unlist(noun)}
    )
  }
  corpus <- Corpus(VectorSource(corpus))
  corpus <- tm_map(corpus, removeWords, myStopWords)
  tdm <- TermDocumentMatrix(corpus, control = list(wordLengths = c(2, Inf)))
  mat <- as.matrix(tdm) # 輸出Term-Document Matrix
  if(ncol(mat)>1){
    mat <- mat[rowSums(mat) >= quantile(rowSums(mat), 0.80), ]
    mat <- mat[, colSums(mat)>0]
    
    # 利用Hierarchical Clustering 清理不屬於學運議題的報導
    dist_dtm <- vegdist(t(mat), method = 'horn')
    hc <- hclust(dist_dtm, method = 'ave')
    k <- ceiling(0.05*length(hc$labels))
    groups <- cutree(hc, k=k) # cut tree into k clusters
    mat=(mat[, which(groups == which.max(table(groups)))])
    mat <- mat[rowSums(mat) > 0, colSums(mat) > 0]
    
    # 計算字詞的相似度與字頻
    Z <- 1 - as.matrix(vegdist(mat, method="horn", binary=FALSE))
    f <- rowSums(mat)
    a <- data.frame(f%*%Z/length(f))
    
  }else{
    # 只有1/0份文件不用分群
    mat <- data.frame(mat[rowSums(mat) >= quantile(rowSums(mat), 0.80), ])
    colnames(mat) <- 1
    
    # 計算字詞的相似度與字頻
    Z <- 1 - as.matrix(vegdist(mat, method="horn", binary=FALSE))
    f <- rowSums(mat)
    a <- t(data.frame(f))
    rownames(a) <- 1
  }
  out <- list(abu=a, fre=f, sim=Z, tdm=mat)
  class(out) <- "news"
  out
}

# 依日期、新聞來源進行計算
for(i in seq_along(levels(news$發佈日期))){
  print(i)
  for(j in seq_along(levels(news$新聞來源))){
    id.j <- which(news$新聞來源==levels(news$新聞來源)[j])
    id.i <- which(news$發佈日期==levels(news$發佈日期)[i])
    id <- intersect(id.i, id.j)
    if(length(id)!=0){
      docs <- as.character(news[id,"內容"])
      tmp <- myDocTerMat(docs)
      date <- as.Date(levels(news$發佈日期)[i])
      write.csv(tmp$tdm, paste("tdm/tdm-",date,"-",levels(news$新聞來源)[j],".csv",sep=""))
      write.csv(tmp$abu, paste("abu/abu-",date,"-",levels(news$新聞來源)[j],".csv",sep=""))
      write.csv(tmp$fre, paste("fre/fre-",date,"-",levels(news$新聞來源)[j],".csv",sep=""))
    }
  }
}
# 處理Live text (0318-0328)
for(i in 1:11){
  docs <- readLines(paste("live/",dir("live")[i], sep=""))
  tmp <- myDocTerMat(docs)
  date <- as.Date(dir("live")[i],"%m%d")
  write.csv(tmp$tdm, paste("tdm/tdm-",date,"-","Live",".csv",sep=""))
  write.csv(tmp$abu, paste("abu/abu-",date,"-","Live",".csv",sep=""))
  write.csv(tmp$fre, paste("fre/fre-",date,"-","Live",".csv",sep=""))
}

# analysis

dat <- list()
media <- date <- ""
freq <- 0
k <- 1
for(i in seq_along(levels(news$發佈日期))){
  for(j in seq_along(levels(news$新聞來源))){
    id.j <- which(news$新聞來源==levels(news$新聞來源)[j])
    id.i <- which(news$發佈日期==levels(news$發佈日期)[i])
    id <- intersect(id.i, id.j)
    if(length(id)!=0){
      tmp <- as.Date(levels(news$發佈日期)[i])
      dat[[k]] <- read.csv(paste("abu/abu-",tmp,"-",levels(news$新聞來源)[j],".csv",sep=""),row.names=1)
      media[k] <- levels(news$新聞來源)[j]
      date[k] <- levels(news$發佈日期)[i]
      freq[k] <- ncol(read.csv(paste("tdm//tdm-",tmp,"-",levels(news$新聞來源)[j],".csv",sep=""),row.names=1))
      k <- k + 1
    }
  }
}
for(i in seq_along(levels(news$發佈日期))){
  tmp <- as.Date(levels(news$發佈日期)[i])
  dat[[k]] <- read.csv(paste("abu/abu-",tmp,"-","Live",".csv",sep=""),row.names=1)
  media[k] <- "Live"
  date[k] <- levels(news$發佈日期)[i]
  freq[k] <- ncol(read.csv(paste("tdm//tdm-",tmp,"-","Live",".csv",sep=""),row.names=1))
  k <- k + 1
}

date <- as.Date.factor(date)
media <-factor(media, levels=c("NowNews", "NTU", "TVBS", "中央社", "中時", "中廣", "民視", "自由", "東森", "華視", "新頭殼", "聯合", "蘋果", "Live"),
                      labels=c("NowNews", "NTU", "TVBS", "CNA", "CTnews", "BCC", "FTV", "LTnews", "ETTV", "CTS", "newtalk", "UDN", "Apple", "Live"))
u <- as.character(unique(unlist(lapply(dat, function(x)as.character(names(x))))))
# u <- as.character(unique(unlist(lapply(dat, function(x)as.character(rownames(x))))))
tmp <- lapply(dat, function(x) rep(as.character(names(x)), 10*x))
# tmp <- lapply(dat, function(x) rep(as.character(rownames(x)), unlist(x)))
tab <- do.call("rbind", lapply(tmp, function(x) table(factor(x, levels=u, labels=u))))
dim(tab)
tab <- tab[,which(colSums(tab)>0)]
rownames(tab) <- media

fit <- prcomp(tab, scale.=TRUE)
vec1 <- fit$rotation[,1]
vec2 <- fit$rotation[,2]
pc1 <- apply(tab, 1, function(x) x%*%vec1)
pc2 <- apply(tab, 1, function(x) x%*%vec2)
out <- data.frame(media=media, x=pc1, y=pc2, colors=media, freq=freq, date=date)
out <- out[out$media=="NTU" | out$media=="CNA" | out$media=="CTnews" | out$media=="LTnews" | out$media=="newtalk" | out$media=="Apple" | out$media=="Live" ,]
out <- out[out$media!="ETTV",]
# write.csv(out,"out-utf8.csv", fileEncoding="UTF-8")
write.csv(out,"out.csv")
colnames(out) <- c("media", "x", "y", "colors", "freq", "date")
M <- gvisMotionChart(data=out, idvar="media", timevar="date", 
                     xvar="x", yvar="y", colorvar="colors")

plot(M)


LT <- news[which(news$發佈日期=="2014/03/22" & news$新聞來源=="自由"),]
as.character(LT$標題)

#----------
tab2 <- list()
for(i in seq_along(levels(media))){
  id <- which(rownames(tab)==levels(media)[i])
  tab2[[i]] <- colSums(tab[id,])
}
tab2 <- do.call("rbind", tab2)
rownames(tab2) <- levels(media)
head(tab2[,1:10])
pca1 <- prcomp(t(tab2))
correlations = as.data.frame(cor(t(tab2), pca1$x))

fit <- prcomp(t(tab2))
vec1 <- fit$rotation[,1]
vec2 <- fit$rotation[,2]
pc1 <- apply(tab2, 1, function(x) x%*%vec1)
pc2 <- apply(tab2, 1, function(x) x%*%vec2)
labels <- c("NowNews", "NTU", "TVBS", "CNA", "CTnews", "BCC", "FTV", "LTnews", "ETTV", "CTS", "newtalk", "UDN", "Apple")

out <- data.frame(media=levels(media), x=pc1, y=pc2, colors=levels(media), freq=labels)
plot(pc1,pc2)
