 library(tm)
 library(tmcn)
 library(Rwordseg)
 library(vegan)
#-----------------------------------------
# 處理字詞庫
#-----------------------------------------
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
                 "位置", "媽媽", "手機", "圖文", "影音", "記者"
)

# main function
myDocTerMat <- function(docs, method="vec", encoding="utf8", clean=TRUE){
  if(method=="vec"){
    corpus <- Corpus(VectorSource(docs)) # 建立語料庫
    if(encoding!="uft8"){
      corpus <- tm_map(corpus, function(word) iconv(word, from=encoding))
    }
    corpus <- tm_map(corpus, removePunctuation) #清除標點符號
    corpus <- tm_map(corpus, function(word) {gsub("[A-Za-z0-9]", "", word)}) #清除英數符號 
    corpus <- lapply(corpus, segmentCN, nature = TRUE)
    corpus <- lapply(corpus, function(sent) sent[names(sent)=="n"])
    
  }else if(method=="dir"){
    corpus <- Corpus(DirSource(docs)) # 建立語料庫
    if(encoding!="uft8"){
      corpus <- tm_map(corpus, function(word) iconv(word, from=encoding))
    }
    corpus <- tm_map(corpus, removePunctuation) #清除標點符號
    corpus <- tm_map(corpus, function(word) {gsub("[A-Za-z0-9]", "", word)}) #清除英數符號
    
    corpus <- lapply(corpus, function(sent) sapply(sent, segmentCN, nature=TRUE))
    
    corpus <- lapply(corpus, function(sent) {
                     unlist(lapply(sent, function(w) w[names(w) == "n"]))
                    })
    
  }
  corpus <- Corpus(VectorSource(corpus))
  corpus <- tm_map(corpus, removeWords, myStopWords)
  tdm <- TermDocumentMatrix(corpus, control = list(wordLengths = c(2, Inf)))
  mat <- as.matrix(tdm) # 輸出Term-Document Matrix
  if(ncol(mat)>1){
    mat <- mat[rowSums(mat) >= quantile(rowSums(mat), 0.80), ]
    mat <- mat[, colSums(mat)>0]
    
    if(clean==TRUE){
    # 利用Hierarchical Clustering 清理不屬於學運議題的報導
      dist_dtm <- vegdist(t(mat), method = 'horn')
      hc <- hclust(dist_dtm, method = 'ave')
      k <- ceiling(0.05*length(hc$labels))
      groups <- cutree(hc, k=k) # cut tree into k clusters
      mat <- mat[, which(groups == which.max(table(groups)))]
    }
    mat <- mat[rowSums(mat) > 0, colSums(mat) > 0]
    f <- rowSums(mat)
    
  }else{
    # 只有1/0份文件不用分群
    mat <- data.frame(mat[rowSums(mat) >= quantile(rowSums(mat), 0.80), ])
    colnames(mat) <- 1
    f <- rowSums(mat)
  }
  out <- list(fre=f, tdm=mat)
  class(out) <- "news"
  out
}