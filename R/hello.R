# Hello, world!
#
# This is an example function named 'hello'
# which prints 'Hello, world!'.
#
# You can learn more about package authoring with RStudio at:
#
#   http://r-pkgs.had.co.nz/
#
# Some useful keyboard shortcuts for package authoring:
#
#   Build and Reload Package:  'Ctrl + Shift + B'
#   Check Package:             'Ctrl + Shift + E'
#   Test Package:              'Ctrl + Shift + T'

mining <- function() {
  library("XML")
  library("httr")
  library("stringr")
  library("igraph")
  library("dplyr")

  data <- list()
  for( i in 1:10){
    url <- paste('bbs/Capricornus/index', i, '.html', sep='')
    html <- content(GET("https://www.ptt.cc/", path = url),as = 'parsed')
    url.list <- xpathSApply(html, "//div[@class='title']/a[@href]", xmlAttrs)
    data <- rbind(data, url.list[[1]])
  }

  data = Filter(function(x) x!= "www.ptt.cc",data)

  popu <- function(link){
    html <- content(GET('https://www.ptt.cc', path = data[[link]]), as = 'parsed')
    poster <- xpathApply(html,"//div[@class='article-metaline']/span[@class='article-meta-value']",xmlValue)[[1]]

    #ID AND 暱稱刪掉
    poster <- str_split_fixed(poster, "\\(",2)[,1]
    puller <- xpathApply(html,"//div[@class='push']/span[@class='f3 hl push-userid']",xmlValue)

    #排除沒有推文的作者
    if (length(puller) >0 ){
      puller <- as.data.frame(matrix(unlist(puller),nrow = length(puller),T))
      edgelist <- merge(poster,puller,all = TRUE)
      return (edgelist)
    }
  }

  edge = list()
  for (l in 1:length(data)){
    edge <- rbind(edge, popu(l))
  }

  edge2 <- group_by(edge,x,V1)
  edge2 <- summarise(edge2,weight=n())

  edge2 <- data.frame(from = edge2$V1 , to = edge2$x , weight = edge2$weight)

  g <- graph.data.frame(edge2 , directed = TRUE)

  V(g)$size <- degree(g,mode = ("in"))/2

  #畫圖
  png(filename = "org_Capricornus.png",height = 1080 , width = 1080)
  plot(g , layout = layout.fruchterman.reingold,vertex.label=NA,edge.arrow.size =1)
  dev.off()

}
