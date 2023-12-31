#13451.2.DYN_HA: wolf plot arcs
#Q: https://www.matthewjockers.net/2015/02/02/syuzhet/
#semantic analysis

#library(httr)
library(jsonlite)
#library(purrr)
#library(stylo)
#library(igraph)
#library(network)
library(syuzhet)

root<-"~/boxHKW/21S/DH"
setwd(local)
local<-paste0(root,"local/DYN/")
setwd(local)
src<-"wolf_FF_1.json"
src<-"wolf_FF-LEN_1.json"
getwd()
#k<-2
dtatxt<-as.data.frame(fromJSON(src))
dtatxt<-(fromJSON(src))
a4<-!is.na(dtatxt[["content"]][["body"]][["text"]][[2]][["pg"]][[2]]$div$`#text`)
dtatxt[["content"]][["body"]][["text"]][[2]][["pg"]][[1]]
dtatxt[["content"]][["body"]][["text"]][[2]][["pg"]][[1]][["div"]][["#text"]]
dtatxt$content$body$text[[2]]$pg[1]
a4<-!is.na(dtatxt[["content"]][["body"]][["text"]][[2]][["pg"]][[1]][["div"]])[,2]
al<-length(a4)
a2<-get_sentiment(dtatxt[["content"]][["body"]][["text"]][[2]][["pg"]][[1]][["div"]][["#text"]][1:al][a4],method = "syuzhet",language = "german")
a3<-get_transformed_values(a2)
# typeof(dtatxt$text[[k]])
# speakerdir<-paste0(root,"/gith/DH_essais/sections/DYN/DYN_HA/corpus/")
# dir.create(speakerdir)
# for (k in 1:length(dtatxt$label)){
#   speakerfile<-paste0(speakerdir,"klemm_",dtatxt$id[k],".txt")
#   writeLines(dtatxt$text[[k]],speakerfile)
#   

a1<-!is.na(dtatxt[["content"]][["body"]][["text"]][["pg"]][["div"]])
al<-length(a1)
a2<-get_sentiment(dtatxt[["content"]][["body"]][["text"]][["pg"]][["div"]][1:al][a1],method = "syuzhet",language = "german")
a3<-get_transformed_values(a2)

#dtatxt$text$body$pg$div[1:19]
a2
a2[4]
o<-order(a2)
a2[o]
dtatxt$text$body$pg$div[o][10]
plot(a3,type="h")
#lines
#?plot
