library(httr)
x<-GET("https://dracor.org/api/corpora/ger/play/schlegel-die-stumme-schoenheit/spoken-text")
re<-content(x,"text")
library(rdracor)
library(mongolite)
library(stringi)
src<-paste0("~/boxhkw/21s/dh/local/R/cred_gener.csv")
cred<-read.csv(src)
# # list.dirs(paste0(getwd(),"/local/DYN"))
# # dir.create(paste0(getwd(),"/local/DYN/db"))
# setwd(paste0(getwd(),"/local/DYN/db"))
# x<-con$find(fields = '{"div":true}',limit = 10)
# con$count()
con<- mongo(collection = "wolfdb003", db ="deadend", url=cred$url[cred$q=="mongodb"])

x<-con$find('{}')
#dta<-dtaimp
dta<-x
#m<-!is.na(x$content)
dtam<-subset(dta,!is.na(dta$content))
dta<-dtam
source("~/Documents/GitHub/DYN_ss22/wriddle_getstopwords.R")
get_types<-function(set,opt){
  #  set
  set$contentp<-gsub("[^A-Za-z0-9äöüß \n]","",set$content) #get clean text
  set$contentp<-gsub("(\n)"," ",set$contentp) #get clean text
  set$contentp<-gsub("(   )"," ",set$contentp) #get clean text
  set$contentp<-gsub("(  )"," ",set$contentp) #get clean text
  set$contentp<-gsub("^( )","",set$contentp) #get clean text
  set$contentp<-gsub("(   )"," ",set$contentp) #get clean text
  set$contentp<-gsub("(  )"," ",set$contentp) #get clean text
  set$contentp<-gsub("^( )","",set$contentp) #get clean text
  m<-matrix(stri_split_boundaries(set$contentp,simplify = T),nrow = 131)
  #wks.
  set$tokens<-stri_count_boundaries(set$contentp) # IMPORTANT: with type=word far too much!
  wolftypes<-stri_split_boundaries(set$contentp)
  types<-lapply(wolftypes,unique)
  ltypes<-lapply(types,length)
  set$types<-unlist(ltypes)
  return(set)
}
dta_t<-get_types(dta,1)
gsub("(\n)","dumm",dta$content[11])
dta_t$contentp[11]
wolfmatrix<-matrix(stri_split_boundaries(dta_t$contentp,simplify = T),nrow = 130)
#wolfmatrix<-wolfmatrix[2:nrow(wolfmatrix),]
wolfmatrix<-gsub("[^A-Za-z0-9äöüß]","",wolfmatrix)
wolfmatrix[11,12]
#library(DramaAnalysis)
#keyn<-keyness(wolfmatrix) #first frequency table of matrix
length(wolfmatrix)
#wf<-wolfmatrix
w.df<-as.data.frame(wolfmatrix)
k<-1
for (k in 1:length(wolfmatrix)){
  t<-wolfmatrix[k]
  
s1<-wolfmatrix[k]==wolfmatrix[1:length(wolfmatrix)]
s1<-sum(s1)

#  s<-sum(grepl(wolfmatrix[k],wolfmatrix))
wf[k]<-s1

print(k)
}
c<-1
r<-11
c<-12
w.f.q<-w.df
wf.2<-wf
for(r in 1:length(w.df[,1])){
  #s2<-array()
  w.a<-w.df[r,][w.df[r,]!=""]
  for (c in 1:length(w.a)){
    s2<-sum(w.a[c]==w.a[1:length(w.a)])
    wf.2[r,c]<-s2
    
    
  }
#  wolfmatrix[11,12]
  print(r)
  
}
wf.3<-as.double(wf.2)/as.double(wf) #relative frequency: sum over text/sum over corpus
wf.1<-wf.3==1
wf.4<-wf.3
wf.4[wf.1]<-wf.3[wf.1]+as.double(wf.2)[wf.1]-1 # mfw for text
wf.4.df<-matrix(wf.4,ncol = length(w.df),byrow = T)
getwd()
githdir<-"~/documents/github/DYN_ss22"
write_csv(data.frame(wf.4.df),paste(githdir,"data/wolf_freq.csv",sep = "/"))
w.df[1,][which.max(wf.4.df[1,])] # mfw top
#head(x2[order(x2,decreasing = T)])
w.df[1,][order(wf.4.df[1,],decreasing = T)][1:10] #10 mfw
n.mfw<-10
n.df<-length(w.df[,1])
#w.mfw<-matrix(ncol = n.mfw,nrow = n.df)
w.mfw<-data.frame(mfw=1:n.df)
r<-2
n.mfw.1<-n.mfw+40
typeof(w.df)
for (r in 1:n.df){
  row<-unlist(w.df[r,])
mfw.a<-unique(row[order(wf.4.df[r,],decreasing = T)]) #10 mfw
mfw.st<-mfw.a%in%wstop
mfw.b<-mfw.a[!mfw.st]
mfw.b<-mfw.b[mfw.b!=""]
mfw.b<-mfw.b[1:n.mfw]
#[1:n.mfw]
w.mfw[r,2:(n.mfw+1)]<-mfw.b
}
write_csv(w.mfw,paste(githdir,"data/wolf_keywords.csv",sep = "/"))

mfw.a
getwd()
library(readr)
write_csv(dta,"db_sf/wolfdbmongo.csv")
typeof(w.mfw[2,])
w.a
typeof(mfw.a)
mfw.a[1]==mfw.a[2]
head(w.df[1,])
sum(w.a=="ich")
w.df[11,12]
d.dfm<-dfm(wolfmatrix)
d.freq<-docfreq(d.dfm,scheme = "count")
d.key<-keyness(d.freq,method = "logratio")

wc<-matrix(stri_count_boundaries(wolfmatrix,type="character"),nrow=130) #character/position
#image(wc)
wc2<-matrix(wc)
wc0<-matrix(nrow = 130,ncol = 344)
for(k in 1:length(wc2)){
  ifelse(wc2[k]==0,wc0[k]<-NA,wc0[k]<-wc2[k])
} # put NA for zero characters
wc3<-matrix(wolfmatrix,nrow = 130)
for(k in 1:length(wc3)){
  ifelse(wolfmatrix[k]=="",wc3[k]<-NA,wc3[k]<-wolfmatrix[k])
} # put NA for zero word on position
typearray2<-array()
for (k in 1:344){
  x<-sum(!is.na(wc3[,k]))
  typearray2[k]<-length(unique(wc3[,k]))/x #distinct words (types/tokens per position)
}
chararray_m<-array()
for (k in 1:344){
  chararray_m[k]<-mean(wc0[,k],na.rm=T)
} # 
wc4<-wc3
