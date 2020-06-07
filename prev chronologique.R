d_fr <<-
  "N:/GDESE/O-GDESE/Special/Special.GFou/Sorties/syntheseGFOU" #repertoire de sortie pour les Rdata France
liste_gf <<-
  rev(dir(d_fr)[grepl("sortieGFRdata", dir(d_fr))])
library(ggplot2)
chrono<-function(var,tmp){
tab<-rep(0,2)
for(i in liste_gf){
  file<-file.path(d_fr,i)
  tab<-rbind(tab,c(as.character(file.info(file)$mtime),as.numeric(window(e(charge_fr(file,acharger=var))*100,start=tmp,end=tmp))))
  
}


    
df<-as.data.frame(tab)
df<-df[-1,]
names(df)<-c("date","prev")
df$date<-as.POSIXct(df$date)
df$prev<-as.numeric(as.character(df$prev))


ggplot(df,aes(date,prev))+geom_line(size=1.1)+geom_point(size=1.3,color="red")+scale_x_datetime(date_breaks="1 week",date_labels="%d %b")+theme_bw()
}
charge_fr<-function(file,acharger=NULL){load(file)
  if (acharger %in% rownames(table_noteRch)) return(ts(table_noteRch[acharger,],start=c(as.numeric(substr(names(table_noteRch[acharger,])[1],1,4)),as.numeric(substr(names(table_noteRch[acharger,])[1],6,6))),frequency=4))
  else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(rownames(table_noteRch))),sep=" "))
}



chrono("pib_7ch",tmp=c(2019,2))
chrono("pardb_men",tmp=c(2019,1))
chrono("pardb_men",tmp=c(2019,2))

chrono("p1e_dim_7ch",tmp=c(2019,3))
chrono("p6_dim_7ch",tmp=c(2019,3))

