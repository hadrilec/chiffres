###fonction qui exporte sous le bon format le graphique (sous format srgg) fait sous R
evol<<-function (ts, n = 1) 
{
  return(window(ts/stats::lag(ts, -n) - 1, start = start(ts), end = end(ts), 
                extend = TRUE))
}

ga2<-function (ts) 
{
  return(g2(ts, "a"))
}

g2<-function (ts, rythme) 
{
  hf <- frequency(ts)
  bf <- switch(rythme, a = 1, s = 2, t = 4)
  if (bf > hf) 
    stop("Erreur=La frequence d'arrivee est superieure à la frequence d'entree")
  return(evol(ts, hf/bf))
}


extraire2<-function (x, point, rythme = NULL) 
{
  
  hf <- frequency(x)
  if (!is.null(rythme)) 
    nf <- switch(rythme, a = 1, s = 2, t = 4)
  else nf <- switch(as.character(hf), `12` = 4, `4` = 1, `2` = 1)
  if (nf >= hf) 
    stop("Erreur dans moyenne : La frequence d'arrivee est superieure a la frequence d'entree")
  start <- start(x)
  if (start[2] > 1) {
    start <- c(start[1], 1)
    dim_ts <- dim(x)[2]
    if (is.null(dim_ts)) 
      dim_ts <- 1
    
    x2 <- ts(matrix(NA, ncol = dim_ts), start = start, 
             end = end(x), frequency = hf)
    
    window(x2, start = start(x), end = end(x)) <- x
    x <- x2
    
  }
  return(window(stats::lag(x, point - 1), frequency = nf, start = start))
}
moyenne2<<-function (x, rythme) 
{
  hf <- frequency(x)
  bf <- switch(rythme, a = 1, s = 2, t = 4)
  if (bf >= hf) 
    stop("Erreur dans moyenne : La frequence d'arrivee est superieure a la frequence d'entree")
  s <- extraire2(x, 1, rythme)
  if (hf > bf) {
    for (i in 2:(hf/bf)) {
      s <- s + extraire2(x, i, rythme)
    }
  }
  return(s/(hf/bf))
}

charge<-function(file,acharger=NULL){load(file)
  if (acharger %in% names(series)) return(series[acharger][[1]])
 else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(names(series))),sep=" "))
 
}

###fonction qui genere des lignes html de tableaux, titre le titre de la ligne, ts la serie a utiliser pour la partie trim,
#base la base dans laquelle se trouve la ts trim
#fonc_trim la fonction pour les donnees trim (VT ou NIV par exemple),
#fonc_an la fonction pour les donnees annuelles (VA ou NIV_A par exemple),
#ts_an la serie pour les donnees annuelles (seulement si differente de la ts trimestriel comme pour les contributions),
#base_an la base dans laquelle se trouve ts_an (seulement si differente de la ts trimestriel comme pour les contributions)
f_row<-function(titre,ts_trim,fonc_trim,fonc_an,base,base_old=NULL,ts_an=NULL,base_an=NULL,base_an_old=NULL,bold=F,dig=1){
  
  if(is.null(ts_an)){ts_an<-ts_trim}
  if(is.null(base_an)){base_an<-base}
  if(is.null(base_old)){base_old<-base}
  if(is.null(base_an_old)){base_an_old<-base_an}
 
  return(paste(ifelse(bold,"<tr style=\"font-weight:bold\">","<tr>"),
    "<td>",titre,"</td><td>",
    paste(c(
      fonc_trim(base,base_old, ts_trim, -9,dig=dig),
      fonc_trim(base,base_old, ts_trim, -8,dig=dig),
      fonc_trim(base,base_old, ts_trim, -7,dig=dig),
      fonc_trim(base,base_old, ts_trim, -6,dig=dig),
      fonc_trim(base,base_old, ts_trim, -5,dig=dig),
      fonc_trim(base,base_old, ts_trim, -4,dig=dig),
      fonc_trim(base,base_old, ts_trim, -3,dig=dig)), collapse = "</td><td>"),"</td><td style=\"background-color:silver\">",paste(c(
        fonc_trim(base,base_old, ts_trim, -2,dig=dig),
        fonc_trim(base,base_old, ts_trim, -1,dig=dig),
        fonc_trim(base,base_old, ts_trim,dig=dig)), collapse = "</td><td style=\"background-color:silver\">"),
    "</td><td style=\"border-left: 3px #111111 solid;\">",
    fonc_an(base_an,base_an_old, ts_an, -2,dig=dig),"</td><td style=\"background-color:silver\">",
    fonc_an(base_an,base_an_old, ts_an,-1,dig=dig),"</td><td style=\"background-color:silver\">",
    fonc_an(base_an,base_an_old, ts_an,dig=dig),
    "</td></tr>",
    sep = ""
  ))
  
}

ligne_date<-function()return(paste(
  "<tr><th></th><th>",
  paste(sapply(c(
    trim[1] + 1 / 4 * trim[2] - 10 / 4,
    trim[1] + 1 / 4 * trim[2] - 9 / 4,
    trim[1] + 1 / 4 * trim[2] - 8 / 4,
    trim[1] + 1 / 4 * trim[2] - 7 / 4,
    trim[1] + 1 / 4 * trim[2] - 6 / 4,
    trim[1] + 1 / 4 * trim[2] - 5 / 4,
    trim[1] + 1 / 4 * trim[2] - 4 / 4,
    trim[1] + 1 / 4 * trim[2] - 3 / 4,
    trim[1] + 1 / 4 * trim[2] - 2 / 4,
    trim[1] + 1 / 4 * trim[2] - 1 / 4
  ), function(x)
    paste(substr(x, 3, 4), "T", (x - floor(x)) * 4 + 1, sep = "")), collapse =
    "</th><th>"),
  "</th><th style=\"border-left: 3px #111111 solid;\">",
  paste(c(annee - 2, annee-1, annee), collapse = "</th><th>"),
  "</th></tr>",
  sep = ""
))

version<-function(i,i_old=NULL){
  date<-as.Date(file.info(i)$mtime)
  if(!is.null(i_old)){date_o<-as.Date(file.info(i_old)$mtime)
  paste("**Pour", i, "Donnees du ",format(date,"%d"),months(date), format(date,"%Y"),substring(file.info(i)$mtime,first=12),"\n","comparees a celles du ",format(date_o,"%d"),months(date_o), format(date_o,"%Y"),substring(file.info(i_old)$mtime,first=12),"**")
  } else paste("**Pour",i,"Donnees du ",format(date,"%d"),months(date), format(date,"%Y"),substring(file.info(i)$mtime,first=12),"**")
  }

VT<-function(rdata,rdata_old=NULL,ts,n=0,sgn=T,pct=T,dig=1){
  new<-formatC(window(round(evol(charge(rdata,ts))*100,digits=dig),start=trim+c(0,n),end=trim+c(0,n)),format='f',digits=dig)
  if(!is.null(rdata_old)){
  old<-formatC(window(round(evol(charge(rdata_old,ts))*100,digits=dig),start=trim+c(0,n),end=trim+c(0,n)), format='f', digits=dig )}
  else {old<-new}
  # if(sgn==T){if(new>0) {new=abs(new)
  # old=abs(old)}}
  affiche(new,old,sgn,pct)
  }


affiche <- function(new, old, sgn,pct)
{
  sg <- ""
  sg_o <- ""
 sg_pct<-""
  if (substring(new, 1, 1) != "-" && sgn) {
    sg <- "+"
  }
  if (substring(old, 1, 1) != "-" && sgn) {
    sg_o <- "+"
  }
  if (substring(new, 1, 1) == "-" && sgn) {
    sg <- "-"
    new <- substring(new, 2)
  }
  if (substring(old, 1, 1) == "-" && sgn) {
    sg_o <- "-"
    old <- substring(old, 2)
  }
  if (substring(new, 1, 1) == "-" && !sgn) {
    new <- substring(new, 2)
  }
  if (substring(old, 1, 1) == "-" && !sgn) {
    old <- substring(old, 2)
  }

  if (exists("excel")) {###pour ecrire dans un Excel directement
   
      paste( sg, sub("\\.", "\\,", new), sep = "")
    
  }
  else {
	if(pct)sg_pct<-"&nbsp;%"
    if (old != new) {
      paste(
        "~~",
        sg_o,
        sub("\\.", "\\,", old),
        "~~ ",
        "**",
        sg,
        sub("\\.", "\\,", new),
        sg_pct,"**",
        sep = ""
      )
    } else {
      paste("**", sg, sub("\\.", "\\,", new), sg_pct,"**", sep = "")
    }
  }
}



VA<-function(rdata,rdata_old=NULL,ts,n=0,sgn=T,pct=T,dig=1){
  new<-formatC(window(round(evol(moyenne2(charge(rdata,ts),"a"))*100,digits=dig),start=annee+n,end=annee+n),format='f',digits=dig)
  
  if(!is.null(rdata_old)){
  old<-formatC(window(round(evol(moyenne2(charge(rdata_old,ts),"a"))*100,digits=dig),start=annee+n,end=annee+n), format='f', digits=dig )}
  else {old<-new
  }
  # if(sgn==T){if(new>0) {new=abs(new)
  # old=abs(old)}}
  affiche(new,old,sgn,pct)}

GA<-function(rdata,rdata_old=NULL,ts,n=0,sgn=T,pct=T,dig=1){
  freq<-frequency(charge(rdata,ts))
  if(freq==4){pt=trim+c(0,n)}else{pt=n}
  new<-formatC(window(round(ga(charge(rdata,ts))*100,digits=dig),start=pt,end=pt),format='f',digits=dig)
  
  if(exists(paste(rdata,"_old",sep=""))){
  old<-formatC(window(round(ga(charge(rdata_old,ts))*100,digits=dig),start=pt,end=pt), format='f', digits=dig )}
  else {old<-new
  }
  affiche(new,old,sgn,pct)}

GA_A<-function(rdata,rdata_old=NULL,ts,n=0,sgn=T,pct=T,dig=1){

  new<-formatC(window(round(ga(moyenne2(charge(rdata,ts),"a"))*100,digits=dig),start=annee+n,end=annee+n),format='f',digits=dig)
  
  if(exists(paste(rdata,"_old",sep=""))){
    old<-formatC(window(round(ga(moyenne2(charge(rdata_old,ts),"a"))*100,digits=dig),start=annee+n,end=annee+n), format='f', digits=dig )}
  else {old<-new
  }
  affiche(new,old,sgn,pct)}



NIV<-function(rdata,rdata_old=NULL,ts,n=0,sgn=T,pct=T,dig=1){
  new<-formatC(window(round(charge(rdata,ts),digits=dig),start=trim+c(0,n),end=trim+c(0,n)),format='f',digits=dig)
  
  if(!is.null(rdata_old)){
  old<-formatC(window(round(charge(rdata_old,ts),digits=dig),start=trim+c(0,n),end=trim+c(0,n)), format='f', digits=dig )}
  else {old<-new
  }
  affiche(new,old,sgn,pct)}

NIV_A<-function(rdata,rdata_old=NULL,ts,n=0,sgn=T,pct=T,dig=1){##fait la moyenne d'une grandeur trimestrielle
  new<-formatC(window(round(moyenne2(charge(rdata,ts),"a"),digits=dig),start=annee+n,end=annee+n),format='f',digits=dig)
  
  if(!is.null(rdata_old)){
  old<-formatC(window(round(moyenne2(charge(rdata_old,ts),"a"),digits=dig),start=annee+n,end=annee+n), format='f', digits=dig )}
  else {old<-new
  }
  affiche(new,old,sgn,pct)}

NIV_AN<-function(rdata,rdata_old=NULL,ts,n=0,sgn=T,pct=T,dig=1){##affiche seulement une valeur annuelle donnee
  new<-formatC(window(round(charge(rdata,ts),digits=dig),start=annee+n,end=annee+n),format='f',digits=dig)
  
  if(!is.null(rdata_old)){
    old<-formatC(window(round(charge(rdata_old,ts),digits=dig),start=annee+n,end=annee+n), format='f', digits=dig )}
  else {old<-new
  }
  affiche(new,old,sgn,pct)}