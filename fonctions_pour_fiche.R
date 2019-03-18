evol<<-function (ts, n = 1) ###fonction pour calculer une variation en pourcentage
{
  return(window(ts/stats::lag(ts, -n) - 1, start = start(ts), end = end(ts), 
                extend = TRUE))
}

ga2<-function (ts) ##fonction pour calcul de glissement annuel
{
  return(g2(ts, "a"))
}

g2<-function (ts, rythme) ##fonction pour glissement en general
{
  hf <- frequency(ts)
  bf <- switch(rythme, a = 1, s = 2, t = 4)
  if (bf > hf) 
    stop("Erreur=La frequence d'arrivee est superieure à la frequence d'entree")
  return(evol(ts, hf/bf))
}


extraire2<-function (x, point, rythme = NULL) ##fonction qui extrait un mois (par exemple) d'une serie trimestrielle
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
moyenne2<<-function (x, rythme)  #fonction de moyenne
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

moy_pond<-function(x){ #fonction de moyenne ponderee speciale pour calculer la contribution annuelle connaissant les contrib trim
  return(
    extraire2(x, 1, 1) + 3 / 4 * extraire2(x, 2, 1) + 1 / 2 * extraire2(x, 3, 1) +
      1 / 4 * extraire2(x, 4, 1) + 3 / 4 * lag(extraire2(x, 4, 1), -1) + 1 / 2 *
      lag(extraire2(x, 3, 1), -1) + 1 / 4 * lag(extraire2(x, 2, 1), -1)
  )
  
}

##le probleme c'est que les donnees France ne sont pas sous le format standardise des Rdata internationaux. Ainsi il y a deux
##fonctions charge, une speciale internationale et une france
charge_int<-function(file,acharger=NULL){load(file)
  if (acharger %in% names(series)) return(series[acharger][[1]])
 else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(names(series))),sep=" "))
 
}

charge_fr<-function(file,acharger=NULL){load(file)
  if (acharger %in% rownames(table_noteRch)) return(ts(table_noteRch[acharger,],start=c(as.numeric(substr(names(table_noteRch[acharger,])[1],1,4)),as.numeric(substr(names(table_noteRch[acharger,])[1],6,6))),frequency=4))
  else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(rownames(table_noteRch))),sep=" "))
}


###fonction qui genere des lignes html de tableaux, titre le titre de la ligne, ts la serie a utiliser pour la partie trim,
#base la base dans laquelle se trouve la ts trim
#fonc_trim la fonction pour les donnees trim (VT ou NIV par exemple),
#fonc_an la fonction pour les donnees annuelles (VA ou NIV_A par exemple),
#ts_an la serie pour les donnees annuelles (seulement si differente de la ts trimestriel comme des fois pour les contributions),
#base_an la base dans laquelle se trouve ts_an (seulement si differente de la ts trimestriel comme pour les contributions)
#base_old ou base_an_old sont des bases à comparer
#bold si on veut la ligne en gras
#dig nombre de decimales a afficher
#n_prev le nombre de cellules qui sont des previsions pour la partie trim (donc le nombre de cellules trim a griser)
f_row<-function(titre,ts_trim,fonc_trim,fonc_an,base,base_old=NULL,ts_an=NULL,base_an=NULL,base_an_old=NULL,bold=F,dig=1,n_prev=2){
  n_prev_an<-ifelse(trim[2]==2,n_prev_an<-round((n_prev+2)/3),round((n_prev+3)/5))##permet de savoit le nombre d'annee a griser selon le nombre de trim a griser
  if(is.null(ts_an)){ts_an<-ts_trim}
  if(is.null(base_old)){base_old<-base}
  if(is.null(base_an)){base_an<-base
  base_an_old<-base_old}
  if(is.null(base_an_old)){base_an_old<-base_an}
 return(paste(ifelse(bold,"<tr style=\"font-weight:bold\">","<tr>"),
    "<td class=\"titre\">",titre,"</td>",
    paste(c(
      fonc_trim(base,base_old, ts_trim, -9,dig=dig),###c'est ici qu'on choisit d'afficher 10 trimestres dans le tableau
      fonc_trim(base,base_old, ts_trim, -8,dig=dig),
      fonc_trim(base,base_old, ts_trim, -7,dig=dig),
      fonc_trim(base,base_old, ts_trim, -6,dig=dig),
      fonc_trim(base,base_old, ts_trim, -5,dig=dig),
      fonc_trim(base,base_old, ts_trim, -4,dig=dig,prev=(n_prev>=5)),
      fonc_trim(base,base_old, ts_trim, -3,dig=dig,prev=(n_prev>=4)), 
      fonc_trim(base,base_old, ts_trim, -2,dig=dig,prev=(n_prev>=3)),
      fonc_trim(base,base_old, ts_trim, -1,dig=dig,prev=(n_prev>=2)),##l'option prev=T permet de griser la cellule, faudrait automatiser dans le futur, mais difficile de transmettre l'info 
      fonc_trim(base,base_old, ts_trim,dig=dig,prev=(n_prev>=1)),
      fonc_an(base_an,base_an_old, ts_an, -2,dig=dig,prev=(n_prev_an>=3)),##ici on choisit d'afficher 3 annees
      fonc_an(base_an,base_an_old, ts_an, -1,dig=dig,prev=(n_prev_an>=2)),
      fonc_an(base_an,base_an_old, ts_an,dig=dig,prev=(n_prev_an>=1)),
      if(trim[2]==2)fonc_an(base_an,base_an_old, ts_an,dig=dig,prev=(n_prev_an>=1),prlg=T)), collapse = ""),##si trim =2 on ajoute la VA prolonge 
    "</tr>",
    sep = ""
  ))
  
}

ligne_date<-function()return(paste(##fonction qui cree la ligne de date, la premiere
  "<tr><th></th><th>",
  paste(sapply(c(
    trim[1] + 1 / 4 * trim[2] - 10 / 4,##ici aussi on choisit d'afficher 10 trimestres...
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
  "</th><th>",ifelse(trim[2]==2,
                                                               
  paste(c(annee - 2, annee-1, paste(annee,"acquis"),paste(annee,"prolonge")), collapse = "</th><th>"),
  paste(c(annee - 2, annee-1, annee), collapse = "</th><th>")),##et trois annees
  "</th></tr>",
  sep = ""
))

version<-function(i,i_old=NULL){##fonction qui affiche les caracteristiques du RData (date de creation) 
  date<-as.Date(file.info(i)$mtime)
  if(!is.null(i_old)){date_o<-as.Date(file.info(i_old)$mtime)
  paste("Pour", i, "Donnees du ",format(date,"%d"),months(date), format(date,"%Y"),substring(file.info(i)$mtime,first=12),"\n","comparees a celles du ",format(date_o,"%d"),months(date_o), format(date_o,"%Y"),substring(file.info(i_old)$mtime,first=12))
  } else paste("Pour",i,"Donnees du ",format(date,"%d"),months(date), format(date,"%Y"),substring(file.info(i)$mtime,first=12))
  }



affiche <- function(new, old,prev=F)##fonction d'affichage appelée par chaque operateur specifique VT, VA, GA etc.
{

    if (old != new) {
      paste(ifelse(prev,"<td style=\"background-color:silver\">","<td>"),
        "<s>",
        sub("\\.", "\\,", old),
        "</s> ",
        sub("\\.", "\\,", new),"</td>",
        sep = ""
      )
    } else {
      paste(ifelse(prev,"<td style=\"background-color:silver\">","<td>"), sub("\\.", "\\,", new),"</td>", sep = "")
    }
  
}

VT<-function(rdata,rdata_old=NULL,ts,n=0,dig=1,prev=F){##Calcul de VT, ts la serie temp, rdata sa base (eventuellement a comparer a rdata_old), n est le décalage a partir du trimestre de reference (variable trim),dig le nombre de decimal, prev si veut griser la case
  new<-formatC(window(round(evol(charge(rdata,ts))*100,digits=dig),start=trim+c(0,n),end=trim+c(0,n)),format='f',digits=dig)
  if(!is.null(rdata_old)){
    old<-formatC(window(round(evol(charge(rdata_old,ts))*100,digits=dig),start=trim+c(0,n),end=trim+c(0,n)), format='f', digits=dig )}
  else {old<-new}
  # if(sgn==T){if(new>0) {new=abs(new)
  # old=abs(old)}}
  affiche(new,old,prev=prev)
}


VA<-function(rdata,rdata_old=NULL,ts,n=0,dig=1,prev=F,prlg=F){##Calcul de VA, ts la serie temp trim, rdata sa base (eventuellement a comparer a rdata_old), n est le décalage a partir de l'annee de reference (variable annee),dig le nombre de decimal, prev si veut griser la case, prlg si on veut pas l'acquis mais la moyenne prolongee
  ifelse(prlg,new<-formatC(round(as.numeric(window(evol(moyenne2(charge(rdata,ts),"a"))*100,start=annee+n,end=annee+n))+3/4*as.numeric(window(evol(charge(rdata,ts))*100,start=trim,end=trim)),digits=dig),format='f',digits=dig),
         new<-formatC(window(round(evol(moyenne2(charge(rdata,ts),"a"))*100,digits=dig),start=annee+n,end=annee+n),format='f',digits=dig))

  if(!is.null(rdata_old)){
    ifelse(prlg,old<-formatC(round(as.numeric(window(evol(moyenne2(charge(rdata_old,ts),"a"))*100,start=annee+n,end=annee+n))+3/4*as.numeric(window(evol(charge(rdata_old,ts))*100,start=trim,end=trim)),digits=dig),format='f',digits=dig),
           old<-formatC(window(round(evol(moyenne2(charge(rdata_old,ts),"a"))*100,digits=dig),start=annee+n,end=annee+n),format='f',digits=dig))
  }
  else {old<-new
  }
  # if(sgn==T){if(new>0) {new=abs(new)
  # old=abs(old)}}
  affiche(new,old,prev=prev)}

GA<-function(rdata,rdata_old=NULL,ts,n=0,dig=1,prev=F){##Calcul de GA, ts la serie temp, rdata sa base (eventuellement a comparer a rdata_old), n est le décalage a partir du trimestre de reference (variable trim),dig le nombre de decimal, prev si veut griser la case

  freq<-frequency(charge(rdata,ts))
  if(freq==4){pt=trim+c(0,n)}else{pt=n}
  
  new<-formatC(window(round(ga2(charge(rdata,ts))*100,digits=dig),start=pt,end=pt),format='f',digits=dig)
  
  if(exists(paste(rdata,"_old",sep=""))){
   
  old<-formatC(window(round(ga2(charge(rdata_old,ts))*100,digits=dig),start=pt,end=pt), format='f', digits=dig )}
    
  else {old<-new
  }
  affiche(new,old,prev=prev)}

GA_A<-function(rdata,rdata_old=NULL,ts,n=0,dig=1,prev=F,prlg=F){##Calcul de GA en moyenne annuelle, ts la serie temp, rdata sa base (eventuellement a comparer a rdata_old), n est le décalage a partir de l'annee de reference (variable annee),dig le nombre de decimal, prev si veut griser la case

  ifelse(prlg,new<-"*",
  new<-formatC(window(round(ga2(moyenne2(charge(rdata,ts),"a"))*100,digits=dig),start=annee+n,end=annee+n),format='f',digits=dig)
  )
  if(exists(paste(rdata,"_old",sep=""))){
    ifelse(prlg,old<-"*",
    old<-formatC(window(round(ga2(moyenne2(charge(rdata_old,ts),"a"))*100,digits=dig),start=annee+n,end=annee+n), format='f', digits=dig )
    )}
  else {old<-new
  }
  affiche(new,old,prev=prev)}



NIV<-function(rdata,rdata_old=NULL,ts,n=0,dig=1,prev=F){##Juste l'affichage du Niveau,  ts la serie temp, rdata sa base (eventuellement a comparer a rdata_old), n est le décalage a partir du trimestre de reference (variable trim),dig le nombre de decimal, prev si veut griser la case

  new<-formatC(window(round(charge(rdata,ts),digits=dig),start=trim+c(0,n),end=trim+c(0,n)),format='f',digits=dig)
  
  if(!is.null(rdata_old)){
  old<-formatC(window(round(charge(rdata_old,ts),digits=dig),start=trim+c(0,n),end=trim+c(0,n)), format='f', digits=dig )}
  else {old<-new
  }
  affiche(new,old,prev=prev)}

NIV_depuis_mensuel<-function(rdata,rdata_old=NULL,ts,n=0,dig=1,prev=F){##Juste l'affichage du Niveau d'une série mensuelle,  ts la serie temp, rdata sa base (eventuellement a comparer a rdata_old), n est le décalage a partir du trimestre de reference (variable trim),dig le nombre de decimal, prev si veut griser la case
  
  new<-formatC(window(round(moyenne2(charge(rdata,ts),"t"),digits=dig),start=trim+c(0,n),end=trim+c(0,n)),format='f',digits=dig)
  
  if(!is.null(rdata_old)){
    old<-formatC(window(round(moyenne2(charge(rdata_old,ts),"t"),digits=dig),start=trim+c(0,n),end=trim+c(0,n)), format='f', digits=dig )}
  else {old<-new
  }
  affiche(new,old,prev=prev)}


NIV_A<-function(rdata,rdata_old=NULL,ts,n=0,dig=1,prev=F,prlg=F){##L'affichage du Niveau en moyenne annuelle,  ts la serie temp, rdata sa base (eventuellement a comparer a rdata_old), n est le décalage a partir du trimestre de reference (variable trim),dig le nombre de decimal, prev si veut griser la case
  ifelse(prlg,new<-"*",
  new<-formatC(window(round(moyenne2(charge(rdata,ts),"a"),digits=dig),start=annee+n,end=annee+n),format='f',digits=dig)
  )
  if(!is.null(rdata_old)){
    ifelse(prlg,old<-"*",
  old<-formatC(window(round(moyenne2(charge(rdata_old,ts),"a"),digits=dig),start=annee+n,end=annee+n), format='f', digits=dig ))}
  else {old<-new
  }
  affiche(new,old,prev=prev)}

NIV_AN<-function(rdata,rdata_old=NULL,ts,n=0,dig=1,prev=F,prlg=F){##l'affichage du Niveau d'une ts deja annuelle,  ts la serie temp, rdata sa base (eventuellement a comparer a rdata_old), n est le décalage a partir de l'annee de reference (variable annee),dig le nombre de decimal, prev si veut griser la case
  ifelse(prlg,new<-"*",
  new<-formatC(window(round(charge(rdata,ts),digits=dig),start=annee+n,end=annee+n),format='f',digits=dig)
  )
  if(!is.null(rdata_old)){ifelse(prlg,old<-"*",
    old<-formatC(window(round(charge(rdata_old,ts),digits=dig),start=annee+n,end=annee+n), format='f', digits=dig ))}
  else {old<-new
  }
  affiche(new,old,prev=prev)}

MOY_PND<-function(rdata,rdata_old=NULL,ts,n=0,dig=1,prev=F,prlg=F){##Calcul de moyenne ponderee (associe a la fonction moy_pond), ts la serie temp, rdata sa base (eventuellement a comparer a rdata_old), n est le décalage a partir du trimestre de reference (variable trim),dig le nombre de decimal, prev si veut griser la case
  ifelse(prlg,new<-"*",
  new<-formatC(window(round(moy_pond(charge(rdata,ts)),digits=dig),start=annee+n,end=annee+n),format='f',digits=dig)
  )
  if(!is.null(rdata_old)){ifelse(prlg,old<-"*",
    old<-formatC(window(round(moy_pond(charge(rdata_old,ts)),digits=dig),start=annee+n,end=annee+n), format='f', digits=dig ))}
  else {old<-new
  }
  affiche(new,old,prev=prev)}


DIFF<-function(rdata,rdata_old=NULL,ts,n=0,dig=1,prev=F){##diffence simple,  ts la serie temp, rdata sa base (eventuellement a comparer a rdata_old), n est le décalage a partir du trimestre de reference (variable trim),dig le nombre de decimal, prev si veut griser la case
  
  new<-formatC(window(round(diff(charge(rdata,ts)),digits=dig),start=trim+c(0,n),end=trim+c(0,n)),format='f',digits=dig)
  
  if(!is.null(rdata_old)){
    old<-formatC(window(round(diff(charge(rdata_old,ts)),digits=dig),start=trim+c(0,n),end=trim+c(0,n)), format='f', digits=dig )}
  else {old<-new
  }
  affiche(new,old,prev=prev)}

DIFF_AN<-function(rdata,rdata_old=NULL,ts,n=0,dig=1,prev=F,prlg=F){##difference simple en annuel,  ts la serie temp, rdata sa base (eventuellement a comparer a rdata_old), n est le décalage a partir de l'annee de reference (variable annee),dig le nombre de decimal, prev si veut griser la case
  ifelse(prlg,new<-"*",
  new<-formatC(window(round(diff(moyenne2(charge(rdata,ts),"a")),digits=dig),start=annee+n,end=annee+n),format='f',digits=dig)
  )
  if(!is.null(rdata_old)){ifelse(prlg,old<-"*",
    old<-formatC(window(round(diff(moyenne2(charge(rdata_old,ts),"a")),digits=dig),start=annee+n,end=annee+n), format='f', digits=dig ))}
  else {old<-new
  }
  affiche(new,old,prev=prev)}