tabl_us<-function(dig=1){
  charge<<-function(file,acharger=NULL){load(file)
    if (acharger %in% names(series)) return(series[acharger][[1]])
    else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(names(series))),sep=" "))
    
  }
  base<<-paste(d_int,"US.Rdata",sep="//")
  base_an<<-paste(d_int,"US_an.Rdata",sep="//")
  base_old<<-NULL
  row1 <-  ligne_date()
  row2 <-f_row("<b>Produit interieur brut</b>","us.pib",VT,VA,base,dig=dig)
  row3 <-f_row("Consommation privee","us.conso",VT,VA,base,dig=dig)
  row4 <-f_row("Investissement prive","us.inv_hs",VT,VA,base,dig=dig)
  row5 <-f_row("Depenses gouvernementales","us.dp",VT,VA,base,dig=dig)
  row6 <-f_row("Exportations","us.exports",VT,VA,base,dig=dig)
  row7 <-f_row("Importations","us.imports",VT,VA,base,dig=dig)
  row8 <-f_row("Demande interieure hors stocks","us.di_contrib",NIV,NIV_AN,base,base_old,"us_an.di.contrib.an",base_an,dig=dig)
  row9 <-f_row("Variation de stocks","us.stocks.contrib",NIV,NIV_AN,base,base_old,"us_an.stocks.contrib.an",base_an,dig=dig)
  row10 <-f_row("Commerce exterieur","us.comext.contrib",NIV,NIV_AN,base,base_old,"us_an.comext.contrib.an",base_an,dig=dig)
  



tabl<-paste("<h1>Etats-Unis</h1>",version(base),"<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,"<tr><td><b>Contributions</b></td></tr>",row8,row9,row10,"</table>",sep="")
tabl2<-gsub("&nbsp;%","",tabl)
tabl3<-gsub("\\+","",tabl2)
tabl4<-gsub("\\**","",tabl3)
tabl5<-gsub(">~~","><s>",tabl4)
tabl6<-gsub("~~","</s>",tabl5)
#kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
return(tabl6)}

