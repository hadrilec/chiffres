tabl_jp<-function(dig=1){
  charge<<-function(file,acharger=NULL){load(file)
    if (acharger %in% names(series)) return(series[acharger][[1]])
    else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(names(series))),sep=" "))
    
  }
  b_int<<-paste(d_int,"JP.Rdata",sep="//")
  b_int_an<<-paste(d_int,"JP_an.Rdata",sep="//")
  b_int_old<<-NULL
  row1 <-  ligne_date()
  row2 <-f_row("<b>Produit interieur brut</b>","jp.pib",VT,VA,b_int,dig=dig)
  row3 <-f_row("Consommation privee","jp.conso",VT,VA,b_int,dig=dig)
  row4 <-f_row("Investissement","jp.inv",VT,VA,b_int,dig=dig)
  row5 <-f_row("Consommation publique","jp.conso_pu",VT,VA,b_int,dig=dig)
  row6 <-f_row("Exportations","jp.exp_trim",VT,VA,b_int,dig=dig)
  row7 <-f_row("Importations","jp.imp",VT,VA,b_int,dig=dig)
  row8 <-f_row("Demande interieure hors stocks","jp.di.contrib",NIV,NIV_AN,b_int,b_int_old,"jp_an.di.contrib.an",b_int_an,dig=dig)
  row9 <-f_row("Variation de stocks","jp.stocks.contrib",NIV,NIV_AN,b_int,b_int_old,"jp_an.stocks.contrib.an",b_int_an,dig=dig)
  row10 <-f_row("Commerce exterieur","jp.comext.contrib",NIV,NIV_AN,b_int,b_int_old,"jp_an.comext.contrib.an",b_int_an,dig=dig)
  
  tabl<-paste("<h1>Japon</h1>",version(b_int),
              "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,"<tr><td><b>Contributions</b></td></tr>",row8,row9,row10,"</table>",sep="")
  
  
tabl2<-gsub("&nbsp;%","",tabl)
tabl3<-gsub("\\+","",tabl2)
tabl4<-gsub("\\**","",tabl3)
tabl5<-gsub(">~~","><s>",tabl4)
tabl6<-gsub("~~","</s>",tabl5)
#kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
return(tabl6)}

