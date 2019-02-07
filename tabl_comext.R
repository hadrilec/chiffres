tabl_comext<-function(dig=1){
  charge<<-function(file,acharger=NULL){load(file)
    if (acharger %in% names(series)) return(series[acharger][[1]])
    else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(names(series))),sep=" "))
    
  }
  base<-paste(d_int,"DM.Rdata",sep="//")
  base_old<-NULL


row1 <-  ligne_date()
row2 <-f_row("Commerce mondial","dm_monde",VT,VA,base,dig=dig)
row3 <-f_row("Importations des economies avancees","m_av",VT,VA,base,dig=dig)
row4 <-f_row("Importations des economies emergentes","m_em",VT,VA,base,dig=dig)
row5 <-f_row("Demande mondiale adresse a la France","dm_france",VT,VA,base,dig=dig)

tabl<-paste("<h1>Commerce mondial et demande adressee a la France</h1><table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,"</table>",sep="")
charge<<-function(file,acharger=NULL){load(file)
  if (acharger %in% rownames(table_noteRch)) return(ts(table_noteRch[acharger,],start=c(as.numeric(substr(names(table_noteRch[acharger,])[1],1,4)),as.numeric(substr(names(table_noteRch[acharger,])[1],6,6))),frequency=4))
  else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(rownames(table_noteRch))),sep=" "))
  
}
row1 <-  ligne_date()
row2 <-f_row("Exportations","p6_d_7ch",VT,VA,b_fr,bold=T,dig=dig)
row3 <-f_row("dont Produits manufactures","p6_dim_7ch",VT,VA,b_fr,dig=dig)
row4 <-f_row("Importations","p7_d_7ch",VT,VA,b_fr,bold=T,dig=dig)
row5 <-f_row("dont Produits manufactures","p7_dim_7ch",VT,VA,b_fr,dig=dig)
row6 <-f_row("Contribution du commerce exterieur","c.solde_d_7ch",NIV,NIV_A,b_fr,bold=T,dig=dig)

tabl_ext<-paste("<h1>Prevision de croissance des echanges exterieurs</h1><table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,"</table>",sep="")


#kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
return(cbind(tabl,tabl_ext))}

