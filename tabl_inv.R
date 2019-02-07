tabl_inv<-function(dig=1){
  charge<<-function(file,acharger=NULL){load(file)
    if (acharger %in% rownames(table_noteRch)) return(ts(table_noteRch[acharger,],start=c(as.numeric(substr(names(table_noteRch[acharger,])[1],1,4)),as.numeric(substr(names(table_noteRch[acharger,])[1],6,6))),frequency=4))
    else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(rownames(table_noteRch))),sep=" "))
    
  }
  row1 <-  ligne_date()
  row2 <-f_row("Produits manufactures","p51s_dim_7ch",VT,VA,b_fr,dig=dig)
  row3 <-f_row("Construction","p51s_fz_7ch",VT,VA,b_fr,dig=dig)
  row4 <-f_row("Autres","p51s_dsm_7ch",VT,VA,b_fr,dig=dig)
  row5 <-f_row("<b>Total</b>","p51s_d_7ch",VT,VA,b_fr,dig=dig)
 
  
tabl<-paste("<h1>Investissement des entreprises</h1><table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,"</table>",sep="")
return(tabl)} #%>%formatStyle(3, border = '1px solid #ddd'))}