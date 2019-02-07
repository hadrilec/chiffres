tabl_pibfr<-function(dig=1){
  charge<<-function(file,acharger=NULL){load(file)
    if (acharger %in% rownames(table_noteRch)) return(ts(table_noteRch[acharger,],start=c(as.numeric(substr(names(table_noteRch[acharger,])[1],1,4)),as.numeric(substr(names(table_noteRch[acharger,])[1],6,6))),frequency=4))
    else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(rownames(table_noteRch))),sep=" "))
    
  }
  row1 <-  ligne_date()
  row2 <-f_row("Produit interieur brut","pib_7ch",VT,VA,b_fr,dig=dig,bold=T)
  row3 <-f_row("Consommation privee","p3m_d_7ch",VT,VA,b_fr,dig=dig)
  row4 <-f_row("Investissement","p51_d_7ch",VT,VA,b_fr,dig=dig)
  row5 <-f_row("Consommation publique","p3pg_d_7ch",VT,VA,b_fr,dig=dig)
  row6 <-f_row("Exportations","p6_d_7ch",VT,VA,b_fr,dig=dig)
  row7 <-f_row("Importations","p7_d_7ch",VT,VA,b_fr,dig=dig)
  row8<-"<tr><td><b>Contributions</b></td></tr>"
  row9 <-f_row("Demande interieure hors stocks","c.dintfhs_d_7ch",NIV,NIV_A,b_fr,dig=dig)
  row10 <-f_row("Variations de stocks","c.p54_d_7ch",NIV,NIV_A,b_fr,dig=dig)
  row11 <-f_row("Commerce exterieure","c.solde_d_7ch",NIV,NIV_A,b_fr,dig=dig)
 
  
tabl<-paste("<h1>Fiche de PIB France</h1><table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,"</table>",sep="")
return(tabl)} #%>%formatStyle(3, border = '1px solid #ddd'))}