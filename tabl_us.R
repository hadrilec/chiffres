tabl_us<-function(dig=1){
  charge<<-function(file,acharger=NULL){load(file)
    if (acharger %in% names(series)) return(series[acharger][[1]])
    else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(names(series))),sep=" "))
    
  }
  base<<-paste(d_int,"US.Rdata",sep="//")
  base_an<<-paste(d_int,"US_an.Rdata",sep="//")
  base_old<<-NULL
  row1 <-  ligne_date()
  row2 <-f_row("Produit interieur brut","us.pib",VT,VA,base,dig=dig,bold=T)
  row3 <-f_row("Consommation privee","us.conso",VT,VA,base,dig=dig)
  row4 <-f_row("Investissement prive","us.inv_hs",VT,VA,base,dig=dig)
  row5 <-f_row("Depenses gouvernementales","us.dp",VT,VA,base,dig=dig)
  row6 <-f_row("Exportations","us.exports",VT,VA,base,dig=dig)
  row7 <-f_row("Importations","us.imports",VT,VA,base,dig=dig)
  row8<-"<tr><td><b>Contributions</b></td></tr>"
  row9 <-f_row("Demande interieure hors stocks","us.di_contrib",NIV,NIV_AN,base,base_old,"us_an.di.contrib.an",base_an,dig=dig)
  row10 <-f_row("Variation de stocks","us.stocks.contrib",NIV,NIV_AN,base,base_old,"us_an.stocks.contrib.an",base_an,dig=dig)
  row11 <-f_row("Commerce exterieur","us.comext.contrib",NIV,NIV_AN,base,base_old,"us_an.comext.contrib.an",base_an,dig=dig)

  tabl<-paste("<h1>Etats-Unis</h1>",version(base),
              "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,"</table>",sep="")
  
  row1 <-  ligne_date()
  row2 <-f_row("RDB","us.cm_rdb",VT,VA,base,bold=T,dig=dig)
  row3 <-f_row("Emploi salarie","us.emp_sal",VT,VA,base,dig=dig)
  row4 <-f_row("Salaires nominaux","us.smpt",VT,VA,base,dig=dig)
  row5 <-f_row("Population active","us.pop_act",VT,VA,base,dig=dig)
  row6 <-f_row("Emploi total","us.emp",VT,VA,base,dig=dig)
  row7 <-f_row("Taux de chomage","us.u",NIV,NIV_A,base,bold=T,dig=dig)
  row8 <-f_row("Inflation energetique","us.ipc_nrj",GA,GA_A,base,dig=dig)
  row9 <-f_row("Inflation alimentaire","us.ipc_fd",GA,GA_A,base,dig=dig)
  row10<-f_row("Inflation sous-jacente","us.ipc_sj",GA,GA_A,base,dig=dig)
  row11<-f_row("Inflation totale","us.ipc",GA,GA_A,base,bold=T,dig=dig)
  row12<-f_row("Deflateur de la consommation","us.defconso",VT,VA,base,dig=dig)
  row13<-f_row("Taux d'epargne","us.txep",NIV,NIV_A,base,bold=T,dig=dig)
  row14<-f_row("Pouvoir d'achat","us.cm_rdbr",VT,VA,base,bold=T,dig=dig)
  
  tabl_cdm<-paste("<h1>Compte des menages</h1>",
                  "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,"</table>",sep="")
  
  #kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
  return(paste(tabl,tabl_cdm,sep=""))}
