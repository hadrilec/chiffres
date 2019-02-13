tabl_jp<-function(dig=1){
  charge<<-charge_int
  base<<-paste(d_int,"JP.Rdata",sep="//")
  base_an<<-paste(d_int,"JP_an.Rdata",sep="//")
  base_old<<-NULL
  row1 <-  ligne_date()
  row2 <-f_row("Produit interieur brut","jp.pib",VT,VA,base,dig=dig,bold=T)
  row3 <-f_row("Consommation privee","jp.conso",VT,VA,base,dig=dig)
  row4 <-f_row("Investissement","jp.inv",VT,VA,base,dig=dig)
  row5 <-f_row("Consommation publique","jp.conso_pu",VT,VA,base,dig=dig)
  row6 <-f_row("Exportations","jp.exp_trim",VT,VA,base,dig=dig)
  row7 <-f_row("Importations","jp.imp",VT,VA,base,dig=dig)
  row8<-"<tr><td><b>Contributions</b></td></tr>"
  row9 <-f_row("Demande interieure hors stocks","jp.di.contrib",NIV,NIV_AN,base,base_old,"jp_an.di.contrib.an",base_an,dig=dig)
  row10 <-f_row("Variation de stocks","jp.stocks.contrib",NIV,NIV_AN,base,base_old,"jp_an.stocks.contrib.an",base_an,dig=dig)
  row11 <-f_row("Commerce exterieur","jp.comext.contrib",NIV,NIV_AN,base,base_old,"jp_an.comext.contrib.an",base_an,dig=dig)
  row12<-"<tr><td><b>Detail investissement</b></td></tr>"
  row13 <-f_row("Investissement public","jp.inv_pu",VT,VA,base,dig=dig)
  row14 <-f_row("Investissement prive des menages","jp.inv_priv_l",VT,VA,base,dig=dig)
  row15 <-f_row("Investissement des entreprises","jp.inv_priv_hl",VT,VA,base,dig=dig)
  
  tabl<-paste("<h1>Japon</h1>",version(base),
              "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,row15,"</table>",sep="")
  
  row1 <-  ligne_date()
  row2 <-f_row("RDB","jp.conso_rdb_cvs",VT,VA,base,bold=T,dig=dig)
  row3 <-f_row("Emploi salarie","jp.emp_salarie_q",VT,VA,base,dig=dig)
  row4 <-f_row("Salaires nominaux","jp.smpt_q",VT,VA,base,dig=dig)
  row5 <-f_row("Population active","jp.popactive_q",VT,VA,base,dig=dig)
  row6 <-f_row("Emploi total","jp.emp",VT,VA,base,dig=dig)
  row7 <-f_row("Taux de chomage","jp.u",NIV,NIV_A,base,bold=T,dig=dig)
  row8 <-f_row("Inflation energetique","jp.ipc_energ",GA,GA_A,base,dig=dig)
  row9 <-f_row("Inflation alimentaire","jp.ipc_alim",GA,GA_A,base,dig=dig)
  row10<-f_row("Inflation sous-jacente","jp.ipc_sj",GA,GA_A,base,dig=dig)
  row11<-f_row("Inflation totale","jp.ipc",GA,GA_A,base,bold=T,dig=dig)
  row12<-f_row("Deflateur de la consommation","jp.defconso",VT,VA,base,dig=dig)
  row13<-f_row("Taux d'epargne","jp.tx_ep",NIV,NIV_A,base,bold=T,dig=dig)
  row14<-f_row("Pouvoir d'achat","jp.rdb_r",VT,VA,base,bold=T,dig=dig)
  
  tabl_cdm<-paste("<h1>Compte des menages</h1>",
                  "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,"</table>",sep="")
  
  #kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
  return(paste(tabl,tabl_cdm,sep=""))}

