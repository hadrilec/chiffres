tabl_es<-function(dig=1){
  charge<<-charge_int
  base<-paste(d_int,"ES.Rdata",sep="//")
  base_an<-paste(d_int,"ES_an.Rdata",sep="//")
  base_old<-NULL
  row1 <-  ligne_date()
  row2 <-f_row("Produit interieur brut","es.td.b1g_7ch_ind",VT,VA,base,dig=dig,bold=T)
  row3 <-f_row("Consommation privee","es.td.p3_s145_7ch_ind",VT,VA,base,dig=dig)
  row4 <-f_row("Investissement","es.td.p51g_7ch_ind",VT,VA,base,dig=dig)
  row5 <-f_row("Consommation publique","es.td.p3_s13_7ch_ind",VT,VA,base,dig=dig)
  row6 <-f_row("Exportations","es.td.p6_7ch",VT,VA,base,dig=dig)
  row7 <-f_row("Importations","es.td.p7_7ch",VT,VA,base,dig=dig)
  row8<-"<tr><td><b>Contributions</b></td></tr>"
  row9 <-f_row("Demande interieure hors stocks","es.di.contrib",NIV,NIV_AN,base,base_old,"es_an.di.contrib.an",base_an,dig=dig)
  row10 <-f_row("Variation de stocks","es.stocks.contrib",NIV,NIV_AN,base,base_old,"es_an.stocks.contrib.an",base_an,dig=dig)
  row11 <-f_row("Commerce exterieur","es.comext.contrib",NIV,NIV_AN,base,base_old,"es_an.comext.contrib.an",base_an,dig=dig)
  row12<-"<tr><td><b>Detail investissement</b></td></tr>"
  row13<-f_row("Investissement en construction","es.td.p51g_n11kg_7ch_ind",VT,VA,base,dig=dig)
  row14<-f_row("Investissement en équipement","es.td.p51g_n11mg_7ch_ind",VT,VA,base,dig=dig)
  
  
  tabl<-paste("<h1>Espagne</h1>",version(base),
              "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,"</table>",sep="")
base_cdm<-paste(d_int,"ES_CDM.Rdata",sep="//")

row1 <-  ligne_date()
row2 <-f_row("RDB","es_cdm.td.b6g_s145_3",VT,VA,base_cdm,bold=T,dig=dig,n_prev=3)
row3 <-f_row("Emploi salarie","es_cdm.td.emploi_sal_3",VT,VA,base_cdm,dig=dig,n_prev=3)
row4 <-f_row("Salaires","es_cdm.smpt",VT,VA,base_cdm,dig=dig,n_prev=3)
row5 <-f_row("Taux d'activite","es_cdm.tx_act",NIV,NIV_A,base_cdm,dig=dig,n_prev=3)
row6 <-f_row("Population active","es_cdm.td.pop_act_3",VT,VA,base_cdm,dig=dig,n_prev=3)
row7 <-f_row("Emploi total","es_cdm.td.emploi_3",VT,VA,base_cdm,dig=dig,n_prev=3)
row8 <-f_row("Taux de chomage","es_cdm.td.tx_chomage",NIV,NIV_A,base_cdm,bold=T,dig=dig,n_prev=2)
row9 <-f_row("Inflation energetique","es_cdm.ipch_trim_nrj_sa",GA,GA_A,base_cdm,dig=dig,n_prev=2)
row10 <-f_row("Inflation alimentaire","es_cdm.ipch_trim_alim_sa",GA,GA_A,base_cdm,dig=dig,n_prev=2)
row11<-f_row("Inflation sous-jacente","es_cdm.ipch_trim_core_sa",GA,GA_A,base_cdm,dig=dig,n_prev=2)
row12<-f_row("Inflation totale","es_cdm.ipch_trim_tot_sa_prev",GA,GA_A,base_cdm,bold=T,dig=dig,n_prev=2)
row13<-f_row("Deflateur de la consommation","es_cdm.td.p3_s145_9",VT,VA,base_cdm,dig=dig,n_prev=3)
row14<-f_row("Taux d'epargne","es_cdm.tx_ep",NIV,NIV_A,base_cdm,bold=T,dig=dig,n_prev=3)


tabl_cdm<-paste("<h1>Compte des menages</h1>",version(base_cdm),
                    "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,"</table>",sep="")

#kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
return(paste(tabl,tabl_cdm,sep=""))}

