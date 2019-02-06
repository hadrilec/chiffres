tabl_es<-function(){
  rdata<-"ES"
  rdata_an<-"ES_an"
  load(paste(d_int,"//",rdata_an,".Rdata",sep="")) 
  series_an<-series
  base_old<-NULL
  base_an_old<-NULL
  if(!is.null(base_old)){load(base_old)
    series_old<-series}
  if(!is.null(base_an_old)){load(base_an_old)
    series_an_old<-series}
  load(paste(d_int,"//",rdata,".Rdata",sep=""))
  
row1 <-  ligne_date()
row2 <-f_row("Produit interieur brut","es.td.b1g_7ch_ind",VT,VA,bold=T)
row3 <-f_row("Consommation privee","es.td.p3_s145_7ch_ind",VT,VA)
row4 <-f_row("Investissement","es.td.p51g_7ch_ind",VT,VA)
row5 <-f_row("Consommation publique","es.td.p3_s13_7ch_ind",VT,VA)
row6 <-f_row("Exportations","es.td.p6_7ch",VT,VA)
row7 <-f_row("Importations","es.td.p7_7ch",VT,VA)
row8 <-f_row("Demande interieure hors stocks","es.di.contrib",NIV,NIV_AN,"es_an.di.contrib.an")
row9 <-f_row("Variation de stocks","es.stocks.contrib",NIV,NIV_AN,"es_an.stocks.contrib.an")
row10 <-f_row("Commerce exterieur","es.comext.contrib",NIV,NIV_AN,"es_an.comext.contrib.an")


tabl<-paste("<h1>Espagne</h1>",version(paste(d_int,"//",rdata,".Rdata",sep="")),
              "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,"<tr><td><b>Contributions</b></td></tr>",row8,row9,row10,"</table>",sep="")
rdata<-"ES_CDM"
base_old<-NULL
base_an_old<-NULL
if(!is.null(base_old)){load(base_old)
  series_old<-series}
if(!is.null(base_an_old)){load(base_an_old)
  series_an_old<-series}
load(paste(d_int,"//",rdata,".Rdata",sep=""))

row1 <-  ligne_date()
row2 <-f_row("RDB","es_cdm.td.b6g_s145_3",VT,VA,bold=T)
row3 <-f_row("Emploi salarie","es_cdm.td.emploi_sal_3",VT,VA)
row4 <-f_row("Salaires","es_cdm.smpt",VT,VA)
row5 <-f_row("Taux d'activite","es_cdm.tx_act",NIV,NIV_A)
row6 <-f_row("Population active","es_cdm.td.pop_act_3",VT,VA)
row7 <-f_row("Emploi total","es_cdm.td.emploi_3",VT,VA)
row8 <-f_row("Taux de chomage","es_cdm.td.tx_chomage",NIV,NIV_A,bold=T)
row9 <-f_row("Inflation energetique","es_cdm.ipch_trim_nrj_sa",GA,GA_A)
row10 <-f_row("Inflation alimentaire","es_cdm.ipch_trim_alim_sa",GA,GA_A)
row11<-f_row("Inflation sous-jacente","es_cdm.ipch_trim_core_sa",GA,GA_A)
row12<-f_row("Inflation totale","es_cdm.ipch_trim_tot_sa_prev",GA,GA_A,bold=T)
row13<-f_row("Deflateur de la consommation","es_cdm.td.p3_s145_9",VT,VA)
row14<-f_row("Taux d'epargne","es_cdm.tx_ep",NIV,NIV_A,bold=T)


tabl_cdm<-paste("<h1>Compte des menages</h1>",version(paste(d_int,"//",rdata,".Rdata",sep="")),
                    "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,"</table>",sep="")


tabl2<-gsub("&nbsp;%","",paste(tabl,tabl_cdm,sep=""))
tabl3<-gsub("\\+","",tabl2)
tabl4<-gsub("\\**","",tabl3)
tabl5<-gsub(">~~","><s>",tabl4)
tabl6<-gsub("~~","</s>",tabl5)
#kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
return(tabl6)}

