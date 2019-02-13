tabl_ze<-function(dig=1){
  charge<<-charge_int
  base<<-paste(d_int,"ZE.Rdata",sep="//")
  base_an<<-paste(d_int,"ZE_an.Rdata",sep="//")
  base_old<<-NULL
  
  row1 <-  ligne_date()
  row2 <-f_row("<b>Produit intérieur brut</b>","ze.pib",VT,VA,base,dig=dig)
row3 <-f_row("Consommation privée","ze.conso",VT,VA,base,dig=dig)
row4 <-f_row("Investissement","ze.fbcf",VT,VA,base,dig=dig)
row5 <-f_row("Consommation publique","ze.conso_pub",VT,VA,base,dig=dig)
row6 <-f_row("Exportations","ze.export",VT,VA,base,dig=dig)
row7 <-f_row("Importations","ze.import",VT,VA,base,dig=dig)
row8<-"<tr><td><b>Contributions</b></td></tr>"
row9 <-f_row("Demande intérieure hors stocks","ze.di.contrib",NIV,NIV_AN,base,base_old,"ze_an.di.contrib.an",base_an,dig=dig)
row10 <-f_row("Variation de stocks","ze.stocks.contrib",NIV,NIV_AN,base,base_old,"ze_an.stocks.contrib.an",base_an,dig=dig)
row11 <-f_row("Commerce extérieur","ze.comext.contrib",NIV,NIV_AN,base,base_old,"ze_an.comext.contrib.an",base_an,dig=dig)
row12<-"<tr><td><b>Detail investissement</b></td></tr>"
row13<-f_row("Investissement en construction","ze.fbcf_cons",VT,VA,base,dig=dig)
row14<-f_row("Investissement en équipement","ze.fbcf_eq",VT,VA,base,dig=dig)


tabl<-paste("<h1>Zone euro</h1>",version(base),
            "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,"</table>",sep="")

base<<-paste(d_int,"ZE_periph.Rdata",sep="//")
base_old<<-NULL

row15 <-  ligne_date()
row16<-f_row("PIB belge","ze_periph.pib_vol_be",VT,VA,base,dig=dig)
row17<-f_row("Importations belges","ze_periph.import_vol_be",VT,VA,base,dig=dig)
row18<-f_row("Exportations belges","ze_periph.export_vol_be",VT,VA,base,dig=dig)
row19<-f_row("PIB NL","ze_periph.pib_vol_nl",VT,VA,base,dig=dig)
row20<-f_row("Importations NL","ze_periph.import_vol_nl",VT,VA,base,dig=dig)
row21<-f_row("Exportations NL","ze_periph.export_vol_nl",VT,VA,base,dig=dig)

tabl_benl<-paste("<h1>Belgique Pays-Bas</h1>",version(base),
                 "<table style=\"width:100%\" border=1>",row15,row16,row17,row18,row19,row20,row21,"</table>",sep="")


base_cdm<-paste(d_int,"ZE_CDM.Rdata",sep="//")

row1 <-  ligne_date()
row2 <-f_row("RDB","ze_cdm.rdb_sa",VT,VA,base_cdm,bold=T,dig=dig)
row3 <-f_row("Emploi salarié","ze_cdm.emploi_sal",VT,VA,base_cdm,dig=dig)
row4 <-f_row("Salaires","ze_cdm.smpt",VT,VA,base_cdm,dig=dig)
row5 <-f_row("Taux d'activité","ze_cdm.tx_activite_sa",NIV,NIV_A,base_cdm,dig=dig)
row6 <-f_row("Population active","ze_cdm.pop_active_sa",VT,VA,base_cdm,dig=dig)
row7 <-f_row("Emploi total","ze_cdm.emploi_tot_sa",VT,VA,base_cdm,dig=dig)
row8 <-f_row("Taux de chômage","ze_cdm.tx_chom",NIV,NIV_A,base_cdm,bold=T,dig=dig)
row9 <-f_row("Inflation énergétique","ze_cdm.ipch_trim_nrj_sa",GA,GA_A,base_cdm,dig=dig)
row10 <-f_row("Inflation alimentaire","ze_cdm.ipch_trim_alim_sa",GA,GA_A,base_cdm,dig=dig)
row11<-f_row("Inflation sous-jacente","ze_cdm.ipch_trim_core_sa",GA,GA_A,base_cdm,dig=dig)
row12<-f_row("Inflation totale","ze_cdm.ipch_trim_tot_sa",GA,GA_A,base_cdm,bold=T,dig=dig)
row13<-f_row("Déflateur de la consommation","ze_cdm.def_conso",VT,VA,base_cdm,dig=dig)
row14<-f_row("Taux d'épargne","ze_cdm.tx_ep",NIV,NIV_A,base_cdm,bold=T,dig=dig)
row15<-f_row("Pouvoir d'achat","ze_cdm.rdbr_sa",VT,VA,base_cdm,bold=T,dig=dig)

tabl_cdm<-paste("<h1>Compte des ménages</h1>",version(base_cdm),
                "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,row15,"</table>",sep="")


tabl2<-gsub("&nbsp;%","",paste(tabl,tabl_benl,tabl_cdm,sep=""))
tabl3<-gsub("\\+","",tabl2)
tabl4<-gsub("\\**","",tabl3)
tabl5<-gsub(">~~","><s>",tabl4)
tabl6<-gsub("~~","</s>",tabl5)
#kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
return(tabl6)}

