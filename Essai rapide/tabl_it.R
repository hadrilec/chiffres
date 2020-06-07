tabl_it<-function(dig=1){
  charge<<-function(file,acharger=NULL){load(file)
    if (acharger %in% names(series)) return(series[acharger][[1]])
    else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(names(series))),sep=" "))
    
  }
  b_int<<-paste(d_int,"IT.Rdata",sep="//")
  b_int_an<<-paste(d_int,"IT_an.Rdata",sep="//")
  b_int_old<<-NULL
row1 <-  ligne_date()
row2 <-f_row("<b>Produit interieur brut</b>","it.td.b1g_s1_7ch",VT,VA,b_int,dig=dig)
row3 <-f_row("Consommation privee","it.td.p3_s145_7ch",VT,VA,b_int,dig=dig)
row4 <-f_row("Investissement","it.td.p51g_7ch",VT,VA,b_int,dig=dig)
row5 <-f_row("Consommation publique","it.td.p3_s13_7ch",VT,VA,b_int,dig=dig)
row6 <-f_row("Exportations","it.td.p6_7ch",VT,VA,b_int,dig=dig)
row7 <-f_row("Importations","it.td.p7_7ch",VT,VA,b_int,dig=dig)
row8 <-f_row("Demande interieure hors stocks","it.di.contrib",NIV,NIV_AN,b_int,b_int_old,"it_an.di.contrib.an",b_int_an,dig=dig)
row9 <-f_row("Variation de stocks","it.stocks.contrib",NIV,NIV_AN,b_int,b_int_old,"it_an.stocks.contrib.an",b_int_an,dig=dig)
row10 <-f_row("Commerce exterieur","it.comext.contrib",NIV,NIV_AN,b_int,b_int_old,"it_an.comext.contrib.an",b_int_an,dig=dig)

tabl<-paste("<h1>Italie</h1>",version(b_int),
              "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,"<tr><td><b>Contributions</b></td></tr>",row8,row9,row10,"</table>",sep="")
b_int_cdm<-paste(d_int,"IT_CDM.Rdata",sep="//")

row1 <-  ligne_date()
row2 <-f_row("RDB Insee","it_cdm.td.rdb_insee_3",VT,VA,b_int_cdm,bold=T,dig=dig)
row3 <-f_row("Emploi salarie","it_cdm.td.emploi_sal_3",VT,VA,b_int_cdm,dig=dig)
row4 <-f_row("Salaires","it_cdm.td.smpt_3",VT,VA,b_int_cdm,dig=dig)
row5 <-f_row("Taux d'activite","it_cdm.td.tx_act",NIV,NIV_A,b_int_cdm,dig=dig)
row6 <-f_row("Population active","it_cdm.td.pop_act_3",VT,VA,b_int_cdm,dig=dig)
row7 <-f_row("Emploi total","it_cdm.td.emploi_3",VT,VA,b_int_cdm,dig=dig)
row8 <-f_row("Taux de chomage","it_cdm.td.tx_chomage",NIV,NIV_AN,b_int_cdm,bold=T,dig=dig)
row9 <-f_row("Inflation energetique","it_cdm.td.ipch_nrj_bis",GA,GA_A,b_int_cdm,dig=dig)
row10<-f_row("Inflation alimentaire","it_cdm.td.ipch_alim_hors_drogue",GA,GA_A,b_int_cdm,dig=dig)
row11<-f_row("Inflation sous-jacente","it_cdm.td.ipch_sj",GA,GA_A,b_int_cdm,dig=dig)
row12<-f_row("Inflation totale","it_cdm.td.ipch_tot",GA,GA_A,b_int_cdm,bold=T,dig=dig)
row13<-f_row("Deflateur de la consommation","it_cdm.td.p3_s145_9",VT,VA,b_int_cdm,dig=dig)
row14<-f_row("Taux d'epargne","it_cdm.td.tx_epargne",NIV,NIV_A,b_int_cdm,bold=T,dig=dig)


tabl_cdm<-paste("<h1>Compte des menages</h1>",version(b_int_cdm),
                "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,"</table>",sep="")


tabl2<-gsub("&nbsp;%","",paste(tabl,tabl_cdm,sep=""))
tabl3<-gsub("\\+","",tabl2)
tabl4<-gsub("\\**","",tabl3)
tabl5<-gsub(">~~","><s>",tabl4)
tabl6<-gsub("~~","</s>",tabl5)
#kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
return(tabl6)}

