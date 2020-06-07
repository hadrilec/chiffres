tabl_al<-function(dig=1){
  charge<<-function(file,acharger=NULL){load(file)
    if (acharger %in% names(series)) return(series[acharger][[1]])
    else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(names(series))),sep=" "))
    
  }
  b_int<<-paste(d_int,"AL.Rdata",sep="//")
  b_int_an<<-paste(d_int,"AL_an.Rdata",sep="//")
  b_int_old<<-NULL
row1 <-  ligne_date()
row2 <-f_row("<b>Produit interieur brut</b>","al.pib",VT,VA,b_int,dig=dig)
row3 <-f_row("Consommation privee","al.conso",VT,VA,b_int,dig=dig)
row4 <-f_row("Investissement","al.fbcf",VT,VA,b_int,dig=dig)
row5 <-f_row("Consommation publique","al.consopub",VT,VA,b_int,dig=dig)
row6 <-f_row("Exportations","al.export",VT,VA,b_int,dig=dig)
row7 <-f_row("Importations","al.import",VT,VA,b_int,dig=dig)
row8 <-f_row("Demande interieure hors stocks","al.di.contrib",NIV,NIV_AN,b_int,b_int_old,"al_an.di.contrib.an",b_int_an,dig=dig)
row9 <-f_row("Variation de stocks","al.stocks.contrib",NIV,NIV_AN,b_int,b_int_old,"al_an.stocks.contrib.an",b_int_an,dig=dig)
row10 <-f_row("Commerce exterieur","al.comext.contrib",NIV,NIV_AN,b_int,b_int_old,"al_an.comext.contrib.an",b_int_an,dig=dig)

tabl<-paste("<h1>Allemagne</h1>",version(b_int),
              "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,"<tr><td><b>Contributions</b></td></tr>",row8,row9,row10,"</table>",sep="")

b_int_cdm<-paste(d_int,"AL_CDM.Rdata",sep="//")

row1 <-  ligne_date()
row2 <-f_row("RDB","al_cdm.rdb_insee",VT,VA,b_int_cdm,bold=T,dig=dig)
row3 <-f_row("Emploi salarie","al_cdm.emploi_sal",VT,VA,b_int_cdm,dig=dig)
row4 <-f_row("Salaires","al_cdm.smpt",VT,VA,b_int_cdm,dig=dig)
row5 <-f_row("Taux d'activite","al_cdm.taux_activite",NIV,NIV_A,b_int_cdm,dig=dig)
row6 <-f_row("Population active","al_cdm.popact",VT,VA,b_int_cdm,dig=dig)
row7 <-f_row("Emploi total","al_cdm.emploi_total",VT,VA,b_int_cdm,dig=dig)
row8 <-f_row("Taux de chomage","al_cdm.tx_u_cnt",NIV,NIV_AN,b_int_cdm,bold=T,dig=dig)
row9 <-f_row("Inflation energetique","al_cdm.ipch_energie_t",GA,GA_A,b_int_cdm,dig=dig)
row10 <-f_row("Inflation alimentaire","al_cdm.ipch_alim_t",GA,GA_A,b_int_cdm,dig=dig)
row11<-f_row("Inflation sous-jacente","al_cdm.ipch_sj_t",GA,GA_A,b_int_cdm,dig=dig)
row12<-f_row("Inflation totale","al_cdm.ipch_t",GA,GA_A,b_int_cdm,bold=T,dig=dig)
row13<-f_row("Deflateur de la consommation","al_cdm.def_conso",VT,VA,b_int_cdm,dig=dig)
row14<-f_row("Taux d'epargne","al_cdm.tx_s_maison_b",NIV,NIV_A,b_int_cdm,bold=T,dig=dig)


tabl_cdm<-paste("<h1>Compte des menages</h1>",version(b_int_cdm),
                "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,"</table>",sep="")


tabl2<-gsub("&nbsp;%","",paste(tabl,tabl_cdm,sep=""))
tabl3<-gsub("\\+","",tabl2)
tabl4<-gsub("\\**","",tabl3)
tabl5<-gsub(">~~","><s>",tabl4)
tabl6<-gsub("~~","</s>",tabl5)
#kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
return(tabl6)}

