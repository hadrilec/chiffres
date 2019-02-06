tabl_al<-function(dig=1){
  charge<<-function(file,acharger=NULL){load(file)
    if (acharger %in% names(series)) return(series[acharger][[1]])
    else stop(paste("La serie",acharger,"n'est pas dans le rdata",file,"\n Le rdata contient:",do.call(paste,as.list(names(series))),sep=" "))
    
  }
  base<<-paste(d_int,"AL.Rdata",sep="//")
  base_an<<-paste(d_int,"AL_an.Rdata",sep="//")
  base_old<<-NULL
row1 <-  ligne_date()
row2 <-f_row("Produit interieur brut","al.pib",VT,VA,base,dig=dig,bold=T)
row3 <-f_row("Consommation privee","al.conso",VT,VA,base,dig=dig)
row4 <-f_row("Investissement","al.fbcf",VT,VA,base,dig=dig)
row5 <-f_row("Consommation publique","al.consopub",VT,VA,base,dig=dig)
row6 <-f_row("Exportations","al.export",VT,VA,base,dig=dig)
row7 <-f_row("Importations","al.import",VT,VA,base,dig=dig)
row8<-"<tr><td><b>Contributions</b></td></tr>"
row9 <-f_row("Demande interieure hors stocks","al.di.contrib",NIV,NIV_AN,base,base_old,"al_an.di.contrib.an",base_an,dig=dig)
row10 <-f_row("Variation de stocks","al.stocks.contrib",NIV,NIV_AN,base,base_old,"al_an.stocks.contrib.an",base_an,dig=dig)
row11 <-f_row("Commerce exterieur","al.comext.contrib",NIV,NIV_AN,base,base_old,"al_an.comext.contrib.an",base_an,dig=dig)
row12<-"<tr><td><b>Detail investissement</b></td></tr>"
row13<-f_row("Investissement en construction","al.fbcfcons",VT,VA,base,dig=dig)
row14<-f_row("Investissement en équipement","al.fbcfeq",VT,VA,base,dig=dig)
row15<-f_row("Investissement autre","al.fbcfautre",VT,VA,base,dig=dig)



tabl<-paste("<h1>Allemagne</h1>",version(base),
              "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,row15,"</table>",sep="")

base_cdm<-paste(d_int,"AL_CDM.Rdata",sep="//")

row1 <-  ligne_date()
row2 <-f_row("RDB","al_cdm.rdb_insee",VT,VA,base_cdm,bold=T,dig=dig)
row3 <-f_row("Emploi salarie","al_cdm.emploi_sal",VT,VA,base_cdm,dig=dig)
row4 <-f_row("Salaires","al_cdm.smpt",VT,VA,base_cdm,dig=dig)
row5 <-f_row("Taux d'activite","al_cdm.taux_activite",NIV,NIV_A,base_cdm,dig=dig)
row6 <-f_row("Population active","al_cdm.popact",VT,VA,base_cdm,dig=dig)
row7 <-f_row("Emploi total","al_cdm.emploi_total",VT,VA,base_cdm,dig=dig)
row8 <-f_row("Taux de chomage","al_cdm.tx_u_cnt",NIV,NIV_A,base_cdm,bold=T,dig=dig)
row9 <-f_row("Inflation energetique","al_cdm.ipch_energie_t",GA,GA_A,base_cdm,dig=dig)
row10 <-f_row("Inflation alimentaire","al_cdm.ipch_alim_t",GA,GA_A,base_cdm,dig=dig)
row11<-f_row("Inflation sous-jacente","al_cdm.ipch_sj_t",GA,GA_A,base_cdm,dig=dig)
row12<-f_row("Inflation totale","al_cdm.ipch_t",GA,GA_A,base_cdm,bold=T,dig=dig)
row13<-f_row("Deflateur de la consommation","al_cdm.def_conso",VT,VA,base_cdm,dig=dig)
row14<-f_row("Taux d'epargne","al_cdm.tx_s_maison_b",NIV,NIV_A,base_cdm,bold=T,dig=dig)


tabl_cdm<-paste("<h1>Compte des menages</h1>",version(base_cdm),
                "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,"</table>",sep="")


tabl2<-gsub("&nbsp;%","",paste(tabl,tabl_cdm,sep=""))
tabl3<-gsub("\\+","",tabl2)
tabl4<-gsub("\\**","",tabl3)
tabl5<-gsub(">~~","><s>",tabl4)
tabl6<-gsub("~~","</s>",tabl5)
#kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
return(tabl6)}

