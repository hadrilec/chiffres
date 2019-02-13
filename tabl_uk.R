tabl_uk<-function(dig=1){
  charge<<-charge_int
  base<<-paste(d_int,"UK.Rdata",sep="//")
  base_old<<-NULL
  row1 <-  ligne_date()
  row2 <-f_row("Produit interieur brut","uk.pib_vol_q",VT,VA,base,dig=dig,bold=T)
  row3 <-f_row("Consommation privee","uk.conso_men_vol_q",VT,VA,base,dig=dig)
  row4 <-f_row("Investissement","uk.imt_vol_q",VT,VA,base,dig=dig)
  row5 <-f_row("Consommation publique","uk.conso_pub_vol_q",VT,VA,base,dig=dig)
  row6 <-f_row("Exportations","uk.x_vol_q",VT,VA,base,dig=dig)
  row7 <-f_row("Importations","uk.m_vol_q",VT,VA,base,dig=dig)
  row8<-"<tr><td><b>Detail investissement</b></td></tr>"
  row9<-f_row("Investissement des entreprises","uk.imt_ent_vol_q",VT,VA,base,dig=dig)
  row10<-f_row("Investissement des menages","uk.imt_log_vol_q",VT,VA,base,dig=dig)
  row11<-f_row("Investissement public","uk.imt_pub_vol_q",VT,VA,base,dig=dig)
  
  tabl<-paste("<h1>Grande-Bretagne</h1>",version(base),
              "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,"</table>",sep="")

  row1 <-  ligne_date()
  row2 <-f_row("RDB","uk.rdb_val_q",VT,VA,base,bold=T,dig=dig)
  row3 <-f_row("Emploi salarie","uk.emp_sal_q",VT,VA,base,dig=dig)
  row4 <-f_row("Salaires nominaux","uk.smpt",VT,VA,base,dig=dig)
  row5 <-f_row("Population active","uk.pop_act_q",VT,VA,base,dig=dig)
  row6 <-f_row("Emploi total","uk.emp_tot_q",VT,VA,base,dig=dig)
  row7 <-f_row("Taux de chomage","uk.tx_u_bit_q",NIV,NIV_A,base,bold=T,dig=dig)
  row8 <-f_row("Inflation energetique","uk.ipc_nrj",GA,GA_A,base,dig=dig)
  row9 <-f_row("Inflation alimentaire","uk.ipc_alim",GA,GA_A,base,dig=dig)
  row10<-f_row("Inflation sous-jacente","uk.ipc_sj",GA,GA_A,base,dig=dig)
  row11<-f_row("Inflation totale","uk.ipc_cvs",GA,GA_A,base,bold=T,dig=dig)
  row12<-f_row("Deflateur de la consommation","uk.deflateur_conso_q",VT,VA,base,dig=dig)
  row13<-f_row("Taux d'epargne","uk.tx_s_q",NIV,NIV_A,base,bold=T,dig=dig)
  row14<-f_row("Pouvoir d'achat (avec fonds de pension)","uk.rdb_reel_fdp_q",VT,VA,base,bold=T,dig=dig)
  
  tabl_cdm<-paste("<h1>Compte des menages</h1>",
                  "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,row13,row14,"</table>",sep="")
  
  #kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
  return(paste(tabl,tabl_cdm,sep=""))}



