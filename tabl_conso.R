tabl_conso<-function(dig=1){
  charge<<-charge_fr
  row1 <-  ligne_date()
  row2 <-f_row("Investissement des menages","p51m_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig,bold=T)
  row3 <-"<tr><td><b>Consommation</b></td></tr>"
  row4 <-f_row("Produits agricoles","p3m_az_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
  row5 <-f_row("Produits manufactures","p3m_dim_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
  row6 <-f_row("Energie, eau, dechets","p3m_de_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
  row7 <-f_row("Commerce","p3m_gz_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
  row8 <-f_row("Services marchands hors commerce","p3m_dsmhc_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
  row9 <-f_row("Services non marchands","p3m_oq_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
  row10 <-f_row("Correction territoriale","p41_pchtr_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
  row11 <-f_row("Depenses totales de consommation des menages","p3m_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig,bold=T)
  row12 <-f_row("Consommation effective totale des menages","p41_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
  
  tabl<-paste("<h1>Consommation et investissement des menages</h1><table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,row11,row12,"</table>",sep="")
    return(tabl)}
