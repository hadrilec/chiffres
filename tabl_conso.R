tabl_conso<-function(dig=1){
  charge<<-charge_fr
  return(paste(
    "<h1>Consommation et investissement des menages</h1><table style=\"width:100%\" border=1>",
  ligne_date(),
  f_row("Investissement des menages","p51m_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig,bold=T),
  "<tr><td><b>Consommation</b></td></tr>",
  f_row("Produits agricoles","p3m_az_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Produits manufactures","p3m_dim_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Energie, eau, dechets","p3m_de_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Commerce","p3m_gz_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Services marchands hors commerce","p3m_dsmhc_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Services non marchands","p3m_oq_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Correction territoriale","p41_pchtr_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Depenses totales de consommation des menages","p3m_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig,bold=T),
  f_row("Consommation effective totale des menages","p41_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  "</table>",sep=""))}

