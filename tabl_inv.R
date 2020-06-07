tabl_inv<-function(dig=1){
  charge<<-charge_fr
return(paste(
  "<h1>Investissement des entreprises</h1><table style=\"width:100%\" border=1>",
  ligne_date(),
  f_row("Produits manufactures","p51s_dim_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Construction","p51s_fz_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Autres","p51s_dsm_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
  f_row("Total","p51s_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig,bold=T),
  "</table>",sep=""))} 