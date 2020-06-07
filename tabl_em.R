tabl_em<-function(dig=1){
  charge<<-charge_int
  return(paste(
    "<h1>Emergents hors Chine</h1><table style=\"width:100%\" border=1>",
  ligne_date(),
  f_row("PIB russe","em.rs_gdp",VT,VA,b_int,b_int_old,dig=dig,n_prev=3),
  f_row("Importations russes","em.rs_m",VT,VA,b_int,b_int_old,dig=dig,n_prev=3),
  f_row("PIB indien","em.in_gdp_cvs",VT,VA,b_int,b_int_old,dig=dig,n_prev=3),
  f_row("Importations indiennes","em.in_m_cvs",VT,VA,b_int,b_int_old,dig=dig,n_prev=3),
  f_row("PIB bresilien","em.br_gdp",VT,VA,b_int,b_int_old,dig=dig,n_prev=3),
  f_row("Importations bresiliennes","em.br_m",VT,VA,b_int,b_int_old,dig=dig,n_prev=3),
  f_row("PIB turc","em.tk_gdp_indice",VT,VA,b_int,b_int_old,dig=dig,n_prev=4),
  f_row("Importations turques","em.tk_m_indice",VT,VA,b_int,b_int_old,dig=dig,n_prev=4),
  f_row("PIB PECO","em.peco_gdp",VT,VA,b_int,b_int_old,dig=dig,n_prev=3),
  "</table>",sep=""))}


