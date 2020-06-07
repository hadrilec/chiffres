tabl_fitaux<-function(dig=1){
  charge<<-charge_int


return(paste(
  "<h1>Taux souverains europeens</h1><table style=\"width:100%\" border=1>",
ligne_date(),
f_row("Taux francais 10 ans","fitaux.fr_gov10y",NIV,NIV_A,b_int,b_int_old,dig=dig,bold=T),
f_row("Taux allemand 10 ans","fitaux.de_gov10y",NIV,NIV_A,b_int,b_int_old,dig=dig),
f_row("Taux italien 10 ans","fitaux.it_gov10y",NIV,NIV_A,b_int,b_int_old,dig=dig),
f_row("Taux espagnol 10 ans","fitaux.es_gov10y",NIV,NIV_A,b_int,b_int_old,dig=dig),
f_row("Taux neerlandais 10 ans","fitaux.nl_gov10y",NIV,NIV_A,b_int,b_int_old,dig=dig),
"</table>",sep="")
)}

