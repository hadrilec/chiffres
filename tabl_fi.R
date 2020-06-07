tabl_fi<-function(dig=1){
  charge<<-charge_int
 
return(paste(
  "<h1>Taux de change</h1>",
  "<table style=\"width:100%\" border=1>",
ligne_date(),
f_row("Dollar en euro","usdeur",NIV_depuis_mensuel,NIV_A,b_int,b_int_old,dig=2,bold=T),
f_row("Livre sterling en euro","gbpeur",NIV_depuis_mensuel,NIV_A,b_int,b_int_old,dig=2),
f_row("Yen en euro","jpyeur",NIV_depuis_mensuel,NIV_A,b_int,b_int_old,dig=2),
"</table>",sep=""))}

