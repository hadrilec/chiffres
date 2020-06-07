tabl_oil<-function(dig=1){
  charge<<-charge_int
 return(paste(
   "<h1>Petrole</h1>",
   "<table style=\"width:100%\" border=1>",
ligne_date(),
f_row("Brent en dollar","brent_m",NIV_depuis_mensuel,NIV_A,b_int,b_int_old,dig=dig,bold=T,n_prev=2),
"</table>",sep=""))}



