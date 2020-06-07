tabl_benl<-function(dig=1){
  charge<<-charge_int
return(paste(
  "<h1>Belgique Pays-Bas</h1>",
  "<table style=\"width:100%\" border=1>",
ligne_date(),
f_row("PIB belge","benl.pib_vol_be",VT,VA,b_int,b_int_old,dig=dig,bold=T,n_prev=3),
f_row("Importations belges","benl.import_vol_be",VT,VA,b_int,b_int_old,dig=dig,n_prev=4),
f_row("Exportations belges","benl.export_vol_be",VT,VA,b_int,b_int_old,dig=dig,n_prev=4),
f_row("PIB NL","benl.pib_vol_nl",VT,VA,b_int,b_int_old,dig=dig,bold=T,n_prev=4),
f_row("Importations NL","benl.import_vol_nl",VT,VA,b_int,b_int_old,dig=dig,n_prev=4),
f_row("Exportations NL","benl.export_vol_nl",VT,VA,b_int,b_int_old,dig=dig,n_prev=4),
"</table>",sep=""))}


