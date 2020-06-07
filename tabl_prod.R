tabl_prod<-function(dig=1){
  charge<<-charge_fr
  
return(paste(
  "<h1>Production par branches</h1><table style=\"width:100%\" border=1>",
ligne_date(),
f_row("Agriculture","p1e_az_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
f_row("Branches manufacturieres","p1e_dim_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
f_row("Energie, eau, dechets","p1e_de_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
f_row("Construction","p1e_fz_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
f_row("Commerce","p1e_gz_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
f_row("Services marchands hors commerce","p1e_dsmhc_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
f_row("Services non marchands","p1e_oq_7ch",VT,VA,b_fr,b_fr_old,dig=dig),
f_row("Total","p1e_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig,bold=T),
"</table>",sep=""))}

