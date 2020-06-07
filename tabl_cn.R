tabl_cn<-function(dig=1){
  charge<<-charge_int
  return(paste(
    "<h1>Chine</h1><table style=\"width:100%\" border=1>",
    ligne_date(),  
    f_row("PIB Insee","cn.pib_cn",VT,VA,b_int,b_int_old,dig=dig),
    f_row("PIB officiel","cn.pib_nbsc",NIV,MOY_PND,b_int,b_int_old,dig=dig),
    f_row("Exportations","cn.exp_cn",VT,VA,b_int,b_int_old,dig=dig),
    f_row("Importations","cn.imp_cn",VT,VA,b_int,b_int_old,dig=dig),
    "</table>",sep=""))}
 


