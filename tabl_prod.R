tabl_prod<-function(dig=1){
  charge<<-charge_fr
  


row1 <-  ligne_date()
row2 <-f_row("Agriculture","p1e_az_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
row3 <-f_row("Branches manufacturieres","p1e_dim_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
row4 <-f_row("Energie, eau, dechets","p1e_de_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
row5 <-f_row("Construction","p1e_fz_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
row6 <-f_row("Commerce","p1e_gz_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
row7 <-f_row("Services marchands hors commerce","p1e_dsmhc_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
row8 <-f_row("Services non marchands","p1e_oq_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
row9 <-f_row("Total","p1e_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig,bold=T)
tabl<-paste("<h1>Production par branches</h1><table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,"</table>",sep="")

return(tabl)}

