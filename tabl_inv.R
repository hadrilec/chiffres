tabl_inv<-function(dig=1){
  charge<<-charge_fr
  row1 <-  ligne_date()
  row2 <-f_row("Produits manufactures","p51s_dim_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
  row3 <-f_row("Construction","p51s_fz_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
  row4 <-f_row("Autres","p51s_dsm_7ch",VT,VA,b_fr,b_fr_old,dig=dig)
  row5 <-f_row("Total","p51s_d_7ch",VT,VA,b_fr,b_fr_old,dig=dig,bold=T)
 
  
tabl<-paste("<h1>Investissement des entreprises</h1><table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,"</table>",sep="")
return(tabl)} #%>%formatStyle(3, border = '1px solid #ddd'))}