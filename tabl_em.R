tabl_em<-function(dig=1){
  charge<<-charge_int
  base<<-paste(d_int,"CN.Rdata",sep="//")
  base_old<<-NULL
  row1 <-  ligne_date()
  row2 <-f_row("PIB Insee","cn.pib_cn",VT,VA,base,dig=dig)
  row3 <-f_row("PIB officiel","cn.pib_nbsc",NIV,NIV_AN,base,dig=dig)
  row4 <-f_row("Exportations","cn.exp_cn",VT,VA,base,dig=dig)
  row5 <-f_row("Importations","cn.imp_cn",VT,VA,base,dig=dig)
 
  tabl_cn<-paste("<h1>Chine</h1>",version(base),
              "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,"</table>",sep="")
  
  
  base<<-paste(d_int,"EM.Rdata",sep="//")
  base_old<<-NULL
  row1 <-  ligne_date()
  row2 <-f_row("PIB russe","em.rs_gdp",VT,VA,base,dig=dig)
  row3 <-f_row("Importations russes","em.rs_m",VT,VA,base,dig=dig)
  row4 <-f_row("PIB indien","em.in_gdp_cvs",VT,VA,base,dig=dig)
  row5 <-f_row("Importations indiennes","em.in_m_cvs",VT,VA,base,dig=dig)
  row6 <-f_row("PIB bresilien","em.br_gdp",VT,VA,base,dig=dig)
  row7 <-f_row("Importations bresiliennes","em.br_m",VT,VA,base,dig=dig)
  row8 <-f_row("PIB turc","em.tk_gdp_indice",VT,VA,base,dig=dig)
  row9 <-f_row("Importations turques","em.tk_m_indice",VT,VA,base,dig=dig)
  row10 <-f_row("PIB PECO","em.peco_gdp",VT,VA,base,dig=dig)
  
  
  tabl_emhc<-paste("<h1>Autres emergents</h1>",version(base),
              "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,row5,row6,row7,row8,row9,row10,"</table>",sep="")
  
 
#kable(cbind(tabl3,tabl_an3),"html")%>%kable_styling(full_width = T)%>%column_spec(11,border_right = T)%>%add_header_above(c(" ", "Trimestrielles" = 10, "Annuelles" = 3))
return(paste(tabl_cn,tabl_emhc,sep=""))}

