tabl_fi<-function(dig=1){
  charge<<-charge_int
  base<<-paste(d_int,"OIL.Rdata",sep="//")
  base_an<<-paste(d_int,"AL_an.Rdata",sep="//")
  base_old<<-NULL
row1 <-  ligne_date()
row2 <-f_row("<b>Brent en dollar</b>","brent_m",NIV,NIV_AN,base,dig=dig)


tabl<-paste("<h1>Petrole</h1>",version(base),
              "<table style=\"width:100%\" border=1>",row1,row2,"</table>",sep="")

base_cdm<-paste(d_int,"FI.change.Rdata",sep="//")

row1 <-  ligne_date()
row2 <-f_row("Dollar en euro","usdeur",NIV,NIV_AN,base_cdm,dig=dig)
row3 <-f_row("Livre sterling en euro","gbpeur",NIV,NIV_AN,base_cdm,dig=dig)
row4 <-f_row("Yen en euro","jpyeur",NIV,NIV_AN,base_cdm,dig=dig)

tabl_cdm<-paste("<h1>Taux de change</h1>",version(base_cdm),
                "<table style=\"width:100%\" border=1>",row1,row2,row3,row4,"</table>",sep="")

return(paste(tabl,tabl_cdm,sep=""))}

