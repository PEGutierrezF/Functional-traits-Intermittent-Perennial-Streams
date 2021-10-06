


FD.output<-dbFD(tr.dist, sp.matrix)
FD.output$FRic
FD.output$FEve
FD.output$FDis
FD.output$FDiv
FD.output$RaoQ


treatment.matrix$FRic<-FD.output$FRic
treatment.matrix$FEve<-FD.output$FEve
treatment.matrix$FDis<-FD.output$FDis
treatment.matrix$FDiv<-FD.output$FDiv
treatment.matrix$RaoQ<-FD.output$RaoQ
treatment.matrix


