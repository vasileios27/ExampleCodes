! ------------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.4 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223,
!           1405.1434, 1411.0675, 1503.03098, 1703.09237, 1706.05372, 1805.07306  
! (c) Florian Staub, Mark Goodsell and Werner Porod 2020  
! ------------------------------------------------------------------------------  
! File created at 6:45 on 26.5.2021   
! ----------------------------------------------------------------------  
 
 
Module Couplings_BGLNCS
 
Use Control 
Use Settings 
Use Model_Data_BGLNCS 
Use Mathematics, Only: CompareMatrices, Adjungate 
 
Contains 
 
 Subroutine AllCouplingsReallyAll(Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,Lam1,Lam3,               & 
& Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,ZH,ZP,g1,g2,ZZ,g3,Y1d11,Y1d12,Y1d13,              & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL,ZDR,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,             & 
& ZEL,ZER,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL,ZUR,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,           & 
& C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv,cplAhAhAh,cplAhAhhh,cplAhhhhh,cplAhHmcHm,       & 
& cplhhhhhh,cplhhHmcHm,cplAhAhAhAh,cplAhAhAhhh,cplAhAhhhhh,cplAhAhHmcHm,cplAhhhhhhh,     & 
& cplAhhhHmcHm,cplhhhhhhhh,cplhhhhHmcHm,cplHmHmcHmcHm,cplAhhhVP,cplAhhhVZ,               & 
& cplAhHmcVWm,cplAhcHmVWm,cplhhHmcVWm,cplhhcHmVWm,cplHmcHmVP,cplHmcHmVZ,cplhhVPVZ,       & 
& cplhhcVWmVWm,cplhhVZVZ,cplHmcVWmVP,cplHmcVWmVZ,cplcHmVPVWm,cplcHmVWmVZ,cplAhAhVPVP,    & 
& cplAhAhVPVZ,cplAhAhcVWmVWm,cplAhAhVZVZ,cplAhHmcVWmVP,cplAhHmcVWmVZ,cplAhcHmVPVWm,      & 
& cplAhcHmVWmVZ,cplhhhhVPVP,cplhhhhVPVZ,cplhhhhcVWmVWm,cplhhhhVZVZ,cplhhHmcVWmVP,        & 
& cplhhHmcVWmVZ,cplhhcHmVPVWm,cplhhcHmVWmVZ,cplHmcHmVPVP,cplHmcHmVPVZ,cplHmcHmcVWmVWm,   & 
& cplHmcHmVZVZ,cplVGVGVG,cplcVWmVPVWm,cplcVWmVWmVZ,cplcFdFdAhL,cplcFdFdAhR,              & 
& cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplFvFvAhL,cplFvFvAhR,cplcFdFdhhL,     & 
& cplcFdFdhhR,cplcFuFdcHmL,cplcFuFdcHmR,cplFvFecHmL,cplFvFecHmR,cplcFeFehhL,             & 
& cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFuHmL,cplcFdFuHmR,cplFvFvhhL,cplFvFvhhR,     & 
& cplcFeFvHmL,cplcFeFvHmR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,cplcFdFdVPR,               & 
& cplcFdFdVZL,cplcFdFdVZR,cplcFuFdcVWmL,cplcFuFdcVWmR,cplFvFecVWmL,cplFvFecVWmR,         & 
& cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVGL,cplcFuFuVGR,               & 
& cplcFuFuVPL,cplcFuFuVPR,cplcFdFuVWmL,cplcFdFuVWmR,cplcFuFuVZL,cplcFuFuVZR,             & 
& cplFvFvVPL,cplFvFvVPR,cplFvFvVZL,cplFvFvVZR,cplcFeFvVWmL,cplcFeFvVWmR,cplVGVGVGVG1,    & 
& cplVGVGVGVG2,cplVGVGVGVG3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,             & 
& cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3,cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2, & 
& cplcVWmcVWmVWmVWm3,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,cplcgGgGVG,         & 
& cplcgWmgAVWm,cplcgWpgAcVWm,cplcgWmgWmVP,cplcgWmgWmVZ,cplcgAgWmcVWm,cplcgZgWmcVWm,      & 
& cplcgWpgWpVP,cplcgAgWpVWm,cplcgZgWpVWm,cplcgWpgWpVZ,cplcgWmgZVWm,cplcgWpgZcVWm,        & 
& cplcgWmgWmAh,cplcgWpgWpAh,cplcgZgAhh,cplcgWmgAHm,cplcgWpgAcHm,cplcgWmgWmhh,            & 
& cplcgZgWmcHm,cplcgWpgWphh,cplcgZgWpHm,cplcgZgZhh,cplcgWmgZHm,cplcgWpgZcHm)

Implicit None 
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3),ZH(3,3),g1,g2,g3

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,ZP(2,2),               & 
& ZZ(2,2),Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3),       & 
& Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3),Y1u11,Y1u12,Y1u21,Y1u22,               & 
& Y2u33,ZUL(3,3),ZUR(3,3),Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: cplAhAhAh(3,3,3),cplAhAhhh(3,3,3),cplAhhhhh(3,3,3),cplAhHmcHm(3,2,2),cplhhhhhh(3,3,3),& 
& cplhhHmcHm(3,2,2),cplAhAhAhAh(3,3,3,3),cplAhAhAhhh(3,3,3,3),cplAhAhhhhh(3,3,3,3),      & 
& cplAhAhHmcHm(3,3,2,2),cplAhhhhhhh(3,3,3,3),cplAhhhHmcHm(3,3,2,2),cplhhhhhhhh(3,3,3,3), & 
& cplhhhhHmcHm(3,3,2,2),cplHmHmcHmcHm(2,2,2,2),cplAhhhVP(3,3),cplAhhhVZ(3,3),            & 
& cplAhHmcVWm(3,2),cplAhcHmVWm(3,2),cplhhHmcVWm(3,2),cplhhcHmVWm(3,2),cplHmcHmVP(2,2),   & 
& cplHmcHmVZ(2,2),cplhhVPVZ(3),cplhhcVWmVWm(3),cplhhVZVZ(3),cplHmcVWmVP(2),              & 
& cplHmcVWmVZ(2),cplcHmVPVWm(2),cplcHmVWmVZ(2),cplAhAhVPVP(3,3),cplAhAhVPVZ(3,3),        & 
& cplAhAhcVWmVWm(3,3),cplAhAhVZVZ(3,3),cplAhHmcVWmVP(3,2),cplAhHmcVWmVZ(3,2),            & 
& cplAhcHmVPVWm(3,2),cplAhcHmVWmVZ(3,2),cplhhhhVPVP(3,3),cplhhhhVPVZ(3,3),               & 
& cplhhhhcVWmVWm(3,3),cplhhhhVZVZ(3,3),cplhhHmcVWmVP(3,2),cplhhHmcVWmVZ(3,2),            & 
& cplhhcHmVPVWm(3,2),cplhhcHmVWmVZ(3,2),cplHmcHmVPVP(2,2),cplHmcHmVPVZ(2,2),             & 
& cplHmcHmcVWmVWm(2,2),cplHmcHmVZVZ(2,2),cplVGVGVG,cplcVWmVPVWm,cplcVWmVWmVZ,            & 
& cplcFdFdAhL(3,3,3),cplcFdFdAhR(3,3,3),cplcFeFeAhL(3,3,3),cplcFeFeAhR(3,3,3),           & 
& cplcFuFuAhL(3,3,3),cplcFuFuAhR(3,3,3),cplFvFvAhL(6,6,3),cplFvFvAhR(6,6,3),             & 
& cplcFdFdhhL(3,3,3),cplcFdFdhhR(3,3,3),cplcFuFdcHmL(3,3,2),cplcFuFdcHmR(3,3,2),         & 
& cplFvFecHmL(6,3,2),cplFvFecHmR(6,3,2),cplcFeFehhL(3,3,3),cplcFeFehhR(3,3,3),           & 
& cplcFuFuhhL(3,3,3),cplcFuFuhhR(3,3,3),cplcFdFuHmL(3,3,2),cplcFdFuHmR(3,3,2),           & 
& cplFvFvhhL(6,6,3),cplFvFvhhR(6,6,3),cplcFeFvHmL(3,6,2),cplcFeFvHmR(3,6,2),             & 
& cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFdFdVZL(3,3),  & 
& cplcFdFdVZR(3,3),cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplFvFecVWmL(6,3),              & 
& cplFvFecVWmR(6,3),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3), & 
& cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFdFuVWmL(3,3), & 
& cplcFdFuVWmR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplFvFvVPL(6,6),cplFvFvVPR(6,6),   & 
& cplFvFvVZL(6,6),cplFvFvVZR(6,6),cplcFeFvVWmL(3,6),cplcFeFvVWmR(3,6),cplVGVGVGVG1,      & 
& cplVGVGVGVG2,cplVGVGVGVG3,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,             & 
& cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3,cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2, & 
& cplcVWmcVWmVWmVWm3,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,cplcgGgGVG,         & 
& cplcgWmgAVWm,cplcgWpgAcVWm,cplcgWmgWmVP,cplcgWmgWmVZ,cplcgAgWmcVWm,cplcgZgWmcVWm,      & 
& cplcgWpgWpVP,cplcgAgWpVWm,cplcgZgWpVWm,cplcgWpgWpVZ,cplcgWmgZVWm,cplcgWpgZcVWm,        & 
& cplcgWmgWmAh(3),cplcgWpgWpAh(3),cplcgZgAhh(3),cplcgWmgAHm(2),cplcgWpgAcHm(2),          & 
& cplcgWmgWmhh(3),cplcgZgWmcHm(2),cplcgWpgWphh(3),cplcgZgWpHm(2),cplcgZgZhh(3),          & 
& cplcgWmgZHm(2),cplcgWpgZcHm(2)

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'AllCouplingsReallyAll'
 
cplAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhAhT(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,cplAhAhAh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhAhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhhhhhT(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingAhHmcHmT(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,ZP,cplAhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplinghhhhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,cplhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhHmcHmT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,              & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZP,cplhhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhAhAhT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,         & 
& Lam3Dash,Aa3,Aa4,ZA,cplAhAhAhAh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhAhhhT(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,cplAhAhAhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhhhhhT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,         & 
& Lam3Dash,Aa3,Aa4,ZH,ZA,cplAhAhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingAhAhHmcHmT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,        & 
& Aa3,Aa4,ZA,ZP,cplAhAhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhhhhhhhT(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,cplAhhhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingAhhhHmcHmT(gt1,gt2,gt3,gt4,Lam4,Aa3,Aa4,ZH,ZA,ZP,cplAhhhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplhhhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplinghhhhhhhhT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,         & 
& Lam3Dash,Aa3,Aa4,ZH,cplhhhhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplhhhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplinghhhhHmcHmT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,        & 
& Aa3,Aa4,ZH,ZP,cplhhhhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplHmHmcHmcHm = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingHmHmcHmcHmT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,ZP,cplHmHmcHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhhhVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVPT(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVP(gt1,gt2))

 End Do 
End Do 


cplAhhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVZT(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVZ(gt1,gt2))

 End Do 
End Do 


cplAhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhHmcVWmT(gt1,gt2,g2,ZA,ZP,cplAhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplAhcHmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhcHmVWmT(gt1,gt2,g2,ZA,ZP,cplAhcHmVWm(gt1,gt2))

 End Do 
End Do 


cplhhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhHmcVWmT(gt1,gt2,g2,ZH,ZP,cplhhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplhhcHmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhcHmVWmT(gt1,gt2,g2,ZH,ZP,cplhhcHmVWm(gt1,gt2))

 End Do 
End Do 


cplHmcHmVP = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVPT(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVP(gt1,gt2))

 End Do 
End Do 


cplHmcHmVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVZT(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVZ(gt1,gt2))

 End Do 
End Do 


cplhhVPVZ = 0._dp 
Do gt1 = 1, 3
Call CouplinghhVPVZT(gt1,g1,g2,v1,v2,ZZ,ZH,cplhhVPVZ(gt1))

End Do 


cplhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
Call CouplinghhcVWmVWmT(gt1,g2,v1,v2,ZH,cplhhcVWmVWm(gt1))

End Do 


cplhhVZVZ = 0._dp 
Do gt1 = 1, 3
Call CouplinghhVZVZT(gt1,g1,g2,v1,v2,ZZ,ZH,cplhhVZVZ(gt1))

End Do 


cplHmcVWmVP = 0._dp 
Do gt1 = 1, 2
Call CouplingHmcVWmVPT(gt1,g1,g2,v1,v2,ZZ,ZP,cplHmcVWmVP(gt1))

End Do 


cplHmcVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingHmcVWmVZT(gt1,g1,g2,v1,v2,ZZ,ZP,cplHmcVWmVZ(gt1))

End Do 


cplcHmVPVWm = 0._dp 
Do gt1 = 1, 2
Call CouplingcHmVPVWmT(gt1,g1,g2,v1,v2,ZZ,ZP,cplcHmVPVWm(gt1))

End Do 


cplcHmVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingcHmVWmVZT(gt1,g1,g2,v1,v2,ZZ,ZP,cplcHmVWmVZ(gt1))

End Do 


cplAhAhVPVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhVPVPT(gt1,gt2,g1,g2,ZZ,ZA,cplAhAhVPVP(gt1,gt2))

 End Do 
End Do 


cplAhAhVPVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhVPVZT(gt1,gt2,g1,g2,ZZ,ZA,cplAhAhVPVZ(gt1,gt2))

 End Do 
End Do 


cplAhAhcVWmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhcVWmVWmT(gt1,gt2,g2,ZA,cplAhAhcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplAhAhVZVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhVZVZT(gt1,gt2,g1,g2,ZZ,ZA,cplAhAhVZVZ(gt1,gt2))

 End Do 
End Do 


cplAhHmcVWmVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhHmcVWmVPT(gt1,gt2,g1,g2,ZZ,ZA,ZP,cplAhHmcVWmVP(gt1,gt2))

 End Do 
End Do 


cplAhHmcVWmVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhHmcVWmVZT(gt1,gt2,g1,g2,ZZ,ZA,ZP,cplAhHmcVWmVZ(gt1,gt2))

 End Do 
End Do 


cplAhcHmVPVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhcHmVPVWmT(gt1,gt2,g1,g2,ZZ,ZA,ZP,cplAhcHmVPVWm(gt1,gt2))

 End Do 
End Do 


cplAhcHmVWmVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhcHmVWmVZT(gt1,gt2,g1,g2,ZZ,ZA,ZP,cplAhcHmVWmVZ(gt1,gt2))

 End Do 
End Do 


cplhhhhVPVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhVPVPT(gt1,gt2,g1,g2,ZZ,ZH,cplhhhhVPVP(gt1,gt2))

 End Do 
End Do 


cplhhhhVPVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhVPVZT(gt1,gt2,g1,g2,ZZ,ZH,cplhhhhVPVZ(gt1,gt2))

 End Do 
End Do 


cplhhhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhcVWmVWmT(gt1,gt2,g2,ZH,cplhhhhcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplhhhhVZVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhVZVZT(gt1,gt2,g1,g2,ZZ,ZH,cplhhhhVZVZ(gt1,gt2))

 End Do 
End Do 


cplhhHmcVWmVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhHmcVWmVPT(gt1,gt2,g1,g2,ZZ,ZH,ZP,cplhhHmcVWmVP(gt1,gt2))

 End Do 
End Do 


cplhhHmcVWmVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhHmcVWmVZT(gt1,gt2,g1,g2,ZZ,ZH,ZP,cplhhHmcVWmVZ(gt1,gt2))

 End Do 
End Do 


cplhhcHmVPVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhcHmVPVWmT(gt1,gt2,g1,g2,ZZ,ZH,ZP,cplhhcHmVPVWm(gt1,gt2))

 End Do 
End Do 


cplhhcHmVWmVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhcHmVWmVZT(gt1,gt2,g1,g2,ZZ,ZH,ZP,cplhhcHmVWmVZ(gt1,gt2))

 End Do 
End Do 


cplHmcHmVPVP = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVPVPT(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVPVP(gt1,gt2))

 End Do 
End Do 


cplHmcHmVPVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVPVZT(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVPVZ(gt1,gt2))

 End Do 
End Do 


cplHmcHmcVWmVWm = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmcVWmVWmT(gt1,gt2,g2,ZP,cplHmcHmcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplHmcHmVZVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVZVZT(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVZVZ(gt1,gt2))

 End Do 
End Do 


cplVGVGVG = 0._dp 
Call CouplingVGVGVGT(g3,cplVGVGVG)



cplcVWmVPVWm = 0._dp 
Call CouplingcVWmVPVWmT(g2,ZZ,cplcVWmVPVWm)



cplcVWmVWmVZ = 0._dp 
Call CouplingcVWmVWmVZT(g2,ZZ,cplcVWmVWmVZ)



cplcFdFdAhL = 0._dp 
cplcFdFdAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdAhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,ZA,ZDL,ZDR,cplcFdFdAhL(gt1,gt2,gt3),cplcFdFdAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFeAhL = 0._dp 
cplcFeFeAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFeAhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZA,ZEL,               & 
& ZER,cplcFeFeAhL(gt1,gt2,gt3),cplcFeFeAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuAhL = 0._dp 
cplcFuFuAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuAhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZA,ZUL,               & 
& ZUR,cplcFuFuAhL(gt1,gt2,gt3),cplcFuFuAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFvAhL = 0._dp 
cplFvFvAhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingFvFvAhT(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,               & 
& C31,C32,BB11,BB12,BB21,BB22,ZA,Vv,cplFvFvAhL(gt1,gt2,gt3),cplFvFvAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdhhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3),cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFdcHmL = 0._dp 
cplcFuFdcHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFdcHmT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,cplcFuFdcHmL(gt1,gt2,gt3)& 
& ,cplcFuFdcHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFecHmL = 0._dp 
cplFvFecHmR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingFvFecHmT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,          & 
& Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,cplFvFecHmL(gt1,gt2,gt3),cplFvFecHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFehhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZH,ZEL,               & 
& ZER,cplcFeFehhL(gt1,gt2,gt3),cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuhhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZH,ZUL,               & 
& ZUR,cplcFuFuhhL(gt1,gt2,gt3),cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFuHmL = 0._dp 
cplcFdFuHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFdFuHmT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,cplcFdFuHmL(gt1,gt2,gt3)  & 
& ,cplcFdFuHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFvhhL = 0._dp 
cplFvFvhhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingFvFvhhT(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,               & 
& C31,C32,BB11,BB12,BB21,BB22,ZH,Vv,cplFvFvhhL(gt1,gt2,gt3),cplFvFvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFvHmL = 0._dp 
cplcFeFvHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
  Do gt3 = 1, 2
Call CouplingcFeFvHmT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,          & 
& Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,cplcFeFvHmL(gt1,gt2,gt3),cplcFeFvHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdVGL = 0._dp 
cplcFdFdVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVGT(gt1,gt2,g3,ZDL,ZDR,cplcFdFdVGL(gt1,gt2),cplcFdFdVGR(gt1,gt2))

 End Do 
End Do 


cplcFdFdVPL = 0._dp 
cplcFdFdVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVPT(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcFdFdVPL(gt1,gt2),cplcFdFdVPR(gt1,gt2))

 End Do 
End Do 


cplcFdFdVZL = 0._dp 
cplcFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVZT(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcFdFdVZL(gt1,gt2),cplcFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcFuFdcVWmL = 0._dp 
cplcFuFdcVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFdcVWmT(gt1,gt2,g2,ZDL,ZUL,cplcFuFdcVWmL(gt1,gt2),cplcFuFdcVWmR(gt1,gt2))

 End Do 
End Do 


cplFvFecVWmL = 0._dp 
cplFvFecVWmR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 3
Call CouplingFvFecVWmT(gt1,gt2,g2,ZEL,Vv,cplFvFecVWmL(gt1,gt2),cplFvFecVWmR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVPL = 0._dp 
cplcFeFeVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVPT(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcFeFeVPL(gt1,gt2),cplcFeFeVPR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVZL = 0._dp 
cplcFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVZT(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcFeFeVZL(gt1,gt2),cplcFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVGL = 0._dp 
cplcFuFuVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVGT(gt1,gt2,g3,ZUL,ZUR,cplcFuFuVGL(gt1,gt2),cplcFuFuVGR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVPL = 0._dp 
cplcFuFuVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVPT(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcFuFuVPL(gt1,gt2),cplcFuFuVPR(gt1,gt2))

 End Do 
End Do 


cplcFdFuVWmL = 0._dp 
cplcFdFuVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFuVWmT(gt1,gt2,g2,ZDL,ZUL,cplcFdFuVWmL(gt1,gt2),cplcFdFuVWmR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVZL = 0._dp 
cplcFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVZT(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcFuFuVZL(gt1,gt2),cplcFuFuVZR(gt1,gt2))

 End Do 
End Do 


cplFvFvVPL = 0._dp 
cplFvFvVPR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
Call CouplingFvFvVPT(gt1,gt2,g1,g2,ZZ,Vv,cplFvFvVPL(gt1,gt2),cplFvFvVPR(gt1,gt2))

 End Do 
End Do 


cplFvFvVZL = 0._dp 
cplFvFvVZR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
Call CouplingFvFvVZT(gt1,gt2,g1,g2,ZZ,Vv,cplFvFvVZL(gt1,gt2),cplFvFvVZR(gt1,gt2))

 End Do 
End Do 


cplcFeFvVWmL = 0._dp 
cplcFeFvVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
Call CouplingcFeFvVWmT(gt1,gt2,g2,ZEL,Vv,cplcFeFvVWmL(gt1,gt2),cplcFeFvVWmR(gt1,gt2))

 End Do 
End Do 


cplVGVGVGVG1 = 0._dp 
cplVGVGVGVG2 = 0._dp 
cplVGVGVGVG3 = 0._dp 
Call CouplingVGVGVGVGT(g3,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3)



cplcVWmVPVPVWm1 = 0._dp 
cplcVWmVPVPVWm2 = 0._dp 
cplcVWmVPVPVWm3 = 0._dp 
Call CouplingcVWmVPVPVWmT(g2,ZZ,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3)



cplcVWmVPVWmVZ1 = 0._dp 
cplcVWmVPVWmVZ2 = 0._dp 
cplcVWmVPVWmVZ3 = 0._dp 
Call CouplingcVWmVPVWmVZT(g2,ZZ,cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3)



cplcVWmcVWmVWmVWm1 = 0._dp 
cplcVWmcVWmVWmVWm2 = 0._dp 
cplcVWmcVWmVWmVWm3 = 0._dp 
Call CouplingcVWmcVWmVWmVWmT(g2,cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3)



cplcVWmVWmVZVZ1 = 0._dp 
cplcVWmVWmVZVZ2 = 0._dp 
cplcVWmVWmVZVZ3 = 0._dp 
Call CouplingcVWmVWmVZVZT(g2,ZZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3)



cplcgGgGVG = 0._dp 
Call CouplingcgGgGVGT(g3,cplcgGgGVG)



cplcgWmgAVWm = 0._dp 
Call CouplingcgWmgAVWmT(g2,ZZ,cplcgWmgAVWm)



cplcgWpgAcVWm = 0._dp 
Call CouplingcgWpgAcVWmT(g2,ZZ,cplcgWpgAcVWm)



cplcgWmgWmVP = 0._dp 
Call CouplingcgWmgWmVPT(g2,ZZ,cplcgWmgWmVP)



cplcgWmgWmVZ = 0._dp 
Call CouplingcgWmgWmVZT(g2,ZZ,cplcgWmgWmVZ)



cplcgAgWmcVWm = 0._dp 
Call CouplingcgAgWmcVWmT(g2,ZZ,cplcgAgWmcVWm)



cplcgZgWmcVWm = 0._dp 
Call CouplingcgZgWmcVWmT(g2,ZZ,cplcgZgWmcVWm)



cplcgWpgWpVP = 0._dp 
Call CouplingcgWpgWpVPT(g2,ZZ,cplcgWpgWpVP)



cplcgAgWpVWm = 0._dp 
Call CouplingcgAgWpVWmT(g2,ZZ,cplcgAgWpVWm)



cplcgZgWpVWm = 0._dp 
Call CouplingcgZgWpVWmT(g2,ZZ,cplcgZgWpVWm)



cplcgWpgWpVZ = 0._dp 
Call CouplingcgWpgWpVZT(g2,ZZ,cplcgWpgWpVZ)



cplcgWmgZVWm = 0._dp 
Call CouplingcgWmgZVWmT(g2,ZZ,cplcgWmgZVWm)



cplcgWpgZcVWm = 0._dp 
Call CouplingcgWpgZcVWmT(g2,ZZ,cplcgWpgZcVWm)



cplcgWmgWmAh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgWmgWmAhT(gt3,g2,v1,v2,ZA,cplcgWmgWmAh(gt3))

End Do 


cplcgWpgWpAh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgWpgWpAhT(gt3,g2,v1,v2,ZA,cplcgWpgWpAh(gt3))

End Do 


cplcgZgAhh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgZgAhhT(gt3,g1,g2,v1,v2,ZZ,ZH,cplcgZgAhh(gt3))

End Do 


cplcgWmgAHm = 0._dp 
Do gt3 = 1, 2
Call CouplingcgWmgAHmT(gt3,g1,g2,v1,v2,ZZ,ZP,cplcgWmgAHm(gt3))

End Do 


cplcgWpgAcHm = 0._dp 
Do gt3 = 1, 2
Call CouplingcgWpgAcHmT(gt3,g1,g2,v1,v2,ZZ,ZP,cplcgWpgAcHm(gt3))

End Do 


cplcgWmgWmhh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgWmgWmhhT(gt3,g2,v1,v2,ZH,cplcgWmgWmhh(gt3))

End Do 


cplcgZgWmcHm = 0._dp 
Do gt3 = 1, 2
Call CouplingcgZgWmcHmT(gt3,g1,g2,v1,v2,ZZ,ZP,cplcgZgWmcHm(gt3))

End Do 


cplcgWpgWphh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgWpgWphhT(gt3,g2,v1,v2,ZH,cplcgWpgWphh(gt3))

End Do 


cplcgZgWpHm = 0._dp 
Do gt3 = 1, 2
Call CouplingcgZgWpHmT(gt3,g1,g2,v1,v2,ZZ,ZP,cplcgZgWpHm(gt3))

End Do 


cplcgZgZhh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgZgZhhT(gt3,g1,g2,v1,v2,ZZ,ZH,cplcgZgZhh(gt3))

End Do 


cplcgWmgZHm = 0._dp 
Do gt3 = 1, 2
Call CouplingcgWmgZHmT(gt3,g1,g2,v1,v2,ZZ,ZP,cplcgWmgZHm(gt3))

End Do 


cplcgWpgZcHm = 0._dp 
Do gt3 = 1, 2
Call CouplingcgWpgZcHmT(gt3,g1,g2,v1,v2,ZZ,ZP,cplcgWpgZcHm(gt3))

End Do 


Iname = Iname - 1 
End Subroutine AllCouplingsReallyAll

Subroutine AllCouplings(Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,Lam1,Lam3,Lam4,Lam2,              & 
& Lam1Dash,Lam2Dash,Lam3Dash,ZH,ZP,g1,g2,ZZ,g3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,            & 
& Y1d23,Y2d31,Y2d32,Y2d33,ZDL,ZDR,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL,ZER,Y1u11,           & 
& Y1u12,Y1u21,Y1u22,Y2u33,ZUL,ZUR,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,             & 
& C32,BB11,BB12,BB21,BB22,Vv,cplAhAhAh,cplAhAhhh,cplAhhhhh,cplAhHmcHm,cplhhhhhh,         & 
& cplhhHmcHm,cplAhhhVP,cplAhhhVZ,cplAhHmcVWm,cplAhcHmVWm,cplhhHmcVWm,cplhhcHmVWm,        & 
& cplHmcHmVP,cplHmcHmVZ,cplhhVPVZ,cplhhcVWmVWm,cplhhVZVZ,cplHmcVWmVP,cplHmcVWmVZ,        & 
& cplcHmVPVWm,cplcHmVWmVZ,cplVGVGVG,cplcVWmVPVWm,cplcVWmVWmVZ,cplcFdFdAhL,               & 
& cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,cplcFuFuAhR,cplFvFvAhL,cplFvFvAhR,     & 
& cplcFdFdhhL,cplcFdFdhhR,cplcFuFdcHmL,cplcFuFdcHmR,cplFvFecHmL,cplFvFecHmR,             & 
& cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,cplcFdFuHmL,cplcFdFuHmR,               & 
& cplFvFvhhL,cplFvFvhhR,cplcFeFvHmL,cplcFeFvHmR,cplcFdFdVGL,cplcFdFdVGR,cplcFdFdVPL,     & 
& cplcFdFdVPR,cplcFdFdVZL,cplcFdFdVZR,cplcFuFdcVWmL,cplcFuFdcVWmR,cplFvFecVWmL,          & 
& cplFvFecVWmR,cplcFeFeVPL,cplcFeFeVPR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVGL,              & 
& cplcFuFuVGR,cplcFuFuVPL,cplcFuFuVPR,cplcFdFuVWmL,cplcFdFuVWmR,cplcFuFuVZL,             & 
& cplcFuFuVZR,cplFvFvVPL,cplFvFvVPR,cplFvFvVZL,cplFvFvVZR,cplcFeFvVWmL,cplcFeFvVWmR)

Implicit None 
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3),ZH(3,3),g1,g2,g3

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,ZP(2,2),               & 
& ZZ(2,2),Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3),       & 
& Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3),Y1u11,Y1u12,Y1u21,Y1u22,               & 
& Y2u33,ZUL(3,3),ZUR(3,3),Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: cplAhAhAh(3,3,3),cplAhAhhh(3,3,3),cplAhhhhh(3,3,3),cplAhHmcHm(3,2,2),cplhhhhhh(3,3,3),& 
& cplhhHmcHm(3,2,2),cplAhhhVP(3,3),cplAhhhVZ(3,3),cplAhHmcVWm(3,2),cplAhcHmVWm(3,2),     & 
& cplhhHmcVWm(3,2),cplhhcHmVWm(3,2),cplHmcHmVP(2,2),cplHmcHmVZ(2,2),cplhhVPVZ(3),        & 
& cplhhcVWmVWm(3),cplhhVZVZ(3),cplHmcVWmVP(2),cplHmcVWmVZ(2),cplcHmVPVWm(2),             & 
& cplcHmVWmVZ(2),cplVGVGVG,cplcVWmVPVWm,cplcVWmVWmVZ,cplcFdFdAhL(3,3,3),cplcFdFdAhR(3,3,3),& 
& cplcFeFeAhL(3,3,3),cplcFeFeAhR(3,3,3),cplcFuFuAhL(3,3,3),cplcFuFuAhR(3,3,3),           & 
& cplFvFvAhL(6,6,3),cplFvFvAhR(6,6,3),cplcFdFdhhL(3,3,3),cplcFdFdhhR(3,3,3),             & 
& cplcFuFdcHmL(3,3,2),cplcFuFdcHmR(3,3,2),cplFvFecHmL(6,3,2),cplFvFecHmR(6,3,2),         & 
& cplcFeFehhL(3,3,3),cplcFeFehhR(3,3,3),cplcFuFuhhL(3,3,3),cplcFuFuhhR(3,3,3),           & 
& cplcFdFuHmL(3,3,2),cplcFdFuHmR(3,3,2),cplFvFvhhL(6,6,3),cplFvFvhhR(6,6,3),             & 
& cplcFeFvHmL(3,6,2),cplcFeFvHmR(3,6,2),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),               & 
& cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFuFdcVWmL(3,3),& 
& cplcFuFdcVWmR(3,3),cplFvFecVWmL(6,3),cplFvFecVWmR(6,3),cplcFeFeVPL(3,3),               & 
& cplcFeFeVPR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),  & 
& cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplcFdFuVWmL(3,3),cplcFdFuVWmR(3,3),cplcFuFuVZL(3,3),& 
& cplcFuFuVZR(3,3),cplFvFvVPL(6,6),cplFvFvVPR(6,6),cplFvFvVZL(6,6),cplFvFvVZR(6,6),      & 
& cplcFeFvVWmL(3,6),cplcFeFvVWmR(3,6)

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'AllCouplings'
 
cplAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhAhT(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,cplAhAhAh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhAhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhhhhhT(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingAhHmcHmT(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,ZP,cplAhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplinghhhhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,cplhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhHmcHmT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,              & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZP,cplhhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVPT(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVP(gt1,gt2))

 End Do 
End Do 


cplAhhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVZT(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVZ(gt1,gt2))

 End Do 
End Do 


cplAhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhHmcVWmT(gt1,gt2,g2,ZA,ZP,cplAhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplAhcHmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhcHmVWmT(gt1,gt2,g2,ZA,ZP,cplAhcHmVWm(gt1,gt2))

 End Do 
End Do 


cplhhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhHmcVWmT(gt1,gt2,g2,ZH,ZP,cplhhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplhhcHmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhcHmVWmT(gt1,gt2,g2,ZH,ZP,cplhhcHmVWm(gt1,gt2))

 End Do 
End Do 


cplHmcHmVP = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVPT(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVP(gt1,gt2))

 End Do 
End Do 


cplHmcHmVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVZT(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVZ(gt1,gt2))

 End Do 
End Do 


cplhhVPVZ = 0._dp 
Do gt1 = 1, 3
Call CouplinghhVPVZT(gt1,g1,g2,v1,v2,ZZ,ZH,cplhhVPVZ(gt1))

End Do 


cplhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
Call CouplinghhcVWmVWmT(gt1,g2,v1,v2,ZH,cplhhcVWmVWm(gt1))

End Do 


cplhhVZVZ = 0._dp 
Do gt1 = 1, 3
Call CouplinghhVZVZT(gt1,g1,g2,v1,v2,ZZ,ZH,cplhhVZVZ(gt1))

End Do 


cplHmcVWmVP = 0._dp 
Do gt1 = 1, 2
Call CouplingHmcVWmVPT(gt1,g1,g2,v1,v2,ZZ,ZP,cplHmcVWmVP(gt1))

End Do 


cplHmcVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingHmcVWmVZT(gt1,g1,g2,v1,v2,ZZ,ZP,cplHmcVWmVZ(gt1))

End Do 


cplcHmVPVWm = 0._dp 
Do gt1 = 1, 2
Call CouplingcHmVPVWmT(gt1,g1,g2,v1,v2,ZZ,ZP,cplcHmVPVWm(gt1))

End Do 


cplcHmVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingcHmVWmVZT(gt1,g1,g2,v1,v2,ZZ,ZP,cplcHmVWmVZ(gt1))

End Do 


cplVGVGVG = 0._dp 
Call CouplingVGVGVGT(g3,cplVGVGVG)



cplcVWmVPVWm = 0._dp 
Call CouplingcVWmVPVWmT(g2,ZZ,cplcVWmVPVWm)



cplcVWmVWmVZ = 0._dp 
Call CouplingcVWmVWmVZT(g2,ZZ,cplcVWmVWmVZ)



cplcFdFdAhL = 0._dp 
cplcFdFdAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdAhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,ZA,ZDL,ZDR,cplcFdFdAhL(gt1,gt2,gt3),cplcFdFdAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFeAhL = 0._dp 
cplcFeFeAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFeAhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZA,ZEL,               & 
& ZER,cplcFeFeAhL(gt1,gt2,gt3),cplcFeFeAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuAhL = 0._dp 
cplcFuFuAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuAhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZA,ZUL,               & 
& ZUR,cplcFuFuAhL(gt1,gt2,gt3),cplcFuFuAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFvAhL = 0._dp 
cplFvFvAhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingFvFvAhT(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,               & 
& C31,C32,BB11,BB12,BB21,BB22,ZA,Vv,cplFvFvAhL(gt1,gt2,gt3),cplFvFvAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdhhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3),cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFdcHmL = 0._dp 
cplcFuFdcHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFdcHmT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,cplcFuFdcHmL(gt1,gt2,gt3)& 
& ,cplcFuFdcHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFecHmL = 0._dp 
cplFvFecHmR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingFvFecHmT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,          & 
& Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,cplFvFecHmL(gt1,gt2,gt3),cplFvFecHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFehhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZH,ZEL,               & 
& ZER,cplcFeFehhL(gt1,gt2,gt3),cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuhhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZH,ZUL,               & 
& ZUR,cplcFuFuhhL(gt1,gt2,gt3),cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFuHmL = 0._dp 
cplcFdFuHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFdFuHmT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,cplcFdFuHmL(gt1,gt2,gt3)  & 
& ,cplcFdFuHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFvhhL = 0._dp 
cplFvFvhhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingFvFvhhT(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,               & 
& C31,C32,BB11,BB12,BB21,BB22,ZH,Vv,cplFvFvhhL(gt1,gt2,gt3),cplFvFvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFvHmL = 0._dp 
cplcFeFvHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
  Do gt3 = 1, 2
Call CouplingcFeFvHmT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,          & 
& Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,cplcFeFvHmL(gt1,gt2,gt3),cplcFeFvHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdVGL = 0._dp 
cplcFdFdVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVGT(gt1,gt2,g3,ZDL,ZDR,cplcFdFdVGL(gt1,gt2),cplcFdFdVGR(gt1,gt2))

 End Do 
End Do 


cplcFdFdVPL = 0._dp 
cplcFdFdVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVPT(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcFdFdVPL(gt1,gt2),cplcFdFdVPR(gt1,gt2))

 End Do 
End Do 


cplcFdFdVZL = 0._dp 
cplcFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVZT(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcFdFdVZL(gt1,gt2),cplcFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcFuFdcVWmL = 0._dp 
cplcFuFdcVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFdcVWmT(gt1,gt2,g2,ZDL,ZUL,cplcFuFdcVWmL(gt1,gt2),cplcFuFdcVWmR(gt1,gt2))

 End Do 
End Do 


cplFvFecVWmL = 0._dp 
cplFvFecVWmR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 3
Call CouplingFvFecVWmT(gt1,gt2,g2,ZEL,Vv,cplFvFecVWmL(gt1,gt2),cplFvFecVWmR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVPL = 0._dp 
cplcFeFeVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVPT(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcFeFeVPL(gt1,gt2),cplcFeFeVPR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVZL = 0._dp 
cplcFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVZT(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcFeFeVZL(gt1,gt2),cplcFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVGL = 0._dp 
cplcFuFuVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVGT(gt1,gt2,g3,ZUL,ZUR,cplcFuFuVGL(gt1,gt2),cplcFuFuVGR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVPL = 0._dp 
cplcFuFuVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVPT(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcFuFuVPL(gt1,gt2),cplcFuFuVPR(gt1,gt2))

 End Do 
End Do 


cplcFdFuVWmL = 0._dp 
cplcFdFuVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFuVWmT(gt1,gt2,g2,ZDL,ZUL,cplcFdFuVWmL(gt1,gt2),cplcFdFuVWmR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVZL = 0._dp 
cplcFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVZT(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcFuFuVZL(gt1,gt2),cplcFuFuVZR(gt1,gt2))

 End Do 
End Do 


cplFvFvVPL = 0._dp 
cplFvFvVPR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
Call CouplingFvFvVPT(gt1,gt2,g1,g2,ZZ,Vv,cplFvFvVPL(gt1,gt2),cplFvFvVPR(gt1,gt2))

 End Do 
End Do 


cplFvFvVZL = 0._dp 
cplFvFvVZR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
Call CouplingFvFvVZT(gt1,gt2,g1,g2,ZZ,Vv,cplFvFvVZL(gt1,gt2),cplFvFvVZR(gt1,gt2))

 End Do 
End Do 


cplcFeFvVWmL = 0._dp 
cplcFeFvVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
Call CouplingcFeFvVWmT(gt1,gt2,g2,ZEL,Vv,cplcFeFvVWmL(gt1,gt2),cplcFeFvVWmR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
End Subroutine AllCouplings

Subroutine CouplingAhAhAhT(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(Aa1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res+(Aa1*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Aa3*v1*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(Aa1*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Aa3*v1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(Aa3*v2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(Aa3*v1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhAhT  
 
 
Subroutine CouplingAhAhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,         & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*v1*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,1)
res = res-(Lam3*v1*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1))
res = res-(Lam4*v1*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1))
res = res+(Aa1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam2Dash*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))
res = res+(Aa3*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam3*v2*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2))
res = res-(Lam4*v2*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2))
res = res-(Aa1*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res+(Aa4*v3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res-2*Lam2*v2*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,2)
res = res-(Aa1*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa4*v3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa3*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa4*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam3Dash*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam2Dash*v3*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,3))
res = res-(Aa1*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa3*v2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa1*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v3*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,3))
res = res+(Aa3*v1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa3*v2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa3*v1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-2*Lam1Dash*v3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhhhT  
 
 
Subroutine CouplingAhhhhhT(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(Aa1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa4*v3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v2*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Aa1*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa3*v1*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(v1*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res+(Aa4*v3*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa1*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(Aa3*v1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa3*v2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa3*v1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa4*v1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhhhT  
 
 
Subroutine CouplingAhHmcHmT(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3)

Complex(dp), Intent(in) :: Lam4,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(Lam4*v2*Conjg(ZP(gt2,2))*ZA(gt1,1)*ZP(gt3,1))/2._dp
res = res+(Lam4*v1*Conjg(ZP(gt2,2))*ZA(gt1,2)*ZP(gt3,1))/2._dp
res = res-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res = res+(Conjg(Aa2)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))/sqrt(2._dp)
res = res-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))
res = res+v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1)
res = res+(Lam4*v2*Conjg(ZP(gt2,1))*ZA(gt1,1)*ZP(gt3,2))/2._dp
res = res-(Lam4*v1*Conjg(ZP(gt2,1))*ZA(gt1,2)*ZP(gt3,2))/2._dp
res = res+(Aa1*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))/sqrt(2._dp)
res = res-((Aa2*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res = res+Aa3*v3*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2)
res = res-(Aa4*v3*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhHmcHmT  
 
 
Subroutine CouplinghhhhhhT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,         & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'Couplinghhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-6*Lam1*v1*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,1)
res = res-(Lam3*v2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1))
res = res-(Lam4*v2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1))
res = res-(Lam2Dash*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,1))
res = res-(Lam3*v2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Lam4*v2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Lam3*v1*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Lam4*v1*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Aa1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Lam2Dash*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,1))
res = res-(Aa1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam2Dash*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))
res = res-(Aa3*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v2*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v2*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam3*v2*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Lam4*v2*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Lam3*v1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Lam4*v1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Aa1*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Lam3*v1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2))
res = res-(Lam4*v1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2))
res = res-6*Lam2*v2*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,2)
res = res-(Lam3Dash*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,2))
res = res-(Aa1*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam3Dash*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,2))
res = res-(Aa3*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam3Dash*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))
res = res-(v1*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(v1*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam2Dash*v3*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,3))
res = res-(Aa1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Lam2Dash*v1*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))
res = res-(Aa3*v2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v3*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,3))
res = res-(Aa3*v1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v2*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))
res = res-(v1*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam2Dash*v1*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))
res = res-(Aa3*v2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa3*v1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v2*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))
res = res-(v1*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-6*Lam1Dash*v3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhhhT  
 
 
Subroutine CouplinghhHmcHmT(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,        & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*v1*Conjg(ZP(gt2,1))*ZH(gt1,1)*ZP(gt3,1)
res = res-(Lam4*v2*Conjg(ZP(gt2,2))*ZH(gt1,1)*ZP(gt3,1))/2._dp
res = res-(Lam3*v2*Conjg(ZP(gt2,1))*ZH(gt1,2)*ZP(gt3,1))
res = res-(Lam4*v1*Conjg(ZP(gt2,2))*ZH(gt1,2)*ZP(gt3,1))/2._dp
res = res-(Lam2Dash*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,1))
res = res-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res = res-((Conjg(Aa2)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))
res = res-(v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))
res = res-(Lam4*v2*Conjg(ZP(gt2,1))*ZH(gt1,1)*ZP(gt3,2))/2._dp
res = res-(Lam3*v1*Conjg(ZP(gt2,2))*ZH(gt1,1)*ZP(gt3,2))
res = res-(Lam4*v1*Conjg(ZP(gt2,1))*ZH(gt1,2)*ZP(gt3,2))/2._dp
res = res-2*Lam2*v2*Conjg(ZP(gt2,2))*ZH(gt1,2)*ZP(gt3,2)
res = res-((Aa1*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res = res-((Aa2*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res = res-(Aa3*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))
res = res-(Aa4*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))
res = res-(Lam3Dash*v3*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhHmcHmT  
 
 
Subroutine CouplingAhAhAhAhT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,            & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhAhAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-6*Lam1*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,1)
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,1))
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,1))
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,1))
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,1))
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,1))
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,1))
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,1))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,1))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,2))
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,2))
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,2))
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,2))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,2))
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,2))
res = res-6*Lam2*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,2)
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,2))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,2))
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,3))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,3))
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))
res = res-6*Lam1Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhAhAhT  
 
 
Subroutine CouplingAhAhAhhhT(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhAhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhAhhhT  
 
 
Subroutine CouplingAhAhhhhhT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,            & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,1)*ZH(gt4,1)
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,1))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res = res-2*Lam2*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,2)*ZH(gt4,2)
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,2))
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))
res = res-(Aa3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))
res = res-2*Lam1Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhhhhhT  
 
 
Subroutine CouplingAhAhHmcHmT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,           & 
& Lam3Dash,Aa3,Aa4,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*Conjg(ZP(gt3,1))*ZA(gt1,1)*ZA(gt2,1)*ZP(gt4,1)
res = res-(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,2)*ZA(gt2,1)*ZP(gt4,1))/2._dp
res = res-(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,1)*ZA(gt2,2)*ZP(gt4,1))/2._dp
res = res-(Lam3*Conjg(ZP(gt3,1))*ZA(gt1,2)*ZA(gt2,2)*ZP(gt4,1))
res = res-(Lam2Dash*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1))
res = res+Conjg(Aa3)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1)
res = res+Conjg(Aa4)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1)
res = res-(Lam3*Conjg(ZP(gt3,2))*ZA(gt1,1)*ZA(gt2,1)*ZP(gt4,2))
res = res-(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,2)*ZA(gt2,1)*ZP(gt4,2))/2._dp
res = res-(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,1)*ZA(gt2,2)*ZP(gt4,2))/2._dp
res = res-2*Lam2*Conjg(ZP(gt3,2))*ZA(gt1,2)*ZA(gt2,2)*ZP(gt4,2)
res = res+Aa3*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2)
res = res+Aa4*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2)
res = res-(Lam3Dash*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhHmcHmT  
 
 
Subroutine CouplingAhhhhhhhT(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhhhhhT  
 
 
Subroutine CouplingAhhhHmcHmT(gt1,gt2,gt3,gt4,Lam4,Aa3,Aa4,ZH,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam4,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,2)*ZH(gt2,1)*ZP(gt4,1))/2._dp
res = res-(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,1)*ZH(gt2,2)*ZP(gt4,1))/2._dp
res = res-(Conjg(Aa3)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res = res+Conjg(Aa4)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,1)
res = res-(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,2)*ZH(gt2,1)*ZP(gt4,2))/2._dp
res = res+(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,1)*ZH(gt2,2)*ZP(gt4,2))/2._dp
res = res+Aa3*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,2)
res = res-(Aa4*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhHmcHmT  
 
 
Subroutine CouplinghhhhhhhhT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,            & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'Couplinghhhhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-6*Lam1*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,1)
res = res-(Lam3*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res = res-(Lam4*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res = res-(Lam2Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,1))
res = res-(Lam3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,1))
res = res-(Lam4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,1))
res = res-(Lam3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,1))
res = res-(Lam4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,1))
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Lam2Dash*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,1))
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Lam2Dash*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))
res = res-(Aa3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Lam3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,2))
res = res-(Lam4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,2))
res = res-(Lam3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,2))
res = res-(Lam4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,2))
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Lam3*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res = res-(Lam4*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res = res-6*Lam2*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,2)
res = res-(Lam3Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,2))
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Lam3Dash*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,2))
res = res-(Aa3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Lam3Dash*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))
res = res-(Lam2Dash*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,3))
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Lam2Dash*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))
res = res-(Aa3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Lam3Dash*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,3))
res = res-(Aa3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Lam3Dash*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))
res = res-(Lam2Dash*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))
res = res-(Aa3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Lam3Dash*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))
res = res-6*Lam1Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhhhhhT  
 
 
Subroutine CouplinghhhhHmcHmT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,           & 
& Lam3Dash,Aa3,Aa4,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*Conjg(ZP(gt3,1))*ZH(gt1,1)*ZH(gt2,1)*ZP(gt4,1)
res = res-(Lam4*Conjg(ZP(gt3,2))*ZH(gt1,2)*ZH(gt2,1)*ZP(gt4,1))/2._dp
res = res-(Lam4*Conjg(ZP(gt3,2))*ZH(gt1,1)*ZH(gt2,2)*ZP(gt4,1))/2._dp
res = res-(Lam3*Conjg(ZP(gt3,1))*ZH(gt1,2)*ZH(gt2,2)*ZP(gt4,1))
res = res-(Lam2Dash*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res = res-(Conjg(Aa3)*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res = res-(Conjg(Aa4)*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res = res-(Lam3*Conjg(ZP(gt3,2))*ZH(gt1,1)*ZH(gt2,1)*ZP(gt4,2))
res = res-(Lam4*Conjg(ZP(gt3,1))*ZH(gt1,2)*ZH(gt2,1)*ZP(gt4,2))/2._dp
res = res-(Lam4*Conjg(ZP(gt3,1))*ZH(gt1,1)*ZH(gt2,2)*ZP(gt4,2))/2._dp
res = res-2*Lam2*Conjg(ZP(gt3,2))*ZH(gt1,2)*ZH(gt2,2)*ZP(gt4,2)
res = res-(Aa3*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
res = res-(Aa4*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
res = res-(Lam3Dash*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhHmcHmT  
 
 
Subroutine CouplingHmHmcHmcHmT(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmHmcHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-4*Lam1*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,1)
res = res-(Lam3*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,2)*ZP(gt4,1))
res = res-(Lam4*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,2)*ZP(gt4,1))
res = res-(Lam3*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,1))
res = res-(Lam4*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,1))
res = res-(Lam3*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,2))
res = res-(Lam4*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,2))
res = res-(Lam3*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,1)*ZP(gt4,2))
res = res-(Lam4*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,1)*ZP(gt4,2))
res = res-4*Lam2*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,2)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmHmcHmcHmT  
 
 
Subroutine CouplingAhhhVPT(gt1,gt2,g1,g2,ZZ,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*ZA(gt1,1)*ZH(gt2,1)*ZZ(1,1))/2._dp
res = res-(g1*ZA(gt1,2)*ZH(gt2,2)*ZZ(1,1))/2._dp
res = res+(g2*ZA(gt1,1)*ZH(gt2,1)*ZZ(2,1))/2._dp
res = res+(g2*ZA(gt1,2)*ZH(gt2,2)*ZZ(2,1))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhVPT  
 
 
Subroutine CouplingAhhhVZT(gt1,gt2,g1,g2,ZZ,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*ZA(gt1,1)*ZH(gt2,1)*ZZ(1,2))/2._dp
res = res-(g1*ZA(gt1,2)*ZH(gt2,2)*ZZ(1,2))/2._dp
res = res+(g2*ZA(gt1,1)*ZH(gt2,1)*ZZ(2,2))/2._dp
res = res+(g2*ZA(gt1,2)*ZH(gt2,2)*ZZ(2,2))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhVZT  
 
 
Subroutine CouplingAhHmcVWmT(gt1,gt2,g2,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZA(3,3)

Complex(dp), Intent(in) :: ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhHmcVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g2*Conjg(ZP(gt2,1))*ZA(gt1,1))/2._dp
res = res-(g2*Conjg(ZP(gt2,2))*ZA(gt1,2))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhHmcVWmT  
 
 
Subroutine CouplingAhcHmVWmT(gt1,gt2,g2,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZA(3,3)

Complex(dp), Intent(in) :: ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhcHmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g2*ZA(gt1,1)*ZP(gt2,1))/2._dp
res = res-(g2*ZA(gt1,2)*ZP(gt2,2))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhcHmVWmT  
 
 
Subroutine CouplinghhHmcVWmT(gt1,gt2,g2,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZH(3,3)

Complex(dp), Intent(in) :: ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhHmcVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2*Conjg(ZP(gt2,1))*ZH(gt1,1))/2._dp
res = res+(g2*Conjg(ZP(gt2,2))*ZH(gt1,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhHmcVWmT  
 
 
Subroutine CouplinghhcHmVWmT(gt1,gt2,g2,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZH(3,3)

Complex(dp), Intent(in) :: ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhcHmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g2*ZH(gt1,1)*ZP(gt2,1))/2._dp
res = res-(g2*ZH(gt1,2)*ZP(gt2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhcHmVWmT  
 
 
Subroutine CouplingHmcHmVPT(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,1))/2._dp
res = res-(g1*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,1))/2._dp
res = res-(g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(2,1))/2._dp
res = res-(g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(2,1))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmVPT  
 
 
Subroutine CouplingHmcHmVZT(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,2))/2._dp
res = res-(g1*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,2))/2._dp
res = res-(g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(2,2))/2._dp
res = res-(g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmVZT  
 
 
Subroutine CouplinghhVPVZT(gt1,g1,g2,v1,v2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhVPVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*v1*ZH(gt1,1)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res+(g1**2*v2*ZH(gt1,2)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res-(g1*g2*v1*ZH(gt1,1)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*v2*ZH(gt1,2)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*v1*ZH(gt1,1)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res-(g1*g2*v2*ZH(gt1,2)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res+(g2**2*v1*ZH(gt1,1)*ZZ(2,1)*ZZ(2,2))/2._dp
res = res+(g2**2*v2*ZH(gt1,2)*ZZ(2,1)*ZZ(2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhVPVZT  
 
 
Subroutine CouplinghhcVWmVWmT(gt1,g2,v1,v2,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g2,v1,v2,ZH(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2**2*v1*ZH(gt1,1))/2._dp
res = res+(g2**2*v2*ZH(gt1,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhcVWmVWmT  
 
 
Subroutine CouplinghhVZVZT(gt1,g1,g2,v1,v2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*v1*ZH(gt1,1)*ZZ(1,2)**2)/2._dp
res = res+(g1**2*v2*ZH(gt1,2)*ZZ(1,2)**2)/2._dp
res = res-(g1*g2*v1*ZH(gt1,1)*ZZ(1,2)*ZZ(2,2))
res = res-(g1*g2*v2*ZH(gt1,2)*ZZ(1,2)*ZZ(2,2))
res = res+(g2**2*v1*ZH(gt1,1)*ZZ(2,2)**2)/2._dp
res = res+(g2**2*v2*ZH(gt1,2)*ZZ(2,2)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhVZVZT  
 
 
Subroutine CouplingHmcVWmVPT(gt1,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcVWmVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*v1*Conjg(ZP(gt1,1))*ZZ(1,1))/2._dp
res = res+(g1*g2*v2*Conjg(ZP(gt1,2))*ZZ(1,1))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcVWmVPT  
 
 
Subroutine CouplingHmcVWmVZT(gt1,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcVWmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*v1*Conjg(ZP(gt1,1))*ZZ(1,2))/2._dp
res = res+(g1*g2*v2*Conjg(ZP(gt1,2))*ZZ(1,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcVWmVZT  
 
 
Subroutine CouplingcHmVPVWmT(gt1,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcHmVPVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*v1*ZP(gt1,1)*ZZ(1,1))/2._dp
res = res+(g1*g2*v2*ZP(gt1,2)*ZZ(1,1))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcHmVPVWmT  
 
 
Subroutine CouplingcHmVWmVZT(gt1,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcHmVWmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*v1*ZP(gt1,1)*ZZ(1,2))/2._dp
res = res+(g1*g2*v2*ZP(gt1,2)*ZZ(1,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcHmVWmVZT  
 
 
Subroutine CouplingAhAhVPVPT(gt1,gt2,g1,g2,ZZ,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhVPVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,1)**2)/2._dp
res = res+(g1**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,1)**2)/2._dp
res = res-(g1*g2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,1)*ZZ(2,1))
res = res-(g1*g2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,1)*ZZ(2,1))
res = res+(g2**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(2,1)**2)/2._dp
res = res+(g2**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(2,1)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhVPVPT  
 
 
Subroutine CouplingAhAhVPVZT(gt1,gt2,g1,g2,ZZ,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhVPVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res+(g1**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res-(g1*g2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res-(g1*g2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res+(g2**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(2,1)*ZZ(2,2))/2._dp
res = res+(g2**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(2,1)*ZZ(2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhVPVZT  
 
 
Subroutine CouplingAhAhcVWmVWmT(gt1,gt2,g2,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZA(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2**2*ZA(gt1,1)*ZA(gt2,1))/2._dp
res = res+(g2**2*ZA(gt1,2)*ZA(gt2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhcVWmVWmT  
 
 
Subroutine CouplingAhAhVZVZT(gt1,gt2,g1,g2,ZZ,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,2)**2)/2._dp
res = res+(g1**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,2)**2)/2._dp
res = res-(g1*g2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,2)*ZZ(2,2))
res = res-(g1*g2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,2)*ZZ(2,2))
res = res+(g2**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(2,2)**2)/2._dp
res = res+(g2**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(2,2)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhVZVZT  
 
 
Subroutine CouplingAhHmcVWmVPT(gt1,gt2,g1,g2,ZZ,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhHmcVWmVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*g2*Conjg(ZP(gt2,1))*ZA(gt1,1)*ZZ(1,1))/2._dp
res = res-(g1*g2*Conjg(ZP(gt2,2))*ZA(gt1,2)*ZZ(1,1))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhHmcVWmVPT  
 
 
Subroutine CouplingAhHmcVWmVZT(gt1,gt2,g1,g2,ZZ,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhHmcVWmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*g2*Conjg(ZP(gt2,1))*ZA(gt1,1)*ZZ(1,2))/2._dp
res = res-(g1*g2*Conjg(ZP(gt2,2))*ZA(gt1,2)*ZZ(1,2))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhHmcVWmVZT  
 
 
Subroutine CouplingAhcHmVPVWmT(gt1,gt2,g1,g2,ZZ,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhcHmVPVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*ZA(gt1,1)*ZP(gt2,1)*ZZ(1,1))/2._dp
res = res+(g1*g2*ZA(gt1,2)*ZP(gt2,2)*ZZ(1,1))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhcHmVPVWmT  
 
 
Subroutine CouplingAhcHmVWmVZT(gt1,gt2,g1,g2,ZZ,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhcHmVWmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*ZA(gt1,1)*ZP(gt2,1)*ZZ(1,2))/2._dp
res = res+(g1*g2*ZA(gt1,2)*ZP(gt2,2)*ZZ(1,2))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhcHmVWmVZT  
 
 
Subroutine CouplinghhhhVPVPT(gt1,gt2,g1,g2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhVPVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,1)**2)/2._dp
res = res+(g1**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,1)**2)/2._dp
res = res-(g1*g2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,1)*ZZ(2,1))
res = res-(g1*g2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,1)*ZZ(2,1))
res = res+(g2**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(2,1)**2)/2._dp
res = res+(g2**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(2,1)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhVPVPT  
 
 
Subroutine CouplinghhhhVPVZT(gt1,gt2,g1,g2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhVPVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res+(g1**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res-(g1*g2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res-(g1*g2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res+(g2**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(2,1)*ZZ(2,2))/2._dp
res = res+(g2**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(2,1)*ZZ(2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhVPVZT  
 
 
Subroutine CouplinghhhhcVWmVWmT(gt1,gt2,g2,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZH(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2**2*ZH(gt1,1)*ZH(gt2,1))/2._dp
res = res+(g2**2*ZH(gt1,2)*ZH(gt2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhcVWmVWmT  
 
 
Subroutine CouplinghhhhVZVZT(gt1,gt2,g1,g2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,2)**2)/2._dp
res = res+(g1**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,2)**2)/2._dp
res = res-(g1*g2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,2)*ZZ(2,2))
res = res-(g1*g2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,2)*ZZ(2,2))
res = res+(g2**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(2,2)**2)/2._dp
res = res+(g2**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(2,2)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhVZVZT  
 
 
Subroutine CouplinghhHmcVWmVPT(gt1,gt2,g1,g2,ZZ,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhHmcVWmVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*Conjg(ZP(gt2,1))*ZH(gt1,1)*ZZ(1,1))/2._dp
res = res+(g1*g2*Conjg(ZP(gt2,2))*ZH(gt1,2)*ZZ(1,1))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhHmcVWmVPT  
 
 
Subroutine CouplinghhHmcVWmVZT(gt1,gt2,g1,g2,ZZ,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhHmcVWmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*Conjg(ZP(gt2,1))*ZH(gt1,1)*ZZ(1,2))/2._dp
res = res+(g1*g2*Conjg(ZP(gt2,2))*ZH(gt1,2)*ZZ(1,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhHmcVWmVZT  
 
 
Subroutine CouplinghhcHmVPVWmT(gt1,gt2,g1,g2,ZZ,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhcHmVPVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*ZH(gt1,1)*ZP(gt2,1)*ZZ(1,1))/2._dp
res = res+(g1*g2*ZH(gt1,2)*ZP(gt2,2)*ZZ(1,1))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhcHmVPVWmT  
 
 
Subroutine CouplinghhcHmVWmVZT(gt1,gt2,g1,g2,ZZ,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhcHmVWmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*ZH(gt1,1)*ZP(gt2,1)*ZZ(1,2))/2._dp
res = res+(g1*g2*ZH(gt1,2)*ZP(gt2,2)*ZZ(1,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhcHmVWmVZT  
 
 
Subroutine CouplingHmcHmVPVPT(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmVPVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,1)**2)/2._dp
res = res+(g1**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,1)**2)/2._dp
res = res+g1*g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,1)*ZZ(2,1)
res = res+g1*g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,1)*ZZ(2,1)
res = res+(g2**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(2,1)**2)/2._dp
res = res+(g2**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(2,1)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmVPVPT  
 
 
Subroutine CouplingHmcHmVPVZT(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmVPVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res+(g1**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res+(g1*g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res+(g1*g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res+(g1*g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res+(g1*g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res+(g2**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(2,1)*ZZ(2,2))/2._dp
res = res+(g2**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(2,1)*ZZ(2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmVPVZT  
 
 
Subroutine CouplingHmcHmcVWmVWmT(gt1,gt2,g2,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2**2*Conjg(ZP(gt1,1))*ZP(gt2,1))/2._dp
res = res+(g2**2*Conjg(ZP(gt1,2))*ZP(gt2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmcVWmVWmT  
 
 
Subroutine CouplingHmcHmVZVZT(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,2)**2)/2._dp
res = res+(g1**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,2)**2)/2._dp
res = res+g1*g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,2)*ZZ(2,2)
res = res+g1*g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,2)*ZZ(2,2)
res = res+(g2**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(2,2)**2)/2._dp
res = res+(g2**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(2,2)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmVZVZT  
 
 
Subroutine CouplingVGVGVGT(g3,res)

Implicit None 

Real(dp), Intent(in) :: g3

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingVGVGVG' 
 
res = 0._dp 
res = res+g3
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingVGVGVGT  
 
 
Subroutine CouplingcVWmVPVWmT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmVPVWm' 
 
res = 0._dp 
res = res+g2*ZZ(2,1)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmVPVWmT  
 
 
Subroutine CouplingcVWmVWmVZT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmVWmVZ' 
 
res = 0._dp 
res = res-(g2*ZZ(2,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmVWmVZT  
 
 
Subroutine CouplingcFdFdAhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,          & 
& Y2d31,Y2d32,Y2d33,ZA,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1d11*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d12*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d13*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d21*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d22*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d23*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2d31*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d32*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d33*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1d11)*ZDR(gt2,1)*ZDL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d12)*ZDR(gt2,2)*ZDL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d13)*ZDR(gt2,3)*ZDL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d21)*ZDR(gt2,1)*ZDL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d22)*ZDR(gt2,2)*ZDL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d23)*ZDR(gt2,3)*ZDL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2d31)*ZDR(gt2,1)*ZDL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2d32)*ZDR(gt2,2)*ZDL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2d33)*ZDR(gt2,3)*ZDL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdAhT  
 
 
Subroutine CouplingcFeFeAhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZA,             & 
& ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFeAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1e11*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e12*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e21*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e22*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2e33*Conjg(ZER(gt1,3))*Conjg(ZEL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1e11)*ZER(gt2,1)*ZEL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1e12)*ZER(gt2,2)*ZEL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1e21)*ZER(gt2,1)*ZEL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1e22)*ZER(gt2,2)*ZEL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2e33)*ZER(gt2,3)*ZEL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFeAhT  
 
 
Subroutine CouplingcFuFuAhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZA,             & 
& ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZUL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1u11)*ZUR(gt2,1)*ZUL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u12)*ZUR(gt2,2)*ZUL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u21)*ZUR(gt2,1)*ZUL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u22)*ZUR(gt2,2)*ZUL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2u33)*ZUR(gt2,3)*ZUL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuAhT  
 
 
Subroutine CouplingFvFvAhT(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,             & 
& C23,C31,C32,BB11,BB12,BB21,BB22,ZA,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1n11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n11*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,4))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n21*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,4))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n12*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,5))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n22*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,5))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2n33*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((Y2n33*Conjg(Vv(gt1,3))*Conjg(Vv(gt2,6))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((BB11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,4))*ZA(gt3,3))/sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C13*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C31*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,5))*ZA(gt3,3))/sqrt(2._dp)
resL = resL-(C23*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C32*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C13*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C31*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C23*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C32*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1n11)*Vv(gt1,4)*Vv(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,5)*Vv(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,4)*Vv(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,5)*Vv(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n11)*Vv(gt1,1)*Vv(gt2,4)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,2)*Vv(gt2,4)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,1)*Vv(gt2,5)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,2)*Vv(gt2,5)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,6)*Vv(gt2,3)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,3)*Vv(gt2,6)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(BB11)*Vv(gt1,4)*Vv(gt2,4)*ZA(gt3,3))/sqrt(2._dp)
resR = resR-(Conjg(BB12)*Vv(gt1,5)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-(Conjg(BB21)*Vv(gt1,5)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,6)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,6)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-(Conjg(BB12)*Vv(gt1,4)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-(Conjg(BB21)*Vv(gt1,4)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-((Conjg(BB22)*Vv(gt1,5)*Vv(gt2,5)*ZA(gt3,3))/sqrt(2._dp))
resR = resR+(Conjg(C23)*Vv(gt1,6)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,6)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,4)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,4)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C23)*Vv(gt1,5)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,5)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvAhT  
 
 
Subroutine CouplingcFdFdhhT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,          & 
& Y2d31,Y2d32,Y2d33,ZH,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1d11*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d12*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d13*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d21*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d22*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d23*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y2d31*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d32*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d33*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR-((Conjg(Y1d11)*ZDR(gt2,1)*ZDL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d12)*ZDR(gt2,2)*ZDL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d13)*ZDR(gt2,3)*ZDL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d21)*ZDR(gt2,1)*ZDL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d22)*ZDR(gt2,2)*ZDL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d23)*ZDR(gt2,3)*ZDL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y2d31)*ZDR(gt2,1)*ZDL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
resR = resR-((Conjg(Y2d32)*ZDR(gt2,2)*ZDL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
resR = resR-((Conjg(Y2d33)*ZDR(gt2,3)*ZDL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdhhT  
 
 
Subroutine CouplingcFuFdcHmT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,               & 
& Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,              & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,              & 
& Y1u22,Y2u33,ZP(2,2),ZDL(3,3),ZDR(3,3),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFdcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZDL(gt2,1))*ZP(gt3,1))
resL = resL-(Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZDL(gt2,1))*ZP(gt3,1))
resL = resL-(Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZDL(gt2,2))*ZP(gt3,1))
resL = resL-(Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZDL(gt2,2))*ZP(gt3,1))
resL = resL-(Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZDL(gt2,3))*ZP(gt3,2))
resR = 0._dp 
resR = resR-(Conjg(Y1d11)*ZDR(gt2,1)*ZUL(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1d12)*ZDR(gt2,2)*ZUL(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1d13)*ZDR(gt2,3)*ZUL(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1d21)*ZDR(gt2,1)*ZUL(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y1d22)*ZDR(gt2,2)*ZUL(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y1d23)*ZDR(gt2,3)*ZUL(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y2d31)*ZDR(gt2,1)*ZUL(gt1,3)*ZP(gt3,2))
resR = resR-(Conjg(Y2d32)*ZDR(gt2,2)*ZUL(gt1,3)*ZP(gt3,2))
resR = resR-(Conjg(Y2d33)*ZDR(gt2,3)*ZUL(gt1,3)*ZP(gt3,2))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFdcHmT  
 
 
Subroutine CouplingFvFecHmT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,          & 
& Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP(2,2),ZEL(3,3),         & 
& ZER(3,3),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFecHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1n11*Conjg(ZEL(gt2,1))*Conjg(Vv(gt1,4))*ZP(gt3,1))
resL = resL-(Y1n21*Conjg(ZEL(gt2,2))*Conjg(Vv(gt1,4))*ZP(gt3,1))
resL = resL-(Y1n12*Conjg(ZEL(gt2,1))*Conjg(Vv(gt1,5))*ZP(gt3,1))
resL = resL-(Y1n22*Conjg(ZEL(gt2,2))*Conjg(Vv(gt1,5))*ZP(gt3,1))
resL = resL-(Y2n33*Conjg(ZEL(gt2,3))*Conjg(Vv(gt1,6))*ZP(gt3,2))
resR = 0._dp 
resR = resR-(Conjg(Y1e11)*ZER(gt2,1)*Vv(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1e12)*ZER(gt2,2)*Vv(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1e21)*ZER(gt2,1)*Vv(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y1e22)*ZER(gt2,2)*Vv(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y2e33)*ZER(gt2,3)*Vv(gt1,3)*ZP(gt3,2))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFecHmT  
 
 
Subroutine CouplingcFeFehhT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZH,             & 
& ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFehh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1e11*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e12*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e21*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e22*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y2e33*Conjg(ZER(gt1,3))*Conjg(ZEL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR-((Conjg(Y1e11)*ZER(gt2,1)*ZEL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1e12)*ZER(gt2,2)*ZEL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1e21)*ZER(gt2,1)*ZEL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1e22)*ZER(gt2,2)*ZEL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y2e33)*ZER(gt2,3)*ZEL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFehhT  
 
 
Subroutine CouplingcFuFuhhT(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZH,             & 
& ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZUL(gt2,3))*ZH(gt3,2))/sqrt(2._dp)
resR = 0._dp 
resR = resR+(Conjg(Y1u11)*ZUR(gt2,1)*ZUL(gt1,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u12)*ZUR(gt2,2)*ZUL(gt1,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u21)*ZUR(gt2,1)*ZUL(gt1,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u22)*ZUR(gt2,2)*ZUL(gt1,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2u33)*ZUR(gt2,3)*ZUL(gt1,3)*ZH(gt3,2))/sqrt(2._dp)
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuhhT  
 
 
Subroutine CouplingcFdFuHmT(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,          & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,              & 
& Y1u22,Y2u33,ZP(2,2),ZDL(3,3),ZDR(3,3),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFuHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1d11*Conjg(ZDR(gt1,1))*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d12*Conjg(ZDR(gt1,2))*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d13*Conjg(ZDR(gt1,3))*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d21*Conjg(ZDR(gt1,1))*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d22*Conjg(ZDR(gt1,2))*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d23*Conjg(ZDR(gt1,3))*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y2d31*Conjg(ZDR(gt1,1))*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
resL = resL-(Y2d32*Conjg(ZDR(gt1,2))*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
resL = resL-(Y2d33*Conjg(ZDR(gt1,3))*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
resR = 0._dp 
resR = resR-(Conjg(Y1u11)*Conjg(ZP(gt3,1))*ZUR(gt2,1)*ZDL(gt1,1))
resR = resR-(Conjg(Y1u12)*Conjg(ZP(gt3,1))*ZUR(gt2,2)*ZDL(gt1,1))
resR = resR-(Conjg(Y1u21)*Conjg(ZP(gt3,1))*ZUR(gt2,1)*ZDL(gt1,2))
resR = resR-(Conjg(Y1u22)*Conjg(ZP(gt3,1))*ZUR(gt2,2)*ZDL(gt1,2))
resR = resR-(Conjg(Y2u33)*Conjg(ZP(gt3,2))*ZUR(gt2,3)*ZDL(gt1,3))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFuHmT  
 
 
Subroutine CouplingFvFvhhT(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,             & 
& C23,C31,C32,BB11,BB12,BB21,BB22,ZH,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(Y1n11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n11*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,4))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n21*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,4))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n12*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,5))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n22*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,5))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y2n33*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,3))*ZH(gt3,2))/sqrt(2._dp)
resL = resL+(Y2n33*Conjg(Vv(gt1,3))*Conjg(Vv(gt2,6))*ZH(gt3,2))/sqrt(2._dp)
resL = resL-((BB11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,4))*ZH(gt3,3))/sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C13*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C31*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,5))*ZH(gt3,3))/sqrt(2._dp)
resL = resL+(C23*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C32*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C13*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C31*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C23*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C32*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1n11)*Vv(gt1,4)*Vv(gt2,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,5)*Vv(gt2,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,4)*Vv(gt2,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,5)*Vv(gt2,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n11)*Vv(gt1,1)*Vv(gt2,4)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,2)*Vv(gt2,4)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,1)*Vv(gt2,5)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,2)*Vv(gt2,5)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,6)*Vv(gt2,3)*ZH(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,3)*Vv(gt2,6)*ZH(gt3,2))/sqrt(2._dp)
resR = resR-((Conjg(BB11)*Vv(gt1,4)*Vv(gt2,4)*ZH(gt3,3))/sqrt(2._dp))
resR = resR+(Conjg(BB12)*Vv(gt1,5)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB21)*Vv(gt1,5)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,6)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,6)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB12)*Vv(gt1,4)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB21)*Vv(gt1,4)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB22)*Vv(gt1,5)*Vv(gt2,5)*ZH(gt3,3))/sqrt(2._dp)
resR = resR+(Conjg(C23)*Vv(gt1,6)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,6)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,4)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,4)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C23)*Vv(gt1,5)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,5)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvhhT  
 
 
Subroutine CouplingcFeFvHmT(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,          & 
& Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP(2,2),ZEL(3,3),         & 
& ZER(3,3),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFvHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1e11*Conjg(ZER(gt1,1))*Conjg(Vv(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1e12*Conjg(ZER(gt1,2))*Conjg(Vv(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1e21*Conjg(ZER(gt1,1))*Conjg(Vv(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y1e22*Conjg(ZER(gt1,2))*Conjg(Vv(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y2e33*Conjg(ZER(gt1,3))*Conjg(Vv(gt2,3))*Conjg(ZP(gt3,2)))
resR = 0._dp 
resR = resR-(Conjg(Y1n11)*Conjg(ZP(gt3,1))*ZEL(gt1,1)*Vv(gt2,4))
resR = resR-(Conjg(Y1n21)*Conjg(ZP(gt3,1))*ZEL(gt1,2)*Vv(gt2,4))
resR = resR-(Conjg(Y1n12)*Conjg(ZP(gt3,1))*ZEL(gt1,1)*Vv(gt2,5))
resR = resR-(Conjg(Y1n22)*Conjg(ZP(gt3,1))*ZEL(gt1,2)*Vv(gt2,5))
resR = resR-(Conjg(Y2n33)*Conjg(ZP(gt3,2))*ZEL(gt1,3)*Vv(gt2,6))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFvHmT  
 
 
Subroutine CouplingcFdFdVGT(gt1,gt2,g3,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3

Complex(dp), Intent(in) :: ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g3*Conjg(ZDL(gt2,1))*ZDL(gt1,1))
resL = resL-(g3*Conjg(ZDL(gt2,2))*ZDL(gt1,2))
resL = resL-(g3*Conjg(ZDL(gt2,3))*ZDL(gt1,3))
resR = 0._dp 
resR = resR-(g3*Conjg(ZDR(gt1,1))*ZDR(gt2,1))
resR = resR-(g3*Conjg(ZDR(gt1,2))*ZDR(gt2,2))
resR = resR-(g3*Conjg(ZDR(gt1,3))*ZDR(gt2,3))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdVGT  
 
 
Subroutine CouplingcFdFdVPT(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g1*Conjg(ZDL(gt2,1))*ZDL(gt1,1)*ZZ(1,1))/6._dp
resL = resL-(g1*Conjg(ZDL(gt2,2))*ZDL(gt1,2)*ZZ(1,1))/6._dp
resL = resL-(g1*Conjg(ZDL(gt2,3))*ZDL(gt1,3)*ZZ(1,1))/6._dp
resL = resL+(g2*Conjg(ZDL(gt2,1))*ZDL(gt1,1)*ZZ(2,1))/2._dp
resL = resL+(g2*Conjg(ZDL(gt2,2))*ZDL(gt1,2)*ZZ(2,1))/2._dp
resL = resL+(g2*Conjg(ZDL(gt2,3))*ZDL(gt1,3)*ZZ(2,1))/2._dp
resR = 0._dp 
resR = resR+(g1*Conjg(ZDR(gt1,1))*ZDR(gt2,1)*ZZ(1,1))/3._dp
resR = resR+(g1*Conjg(ZDR(gt1,2))*ZDR(gt2,2)*ZZ(1,1))/3._dp
resR = resR+(g1*Conjg(ZDR(gt1,3))*ZDR(gt2,3)*ZZ(1,1))/3._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdVPT  
 
 
Subroutine CouplingcFdFdVZT(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g1*Conjg(ZDL(gt2,1))*ZDL(gt1,1)*ZZ(1,2))/6._dp
resL = resL-(g1*Conjg(ZDL(gt2,2))*ZDL(gt1,2)*ZZ(1,2))/6._dp
resL = resL-(g1*Conjg(ZDL(gt2,3))*ZDL(gt1,3)*ZZ(1,2))/6._dp
resL = resL+(g2*Conjg(ZDL(gt2,1))*ZDL(gt1,1)*ZZ(2,2))/2._dp
resL = resL+(g2*Conjg(ZDL(gt2,2))*ZDL(gt1,2)*ZZ(2,2))/2._dp
resL = resL+(g2*Conjg(ZDL(gt2,3))*ZDL(gt1,3)*ZZ(2,2))/2._dp
resR = 0._dp 
resR = resR+(g1*Conjg(ZDR(gt1,1))*ZDR(gt2,1)*ZZ(1,2))/3._dp
resR = resR+(g1*Conjg(ZDR(gt1,2))*ZDR(gt2,2)*ZZ(1,2))/3._dp
resR = resR+(g1*Conjg(ZDR(gt1,3))*ZDR(gt2,3)*ZZ(1,2))/3._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdVZT  
 
 
Subroutine CouplingcFuFdcVWmT(gt1,gt2,g2,ZDL,ZUL,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZDL(3,3),ZUL(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFdcVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((g2*Conjg(ZDL(gt2,1))*ZUL(gt1,1))/sqrt(2._dp))
resL = resL-((g2*Conjg(ZDL(gt2,2))*ZUL(gt1,2))/sqrt(2._dp))
resL = resL-((g2*Conjg(ZDL(gt2,3))*ZUL(gt1,3))/sqrt(2._dp))
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFdcVWmT  
 
 
Subroutine CouplingFvFecVWmT(gt1,gt2,g2,ZEL,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZEL(3,3),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFecVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((g2*Conjg(ZEL(gt2,1))*Vv(gt1,1))/sqrt(2._dp))
resL = resL-((g2*Conjg(ZEL(gt2,2))*Vv(gt1,2))/sqrt(2._dp))
resL = resL-((g2*Conjg(ZEL(gt2,3))*Vv(gt1,3))/sqrt(2._dp))
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFecVWmT  
 
 
Subroutine CouplingcFeFeVPT(gt1,gt2,g1,g2,ZZ,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFeVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(g1*Conjg(ZEL(gt2,1))*ZEL(gt1,1)*ZZ(1,1))/2._dp
resL = resL+(g1*Conjg(ZEL(gt2,2))*ZEL(gt1,2)*ZZ(1,1))/2._dp
resL = resL+(g1*Conjg(ZEL(gt2,3))*ZEL(gt1,3)*ZZ(1,1))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,1))*ZEL(gt1,1)*ZZ(2,1))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,2))*ZEL(gt1,2)*ZZ(2,1))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,3))*ZEL(gt1,3)*ZZ(2,1))/2._dp
resR = 0._dp 
resR = resR+g1*Conjg(ZER(gt1,1))*ZER(gt2,1)*ZZ(1,1)
resR = resR+g1*Conjg(ZER(gt1,2))*ZER(gt2,2)*ZZ(1,1)
resR = resR+g1*Conjg(ZER(gt1,3))*ZER(gt2,3)*ZZ(1,1)
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFeVPT  
 
 
Subroutine CouplingcFeFeVZT(gt1,gt2,g1,g2,ZZ,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFeVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(g1*Conjg(ZEL(gt2,1))*ZEL(gt1,1)*ZZ(1,2))/2._dp
resL = resL+(g1*Conjg(ZEL(gt2,2))*ZEL(gt1,2)*ZZ(1,2))/2._dp
resL = resL+(g1*Conjg(ZEL(gt2,3))*ZEL(gt1,3)*ZZ(1,2))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,1))*ZEL(gt1,1)*ZZ(2,2))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,2))*ZEL(gt1,2)*ZZ(2,2))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,3))*ZEL(gt1,3)*ZZ(2,2))/2._dp
resR = 0._dp 
resR = resR+g1*Conjg(ZER(gt1,1))*ZER(gt2,1)*ZZ(1,2)
resR = resR+g1*Conjg(ZER(gt1,2))*ZER(gt2,2)*ZZ(1,2)
resR = resR+g1*Conjg(ZER(gt1,3))*ZER(gt2,3)*ZZ(1,2)
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFeVZT  
 
 
Subroutine CouplingcFuFuVGT(gt1,gt2,g3,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3

Complex(dp), Intent(in) :: ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g3*Conjg(ZUL(gt2,1))*ZUL(gt1,1))
resL = resL-(g3*Conjg(ZUL(gt2,2))*ZUL(gt1,2))
resL = resL-(g3*Conjg(ZUL(gt2,3))*ZUL(gt1,3))
resR = 0._dp 
resR = resR-(g3*Conjg(ZUR(gt1,1))*ZUR(gt2,1))
resR = resR-(g3*Conjg(ZUR(gt1,2))*ZUR(gt2,2))
resR = resR-(g3*Conjg(ZUR(gt1,3))*ZUR(gt2,3))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuVGT  
 
 
Subroutine CouplingcFuFuVPT(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g1*Conjg(ZUL(gt2,1))*ZUL(gt1,1)*ZZ(1,1))/6._dp
resL = resL-(g1*Conjg(ZUL(gt2,2))*ZUL(gt1,2)*ZZ(1,1))/6._dp
resL = resL-(g1*Conjg(ZUL(gt2,3))*ZUL(gt1,3)*ZZ(1,1))/6._dp
resL = resL-(g2*Conjg(ZUL(gt2,1))*ZUL(gt1,1)*ZZ(2,1))/2._dp
resL = resL-(g2*Conjg(ZUL(gt2,2))*ZUL(gt1,2)*ZZ(2,1))/2._dp
resL = resL-(g2*Conjg(ZUL(gt2,3))*ZUL(gt1,3)*ZZ(2,1))/2._dp
resR = 0._dp 
resR = resR+(-2*g1*Conjg(ZUR(gt1,1))*ZUR(gt2,1)*ZZ(1,1))/3._dp
resR = resR+(-2*g1*Conjg(ZUR(gt1,2))*ZUR(gt2,2)*ZZ(1,1))/3._dp
resR = resR+(-2*g1*Conjg(ZUR(gt1,3))*ZUR(gt2,3)*ZZ(1,1))/3._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuVPT  
 
 
Subroutine CouplingcFdFuVWmT(gt1,gt2,g2,ZDL,ZUL,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZDL(3,3),ZUL(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFuVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((g2*Conjg(ZUL(gt2,1))*ZDL(gt1,1))/sqrt(2._dp))
resL = resL-((g2*Conjg(ZUL(gt2,2))*ZDL(gt1,2))/sqrt(2._dp))
resL = resL-((g2*Conjg(ZUL(gt2,3))*ZDL(gt1,3))/sqrt(2._dp))
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFuVWmT  
 
 
Subroutine CouplingcFuFuVZT(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g1*Conjg(ZUL(gt2,1))*ZUL(gt1,1)*ZZ(1,2))/6._dp
resL = resL-(g1*Conjg(ZUL(gt2,2))*ZUL(gt1,2)*ZZ(1,2))/6._dp
resL = resL-(g1*Conjg(ZUL(gt2,3))*ZUL(gt1,3)*ZZ(1,2))/6._dp
resL = resL-(g2*Conjg(ZUL(gt2,1))*ZUL(gt1,1)*ZZ(2,2))/2._dp
resL = resL-(g2*Conjg(ZUL(gt2,2))*ZUL(gt1,2)*ZZ(2,2))/2._dp
resL = resL-(g2*Conjg(ZUL(gt2,3))*ZUL(gt1,3)*ZZ(2,2))/2._dp
resR = 0._dp 
resR = resR+(-2*g1*Conjg(ZUR(gt1,1))*ZUR(gt2,1)*ZZ(1,2))/3._dp
resR = resR+(-2*g1*Conjg(ZUR(gt1,2))*ZUR(gt2,2)*ZZ(1,2))/3._dp
resR = resR+(-2*g1*Conjg(ZUR(gt1,3))*ZUR(gt2,3)*ZZ(1,2))/3._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuVZT  
 
 
Subroutine CouplingFvFvVPT(gt1,gt2,g1,g2,ZZ,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(g1*Conjg(Vv(gt2,1))*Vv(gt1,1)*ZZ(1,1))/2._dp
resL = resL+(g1*Conjg(Vv(gt2,2))*Vv(gt1,2)*ZZ(1,1))/2._dp
resL = resL+(g1*Conjg(Vv(gt2,3))*Vv(gt1,3)*ZZ(1,1))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,1))*Vv(gt1,1)*ZZ(2,1))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,2))*Vv(gt1,2)*ZZ(2,1))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,3))*Vv(gt1,3)*ZZ(2,1))/2._dp
resR = 0._dp 
resR = resR-(g1*Conjg(Vv(gt1,1))*Vv(gt2,1)*ZZ(1,1))/2._dp
resR = resR-(g1*Conjg(Vv(gt1,2))*Vv(gt2,2)*ZZ(1,1))/2._dp
resR = resR-(g1*Conjg(Vv(gt1,3))*Vv(gt2,3)*ZZ(1,1))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,1))*Vv(gt2,1)*ZZ(2,1))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,2))*Vv(gt2,2)*ZZ(2,1))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,3))*Vv(gt2,3)*ZZ(2,1))/2._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvVPT  
 
 
Subroutine CouplingFvFvVZT(gt1,gt2,g1,g2,ZZ,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(g1*Conjg(Vv(gt2,1))*Vv(gt1,1)*ZZ(1,2))/2._dp
resL = resL+(g1*Conjg(Vv(gt2,2))*Vv(gt1,2)*ZZ(1,2))/2._dp
resL = resL+(g1*Conjg(Vv(gt2,3))*Vv(gt1,3)*ZZ(1,2))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,1))*Vv(gt1,1)*ZZ(2,2))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,2))*Vv(gt1,2)*ZZ(2,2))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,3))*Vv(gt1,3)*ZZ(2,2))/2._dp
resR = 0._dp 
resR = resR-(g1*Conjg(Vv(gt1,1))*Vv(gt2,1)*ZZ(1,2))/2._dp
resR = resR-(g1*Conjg(Vv(gt1,2))*Vv(gt2,2)*ZZ(1,2))/2._dp
resR = resR-(g1*Conjg(Vv(gt1,3))*Vv(gt2,3)*ZZ(1,2))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,1))*Vv(gt2,1)*ZZ(2,2))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,2))*Vv(gt2,2)*ZZ(2,2))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,3))*Vv(gt2,3)*ZZ(2,2))/2._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvVZT  
 
 
Subroutine CouplingcFeFvVWmT(gt1,gt2,g2,ZEL,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZEL(3,3),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFvVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((g2*Conjg(Vv(gt2,1))*ZEL(gt1,1))/sqrt(2._dp))
resL = resL-((g2*Conjg(Vv(gt2,2))*ZEL(gt1,2))/sqrt(2._dp))
resL = resL-((g2*Conjg(Vv(gt2,3))*ZEL(gt1,3))/sqrt(2._dp))
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFvVWmT  
 
 
Subroutine CouplingVGVGVGVGT(g3,res1,res2,res3)

Implicit None 

Real(dp), Intent(in) :: g3

Complex(dp), Intent(out) :: res1, res2, res3 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingVGVGVGVG' 
 
res1 = 0._dp 
res1 = res1-16*g3**2
res2 = 0._dp 
res3 = 0._dp 
res3 = res3+16*g3**2
If ((Real(res1,dp).ne.Real(res1,dp)).or.(Real(res2,dp).ne.Real(res2,dp)).or.(Real(res3,dp).ne.Real(res3,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingVGVGVGVGT  
 
 
Subroutine CouplingcVWmVPVPVWmT(g2,ZZ,res1,res2,res3)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res1, res2, res3 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmVPVPVWm' 
 
res1 = 0._dp 
res1 = res1+g2**2*ZZ(2,1)**2
res2 = 0._dp 
res2 = res2+g2**2*ZZ(2,1)**2
res3 = 0._dp 
res3 = res3-2*g2**2*ZZ(2,1)**2
If ((Real(res1,dp).ne.Real(res1,dp)).or.(Real(res2,dp).ne.Real(res2,dp)).or.(Real(res3,dp).ne.Real(res3,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmVPVPVWmT  
 
 
Subroutine CouplingcVWmVPVWmVZT(g2,ZZ,res1,res2,res3)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res1, res2, res3 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmVPVWmVZ' 
 
res1 = 0._dp 
res1 = res1+g2**2*ZZ(2,1)*ZZ(2,2)
res2 = 0._dp 
res2 = res2-2*g2**2*ZZ(2,1)*ZZ(2,2)
res3 = 0._dp 
res3 = res3+g2**2*ZZ(2,1)*ZZ(2,2)
If ((Real(res1,dp).ne.Real(res1,dp)).or.(Real(res2,dp).ne.Real(res2,dp)).or.(Real(res3,dp).ne.Real(res3,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmVPVWmVZT  
 
 
Subroutine CouplingcVWmcVWmVWmVWmT(g2,res1,res2,res3)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(out) :: res1, res2, res3 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmcVWmVWmVWm' 
 
res1 = 0._dp 
res1 = res1+2*g2**2
res2 = 0._dp 
res2 = res2-g2**2
res3 = 0._dp 
res3 = res3-g2**2
If ((Real(res1,dp).ne.Real(res1,dp)).or.(Real(res2,dp).ne.Real(res2,dp)).or.(Real(res3,dp).ne.Real(res3,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmcVWmVWmVWmT  
 
 
Subroutine CouplingcVWmVWmVZVZT(g2,ZZ,res1,res2,res3)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res1, res2, res3 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmVWmVZVZ' 
 
res1 = 0._dp 
res1 = res1-2*g2**2*ZZ(2,2)**2
res2 = 0._dp 
res2 = res2+g2**2*ZZ(2,2)**2
res3 = 0._dp 
res3 = res3+g2**2*ZZ(2,2)**2
If ((Real(res1,dp).ne.Real(res1,dp)).or.(Real(res2,dp).ne.Real(res2,dp)).or.(Real(res3,dp).ne.Real(res3,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmVWmVZVZT  
 
 
Subroutine CouplingcgGgGVGT(g3,res)

Implicit None 

Real(dp), Intent(in) :: g3

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgGgGVG' 
 
res = 0._dp 
res = res+g3
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgGgGVGT  
 
 
Subroutine CouplingcgWmgAVWmT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgAVWm' 
 
res = 0._dp 
res = res+g2*ZZ(2,1)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgAVWmT  
 
 
Subroutine CouplingcgWpgAcVWmT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgAcVWm' 
 
res = 0._dp 
res = res-(g2*Conjg(ZZ(2,1)))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgAcVWmT  
 
 
Subroutine CouplingcgWmgWmVPT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgWmVP' 
 
res = 0._dp 
res = res-(g2*ZZ(2,1))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgWmVPT  
 
 
Subroutine CouplingcgWmgWmVZT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgWmVZ' 
 
res = 0._dp 
res = res-(g2*ZZ(2,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgWmVZT  
 
 
Subroutine CouplingcgAgWmcVWmT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgAgWmcVWm' 
 
res = 0._dp 
res = res+g2*Conjg(ZZ(2,1))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgAgWmcVWmT  
 
 
Subroutine CouplingcgZgWmcVWmT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgZgWmcVWm' 
 
res = 0._dp 
res = res+g2*Conjg(ZZ(2,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgZgWmcVWmT  
 
 
Subroutine CouplingcgWpgWpVPT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgWpVP' 
 
res = 0._dp 
res = res+g2*Conjg(ZZ(2,1))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgWpVPT  
 
 
Subroutine CouplingcgAgWpVWmT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgAgWpVWm' 
 
res = 0._dp 
res = res-(g2*Conjg(ZZ(2,1)))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgAgWpVWmT  
 
 
Subroutine CouplingcgZgWpVWmT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgZgWpVWm' 
 
res = 0._dp 
res = res-(g2*Conjg(ZZ(2,2)))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgZgWpVWmT  
 
 
Subroutine CouplingcgWpgWpVZT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgWpVZ' 
 
res = 0._dp 
res = res+g2*Conjg(ZZ(2,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgWpVZT  
 
 
Subroutine CouplingcgWmgZVWmT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgZVWm' 
 
res = 0._dp 
res = res+g2*ZZ(2,2)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgZVWmT  
 
 
Subroutine CouplingcgWpgZcVWmT(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgZcVWm' 
 
res = 0._dp 
res = res-(g2*Conjg(ZZ(2,2)))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgZcVWmT  
 
 
Subroutine CouplingcgWmgWmAhT(gt3,g2,v1,v2,ZA,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g2,v1,v2,ZA(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgWmAh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g2**2*v1*RXiWm*ZA(gt3,1))/4._dp
res = res-(g2**2*v2*RXiWm*ZA(gt3,2))/4._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgWmAhT  
 
 
Subroutine CouplingcgWpgWpAhT(gt3,g2,v1,v2,ZA,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g2,v1,v2,ZA(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgWpAh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2**2*v1*RXiWm*ZA(gt3,1))/4._dp
res = res+(g2**2*v2*RXiWm*ZA(gt3,2))/4._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgWpAhT  
 
 
Subroutine CouplingcgZgAhhT(gt3,g1,g2,v1,v2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgZgAhh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1**2*v1*Conjg(ZZ(1,2))*RXiZ*ZH(gt3,1)*ZZ(1,1))/8._dp
res = res+(g1*g2*v1*Conjg(ZZ(2,2))*RXiZ*ZH(gt3,1)*ZZ(1,1))/8._dp
res = res-(g1**2*v2*Conjg(ZZ(1,2))*RXiZ*ZH(gt3,2)*ZZ(1,1))/8._dp
res = res+(g1*g2*v2*Conjg(ZZ(2,2))*RXiZ*ZH(gt3,2)*ZZ(1,1))/8._dp
res = res-(g1**2*v1*RXiZ*ZH(gt3,1)*ZZ(1,1)*ZZ(1,2))/8._dp
res = res-(g1**2*v2*RXiZ*ZH(gt3,2)*ZZ(1,1)*ZZ(1,2))/8._dp
res = res+(g1*g2*v1*Conjg(ZZ(1,2))*RXiZ*ZH(gt3,1)*ZZ(2,1))/8._dp
res = res-(g2**2*v1*Conjg(ZZ(2,2))*RXiZ*ZH(gt3,1)*ZZ(2,1))/8._dp
res = res+(g1*g2*v2*Conjg(ZZ(1,2))*RXiZ*ZH(gt3,2)*ZZ(2,1))/8._dp
res = res-(g2**2*v2*Conjg(ZZ(2,2))*RXiZ*ZH(gt3,2)*ZZ(2,1))/8._dp
res = res+(g1*g2*v1*RXiZ*ZH(gt3,1)*ZZ(1,2)*ZZ(2,1))/8._dp
res = res+(g1*g2*v2*RXiZ*ZH(gt3,2)*ZZ(1,2)*ZZ(2,1))/8._dp
res = res+(g1*g2*v1*RXiZ*ZH(gt3,1)*ZZ(1,1)*ZZ(2,2))/8._dp
res = res+(g1*g2*v2*RXiZ*ZH(gt3,2)*ZZ(1,1)*ZZ(2,2))/8._dp
res = res-(g2**2*v1*RXiZ*ZH(gt3,1)*ZZ(2,1)*ZZ(2,2))/8._dp
res = res-(g2**2*v2*RXiZ*ZH(gt3,2)*ZZ(2,1)*ZZ(2,2))/8._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgZgAhhT  
 
 
Subroutine CouplingcgWmgAHmT(gt3,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgAHm' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*g2*v1*Conjg(ZP(gt3,1))*Conjg(ZZ(1,1))*RXiWm)/4._dp
res = res-(g1*g2*v2*Conjg(ZP(gt3,2))*Conjg(ZZ(1,1))*RXiWm)/4._dp
res = res-(g2**2*v1*Conjg(ZP(gt3,1))*Conjg(ZZ(2,1))*RXiWm)/4._dp
res = res-(g2**2*v2*Conjg(ZP(gt3,2))*Conjg(ZZ(2,1))*RXiWm)/4._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgAHmT  
 
 
Subroutine CouplingcgWpgAcHmT(gt3,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgAcHm' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*g2*v1*RXiWm*ZP(gt3,1)*ZZ(1,1))/4._dp
res = res-(g1*g2*v2*RXiWm*ZP(gt3,2)*ZZ(1,1))/4._dp
res = res-(g2**2*v1*RXiWm*ZP(gt3,1)*ZZ(2,1))/4._dp
res = res-(g2**2*v2*RXiWm*ZP(gt3,2)*ZZ(2,1))/4._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgAcHmT  
 
 
Subroutine CouplingcgWmgWmhhT(gt3,g2,v1,v2,ZH,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g2,v1,v2,ZH(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgWmhh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g2**2*v1*RXiWm*ZH(gt3,1))/4._dp
res = res-(g2**2*v2*RXiWm*ZH(gt3,2))/4._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgWmhhT  
 
 
Subroutine CouplingcgZgWmcHmT(gt3,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgZgWmcHm' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*g2*v1*Conjg(ZZ(1,2))*RXiZ*ZP(gt3,1))/8._dp
res = res+(g2**2*v1*Conjg(ZZ(2,2))*RXiZ*ZP(gt3,1))/8._dp
res = res-(g1*g2*v2*Conjg(ZZ(1,2))*RXiZ*ZP(gt3,2))/8._dp
res = res+(g2**2*v2*Conjg(ZZ(2,2))*RXiZ*ZP(gt3,2))/8._dp
res = res-(g1*g2*v1*RXiZ*ZP(gt3,1)*ZZ(1,2))/8._dp
res = res-(g1*g2*v2*RXiZ*ZP(gt3,2)*ZZ(1,2))/8._dp
res = res+(g2**2*v1*RXiZ*ZP(gt3,1)*ZZ(2,2))/8._dp
res = res+(g2**2*v2*RXiZ*ZP(gt3,2)*ZZ(2,2))/8._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgZgWmcHmT  
 
 
Subroutine CouplingcgWpgWphhT(gt3,g2,v1,v2,ZH,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g2,v1,v2,ZH(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgWphh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g2**2*v1*RXiWm*ZH(gt3,1))/4._dp
res = res-(g2**2*v2*RXiWm*ZH(gt3,2))/4._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgWphhT  
 
 
Subroutine CouplingcgZgWpHmT(gt3,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgZgWpHm' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*g2*v1*Conjg(ZP(gt3,1))*Conjg(ZZ(1,2))*RXiZ)/8._dp
res = res-(g1*g2*v2*Conjg(ZP(gt3,2))*Conjg(ZZ(1,2))*RXiZ)/8._dp
res = res+(g2**2*v1*Conjg(ZP(gt3,1))*Conjg(ZZ(2,2))*RXiZ)/8._dp
res = res+(g2**2*v2*Conjg(ZP(gt3,2))*Conjg(ZZ(2,2))*RXiZ)/8._dp
res = res-(g1*g2*v1*Conjg(ZP(gt3,1))*RXiZ*ZZ(1,2))/8._dp
res = res-(g1*g2*v2*Conjg(ZP(gt3,2))*RXiZ*ZZ(1,2))/8._dp
res = res+(g2**2*v1*Conjg(ZP(gt3,1))*RXiZ*ZZ(2,2))/8._dp
res = res+(g2**2*v2*Conjg(ZP(gt3,2))*RXiZ*ZZ(2,2))/8._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgZgWpHmT  
 
 
Subroutine CouplingcgZgZhhT(gt3,g1,g2,v1,v2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgZgZhh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1**2*v1*Conjg(ZZ(1,2))*RXiZ*ZH(gt3,1)*ZZ(1,2))/8._dp
res = res+(g1*g2*v1*Conjg(ZZ(2,2))*RXiZ*ZH(gt3,1)*ZZ(1,2))/8._dp
res = res-(g1**2*v2*Conjg(ZZ(1,2))*RXiZ*ZH(gt3,2)*ZZ(1,2))/8._dp
res = res+(g1*g2*v2*Conjg(ZZ(2,2))*RXiZ*ZH(gt3,2)*ZZ(1,2))/8._dp
res = res-(g1**2*v1*RXiZ*ZH(gt3,1)*ZZ(1,2)**2)/8._dp
res = res-(g1**2*v2*RXiZ*ZH(gt3,2)*ZZ(1,2)**2)/8._dp
res = res+(g1*g2*v1*Conjg(ZZ(1,2))*RXiZ*ZH(gt3,1)*ZZ(2,2))/8._dp
res = res-(g2**2*v1*Conjg(ZZ(2,2))*RXiZ*ZH(gt3,1)*ZZ(2,2))/8._dp
res = res+(g1*g2*v2*Conjg(ZZ(1,2))*RXiZ*ZH(gt3,2)*ZZ(2,2))/8._dp
res = res-(g2**2*v2*Conjg(ZZ(2,2))*RXiZ*ZH(gt3,2)*ZZ(2,2))/8._dp
res = res+(g1*g2*v1*RXiZ*ZH(gt3,1)*ZZ(1,2)*ZZ(2,2))/4._dp
res = res+(g1*g2*v2*RXiZ*ZH(gt3,2)*ZZ(1,2)*ZZ(2,2))/4._dp
res = res-(g2**2*v1*RXiZ*ZH(gt3,1)*ZZ(2,2)**2)/8._dp
res = res-(g2**2*v2*RXiZ*ZH(gt3,2)*ZZ(2,2)**2)/8._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgZgZhhT  
 
 
Subroutine CouplingcgWmgZHmT(gt3,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgZHm' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*g2*v1*Conjg(ZP(gt3,1))*Conjg(ZZ(1,2))*RXiWm)/4._dp
res = res-(g1*g2*v2*Conjg(ZP(gt3,2))*Conjg(ZZ(1,2))*RXiWm)/4._dp
res = res-(g2**2*v1*Conjg(ZP(gt3,1))*Conjg(ZZ(2,2))*RXiWm)/4._dp
res = res-(g2**2*v2*Conjg(ZP(gt3,2))*Conjg(ZZ(2,2))*RXiWm)/4._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgZHmT  
 
 
Subroutine CouplingcgWpgZcHmT(gt3,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgZcHm' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*g2*v1*RXiWm*ZP(gt3,1)*ZZ(1,2))/4._dp
res = res-(g1*g2*v2*RXiWm*ZP(gt3,2)*ZZ(1,2))/4._dp
res = res-(g2**2*v1*RXiWm*ZP(gt3,1)*ZZ(2,2))/4._dp
res = res-(g2**2*v2*RXiWm*ZP(gt3,2)*ZZ(2,2))/4._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgZcHmT  
 
 
Subroutine CouplingsForEffPot4(Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,        & 
& Aa3,Aa4,ZA,ZH,ZP,cplAhAhAhAh,cplAhAhhhhh,cplAhAhHmcHm,cplhhhhhhhh,cplhhhhHmcHm,        & 
& cplHmHmcHmcHm)

Implicit None 
Real(dp), Intent(in) :: ZA(3,3),ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: cplAhAhAhAh(3,3,3,3),cplAhAhhhhh(3,3,3,3),cplAhAhHmcHm(3,3,2,2),cplhhhhhhhh(3,3,3,3), & 
& cplhhhhHmcHm(3,3,2,2),cplHmHmcHmcHm(2,2,2,2)

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'CouplingsForEffPot4'
 
cplAhAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhAhAh2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa3,Aa4,ZA,cplAhAhAhAh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhhhhh2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa3,Aa4,ZH,ZA,cplAhAhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingAhAhHmcHm2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,       & 
& Aa3,Aa4,ZA,ZP,cplAhAhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplhhhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call Couplinghhhhhhhh2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa3,Aa4,ZH,cplhhhhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplhhhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplinghhhhHmcHm2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,       & 
& Aa3,Aa4,ZH,ZP,cplhhhhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplHmHmcHmcHm = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingHmHmcHmcHm2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,ZP,cplHmHmcHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


Iname = Iname - 1 
End Subroutine CouplingsForEffPot4

Subroutine CouplingsForEffPot3(Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,Lam1,Lam3,Lam4,            & 
& Lam2,Lam1Dash,Lam2Dash,Lam3Dash,ZH,ZP,g3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,          & 
& Y2d31,Y2d32,Y2d33,ZDL,ZDR,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL,ZER,Y1u11,Y1u12,           & 
& Y1u21,Y1u22,Y2u33,ZUL,ZUR,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,               & 
& BB11,BB12,BB21,BB22,Vv,cplAhAhAh,cplAhAhhh,cplAhhhhh,cplAhHmcHm,cplhhhhhh,             & 
& cplhhHmcHm,cplVGVGVG,cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,      & 
& cplcFuFuAhR,cplFvFvAhL,cplFvFvAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdcHmL,cplcFuFdcHmR,   & 
& cplFvFecHmL,cplFvFecHmR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFdFuHmL,cplcFdFuHmR,cplFvFvhhL,cplFvFvhhR,cplcFeFvHmL,cplcFeFvHmR,cplcFdFdVGL,     & 
& cplcFdFdVGR,cplcFuFuVGL,cplcFuFuVGR)

Implicit None 
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3),ZH(3,3),g3

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,ZP(2,2),               & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3),               & 
& Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3),Y1u11,Y1u12,Y1u21,Y1u22,               & 
& Y2u33,ZUL(3,3),ZUR(3,3),Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: cplAhAhAh(3,3,3),cplAhAhhh(3,3,3),cplAhhhhh(3,3,3),cplAhHmcHm(3,2,2),cplhhhhhh(3,3,3),& 
& cplhhHmcHm(3,2,2),cplVGVGVG,cplcFdFdAhL(3,3,3),cplcFdFdAhR(3,3,3),cplcFeFeAhL(3,3,3),  & 
& cplcFeFeAhR(3,3,3),cplcFuFuAhL(3,3,3),cplcFuFuAhR(3,3,3),cplFvFvAhL(6,6,3),            & 
& cplFvFvAhR(6,6,3),cplcFdFdhhL(3,3,3),cplcFdFdhhR(3,3,3),cplcFuFdcHmL(3,3,2),           & 
& cplcFuFdcHmR(3,3,2),cplFvFecHmL(6,3,2),cplFvFecHmR(6,3,2),cplcFeFehhL(3,3,3),          & 
& cplcFeFehhR(3,3,3),cplcFuFuhhL(3,3,3),cplcFuFuhhR(3,3,3),cplcFdFuHmL(3,3,2),           & 
& cplcFdFuHmR(3,3,2),cplFvFvhhL(6,6,3),cplFvFvhhR(6,6,3),cplcFeFvHmL(3,6,2),             & 
& cplcFeFvHmR(3,6,2),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3)

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'CouplingsForEffPot3'
 
cplAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhAh2L(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,cplAhAhAh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhhh2L(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhAhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhhhhh2L(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingAhHmcHm2L(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,ZP,               & 
& cplAhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call Couplinghhhhhh2L(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,cplhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhHmcHm2L(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,             & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZP,cplhhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplVGVGVG = 0._dp 
Call CouplingVGVGVG2L(g3,cplVGVGVG)



cplcFdFdAhL = 0._dp 
cplcFdFdAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdAh2L(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,ZA,ZDL,ZDR,cplcFdFdAhL(gt1,gt2,gt3),cplcFdFdAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFeAhL = 0._dp 
cplcFeFeAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFeAh2L(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZA,ZEL,              & 
& ZER,cplcFeFeAhL(gt1,gt2,gt3),cplcFeFeAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuAhL = 0._dp 
cplcFuFuAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuAh2L(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZA,ZUL,              & 
& ZUR,cplcFuFuAhL(gt1,gt2,gt3),cplcFuFuAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFvAhL = 0._dp 
cplFvFvAhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingFvFvAh2L(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,              & 
& C31,C32,BB11,BB12,BB21,BB22,ZA,Vv,cplFvFvAhL(gt1,gt2,gt3),cplFvFvAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdhh2L(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3),cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFdcHmL = 0._dp 
cplcFuFdcHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFdcHm2L(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,              & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,cplcFuFdcHmL(gt1,gt2,gt3)& 
& ,cplcFuFdcHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFecHmL = 0._dp 
cplFvFecHmR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingFvFecHm2L(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,               & 
& Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,cplFvFecHmL(gt1,gt2,gt3),cplFvFecHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFehh2L(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZH,ZEL,              & 
& ZER,cplcFeFehhL(gt1,gt2,gt3),cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuhh2L(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZH,ZUL,              & 
& ZUR,cplcFuFuhhL(gt1,gt2,gt3),cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFuHmL = 0._dp 
cplcFdFuHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFdFuHm2L(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,cplcFdFuHmL(gt1,gt2,gt3)& 
& ,cplcFdFuHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFvhhL = 0._dp 
cplFvFvhhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingFvFvhh2L(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,              & 
& C31,C32,BB11,BB12,BB21,BB22,ZH,Vv,cplFvFvhhL(gt1,gt2,gt3),cplFvFvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFvHmL = 0._dp 
cplcFeFvHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
  Do gt3 = 1, 2
Call CouplingcFeFvHm2L(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,               & 
& Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,cplcFeFvHmL(gt1,gt2,gt3),cplcFeFvHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdVGL = 0._dp 
cplcFdFdVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVG2L(gt1,gt2,g3,ZDL,ZDR,cplcFdFdVGL(gt1,gt2),cplcFdFdVGR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVGL = 0._dp 
cplcFuFuVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVG2L(gt1,gt2,g3,ZUL,ZUR,cplcFuFuVGL(gt1,gt2),cplcFuFuVGR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
End Subroutine CouplingsForEffPot3

Subroutine CouplingAhAhAhAh2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,           & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhAhAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-6*Lam1*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,1)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-6*Lam2*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,2)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-6*Lam1Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,3)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhAhAh2L  
 
 
Subroutine CouplingAhAhhhhh2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,           & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-2*Lam1*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,1)*ZH(gt4,1)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-2*Lam2*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,2)*ZH(gt4,2)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-2*Lam1Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,3)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhhhhh2L  
 
 
Subroutine CouplingAhAhHmcHm2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,          & 
& Lam3Dash,Aa3,Aa4,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-2*Lam1*Conjg(ZP(gt3,1))*ZA(gt1,1)*ZA(gt2,1)*ZP(gt4,1)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,2)*ZA(gt2,1)*ZP(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,1)*ZA(gt2,2)*ZP(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*Conjg(ZP(gt3,1))*ZA(gt1,2)*ZA(gt2,2)*ZP(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+Conjg(Aa3)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+Conjg(Aa4)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*Conjg(ZP(gt3,2))*ZA(gt1,1)*ZA(gt2,1)*ZP(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,2)*ZA(gt2,1)*ZP(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,1)*ZA(gt2,2)*ZP(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-2*Lam2*Conjg(ZP(gt3,2))*ZA(gt1,2)*ZA(gt2,2)*ZP(gt4,2)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+Aa3*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res+Aa4*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2))
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhHmcHm2L  
 
 
Subroutine Couplinghhhhhhhh2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,           & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'Couplinghhhhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-6*Lam1*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,1)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-6*Lam2*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,2)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-6*Lam1Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,3)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine Couplinghhhhhhhh2L  
 
 
Subroutine CouplinghhhhHmcHm2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,          & 
& Lam3Dash,Aa3,Aa4,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-2*Lam1*Conjg(ZP(gt3,1))*ZH(gt1,1)*ZH(gt2,1)*ZP(gt4,1)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt3,2))*ZH(gt1,2)*ZH(gt2,1)*ZP(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt3,2))*ZH(gt1,1)*ZH(gt2,2)*ZP(gt4,1))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*Conjg(ZP(gt3,1))*ZH(gt1,2)*ZH(gt2,2)*ZP(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam2Dash*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3*Conjg(ZP(gt3,2))*ZH(gt1,1)*ZH(gt2,1)*ZP(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt3,1))*ZH(gt1,2)*ZH(gt2,1)*ZP(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt3,1))*ZH(gt1,1)*ZH(gt2,2)*ZP(gt4,2))/2._dp
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-2*Lam2*Conjg(ZP(gt3,2))*ZH(gt1,2)*ZH(gt2,2)*ZP(gt4,2)
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa3*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Aa4*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
End If 
If ((gt1.eq.gt2).And.(gt3.eq.gt4)) Then 
res = res-(Lam3Dash*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhHmcHm2L  
 
 
Subroutine CouplingHmHmcHmcHm2L(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmHmcHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((gt1.eq.gt3).And.(gt2.eq.gt4)) Then 
res = res-4*Lam1*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,1)
End If 
If ((gt1.eq.gt3).And.(gt2.eq.gt4)) Then 
res = res-(Lam3*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,2)*ZP(gt4,1))
End If 
If ((gt1.eq.gt3).And.(gt2.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,2)*ZP(gt4,1))
End If 
If ((gt1.eq.gt3).And.(gt2.eq.gt4)) Then 
res = res-(Lam3*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,1))
End If 
If ((gt1.eq.gt3).And.(gt2.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,1))
End If 
If ((gt1.eq.gt3).And.(gt2.eq.gt4)) Then 
res = res-(Lam3*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,2))
End If 
If ((gt1.eq.gt3).And.(gt2.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,2))
End If 
If ((gt1.eq.gt3).And.(gt2.eq.gt4)) Then 
res = res-(Lam3*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,1)*ZP(gt4,2))
End If 
If ((gt1.eq.gt3).And.(gt2.eq.gt4)) Then 
res = res-(Lam4*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,1)*ZP(gt4,2))
End If 
If ((gt1.eq.gt3).And.(gt2.eq.gt4)) Then 
res = res-4*Lam2*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,2)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmHmcHmcHm2L  
 
 
Subroutine CouplingAhAhAh2L(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(Aa1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res+(Aa1*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Aa3*v1*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(Aa1*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Aa3*v1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(Aa3*v2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(Aa3*v1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhAh2L  
 
 
Subroutine CouplingAhAhhh2L(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*v1*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,1)
res = res-(Lam3*v1*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1))
res = res-(Lam4*v1*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1))
res = res+(Aa1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam2Dash*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))
res = res+(Aa3*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam3*v2*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2))
res = res-(Lam4*v2*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2))
res = res-(Aa1*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res+(Aa4*v3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res-2*Lam2*v2*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,2)
res = res-(Aa1*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa4*v3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa3*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa4*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam3Dash*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam2Dash*v3*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,3))
res = res-(Aa1*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa3*v2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa1*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v3*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,3))
res = res+(Aa3*v1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa3*v2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa3*v1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-2*Lam1Dash*v3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhhh2L  
 
 
Subroutine CouplingAhhhhh2L(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(Aa1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa4*v3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v2*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Aa1*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa3*v1*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(v1*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res+(Aa4*v3*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa1*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(Aa3*v1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa3*v2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa3*v1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa4*v1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhhh2L  
 
 
Subroutine CouplingAhHmcHm2L(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,               & 
& ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3)

Complex(dp), Intent(in) :: Lam4,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(Lam4*v2*Conjg(ZP(gt2,2))*ZA(gt1,1)*ZP(gt3,1))/2._dp
res = res+(Lam4*v1*Conjg(ZP(gt2,2))*ZA(gt1,2)*ZP(gt3,1))/2._dp
res = res-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res = res+(Conjg(Aa2)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))/sqrt(2._dp)
res = res-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))
res = res+v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1)
res = res+(Lam4*v2*Conjg(ZP(gt2,1))*ZA(gt1,1)*ZP(gt3,2))/2._dp
res = res-(Lam4*v1*Conjg(ZP(gt2,1))*ZA(gt1,2)*ZP(gt3,2))/2._dp
res = res+(Aa1*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))/sqrt(2._dp)
res = res-((Aa2*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res = res+Aa3*v3*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2)
res = res-(Aa4*v3*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhHmcHm2L  
 
 
Subroutine Couplinghhhhhh2L(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'Couplinghhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-6*Lam1*v1*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,1)
res = res-(Lam3*v2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1))
res = res-(Lam4*v2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1))
res = res-(Lam2Dash*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,1))
res = res-(Lam3*v2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Lam4*v2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Lam3*v1*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Lam4*v1*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Aa1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Lam2Dash*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,1))
res = res-(Aa1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam2Dash*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))
res = res-(Aa3*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v2*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v2*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam3*v2*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Lam4*v2*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Lam3*v1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Lam4*v1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Aa1*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Lam3*v1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2))
res = res-(Lam4*v1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2))
res = res-6*Lam2*v2*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,2)
res = res-(Lam3Dash*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,2))
res = res-(Aa1*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam3Dash*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,2))
res = res-(Aa3*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam3Dash*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))
res = res-(v1*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(v1*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam2Dash*v3*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,3))
res = res-(Aa1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Lam2Dash*v1*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))
res = res-(Aa3*v2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v3*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,3))
res = res-(Aa3*v1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v2*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))
res = res-(v1*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam2Dash*v1*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))
res = res-(Aa3*v2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa3*v1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v2*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))
res = res-(v1*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-6*Lam1Dash*v3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine Couplinghhhhhh2L  
 
 
Subroutine CouplinghhHmcHm2L(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,       & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*v1*Conjg(ZP(gt2,1))*ZH(gt1,1)*ZP(gt3,1)
res = res-(Lam4*v2*Conjg(ZP(gt2,2))*ZH(gt1,1)*ZP(gt3,1))/2._dp
res = res-(Lam3*v2*Conjg(ZP(gt2,1))*ZH(gt1,2)*ZP(gt3,1))
res = res-(Lam4*v1*Conjg(ZP(gt2,2))*ZH(gt1,2)*ZP(gt3,1))/2._dp
res = res-(Lam2Dash*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,1))
res = res-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res = res-((Conjg(Aa2)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))
res = res-(v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))
res = res-(Lam4*v2*Conjg(ZP(gt2,1))*ZH(gt1,1)*ZP(gt3,2))/2._dp
res = res-(Lam3*v1*Conjg(ZP(gt2,2))*ZH(gt1,1)*ZP(gt3,2))
res = res-(Lam4*v1*Conjg(ZP(gt2,1))*ZH(gt1,2)*ZP(gt3,2))/2._dp
res = res-2*Lam2*v2*Conjg(ZP(gt2,2))*ZH(gt1,2)*ZP(gt3,2)
res = res-((Aa1*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res = res-((Aa2*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res = res-(Aa3*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))
res = res-(Aa4*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))
res = res-(Lam3Dash*v3*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhHmcHm2L  
 
 
Subroutine CouplingVGVGVG2L(g3,res)

Implicit None 

Real(dp), Intent(in) :: g3

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingVGVGVG' 
 
res = 0._dp 
res = res+g3
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingVGVGVG2L  
 
 
Subroutine CouplingcFdFdAh2L(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,               & 
& Y1d23,Y2d31,Y2d32,Y2d33,ZA,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1d11*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d12*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d13*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d21*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d22*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d23*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2d31*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d32*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d33*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1d11)*ZDR(gt2,1)*ZDL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d12)*ZDR(gt2,2)*ZDL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d13)*ZDR(gt2,3)*ZDL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d21)*ZDR(gt2,1)*ZDL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d22)*ZDR(gt2,2)*ZDL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d23)*ZDR(gt2,3)*ZDL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2d31)*ZDR(gt2,1)*ZDL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2d32)*ZDR(gt2,2)*ZDL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2d33)*ZDR(gt2,3)*ZDL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdAh2L  
 
 
Subroutine CouplingcFeFeAh2L(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& ZA,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFeAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1e11*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e12*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e21*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e22*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2e33*Conjg(ZER(gt1,3))*Conjg(ZEL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1e11)*ZER(gt2,1)*ZEL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1e12)*ZER(gt2,2)*ZEL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1e21)*ZER(gt2,1)*ZEL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1e22)*ZER(gt2,2)*ZEL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2e33)*ZER(gt2,3)*ZEL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFeAh2L  
 
 
Subroutine CouplingcFuFuAh2L(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,               & 
& ZA,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZUL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1u11)*ZUR(gt2,1)*ZUL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u12)*ZUR(gt2,2)*ZUL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u21)*ZUR(gt2,1)*ZUL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u22)*ZUR(gt2,2)*ZUL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2u33)*ZUR(gt2,3)*ZUL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuAh2L  
 
 
Subroutine CouplingFvFvAh2L(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,            & 
& C23,C31,C32,BB11,BB12,BB21,BB22,ZA,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1n11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n11*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,4))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n21*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,4))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n12*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,5))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n22*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,5))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2n33*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((Y2n33*Conjg(Vv(gt1,3))*Conjg(Vv(gt2,6))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((BB11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,4))*ZA(gt3,3))/sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C13*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C31*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,5))*ZA(gt3,3))/sqrt(2._dp)
resL = resL-(C23*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C32*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C13*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C31*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C23*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C32*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1n11)*Vv(gt1,4)*Vv(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,5)*Vv(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,4)*Vv(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,5)*Vv(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n11)*Vv(gt1,1)*Vv(gt2,4)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,2)*Vv(gt2,4)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,1)*Vv(gt2,5)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,2)*Vv(gt2,5)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,6)*Vv(gt2,3)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,3)*Vv(gt2,6)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(BB11)*Vv(gt1,4)*Vv(gt2,4)*ZA(gt3,3))/sqrt(2._dp)
resR = resR-(Conjg(BB12)*Vv(gt1,5)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-(Conjg(BB21)*Vv(gt1,5)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,6)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,6)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-(Conjg(BB12)*Vv(gt1,4)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-(Conjg(BB21)*Vv(gt1,4)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-((Conjg(BB22)*Vv(gt1,5)*Vv(gt2,5)*ZA(gt3,3))/sqrt(2._dp))
resR = resR+(Conjg(C23)*Vv(gt1,6)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,6)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,4)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,4)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C23)*Vv(gt1,5)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,5)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvAh2L  
 
 
Subroutine CouplingcFdFdhh2L(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,               & 
& Y1d23,Y2d31,Y2d32,Y2d33,ZH,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1d11*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d12*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d13*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d21*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d22*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d23*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y2d31*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d32*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d33*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR-((Conjg(Y1d11)*ZDR(gt2,1)*ZDL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d12)*ZDR(gt2,2)*ZDL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d13)*ZDR(gt2,3)*ZDL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d21)*ZDR(gt2,1)*ZDL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d22)*ZDR(gt2,2)*ZDL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d23)*ZDR(gt2,3)*ZDL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y2d31)*ZDR(gt2,1)*ZDL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
resR = resR-((Conjg(Y2d32)*ZDR(gt2,2)*ZDL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
resR = resR-((Conjg(Y2d33)*ZDR(gt2,3)*ZDL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdhh2L  
 
 
Subroutine CouplingcFuFdcHm2L(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,              & 
& Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,              & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,              & 
& Y1u22,Y2u33,ZP(2,2),ZDL(3,3),ZDR(3,3),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFdcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZDL(gt2,1))*ZP(gt3,1))
resL = resL-(Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZDL(gt2,1))*ZP(gt3,1))
resL = resL-(Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZDL(gt2,2))*ZP(gt3,1))
resL = resL-(Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZDL(gt2,2))*ZP(gt3,1))
resL = resL-(Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZDL(gt2,3))*ZP(gt3,2))
resR = 0._dp 
resR = resR-(Conjg(Y1d11)*ZDR(gt2,1)*ZUL(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1d12)*ZDR(gt2,2)*ZUL(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1d13)*ZDR(gt2,3)*ZUL(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1d21)*ZDR(gt2,1)*ZUL(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y1d22)*ZDR(gt2,2)*ZUL(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y1d23)*ZDR(gt2,3)*ZUL(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y2d31)*ZDR(gt2,1)*ZUL(gt1,3)*ZP(gt3,2))
resR = resR-(Conjg(Y2d32)*ZDR(gt2,2)*ZUL(gt1,3)*ZP(gt3,2))
resR = resR-(Conjg(Y2d33)*ZDR(gt2,3)*ZUL(gt1,3)*ZP(gt3,2))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFdcHm2L  
 
 
Subroutine CouplingFvFecHm2L(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP(2,2),ZEL(3,3),         & 
& ZER(3,3),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFecHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1n11*Conjg(ZEL(gt2,1))*Conjg(Vv(gt1,4))*ZP(gt3,1))
resL = resL-(Y1n21*Conjg(ZEL(gt2,2))*Conjg(Vv(gt1,4))*ZP(gt3,1))
resL = resL-(Y1n12*Conjg(ZEL(gt2,1))*Conjg(Vv(gt1,5))*ZP(gt3,1))
resL = resL-(Y1n22*Conjg(ZEL(gt2,2))*Conjg(Vv(gt1,5))*ZP(gt3,1))
resL = resL-(Y2n33*Conjg(ZEL(gt2,3))*Conjg(Vv(gt1,6))*ZP(gt3,2))
resR = 0._dp 
resR = resR-(Conjg(Y1e11)*ZER(gt2,1)*Vv(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1e12)*ZER(gt2,2)*Vv(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1e21)*ZER(gt2,1)*Vv(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y1e22)*ZER(gt2,2)*Vv(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y2e33)*ZER(gt2,3)*Vv(gt1,3)*ZP(gt3,2))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFecHm2L  
 
 
Subroutine CouplingcFeFehh2L(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& ZH,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFehh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1e11*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e12*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e21*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e22*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y2e33*Conjg(ZER(gt1,3))*Conjg(ZEL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR-((Conjg(Y1e11)*ZER(gt2,1)*ZEL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1e12)*ZER(gt2,2)*ZEL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1e21)*ZER(gt2,1)*ZEL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1e22)*ZER(gt2,2)*ZEL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y2e33)*ZER(gt2,3)*ZEL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFehh2L  
 
 
Subroutine CouplingcFuFuhh2L(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,               & 
& ZH,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZUL(gt2,3))*ZH(gt3,2))/sqrt(2._dp)
resR = 0._dp 
resR = resR+(Conjg(Y1u11)*ZUR(gt2,1)*ZUL(gt1,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u12)*ZUR(gt2,2)*ZUL(gt1,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u21)*ZUR(gt2,1)*ZUL(gt1,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u22)*ZUR(gt2,2)*ZUL(gt1,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2u33)*ZUR(gt2,3)*ZUL(gt1,3)*ZH(gt3,2))/sqrt(2._dp)
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuhh2L  
 
 
Subroutine CouplingcFdFuHm2L(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,               & 
& Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,              & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,              & 
& Y1u22,Y2u33,ZP(2,2),ZDL(3,3),ZDR(3,3),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFuHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1d11*Conjg(ZDR(gt1,1))*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d12*Conjg(ZDR(gt1,2))*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d13*Conjg(ZDR(gt1,3))*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d21*Conjg(ZDR(gt1,1))*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d22*Conjg(ZDR(gt1,2))*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d23*Conjg(ZDR(gt1,3))*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y2d31*Conjg(ZDR(gt1,1))*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
resL = resL-(Y2d32*Conjg(ZDR(gt1,2))*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
resL = resL-(Y2d33*Conjg(ZDR(gt1,3))*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
resR = 0._dp 
resR = resR-(Conjg(Y1u11)*Conjg(ZP(gt3,1))*ZUR(gt2,1)*ZDL(gt1,1))
resR = resR-(Conjg(Y1u12)*Conjg(ZP(gt3,1))*ZUR(gt2,2)*ZDL(gt1,1))
resR = resR-(Conjg(Y1u21)*Conjg(ZP(gt3,1))*ZUR(gt2,1)*ZDL(gt1,2))
resR = resR-(Conjg(Y1u22)*Conjg(ZP(gt3,1))*ZUR(gt2,2)*ZDL(gt1,2))
resR = resR-(Conjg(Y2u33)*Conjg(ZP(gt3,2))*ZUR(gt2,3)*ZDL(gt1,3))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFuHm2L  
 
 
Subroutine CouplingFvFvhh2L(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,            & 
& C23,C31,C32,BB11,BB12,BB21,BB22,ZH,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(Y1n11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n11*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,4))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n21*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,4))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n12*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,5))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n22*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,5))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y2n33*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,3))*ZH(gt3,2))/sqrt(2._dp)
resL = resL+(Y2n33*Conjg(Vv(gt1,3))*Conjg(Vv(gt2,6))*ZH(gt3,2))/sqrt(2._dp)
resL = resL-((BB11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,4))*ZH(gt3,3))/sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C13*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C31*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,5))*ZH(gt3,3))/sqrt(2._dp)
resL = resL+(C23*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C32*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C13*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C31*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C23*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C32*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1n11)*Vv(gt1,4)*Vv(gt2,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,5)*Vv(gt2,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,4)*Vv(gt2,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,5)*Vv(gt2,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n11)*Vv(gt1,1)*Vv(gt2,4)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,2)*Vv(gt2,4)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,1)*Vv(gt2,5)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,2)*Vv(gt2,5)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,6)*Vv(gt2,3)*ZH(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,3)*Vv(gt2,6)*ZH(gt3,2))/sqrt(2._dp)
resR = resR-((Conjg(BB11)*Vv(gt1,4)*Vv(gt2,4)*ZH(gt3,3))/sqrt(2._dp))
resR = resR+(Conjg(BB12)*Vv(gt1,5)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB21)*Vv(gt1,5)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,6)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,6)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB12)*Vv(gt1,4)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB21)*Vv(gt1,4)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB22)*Vv(gt1,5)*Vv(gt2,5)*ZH(gt3,3))/sqrt(2._dp)
resR = resR+(Conjg(C23)*Vv(gt1,6)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,6)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,4)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,4)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C23)*Vv(gt1,5)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,5)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvhh2L  
 
 
Subroutine CouplingcFeFvHm2L(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP(2,2),ZEL(3,3),         & 
& ZER(3,3),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFvHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1e11*Conjg(ZER(gt1,1))*Conjg(Vv(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1e12*Conjg(ZER(gt1,2))*Conjg(Vv(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1e21*Conjg(ZER(gt1,1))*Conjg(Vv(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y1e22*Conjg(ZER(gt1,2))*Conjg(Vv(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y2e33*Conjg(ZER(gt1,3))*Conjg(Vv(gt2,3))*Conjg(ZP(gt3,2)))
resR = 0._dp 
resR = resR-(Conjg(Y1n11)*Conjg(ZP(gt3,1))*ZEL(gt1,1)*Vv(gt2,4))
resR = resR-(Conjg(Y1n21)*Conjg(ZP(gt3,1))*ZEL(gt1,2)*Vv(gt2,4))
resR = resR-(Conjg(Y1n12)*Conjg(ZP(gt3,1))*ZEL(gt1,1)*Vv(gt2,5))
resR = resR-(Conjg(Y1n22)*Conjg(ZP(gt3,1))*ZEL(gt1,2)*Vv(gt2,5))
resR = resR-(Conjg(Y2n33)*Conjg(ZP(gt3,2))*ZEL(gt1,3)*Vv(gt2,6))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFvHm2L  
 
 
Subroutine CouplingcFdFdVG2L(gt1,gt2,g3,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3

Complex(dp), Intent(in) :: ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g3*Conjg(ZDL(gt2,1))*ZDL(gt1,1))
resL = resL-(g3*Conjg(ZDL(gt2,2))*ZDL(gt1,2))
resL = resL-(g3*Conjg(ZDL(gt2,3))*ZDL(gt1,3))
resR = 0._dp 
resR = resR-(g3*Conjg(ZDR(gt1,1))*ZDR(gt2,1))
resR = resR-(g3*Conjg(ZDR(gt1,2))*ZDR(gt2,2))
resR = resR-(g3*Conjg(ZDR(gt1,3))*ZDR(gt2,3))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdVG2L  
 
 
Subroutine CouplingcFuFuVG2L(gt1,gt2,g3,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3

Complex(dp), Intent(in) :: ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g3*Conjg(ZUL(gt2,1))*ZUL(gt1,1))
resL = resL-(g3*Conjg(ZUL(gt2,2))*ZUL(gt1,2))
resL = resL-(g3*Conjg(ZUL(gt2,3))*ZUL(gt1,3))
resR = 0._dp 
resR = resR-(g3*Conjg(ZUR(gt1,1))*ZUR(gt2,1))
resR = resR-(g3*Conjg(ZUR(gt1,2))*ZUR(gt2,2))
resR = resR-(g3*Conjg(ZUR(gt1,3))*ZUR(gt2,3))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuVG2L  
 
 
Subroutine CouplingsFor2LPole3(Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,Lam1,Lam3,Lam4,            & 
& Lam2,Lam1Dash,Lam2Dash,Lam3Dash,ZH,ZP,g3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,          & 
& Y2d31,Y2d32,Y2d33,ZDL,ZDR,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL,ZER,Y1u11,Y1u12,           & 
& Y1u21,Y1u22,Y2u33,ZUL,ZUR,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,               & 
& BB11,BB12,BB21,BB22,Vv,cplAhAhAh,cplAhAhhh,cplAhhhhh,cplAhHmcHm,cplhhhhhh,             & 
& cplhhHmcHm,cplVGVGVG,cplcFdFdAhL,cplcFdFdAhR,cplcFeFeAhL,cplcFeFeAhR,cplcFuFuAhL,      & 
& cplcFuFuAhR,cplFvFvAhL,cplFvFvAhR,cplcFdFdhhL,cplcFdFdhhR,cplcFuFdcHmL,cplcFuFdcHmR,   & 
& cplFvFecHmL,cplFvFecHmR,cplcFeFehhL,cplcFeFehhR,cplcFuFuhhL,cplcFuFuhhR,               & 
& cplcFdFuHmL,cplcFdFuHmR,cplFvFvhhL,cplFvFvhhR,cplcFeFvHmL,cplcFeFvHmR,cplcFdFdVGL,     & 
& cplcFdFdVGR,cplcFuFuVGL,cplcFuFuVGR)

Implicit None 
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3),ZH(3,3),g3

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,ZP(2,2),               & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3),               & 
& Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3),Y1u11,Y1u12,Y1u21,Y1u22,               & 
& Y2u33,ZUL(3,3),ZUR(3,3),Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: cplAhAhAh(3,3,3),cplAhAhhh(3,3,3),cplAhhhhh(3,3,3),cplAhHmcHm(3,2,2),cplhhhhhh(3,3,3),& 
& cplhhHmcHm(3,2,2),cplVGVGVG,cplcFdFdAhL(3,3,3),cplcFdFdAhR(3,3,3),cplcFeFeAhL(3,3,3),  & 
& cplcFeFeAhR(3,3,3),cplcFuFuAhL(3,3,3),cplcFuFuAhR(3,3,3),cplFvFvAhL(6,6,3),            & 
& cplFvFvAhR(6,6,3),cplcFdFdhhL(3,3,3),cplcFdFdhhR(3,3,3),cplcFuFdcHmL(3,3,2),           & 
& cplcFuFdcHmR(3,3,2),cplFvFecHmL(6,3,2),cplFvFecHmR(6,3,2),cplcFeFehhL(3,3,3),          & 
& cplcFeFehhR(3,3,3),cplcFuFuhhL(3,3,3),cplcFuFuhhR(3,3,3),cplcFdFuHmL(3,3,2),           & 
& cplcFdFuHmR(3,3,2),cplFvFvhhL(6,6,3),cplFvFvhhR(6,6,3),cplcFeFvHmL(3,6,2),             & 
& cplcFeFvHmR(3,6,2),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),cplcFuFuVGL(3,3),cplcFuFuVGR(3,3)

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'CouplingsFor2LPole3'
 
cplAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhAh2LP(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,cplAhAhAh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhhh2LP(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,             & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhAhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhhhhh2LP(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingAhHmcHm2LP(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,ZP,              & 
& cplAhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call Couplinghhhhhh2LP(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,             & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,cplhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhHmcHm2LP(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,            & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZP,cplhhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplVGVGVG = 0._dp 
Call CouplingVGVGVG2LP(g3,cplVGVGVG)



cplcFdFdAhL = 0._dp 
cplcFdFdAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdAh2LP(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,              & 
& Y2d31,Y2d32,Y2d33,ZA,ZDL,ZDR,cplcFdFdAhL(gt1,gt2,gt3),cplcFdFdAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFeAhL = 0._dp 
cplcFeFeAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFeAh2LP(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZA,ZEL,             & 
& ZER,cplcFeFeAhL(gt1,gt2,gt3),cplcFeFeAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuAhL = 0._dp 
cplcFuFuAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuAh2LP(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZA,ZUL,             & 
& ZUR,cplcFuFuAhL(gt1,gt2,gt3),cplcFuFuAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFvAhL = 0._dp 
cplFvFvAhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingFvFvAh2LP(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,             & 
& C31,C32,BB11,BB12,BB21,BB22,ZA,Vv,cplFvFvAhL(gt1,gt2,gt3),cplFvFvAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdhhL = 0._dp 
cplcFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdhh2LP(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,              & 
& Y2d31,Y2d32,Y2d33,ZH,ZDL,ZDR,cplcFdFdhhL(gt1,gt2,gt3),cplcFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFdcHmL = 0._dp 
cplcFuFdcHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFdcHm2LP(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,             & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,cplcFuFdcHmL(gt1,gt2,gt3)& 
& ,cplcFuFdcHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFecHmL = 0._dp 
cplFvFecHmR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingFvFecHm2LP(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,              & 
& Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,cplFvFecHmL(gt1,gt2,gt3),cplFvFecHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFehhL = 0._dp 
cplcFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFehh2LP(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZH,ZEL,             & 
& ZER,cplcFeFehhL(gt1,gt2,gt3),cplcFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuhhL = 0._dp 
cplcFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuhh2LP(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZH,ZUL,             & 
& ZUR,cplcFuFuhhL(gt1,gt2,gt3),cplcFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFuHmL = 0._dp 
cplcFdFuHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFdFuHm2LP(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,              & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,cplcFdFuHmL(gt1,gt2,gt3)& 
& ,cplcFdFuHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFvhhL = 0._dp 
cplFvFvhhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingFvFvhh2LP(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,             & 
& C31,C32,BB11,BB12,BB21,BB22,ZH,Vv,cplFvFvhhL(gt1,gt2,gt3),cplFvFvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFvHmL = 0._dp 
cplcFeFvHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
  Do gt3 = 1, 2
Call CouplingcFeFvHm2LP(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,              & 
& Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,cplcFeFvHmL(gt1,gt2,gt3),cplcFeFvHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdVGL = 0._dp 
cplcFdFdVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVG2LP(gt1,gt2,g3,ZDL,ZDR,cplcFdFdVGL(gt1,gt2),cplcFdFdVGR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVGL = 0._dp 
cplcFuFuVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVG2LP(gt1,gt2,g3,ZUL,ZUR,cplcFuFuVGL(gt1,gt2),cplcFuFuVGR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
End Subroutine CouplingsFor2LPole3

Subroutine CouplingAhAhAh2LP(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(Aa1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res-(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res = res+(Aa1*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Aa3*v1*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res = res+(Aa1*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Aa3*v1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res = res+(Aa3*v2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(Aa3*v1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhAh2LP  
 
 
Subroutine CouplingAhAhhh2LP(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,       & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*v1*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,1)
res = res-(Lam3*v1*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1))
res = res-(Lam4*v1*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1))
res = res+(Aa1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam2Dash*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))
res = res+(Aa3*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam3*v2*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2))
res = res-(Lam4*v2*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2))
res = res-(Aa1*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res+(Aa4*v3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res = res-2*Lam2*v2*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,2)
res = res-(Aa1*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa4*v3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa3*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa4*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam3Dash*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam2Dash*v3*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,3))
res = res-(Aa1*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa3*v2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa1*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v3*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,3))
res = res+(Aa3*v1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa3*v2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa4*v2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa3*v1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v1*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res = res-2*Lam1Dash*v3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhhh2LP  
 
 
Subroutine CouplingAhhhhh2LP(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(Aa1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa4*v3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v2*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res+(Aa1*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Aa1*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa3*v1*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(v1*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res+(Aa1*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa2*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Aa3*v3*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res+(Aa4*v3*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res+(Aa3*v2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa1*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(Conjg(Aa1)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res+(v3*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(Aa3*v1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa3*v2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(v2*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa3*v1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res+(Aa4*v1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhhh2LP  
 
 
Subroutine CouplingAhHmcHm2LP(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,              & 
& ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3)

Complex(dp), Intent(in) :: Lam4,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(Lam4*v2*Conjg(ZP(gt2,2))*ZA(gt1,1)*ZP(gt3,1))/2._dp
res = res+(Lam4*v1*Conjg(ZP(gt2,2))*ZA(gt1,2)*ZP(gt3,1))/2._dp
res = res-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res = res+(Conjg(Aa2)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))/sqrt(2._dp)
res = res-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))
res = res+v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1)
res = res+(Lam4*v2*Conjg(ZP(gt2,1))*ZA(gt1,1)*ZP(gt3,2))/2._dp
res = res-(Lam4*v1*Conjg(ZP(gt2,1))*ZA(gt1,2)*ZP(gt3,2))/2._dp
res = res+(Aa1*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))/sqrt(2._dp)
res = res-((Aa2*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res = res+Aa3*v3*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2)
res = res-(Aa4*v3*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhHmcHm2LP  
 
 
Subroutine Couplinghhhhhh2LP(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,       & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'Couplinghhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-6*Lam1*v1*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,1)
res = res-(Lam3*v2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1))
res = res-(Lam4*v2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1))
res = res-(Lam2Dash*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,1))
res = res-(Lam3*v2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Lam4*v2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Lam3*v1*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Lam4*v1*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1))
res = res-(Aa1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res = res-(Lam2Dash*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,1))
res = res-(Aa1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam2Dash*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))
res = res-(Aa3*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Aa4*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v2*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(v2*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res = res-(Lam3*v2*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Lam4*v2*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Lam3*v1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Lam4*v1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2))
res = res-(Aa1*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res = res-(Lam3*v1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2))
res = res-(Lam4*v1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2))
res = res-6*Lam2*v2*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,2)
res = res-(Lam3Dash*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,2))
res = res-(Aa1*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam3Dash*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,2))
res = res-(Aa3*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Aa4*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam3Dash*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))
res = res-(v1*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(v1*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res = res-(Lam2Dash*v3*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,3))
res = res-(Aa1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Lam2Dash*v1*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))
res = res-(Aa3*v2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res = res-(Aa1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Aa3*v3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Conjg(Aa1)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(Conjg(Aa2)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v3*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v3*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,3))
res = res-(Aa3*v1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v2*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))
res = res-(v1*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res = res-(Lam2Dash*v1*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))
res = res-(Aa3*v2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v2*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa3*v1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Aa4*v1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(Lam3Dash*v2*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))
res = res-(v1*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-(v1*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res = res-6*Lam1Dash*v3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine Couplinghhhhhh2LP  
 
 
Subroutine CouplinghhHmcHm2LP(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,               & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*v1*Conjg(ZP(gt2,1))*ZH(gt1,1)*ZP(gt3,1)
res = res-(Lam4*v2*Conjg(ZP(gt2,2))*ZH(gt1,1)*ZP(gt3,1))/2._dp
res = res-(Lam3*v2*Conjg(ZP(gt2,1))*ZH(gt1,2)*ZP(gt3,1))
res = res-(Lam4*v1*Conjg(ZP(gt2,2))*ZH(gt1,2)*ZP(gt3,1))/2._dp
res = res-(Lam2Dash*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,1))
res = res-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res = res-((Conjg(Aa2)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res = res-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))
res = res-(v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))
res = res-(Lam4*v2*Conjg(ZP(gt2,1))*ZH(gt1,1)*ZP(gt3,2))/2._dp
res = res-(Lam3*v1*Conjg(ZP(gt2,2))*ZH(gt1,1)*ZP(gt3,2))
res = res-(Lam4*v1*Conjg(ZP(gt2,1))*ZH(gt1,2)*ZP(gt3,2))/2._dp
res = res-2*Lam2*v2*Conjg(ZP(gt2,2))*ZH(gt1,2)*ZP(gt3,2)
res = res-((Aa1*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res = res-((Aa2*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res = res-(Aa3*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))
res = res-(Aa4*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))
res = res-(Lam3Dash*v3*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhHmcHm2LP  
 
 
Subroutine CouplingVGVGVG2LP(g3,res)

Implicit None 

Real(dp), Intent(in) :: g3

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingVGVGVG' 
 
res = 0._dp 
res = res+g3
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingVGVGVG2LP  
 
 
Subroutine CouplingcFdFdAh2LP(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,              & 
& Y1d23,Y2d31,Y2d32,Y2d33,ZA,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1d11*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d12*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d13*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d21*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d22*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d23*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2d31*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d32*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d33*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1d11)*ZDR(gt2,1)*ZDL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d12)*ZDR(gt2,2)*ZDL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d13)*ZDR(gt2,3)*ZDL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d21)*ZDR(gt2,1)*ZDL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d22)*ZDR(gt2,2)*ZDL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1d23)*ZDR(gt2,3)*ZDL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2d31)*ZDR(gt2,1)*ZDL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2d32)*ZDR(gt2,2)*ZDL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2d33)*ZDR(gt2,3)*ZDL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdAh2LP  
 
 
Subroutine CouplingcFeFeAh2LP(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,              & 
& ZA,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFeAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1e11*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e12*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e21*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e22*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2e33*Conjg(ZER(gt1,3))*Conjg(ZEL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1e11)*ZER(gt2,1)*ZEL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1e12)*ZER(gt2,2)*ZEL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1e21)*ZER(gt2,1)*ZEL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1e22)*ZER(gt2,2)*ZEL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2e33)*ZER(gt2,3)*ZEL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFeAh2LP  
 
 
Subroutine CouplingcFuFuAh2LP(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,              & 
& ZA,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZUL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1u11)*ZUR(gt2,1)*ZUL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u12)*ZUR(gt2,2)*ZUL(gt1,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u21)*ZUR(gt2,1)*ZUL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u22)*ZUR(gt2,2)*ZUL(gt1,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2u33)*ZUR(gt2,3)*ZUL(gt1,3)*ZA(gt3,2))/sqrt(2._dp)
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuAh2LP  
 
 
Subroutine CouplingFvFvAh2LP(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,               & 
& C13,C23,C31,C32,BB11,BB12,BB21,BB22,ZA,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1n11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n11*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,4))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n21*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,4))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n12*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,5))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y1n22*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,5))*ZA(gt3,1))/sqrt(2._dp))
resL = resL-((Y2n33*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((Y2n33*Conjg(Vv(gt1,3))*Conjg(Vv(gt2,6))*ZA(gt3,2))/sqrt(2._dp))
resL = resL-((BB11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,4))*ZA(gt3,3))/sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C13*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C31*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,5))*ZA(gt3,3))/sqrt(2._dp)
resL = resL-(C23*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C32*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C13*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C31*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C23*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL-(C32*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1n11)*Vv(gt1,4)*Vv(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,5)*Vv(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,4)*Vv(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,5)*Vv(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n11)*Vv(gt1,1)*Vv(gt2,4)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,2)*Vv(gt2,4)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,1)*Vv(gt2,5)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,2)*Vv(gt2,5)*ZA(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,6)*Vv(gt2,3)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,3)*Vv(gt2,6)*ZA(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(BB11)*Vv(gt1,4)*Vv(gt2,4)*ZA(gt3,3))/sqrt(2._dp)
resR = resR-(Conjg(BB12)*Vv(gt1,5)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-(Conjg(BB21)*Vv(gt1,5)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,6)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,6)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-(Conjg(BB12)*Vv(gt1,4)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-(Conjg(BB21)*Vv(gt1,4)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR-((Conjg(BB22)*Vv(gt1,5)*Vv(gt2,5)*ZA(gt3,3))/sqrt(2._dp))
resR = resR+(Conjg(C23)*Vv(gt1,6)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,6)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,4)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,4)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C23)*Vv(gt1,5)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,5)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvAh2LP  
 
 
Subroutine CouplingcFdFdhh2LP(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,              & 
& Y1d23,Y2d31,Y2d32,Y2d33,ZH,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1d11*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d12*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d13*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d21*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d22*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1d23*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y2d31*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d32*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resL = resL-((Y2d33*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR-((Conjg(Y1d11)*ZDR(gt2,1)*ZDL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d12)*ZDR(gt2,2)*ZDL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d13)*ZDR(gt2,3)*ZDL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d21)*ZDR(gt2,1)*ZDL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d22)*ZDR(gt2,2)*ZDL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1d23)*ZDR(gt2,3)*ZDL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y2d31)*ZDR(gt2,1)*ZDL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
resR = resR-((Conjg(Y2d32)*ZDR(gt2,2)*ZDL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
resR = resR-((Conjg(Y2d33)*ZDR(gt2,3)*ZDL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdhh2LP  
 
 
Subroutine CouplingcFuFdcHm2LP(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,             & 
& Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,              & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,              & 
& Y1u22,Y2u33,ZP(2,2),ZDL(3,3),ZDR(3,3),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFdcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZDL(gt2,1))*ZP(gt3,1))
resL = resL-(Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZDL(gt2,1))*ZP(gt3,1))
resL = resL-(Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZDL(gt2,2))*ZP(gt3,1))
resL = resL-(Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZDL(gt2,2))*ZP(gt3,1))
resL = resL-(Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZDL(gt2,3))*ZP(gt3,2))
resR = 0._dp 
resR = resR-(Conjg(Y1d11)*ZDR(gt2,1)*ZUL(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1d12)*ZDR(gt2,2)*ZUL(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1d13)*ZDR(gt2,3)*ZUL(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1d21)*ZDR(gt2,1)*ZUL(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y1d22)*ZDR(gt2,2)*ZUL(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y1d23)*ZDR(gt2,3)*ZUL(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y2d31)*ZDR(gt2,1)*ZUL(gt1,3)*ZP(gt3,2))
resR = resR-(Conjg(Y2d32)*ZDR(gt2,2)*ZUL(gt1,3)*ZP(gt3,2))
resR = resR-(Conjg(Y2d33)*ZDR(gt2,3)*ZUL(gt1,3)*ZP(gt3,2))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFdcHm2LP  
 
 
Subroutine CouplingFvFecHm2LP(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,              & 
& Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP(2,2),ZEL(3,3),         & 
& ZER(3,3),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFecHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1n11*Conjg(ZEL(gt2,1))*Conjg(Vv(gt1,4))*ZP(gt3,1))
resL = resL-(Y1n21*Conjg(ZEL(gt2,2))*Conjg(Vv(gt1,4))*ZP(gt3,1))
resL = resL-(Y1n12*Conjg(ZEL(gt2,1))*Conjg(Vv(gt1,5))*ZP(gt3,1))
resL = resL-(Y1n22*Conjg(ZEL(gt2,2))*Conjg(Vv(gt1,5))*ZP(gt3,1))
resL = resL-(Y2n33*Conjg(ZEL(gt2,3))*Conjg(Vv(gt1,6))*ZP(gt3,2))
resR = 0._dp 
resR = resR-(Conjg(Y1e11)*ZER(gt2,1)*Vv(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1e12)*ZER(gt2,2)*Vv(gt1,1)*ZP(gt3,1))
resR = resR-(Conjg(Y1e21)*ZER(gt2,1)*Vv(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y1e22)*ZER(gt2,2)*Vv(gt1,2)*ZP(gt3,1))
resR = resR-(Conjg(Y2e33)*ZER(gt2,3)*Vv(gt1,3)*ZP(gt3,2))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFecHm2LP  
 
 
Subroutine CouplingcFeFehh2LP(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,              & 
& ZH,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFehh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((Y1e11*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e12*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e21*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y1e22*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
resL = resL-((Y2e33*Conjg(ZER(gt1,3))*Conjg(ZEL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
resR = 0._dp 
resR = resR-((Conjg(Y1e11)*ZER(gt2,1)*ZEL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1e12)*ZER(gt2,2)*ZEL(gt1,1)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1e21)*ZER(gt2,1)*ZEL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y1e22)*ZER(gt2,2)*ZEL(gt1,2)*ZH(gt3,1))/sqrt(2._dp))
resR = resR-((Conjg(Y2e33)*ZER(gt2,3)*ZEL(gt1,3)*ZH(gt3,2))/sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFehh2LP  
 
 
Subroutine CouplingcFuFuhh2LP(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,              & 
& ZH,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZUL(gt2,3))*ZH(gt3,2))/sqrt(2._dp)
resR = 0._dp 
resR = resR+(Conjg(Y1u11)*ZUR(gt2,1)*ZUL(gt1,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u12)*ZUR(gt2,2)*ZUL(gt1,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u21)*ZUR(gt2,1)*ZUL(gt1,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1u22)*ZUR(gt2,2)*ZUL(gt1,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2u33)*ZUR(gt2,3)*ZUL(gt1,3)*ZH(gt3,2))/sqrt(2._dp)
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuhh2LP  
 
 
Subroutine CouplingcFdFuHm2LP(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,              & 
& Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,ZUL,ZUR,              & 
& resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,              & 
& Y1u22,Y2u33,ZP(2,2),ZDL(3,3),ZDR(3,3),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFuHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1d11*Conjg(ZDR(gt1,1))*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d12*Conjg(ZDR(gt1,2))*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d13*Conjg(ZDR(gt1,3))*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d21*Conjg(ZDR(gt1,1))*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d22*Conjg(ZDR(gt1,2))*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y1d23*Conjg(ZDR(gt1,3))*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y2d31*Conjg(ZDR(gt1,1))*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
resL = resL-(Y2d32*Conjg(ZDR(gt1,2))*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
resL = resL-(Y2d33*Conjg(ZDR(gt1,3))*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
resR = 0._dp 
resR = resR-(Conjg(Y1u11)*Conjg(ZP(gt3,1))*ZUR(gt2,1)*ZDL(gt1,1))
resR = resR-(Conjg(Y1u12)*Conjg(ZP(gt3,1))*ZUR(gt2,2)*ZDL(gt1,1))
resR = resR-(Conjg(Y1u21)*Conjg(ZP(gt3,1))*ZUR(gt2,1)*ZDL(gt1,2))
resR = resR-(Conjg(Y1u22)*Conjg(ZP(gt3,1))*ZUR(gt2,2)*ZDL(gt1,2))
resR = resR-(Conjg(Y2u33)*Conjg(ZP(gt3,2))*ZUR(gt2,3)*ZDL(gt1,3))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFuHm2LP  
 
 
Subroutine CouplingFvFvhh2LP(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,               & 
& C13,C23,C31,C32,BB11,BB12,BB21,BB22,ZH,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(Y1n11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n11*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,4))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n21*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,4))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n12*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,5))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y1n22*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,5))*ZH(gt3,1))/sqrt(2._dp)
resL = resL+(Y2n33*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,3))*ZH(gt3,2))/sqrt(2._dp)
resL = resL+(Y2n33*Conjg(Vv(gt1,3))*Conjg(Vv(gt2,6))*ZH(gt3,2))/sqrt(2._dp)
resL = resL-((BB11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,4))*ZH(gt3,3))/sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C13*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C31*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB12*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(BB22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,5))*ZH(gt3,3))/sqrt(2._dp)
resL = resL+(C23*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C32*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C13*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C31*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C23*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resL = resL+(C32*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = 0._dp 
resR = resR+(Conjg(Y1n11)*Vv(gt1,4)*Vv(gt2,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,5)*Vv(gt2,1)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,4)*Vv(gt2,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,5)*Vv(gt2,2)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n11)*Vv(gt1,1)*Vv(gt2,4)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n21)*Vv(gt1,2)*Vv(gt2,4)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n12)*Vv(gt1,1)*Vv(gt2,5)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y1n22)*Vv(gt1,2)*Vv(gt2,5)*ZH(gt3,1))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,6)*Vv(gt2,3)*ZH(gt3,2))/sqrt(2._dp)
resR = resR+(Conjg(Y2n33)*Vv(gt1,3)*Vv(gt2,6)*ZH(gt3,2))/sqrt(2._dp)
resR = resR-((Conjg(BB11)*Vv(gt1,4)*Vv(gt2,4)*ZH(gt3,3))/sqrt(2._dp))
resR = resR+(Conjg(BB12)*Vv(gt1,5)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB21)*Vv(gt1,5)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,6)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,6)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB12)*Vv(gt1,4)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB21)*Vv(gt1,4)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(BB22)*Vv(gt1,5)*Vv(gt2,5)*ZH(gt3,3))/sqrt(2._dp)
resR = resR+(Conjg(C23)*Vv(gt1,6)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,6)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C13)*Vv(gt1,4)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C31)*Vv(gt1,4)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C23)*Vv(gt1,5)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
resR = resR+(Conjg(C32)*Vv(gt1,5)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvhh2LP  
 
 
Subroutine CouplingcFeFvHm2LP(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,              & 
& Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP(2,2),ZEL(3,3),         & 
& ZER(3,3),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFvHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(Y1e11*Conjg(ZER(gt1,1))*Conjg(Vv(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1e12*Conjg(ZER(gt1,2))*Conjg(Vv(gt2,1))*Conjg(ZP(gt3,1)))
resL = resL-(Y1e21*Conjg(ZER(gt1,1))*Conjg(Vv(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y1e22*Conjg(ZER(gt1,2))*Conjg(Vv(gt2,2))*Conjg(ZP(gt3,1)))
resL = resL-(Y2e33*Conjg(ZER(gt1,3))*Conjg(Vv(gt2,3))*Conjg(ZP(gt3,2)))
resR = 0._dp 
resR = resR-(Conjg(Y1n11)*Conjg(ZP(gt3,1))*ZEL(gt1,1)*Vv(gt2,4))
resR = resR-(Conjg(Y1n21)*Conjg(ZP(gt3,1))*ZEL(gt1,2)*Vv(gt2,4))
resR = resR-(Conjg(Y1n12)*Conjg(ZP(gt3,1))*ZEL(gt1,1)*Vv(gt2,5))
resR = resR-(Conjg(Y1n22)*Conjg(ZP(gt3,1))*ZEL(gt1,2)*Vv(gt2,5))
resR = resR-(Conjg(Y2n33)*Conjg(ZP(gt3,2))*ZEL(gt1,3)*Vv(gt2,6))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFvHm2LP  
 
 
Subroutine CouplingcFdFdVG2LP(gt1,gt2,g3,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3

Complex(dp), Intent(in) :: ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g3*Conjg(ZDL(gt2,1))*ZDL(gt1,1))
resL = resL-(g3*Conjg(ZDL(gt2,2))*ZDL(gt1,2))
resL = resL-(g3*Conjg(ZDL(gt2,3))*ZDL(gt1,3))
resR = 0._dp 
resR = resR-(g3*Conjg(ZDR(gt1,1))*ZDR(gt2,1))
resR = resR-(g3*Conjg(ZDR(gt1,2))*ZDR(gt2,2))
resR = resR-(g3*Conjg(ZDR(gt1,3))*ZDR(gt2,3))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdVG2LP  
 
 
Subroutine CouplingcFuFuVG2LP(gt1,gt2,g3,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3

Complex(dp), Intent(in) :: ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g3*Conjg(ZUL(gt2,1))*ZUL(gt1,1))
resL = resL-(g3*Conjg(ZUL(gt2,2))*ZUL(gt1,2))
resL = resL-(g3*Conjg(ZUL(gt2,3))*ZUL(gt1,3))
resR = 0._dp 
resR = resR-(g3*Conjg(ZUR(gt1,1))*ZUR(gt2,1))
resR = resR-(g3*Conjg(ZUR(gt1,2))*ZUR(gt2,2))
resR = resR-(g3*Conjg(ZUR(gt1,3))*ZUR(gt2,3))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuVG2LP  
 
 
Subroutine CouplingsFor2LPole4(Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,        & 
& Aa3,Aa4,ZA,ZH,ZP,cplAhAhAhAh,cplAhAhAhhh,cplAhAhhhhh,cplAhAhHmcHm,cplAhhhhhhh,         & 
& cplAhhhHmcHm,cplhhhhhhhh,cplhhhhHmcHm,cplHmHmcHmcHm)

Implicit None 
Real(dp), Intent(in) :: ZA(3,3),ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: cplAhAhAhAh(3,3,3,3),cplAhAhAhhh(3,3,3,3),cplAhAhhhhh(3,3,3,3),cplAhAhHmcHm(3,3,2,2), & 
& cplAhhhhhhh(3,3,3,3),cplAhhhHmcHm(3,3,2,2),cplhhhhhhhh(3,3,3,3),cplhhhhHmcHm(3,3,2,2), & 
& cplHmHmcHmcHm(2,2,2,2)

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'CouplingsFor2LPole4'
 
cplAhAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhAhAh2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,       & 
& Lam3Dash,Aa3,Aa4,ZA,cplAhAhAhAh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhAhhh2LP(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,cplAhAhAhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhhhhh2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,       & 
& Lam3Dash,Aa3,Aa4,ZH,ZA,cplAhAhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingAhAhHmcHm2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,               & 
& Lam3Dash,Aa3,Aa4,ZA,ZP,cplAhAhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhhhhhhh2LP(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,cplAhhhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingAhhhHmcHm2LP(gt1,gt2,gt3,gt4,Lam4,Aa3,Aa4,ZH,ZA,ZP,cplAhhhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplhhhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call Couplinghhhhhhhh2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,       & 
& Lam3Dash,Aa3,Aa4,ZH,cplhhhhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplhhhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplinghhhhHmcHm2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,               & 
& Lam3Dash,Aa3,Aa4,ZH,ZP,cplhhhhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplHmHmcHmcHm = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingHmHmcHmcHm2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,ZP,cplHmHmcHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


Iname = Iname - 1 
End Subroutine CouplingsFor2LPole4

Subroutine CouplingAhAhAhAh2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,          & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhAhAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-6*Lam1*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,1)
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,1))
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,1))
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,1))
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,1))
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,1))
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,1))
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,1))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,1))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,2))
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,2))
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,2))
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,2))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,2))
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,2))
res = res-6*Lam2*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,2)
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,2))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,2))
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,3))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,3))
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))
res = res-6*Lam1Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhAhAh2LP  
 
 
Subroutine CouplingAhAhAhhh2LP(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhAhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhAhhh2LP  
 
 
Subroutine CouplingAhAhhhhh2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,          & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,1)*ZH(gt4,1)
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,1))
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res = res-2*Lam2*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,2)*ZH(gt4,2)
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,2))
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))
res = res-(Aa3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))
res = res-2*Lam1Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhhhhh2LP  
 
 
Subroutine CouplingAhAhHmcHm2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,         & 
& Lam3Dash,Aa3,Aa4,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*Conjg(ZP(gt3,1))*ZA(gt1,1)*ZA(gt2,1)*ZP(gt4,1)
res = res-(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,2)*ZA(gt2,1)*ZP(gt4,1))/2._dp
res = res-(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,1)*ZA(gt2,2)*ZP(gt4,1))/2._dp
res = res-(Lam3*Conjg(ZP(gt3,1))*ZA(gt1,2)*ZA(gt2,2)*ZP(gt4,1))
res = res-(Lam2Dash*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1))
res = res+Conjg(Aa3)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1)
res = res+Conjg(Aa4)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1)
res = res-(Lam3*Conjg(ZP(gt3,2))*ZA(gt1,1)*ZA(gt2,1)*ZP(gt4,2))
res = res-(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,2)*ZA(gt2,1)*ZP(gt4,2))/2._dp
res = res-(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,1)*ZA(gt2,2)*ZP(gt4,2))/2._dp
res = res-2*Lam2*Conjg(ZP(gt3,2))*ZA(gt1,2)*ZA(gt2,2)*ZP(gt4,2)
res = res+Aa3*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2)
res = res+Aa4*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2)
res = res-(Lam3Dash*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhHmcHm2LP  
 
 
Subroutine CouplingAhhhhhhh2LP(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res+(Aa3*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Aa4*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhhhhh2LP  
 
 
Subroutine CouplingAhhhHmcHm2LP(gt1,gt2,gt3,gt4,Lam4,Aa3,Aa4,ZH,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam4,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,2)*ZH(gt2,1)*ZP(gt4,1))/2._dp
res = res-(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,1)*ZH(gt2,2)*ZP(gt4,1))/2._dp
res = res-(Conjg(Aa3)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res = res+Conjg(Aa4)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,1)
res = res-(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,2)*ZH(gt2,1)*ZP(gt4,2))/2._dp
res = res+(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,1)*ZH(gt2,2)*ZP(gt4,2))/2._dp
res = res+Aa3*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,2)
res = res-(Aa4*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhHmcHm2LP  
 
 
Subroutine Couplinghhhhhhhh2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,          & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'Couplinghhhhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-6*Lam1*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,1)
res = res-(Lam3*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res = res-(Lam4*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res = res-(Lam2Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,1))
res = res-(Lam3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,1))
res = res-(Lam4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,1))
res = res-(Lam3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,1))
res = res-(Lam4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,1))
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res = res-(Lam2Dash*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,1))
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Lam2Dash*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))
res = res-(Aa3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Aa4*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res = res-(Lam3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,2))
res = res-(Lam4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,2))
res = res-(Lam3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,2))
res = res-(Lam4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,2))
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res = res-(Lam3*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res = res-(Lam4*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res = res-6*Lam2*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,2)
res = res-(Lam3Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,2))
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Lam3Dash*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,2))
res = res-(Aa3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Aa4*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res = res-(Lam3Dash*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))
res = res-(Lam2Dash*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,3))
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Lam2Dash*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))
res = res-(Aa3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Lam3Dash*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,3))
res = res-(Aa3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res = res-(Lam3Dash*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))
res = res-(Lam2Dash*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))
res = res-(Aa3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Aa4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res = res-(Lam3Dash*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))
res = res-6*Lam1Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,3)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine Couplinghhhhhhhh2LP  
 
 
Subroutine CouplinghhhhHmcHm2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,         & 
& Lam3Dash,Aa3,Aa4,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-2*Lam1*Conjg(ZP(gt3,1))*ZH(gt1,1)*ZH(gt2,1)*ZP(gt4,1)
res = res-(Lam4*Conjg(ZP(gt3,2))*ZH(gt1,2)*ZH(gt2,1)*ZP(gt4,1))/2._dp
res = res-(Lam4*Conjg(ZP(gt3,2))*ZH(gt1,1)*ZH(gt2,2)*ZP(gt4,1))/2._dp
res = res-(Lam3*Conjg(ZP(gt3,1))*ZH(gt1,2)*ZH(gt2,2)*ZP(gt4,1))
res = res-(Lam2Dash*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res = res-(Conjg(Aa3)*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res = res-(Conjg(Aa4)*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res = res-(Lam3*Conjg(ZP(gt3,2))*ZH(gt1,1)*ZH(gt2,1)*ZP(gt4,2))
res = res-(Lam4*Conjg(ZP(gt3,1))*ZH(gt1,2)*ZH(gt2,1)*ZP(gt4,2))/2._dp
res = res-(Lam4*Conjg(ZP(gt3,1))*ZH(gt1,1)*ZH(gt2,2)*ZP(gt4,2))/2._dp
res = res-2*Lam2*Conjg(ZP(gt3,2))*ZH(gt1,2)*ZH(gt2,2)*ZP(gt4,2)
res = res-(Aa3*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
res = res-(Aa4*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
res = res-(Lam3Dash*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhHmcHm2LP  
 
 
Subroutine CouplingHmHmcHmcHm2LP(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmHmcHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-4*Lam1*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,1)
res = res-(Lam3*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,2)*ZP(gt4,1))
res = res-(Lam4*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,2)*ZP(gt4,1))
res = res-(Lam3*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,1))
res = res-(Lam4*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,1))
res = res-(Lam3*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,2))
res = res-(Lam4*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,2))
res = res-(Lam3*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,1)*ZP(gt4,2))
res = res-(Lam4*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,1)*ZP(gt4,2))
res = res-4*Lam2*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,2)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmHmcHmcHm2LP  
 
 
Subroutine CouplingsForLoopMasses(Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,ZH,g1,g2,ZZ,Y1d11,Y1d12,Y1d13,Y1d21,              & 
& Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL,ZDR,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL,               & 
& ZER,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL,ZUR,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,               & 
& C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv,ZP,g3,cplAhAhUhh,cplAhUhhhh,cplAhUhhVP,         & 
& cplAhUhhVZ,cplcFdFdUhhL,cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,           & 
& cplcFuFuUhhR,cplFvFvUhhL,cplFvFvUhhR,cplcgWmgWmUhh,cplcgWpgWpUhh,cplcgZgZUhh,          & 
& cplUhhhhhh,cplUhhHmcHm,cplUhhHmcVWm,cplUhhVPVZ,cplUhhcVWmVWm,cplUhhVZVZ,               & 
& cplAhAhUhhUhh,cplUhhUhhhhhh,cplUhhUhhHmcHm,cplUhhUhhVPVP,cplUhhUhhcVWmVWm,             & 
& cplUhhUhhVZVZ,cplUAhAhAh,cplUAhAhhh,cplcFdFdUAhL,cplcFdFdUAhR,cplcFeFeUAhL,            & 
& cplcFeFeUAhR,cplcFuFuUAhL,cplcFuFuUAhR,cplFvFvUAhL,cplFvFvUAhR,cplcgWmgWmUAh,          & 
& cplcgWpgWpUAh,cplUAhhhhh,cplUAhhhVP,cplUAhhhVZ,cplUAhHmcHm,cplUAhHmcVWm,               & 
& cplUAhUAhAhAh,cplUAhUAhhhhh,cplUAhUAhHmcHm,cplUAhUAhVPVP,cplUAhUAhcVWmVWm,             & 
& cplUAhUAhVZVZ,cplAhHmcUHm,cplAhcUHmVWm,cplcFuFdcUHmL,cplcFuFdcUHmR,cplFvFecUHmL,       & 
& cplFvFecUHmR,cplcgZgWmcUHm,cplcgWmgZUHm,cplcgWpgZcUHm,cplcgZgWpUHm,cplhhHmcUHm,        & 
& cplhhcUHmVWm,cplHmcUHmVP,cplHmcUHmVZ,cplcUHmVPVWm,cplcUHmVWmVZ,cplAhAhUHmcUHm,         & 
& cplhhhhUHmcUHm,cplUHmHmcUHmcHm,cplUHmcUHmVPVP,cplUHmcUHmcVWmVWm,cplUHmcUHmVZVZ,        & 
& cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,cplcUFdFdVGR,         & 
& cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFuHmL,cplcUFdFuHmR,         & 
& cplcUFdFuVWmL,cplcUFdFuVWmR,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdcHmL,cplcUFuFdcHmR,     & 
& cplcUFuFdcVWmL,cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,     & 
& cplcUFuFuVPL,cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,cplcUFeFeAhL,cplcUFeFeAhR,         & 
& cplcUFeFehhL,cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,         & 
& cplcUFeFvHmL,cplcUFeFvHmR,cplcUFeFvVWmL,cplcUFeFvVWmR,cplUFvFvAhL,cplUFvFvAhR,         & 
& cplUFvFecHmL,cplUFvFecHmR,cplUFvFecVWmL,cplUFvFecVWmR,cplUFvFvhhL,cplUFvFvhhR,         & 
& cplUFvFvVPL,cplUFvFvVPR,cplUFvFvVZL,cplUFvFvVZR,cplcFeUFvHmL,cplcFeUFvHmR,             & 
& cplcFeUFvVWmL,cplcFeUFvVWmR,cplcFdFdVGL,cplcFdFdVGR,cplcFuFuVGL,cplcFuFuVGR,           & 
& cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3,cplAhhhVP,cplcFdFdVPL,     & 
& cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,cplcFuFuVPL,cplcFuFuVPR,cplFvFvVPL,cplFvFvVPR,     & 
& cplcgWmgWmVP,cplcgWpgWpVP,cplhhVPVZ,cplHmcHmVP,cplHmcVWmVP,cplcVWmVPVWm,               & 
& cplAhAhVPVP,cplhhhhVPVP,cplHmcHmVPVP,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,  & 
& cplAhhhVZ,cplcFdFdVZL,cplcFdFdVZR,cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,     & 
& cplFvFvVZL,cplFvFvVZR,cplcgWmgWmVZ,cplcgWpgWpVZ,cplhhVZVZ,cplHmcHmVZ,cplHmcVWmVZ,      & 
& cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,cplHmcHmVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,     & 
& cplcVWmVWmVZVZ3,cplAhHmcVWm,cplcFuFdcVWmL,cplcFuFdcVWmR,cplFvFecVWmL,cplFvFecVWmR,     & 
& cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,cplhhHmcVWm,cplhhcVWmVWm,      & 
& cplAhAhcVWmVWm,cplhhhhcVWmVWm,cplHmcHmcVWmVWm,cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,   & 
& cplcVWmcVWmVWmVWm3,cplcHmVWmVZ,cplcHmVPVWm,cplAhAhVPVZ,cplhhhhVPVZ,cplHmcHmVPVZ,       & 
& cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3)

Implicit None 
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3),ZH(3,3),g1,g2,g3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4,ZZ(2,2),               & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3),               & 
& Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3),Y1u11,Y1u12,Y1u21,Y1u22,               & 
& Y2u33,ZUL(3,3),ZUR(3,3),Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Vv(6,6),ZP(2,2)

Complex(dp), Intent(out) :: cplAhAhUhh(3,3,3),cplAhUhhhh(3,3,3),cplAhUhhVP(3,3),cplAhUhhVZ(3,3),cplcFdFdUhhL(3,3,3),& 
& cplcFdFdUhhR(3,3,3),cplcFeFeUhhL(3,3,3),cplcFeFeUhhR(3,3,3),cplcFuFuUhhL(3,3,3),       & 
& cplcFuFuUhhR(3,3,3),cplFvFvUhhL(6,6,3),cplFvFvUhhR(6,6,3),cplcgWmgWmUhh(3),            & 
& cplcgWpgWpUhh(3),cplcgZgZUhh(3),cplUhhhhhh(3,3,3),cplUhhHmcHm(3,2,2),cplUhhHmcVWm(3,2),& 
& cplUhhVPVZ(3),cplUhhcVWmVWm(3),cplUhhVZVZ(3),cplAhAhUhhUhh(3,3,3,3),cplUhhUhhhhhh(3,3,3,3),& 
& cplUhhUhhHmcHm(3,3,2,2),cplUhhUhhVPVP(3,3),cplUhhUhhcVWmVWm(3,3),cplUhhUhhVZVZ(3,3),   & 
& cplUAhAhAh(3,3,3),cplUAhAhhh(3,3,3),cplcFdFdUAhL(3,3,3),cplcFdFdUAhR(3,3,3),           & 
& cplcFeFeUAhL(3,3,3),cplcFeFeUAhR(3,3,3),cplcFuFuUAhL(3,3,3),cplcFuFuUAhR(3,3,3),       & 
& cplFvFvUAhL(6,6,3),cplFvFvUAhR(6,6,3),cplcgWmgWmUAh(3),cplcgWpgWpUAh(3),               & 
& cplUAhhhhh(3,3,3),cplUAhhhVP(3,3),cplUAhhhVZ(3,3),cplUAhHmcHm(3,2,2),cplUAhHmcVWm(3,2),& 
& cplUAhUAhAhAh(3,3,3,3),cplUAhUAhhhhh(3,3,3,3),cplUAhUAhHmcHm(3,3,2,2),cplUAhUAhVPVP(3,3),& 
& cplUAhUAhcVWmVWm(3,3),cplUAhUAhVZVZ(3,3),cplAhHmcUHm(3,2,2),cplAhcUHmVWm(3,2),         & 
& cplcFuFdcUHmL(3,3,2),cplcFuFdcUHmR(3,3,2),cplFvFecUHmL(6,3,2),cplFvFecUHmR(6,3,2),     & 
& cplcgZgWmcUHm(2),cplcgWmgZUHm(2),cplcgWpgZcUHm(2),cplcgZgWpUHm(2),cplhhHmcUHm(3,2,2),  & 
& cplhhcUHmVWm(3,2),cplHmcUHmVP(2,2),cplHmcUHmVZ(2,2),cplcUHmVPVWm(2),cplcUHmVWmVZ(2),   & 
& cplAhAhUHmcUHm(3,3,2,2),cplhhhhUHmcUHm(3,3,2,2),cplUHmHmcUHmcHm(2,2,2,2),              & 
& cplUHmcUHmVPVP(2,2),cplUHmcUHmcVWmVWm(2,2),cplUHmcUHmVZVZ(2,2),cplcUFdFdAhL(3,3,3),    & 
& cplcUFdFdAhR(3,3,3),cplcUFdFdhhL(3,3,3),cplcUFdFdhhR(3,3,3),cplcUFdFdVGL(3,3),         & 
& cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),cplcUFdFdVZL(3,3),               & 
& cplcUFdFdVZR(3,3),cplcUFdFuHmL(3,3,2),cplcUFdFuHmR(3,3,2),cplcUFdFuVWmL(3,3),          & 
& cplcUFdFuVWmR(3,3),cplcUFuFuAhL(3,3,3),cplcUFuFuAhR(3,3,3),cplcUFuFdcHmL(3,3,2),       & 
& cplcUFuFdcHmR(3,3,2),cplcUFuFdcVWmL(3,3),cplcUFuFdcVWmR(3,3),cplcUFuFuhhL(3,3,3),      & 
& cplcUFuFuhhR(3,3,3),cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),cplcUFuFuVPL(3,3),             & 
& cplcUFuFuVPR(3,3),cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3),cplcUFeFeAhL(3,3,3),             & 
& cplcUFeFeAhR(3,3,3),cplcUFeFehhL(3,3,3),cplcUFeFehhR(3,3,3),cplcUFeFeVPL(3,3),         & 
& cplcUFeFeVPR(3,3),cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),cplcUFeFvHmL(3,6,2),             & 
& cplcUFeFvHmR(3,6,2),cplcUFeFvVWmL(3,6),cplcUFeFvVWmR(3,6),cplUFvFvAhL(6,6,3),          & 
& cplUFvFvAhR(6,6,3),cplUFvFecHmL(6,3,2),cplUFvFecHmR(6,3,2),cplUFvFecVWmL(6,3),         & 
& cplUFvFecVWmR(6,3),cplUFvFvhhL(6,6,3),cplUFvFvhhR(6,6,3),cplUFvFvVPL(6,6),             & 
& cplUFvFvVPR(6,6),cplUFvFvVZL(6,6),cplUFvFvVZR(6,6),cplcFeUFvHmL(3,6,2),cplcFeUFvHmR(3,6,2),& 
& cplcFeUFvVWmL(3,6),cplcFeUFvVWmR(3,6),cplcFdFdVGL(3,3),cplcFdFdVGR(3,3),               & 
& cplcFuFuVGL(3,3),cplcFuFuVGR(3,3),cplcgGgGVG,cplVGVGVG,cplVGVGVGVG1,cplVGVGVGVG2,      & 
& cplVGVGVGVG3,cplAhhhVP(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFeFeVPL(3,3),        & 
& cplcFeFeVPR(3,3),cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplFvFvVPL(6,6),cplFvFvVPR(6,6),    & 
& cplcgWmgWmVP,cplcgWpgWpVP,cplhhVPVZ(3),cplHmcHmVP(2,2),cplHmcVWmVP(2),cplcVWmVPVWm,    & 
& cplAhAhVPVP(3,3),cplhhhhVPVP(3,3),cplHmcHmVPVP(2,2),cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,   & 
& cplcVWmVPVPVWm3,cplAhhhVZ(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVZL(3,3)

Complex(dp), Intent(out) :: cplcFeFeVZR(3,3),cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplFvFvVZL(6,6),cplFvFvVZR(6,6),    & 
& cplcgWmgWmVZ,cplcgWpgWpVZ,cplhhVZVZ(3),cplHmcHmVZ(2,2),cplHmcVWmVZ(2),cplcVWmVWmVZ,    & 
& cplAhAhVZVZ(3,3),cplhhhhVZVZ(3,3),cplHmcHmVZVZ(2,2),cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,   & 
& cplcVWmVWmVZVZ3,cplAhHmcVWm(3,2),cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplFvFecVWmL(6,3),& 
& cplFvFecVWmR(6,3),cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,             & 
& cplhhHmcVWm(3,2),cplhhcVWmVWm(3),cplAhAhcVWmVWm(3,3),cplhhhhcVWmVWm(3,3),              & 
& cplHmcHmcVWmVWm(2,2),cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,         & 
& cplcHmVWmVZ(2),cplcHmVPVWm(2),cplAhAhVPVZ(3,3),cplhhhhVPVZ(3,3),cplHmcHmVPVZ(2,2),     & 
& cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'CouplingsForLoopMasses'
 
cplAhAhUhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhUhhL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,cplAhAhUhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhUhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhUhhhhL(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhUhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhUhhVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhUhhVPL(gt1,gt2,g1,g2,ZZ,ZA,cplAhUhhVP(gt1,gt2))

 End Do 
End Do 


cplAhUhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhUhhVZL(gt1,gt2,g1,g2,ZZ,ZA,cplAhUhhVZ(gt1,gt2))

 End Do 
End Do 


cplcFdFdUhhL = 0._dp 
cplcFdFdUhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdUhhL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,ZDL,ZDR,cplcFdFdUhhL(gt1,gt2,gt3),cplcFdFdUhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFeUhhL = 0._dp 
cplcFeFeUhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFeUhhL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL,ZER,             & 
& cplcFeFeUhhL(gt1,gt2,gt3),cplcFeFeUhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuUhhL = 0._dp 
cplcFuFuUhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuUhhL(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL,ZUR,             & 
& cplcFuFuUhhL(gt1,gt2,gt3),cplcFuFuUhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFvUhhL = 0._dp 
cplFvFvUhhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingFvFvUhhL(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,              & 
& C31,C32,BB11,BB12,BB21,BB22,Vv,cplFvFvUhhL(gt1,gt2,gt3),cplFvFvUhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcgWmgWmUhh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgWmgWmUhhL(gt3,g2,v1,v2,cplcgWmgWmUhh(gt3))

End Do 


cplcgWpgWpUhh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgWpgWpUhhL(gt3,g2,v1,v2,cplcgWpgWpUhh(gt3))

End Do 


cplcgZgZUhh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgZgZUhhL(gt3,g1,g2,v1,v2,ZZ,cplcgZgZUhh(gt3))

End Do 


cplUhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingUhhhhhhL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,cplUhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplUhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingUhhHmcHmL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,             & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZP,cplUhhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplUhhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingUhhHmcVWmL(gt1,gt2,g2,ZP,cplUhhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplUhhVPVZ = 0._dp 
Do gt1 = 1, 3
Call CouplingUhhVPVZL(gt1,g1,g2,v1,v2,ZZ,cplUhhVPVZ(gt1))

End Do 


cplUhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
Call CouplingUhhcVWmVWmL(gt1,g2,v1,v2,cplUhhcVWmVWm(gt1))

End Do 


cplUhhVZVZ = 0._dp 
Do gt1 = 1, 3
Call CouplingUhhVZVZL(gt1,g1,g2,v1,v2,ZZ,cplUhhVZVZ(gt1))

End Do 


cplAhAhUhhUhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhUhhUhhL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,       & 
& Lam3Dash,Aa3,Aa4,ZA,cplAhAhUhhUhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplUhhUhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingUhhUhhhhhhL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,       & 
& Lam3Dash,Aa3,Aa4,ZH,cplUhhUhhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplUhhUhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingUhhUhhHmcHmL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,               & 
& Lam3Dash,Aa3,Aa4,ZP,cplUhhUhhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplUhhUhhVPVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingUhhUhhVPVPL(gt1,gt2,g1,g2,ZZ,cplUhhUhhVPVP(gt1,gt2))

 End Do 
End Do 


cplUhhUhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingUhhUhhcVWmVWmL(gt1,gt2,g2,cplUhhUhhcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplUhhUhhVZVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingUhhUhhVZVZL(gt1,gt2,g1,g2,ZZ,cplUhhUhhVZVZ(gt1,gt2))

 End Do 
End Do 


cplUAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingUAhAhAhL(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,cplUAhAhAh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplUAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingUAhAhhhL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplUAhAhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFdFdUAhL = 0._dp 
cplcFdFdUAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdUAhL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,ZDL,ZDR,cplcFdFdUAhL(gt1,gt2,gt3),cplcFdFdUAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFeUAhL = 0._dp 
cplcFeFeUAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFeUAhL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL,ZER,             & 
& cplcFeFeUAhL(gt1,gt2,gt3),cplcFeFeUAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuUAhL = 0._dp 
cplcFuFuUAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuUAhL(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL,ZUR,             & 
& cplcFuFuUAhL(gt1,gt2,gt3),cplcFuFuUAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFvUAhL = 0._dp 
cplFvFvUAhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingFvFvUAhL(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,              & 
& C31,C32,BB11,BB12,BB21,BB22,Vv,cplFvFvUAhL(gt1,gt2,gt3),cplFvFvUAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcgWmgWmUAh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgWmgWmUAhL(gt3,g2,v1,v2,cplcgWmgWmUAh(gt3))

End Do 


cplcgWpgWpUAh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgWpgWpUAhL(gt3,g2,v1,v2,cplcgWpgWpUAh(gt3))

End Do 


cplUAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingUAhhhhhL(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,cplUAhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplUAhhhVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingUAhhhVPL(gt1,gt2,g1,g2,ZZ,ZH,cplUAhhhVP(gt1,gt2))

 End Do 
End Do 


cplUAhhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingUAhhhVZL(gt1,gt2,g1,g2,ZZ,ZH,cplUAhhhVZ(gt1,gt2))

 End Do 
End Do 


cplUAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingUAhHmcHmL(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZP,cplUAhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplUAhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingUAhHmcVWmL(gt1,gt2,g2,ZP,cplUAhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplUAhUAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingUAhUAhAhAhL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,       & 
& Lam3Dash,Aa3,Aa4,ZA,cplUAhUAhAhAh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplUAhUAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingUAhUAhhhhhL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,       & 
& Lam3Dash,Aa3,Aa4,ZH,cplUAhUAhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplUAhUAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingUAhUAhHmcHmL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,               & 
& Lam3Dash,Aa3,Aa4,ZP,cplUAhUAhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplUAhUAhVPVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingUAhUAhVPVPL(gt1,gt2,g1,g2,ZZ,cplUAhUAhVPVP(gt1,gt2))

 End Do 
End Do 


cplUAhUAhcVWmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingUAhUAhcVWmVWmL(gt1,gt2,g2,cplUAhUAhcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplUAhUAhVZVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingUAhUAhVZVZL(gt1,gt2,g1,g2,ZZ,cplUAhUAhVZVZ(gt1,gt2))

 End Do 
End Do 


cplAhHmcUHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingAhHmcUHmL(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,ZP,               & 
& cplAhHmcUHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhcUHmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhcUHmVWmL(gt1,gt2,g2,ZA,cplAhcUHmVWm(gt1,gt2))

 End Do 
End Do 


cplcFuFdcUHmL = 0._dp 
cplcFuFdcUHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcFuFdcUHmL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,              & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZDL,ZDR,ZUL,ZUR,cplcFuFdcUHmL(gt1,gt2,gt3)& 
& ,cplcFuFdcUHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFecUHmL = 0._dp 
cplFvFecUHmR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingFvFecUHmL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,               & 
& Y1n12,Y1n21,Y1n22,Y2n33,ZEL,ZER,Vv,cplFvFecUHmL(gt1,gt2,gt3),cplFvFecUHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcgZgWmcUHm = 0._dp 
Do gt3 = 1, 2
Call CouplingcgZgWmcUHmL(gt3,g1,g2,v1,v2,ZZ,cplcgZgWmcUHm(gt3))

End Do 


cplcgWmgZUHm = 0._dp 
Do gt3 = 1, 2
Call CouplingcgWmgZUHmL(gt3,g1,g2,v1,v2,ZZ,cplcgWmgZUHm(gt3))

End Do 


cplcgWpgZcUHm = 0._dp 
Do gt3 = 1, 2
Call CouplingcgWpgZcUHmL(gt3,g1,g2,v1,v2,ZZ,cplcgWpgZcUHm(gt3))

End Do 


cplcgZgWpUHm = 0._dp 
Do gt3 = 1, 2
Call CouplingcgZgWpUHmL(gt3,g1,g2,v1,v2,ZZ,cplcgZgWpUHm(gt3))

End Do 


cplhhHmcUHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhHmcUHmL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,             & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZP,cplhhHmcUHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhcUHmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhcUHmVWmL(gt1,gt2,g2,ZH,cplhhcUHmVWm(gt1,gt2))

 End Do 
End Do 


cplHmcUHmVP = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcUHmVPL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcUHmVP(gt1,gt2))

 End Do 
End Do 


cplHmcUHmVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcUHmVZL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcUHmVZ(gt1,gt2))

 End Do 
End Do 


cplcUHmVPVWm = 0._dp 
Do gt1 = 1, 2
Call CouplingcUHmVPVWmL(gt1,g1,g2,v1,v2,ZZ,cplcUHmVPVWm(gt1))

End Do 


cplcUHmVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingcUHmVWmVZL(gt1,g1,g2,v1,v2,ZZ,cplcUHmVWmVZ(gt1))

End Do 


cplAhAhUHmcUHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingAhAhUHmcUHmL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,               & 
& Lam3Dash,Aa3,Aa4,ZA,cplAhAhUHmcUHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplhhhhUHmcUHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplinghhhhUHmcUHmL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,               & 
& Lam3Dash,Aa3,Aa4,ZH,cplhhhhUHmcUHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplUHmHmcUHmcHm = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingUHmHmcUHmcHmL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,ZP,cplUHmHmcUHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplUHmcUHmVPVP = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingUHmcUHmVPVPL(gt1,gt2,g1,g2,ZZ,cplUHmcUHmVPVP(gt1,gt2))

 End Do 
End Do 


cplUHmcUHmcVWmVWm = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingUHmcUHmcVWmVWmL(gt1,gt2,g2,cplUHmcUHmcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplUHmcUHmVZVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingUHmcUHmVZVZL(gt1,gt2,g1,g2,ZZ,cplUHmcUHmVZVZ(gt1,gt2))

 End Do 
End Do 


cplcUFdFdAhL = 0._dp 
cplcUFdFdAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFdFdAhL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,ZA,ZDL,ZDR,cplcUFdFdAhL(gt1,gt2,gt3),cplcUFdFdAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFdFdhhL = 0._dp 
cplcUFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFdFdhhL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,ZH,ZDL,ZDR,cplcUFdFdhhL(gt1,gt2,gt3),cplcUFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFdFdVGL = 0._dp 
cplcUFdFdVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFdFdVGL(gt1,gt2,g3,ZDL,ZDR,cplcUFdFdVGL(gt1,gt2),cplcUFdFdVGR(gt1,gt2))

 End Do 
End Do 


cplcUFdFdVPL = 0._dp 
cplcUFdFdVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFdFdVPL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcUFdFdVPL(gt1,gt2),cplcUFdFdVPR(gt1,gt2))

 End Do 
End Do 


cplcUFdFdVZL = 0._dp 
cplcUFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFdFdVZL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcUFdFdVZL(gt1,gt2),cplcUFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcUFdFuHmL = 0._dp 
cplcUFdFuHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcUFdFuHmL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZUL,ZUR,cplcUFdFuHmL(gt1,gt2,gt3)   & 
& ,cplcUFdFuHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFdFuVWmL = 0._dp 
cplcUFdFuVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFdFuVWmL(gt1,gt2,g2,ZUL,cplcUFdFuVWmL(gt1,gt2),cplcUFdFuVWmR(gt1,gt2))

 End Do 
End Do 


cplcUFuFuAhL = 0._dp 
cplcUFuFuAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFuFuAhL(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZA,ZUL,              & 
& ZUR,cplcUFuFuAhL(gt1,gt2,gt3),cplcUFuFuAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFuFdcHmL = 0._dp 
cplcUFuFdcHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcUFuFdcHmL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,              & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,cplcUFuFdcHmL(gt1,gt2,gt3)  & 
& ,cplcUFuFdcHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFuFdcVWmL = 0._dp 
cplcUFuFdcVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFuFdcVWmL(gt1,gt2,g2,ZDL,cplcUFuFdcVWmL(gt1,gt2),cplcUFuFdcVWmR(gt1,gt2))

 End Do 
End Do 


cplcUFuFuhhL = 0._dp 
cplcUFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFuFuhhL(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZH,ZUL,              & 
& ZUR,cplcUFuFuhhL(gt1,gt2,gt3),cplcUFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFuFuVGL = 0._dp 
cplcUFuFuVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFuFuVGL(gt1,gt2,g3,ZUL,ZUR,cplcUFuFuVGL(gt1,gt2),cplcUFuFuVGR(gt1,gt2))

 End Do 
End Do 


cplcUFuFuVPL = 0._dp 
cplcUFuFuVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFuFuVPL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcUFuFuVPL(gt1,gt2),cplcUFuFuVPR(gt1,gt2))

 End Do 
End Do 


cplcUFuFuVZL = 0._dp 
cplcUFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFuFuVZL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcUFuFuVZL(gt1,gt2),cplcUFuFuVZR(gt1,gt2))

 End Do 
End Do 


cplcUFeFeAhL = 0._dp 
cplcUFeFeAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFeFeAhL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZA,ZEL,              & 
& ZER,cplcUFeFeAhL(gt1,gt2,gt3),cplcUFeFeAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFeFehhL = 0._dp 
cplcUFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFeFehhL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZH,ZEL,              & 
& ZER,cplcUFeFehhL(gt1,gt2,gt3),cplcUFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFeFeVPL = 0._dp 
cplcUFeFeVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFeFeVPL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcUFeFeVPL(gt1,gt2),cplcUFeFeVPR(gt1,gt2))

 End Do 
End Do 


cplcUFeFeVZL = 0._dp 
cplcUFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFeFeVZL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcUFeFeVZL(gt1,gt2),cplcUFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcUFeFvHmL = 0._dp 
cplcUFeFvHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
  Do gt3 = 1, 2
Call CouplingcUFeFvHmL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,               & 
& Y1n12,Y1n21,Y1n22,Y2n33,ZP,Vv,cplcUFeFvHmL(gt1,gt2,gt3),cplcUFeFvHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFeFvVWmL = 0._dp 
cplcUFeFvVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
Call CouplingcUFeFvVWmL(gt1,gt2,g2,Vv,cplcUFeFvVWmL(gt1,gt2),cplcUFeFvVWmR(gt1,gt2))

 End Do 
End Do 


cplUFvFvAhL = 0._dp 
cplUFvFvAhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingUFvFvAhL(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,              & 
& C31,C32,BB11,BB12,BB21,BB22,ZA,Vv,cplUFvFvAhL(gt1,gt2,gt3),cplUFvFvAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplUFvFecHmL = 0._dp 
cplUFvFecHmR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingUFvFecHmL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,               & 
& Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,cplUFvFecHmL(gt1,gt2,gt3),cplUFvFecHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplUFvFecVWmL = 0._dp 
cplUFvFecVWmR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 3
Call CouplingUFvFecVWmL(gt1,gt2,g2,ZEL,cplUFvFecVWmL(gt1,gt2),cplUFvFecVWmR(gt1,gt2))

 End Do 
End Do 


cplUFvFvhhL = 0._dp 
cplUFvFvhhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingUFvFvhhL(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,              & 
& C31,C32,BB11,BB12,BB21,BB22,ZH,Vv,cplUFvFvhhL(gt1,gt2,gt3),cplUFvFvhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplUFvFvVPL = 0._dp 
cplUFvFvVPR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
Call CouplingUFvFvVPL(gt1,gt2,g1,g2,ZZ,Vv,cplUFvFvVPL(gt1,gt2),cplUFvFvVPR(gt1,gt2))

 End Do 
End Do 


cplUFvFvVZL = 0._dp 
cplUFvFvVZR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
Call CouplingUFvFvVZL(gt1,gt2,g1,g2,ZZ,Vv,cplUFvFvVZL(gt1,gt2),cplUFvFvVZR(gt1,gt2))

 End Do 
End Do 


cplcFeUFvHmL = 0._dp 
cplcFeUFvHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
  Do gt3 = 1, 2
Call CouplingcFeUFvHmL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,               & 
& Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,cplcFeUFvHmL(gt1,gt2,gt3),cplcFeUFvHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeUFvVWmL = 0._dp 
cplcFeUFvVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
Call CouplingcFeUFvVWmL(gt1,gt2,g2,ZEL,cplcFeUFvVWmL(gt1,gt2),cplcFeUFvVWmR(gt1,gt2))

 End Do 
End Do 


cplcFdFdVGL = 0._dp 
cplcFdFdVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVGL(gt1,gt2,g3,ZDL,ZDR,cplcFdFdVGL(gt1,gt2),cplcFdFdVGR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVGL = 0._dp 
cplcFuFuVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVGL(gt1,gt2,g3,ZUL,ZUR,cplcFuFuVGL(gt1,gt2),cplcFuFuVGR(gt1,gt2))

 End Do 
End Do 


cplcgGgGVG = 0._dp 
Call CouplingcgGgGVGL(g3,cplcgGgGVG)



cplVGVGVG = 0._dp 
Call CouplingVGVGVGL(g3,cplVGVGVG)



cplVGVGVGVG1 = 0._dp 
cplVGVGVGVG2 = 0._dp 
cplVGVGVGVG3 = 0._dp 
Call CouplingVGVGVGVGL(g3,cplVGVGVGVG1,cplVGVGVGVG2,cplVGVGVGVG3)



cplAhhhVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVPL(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVP(gt1,gt2))

 End Do 
End Do 


cplcFdFdVPL = 0._dp 
cplcFdFdVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVPL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcFdFdVPL(gt1,gt2),cplcFdFdVPR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVPL = 0._dp 
cplcFeFeVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVPL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcFeFeVPL(gt1,gt2),cplcFeFeVPR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVPL = 0._dp 
cplcFuFuVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVPL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcFuFuVPL(gt1,gt2),cplcFuFuVPR(gt1,gt2))

 End Do 
End Do 


cplFvFvVPL = 0._dp 
cplFvFvVPR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
Call CouplingFvFvVPL(gt1,gt2,g1,g2,ZZ,Vv,cplFvFvVPL(gt1,gt2),cplFvFvVPR(gt1,gt2))

 End Do 
End Do 


cplcgWmgWmVP = 0._dp 
Call CouplingcgWmgWmVPL(g2,ZZ,cplcgWmgWmVP)



cplcgWpgWpVP = 0._dp 
Call CouplingcgWpgWpVPL(g2,ZZ,cplcgWpgWpVP)



cplhhVPVZ = 0._dp 
Do gt1 = 1, 3
Call CouplinghhVPVZL(gt1,g1,g2,v1,v2,ZZ,ZH,cplhhVPVZ(gt1))

End Do 


cplHmcHmVP = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVPL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVP(gt1,gt2))

 End Do 
End Do 


cplHmcVWmVP = 0._dp 
Do gt1 = 1, 2
Call CouplingHmcVWmVPL(gt1,g1,g2,v1,v2,ZZ,ZP,cplHmcVWmVP(gt1))

End Do 


cplcVWmVPVWm = 0._dp 
Call CouplingcVWmVPVWmL(g2,ZZ,cplcVWmVPVWm)



cplAhAhVPVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhVPVPL(gt1,gt2,g1,g2,ZZ,ZA,cplAhAhVPVP(gt1,gt2))

 End Do 
End Do 


cplhhhhVPVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhVPVPL(gt1,gt2,g1,g2,ZZ,ZH,cplhhhhVPVP(gt1,gt2))

 End Do 
End Do 


cplHmcHmVPVP = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVPVPL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVPVP(gt1,gt2))

 End Do 
End Do 


cplcVWmVPVPVWm1 = 0._dp 
cplcVWmVPVPVWm2 = 0._dp 
cplcVWmVPVPVWm3 = 0._dp 
Call CouplingcVWmVPVPVWmL(g2,ZZ,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3)



cplAhhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVZL(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVZ(gt1,gt2))

 End Do 
End Do 


cplcFdFdVZL = 0._dp 
cplcFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVZL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcFdFdVZL(gt1,gt2),cplcFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVZL = 0._dp 
cplcFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVZL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcFeFeVZL(gt1,gt2),cplcFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVZL = 0._dp 
cplcFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVZL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcFuFuVZL(gt1,gt2),cplcFuFuVZR(gt1,gt2))

 End Do 
End Do 


cplFvFvVZL = 0._dp 
cplFvFvVZR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
Call CouplingFvFvVZL(gt1,gt2,g1,g2,ZZ,Vv,cplFvFvVZL(gt1,gt2),cplFvFvVZR(gt1,gt2))

 End Do 
End Do 


cplcgWmgWmVZ = 0._dp 
Call CouplingcgWmgWmVZL(g2,ZZ,cplcgWmgWmVZ)



cplcgWpgWpVZ = 0._dp 
Call CouplingcgWpgWpVZL(g2,ZZ,cplcgWpgWpVZ)



cplhhVZVZ = 0._dp 
Do gt1 = 1, 3
Call CouplinghhVZVZL(gt1,g1,g2,v1,v2,ZZ,ZH,cplhhVZVZ(gt1))

End Do 


cplHmcHmVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVZL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVZ(gt1,gt2))

 End Do 
End Do 


cplHmcVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingHmcVWmVZL(gt1,g1,g2,v1,v2,ZZ,ZP,cplHmcVWmVZ(gt1))

End Do 


cplcVWmVWmVZ = 0._dp 
Call CouplingcVWmVWmVZL(g2,ZZ,cplcVWmVWmVZ)



cplAhAhVZVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhVZVZL(gt1,gt2,g1,g2,ZZ,ZA,cplAhAhVZVZ(gt1,gt2))

 End Do 
End Do 


cplhhhhVZVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhVZVZL(gt1,gt2,g1,g2,ZZ,ZH,cplhhhhVZVZ(gt1,gt2))

 End Do 
End Do 


cplHmcHmVZVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVZVZL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVZVZ(gt1,gt2))

 End Do 
End Do 


cplcVWmVWmVZVZ1 = 0._dp 
cplcVWmVWmVZVZ2 = 0._dp 
cplcVWmVWmVZVZ3 = 0._dp 
Call CouplingcVWmVWmVZVZL(g2,ZZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3)



cplAhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhHmcVWmL(gt1,gt2,g2,ZA,ZP,cplAhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplcFuFdcVWmL = 0._dp 
cplcFuFdcVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFdcVWmL(gt1,gt2,g2,ZDL,ZUL,cplcFuFdcVWmL(gt1,gt2),cplcFuFdcVWmR(gt1,gt2))

 End Do 
End Do 


cplFvFecVWmL = 0._dp 
cplFvFecVWmR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 3
Call CouplingFvFecVWmL(gt1,gt2,g2,ZEL,Vv,cplFvFecVWmL(gt1,gt2),cplFvFecVWmR(gt1,gt2))

 End Do 
End Do 


cplcgWpgAcVWm = 0._dp 
Call CouplingcgWpgAcVWmL(g2,ZZ,cplcgWpgAcVWm)



cplcgAgWmcVWm = 0._dp 
Call CouplingcgAgWmcVWmL(g2,ZZ,cplcgAgWmcVWm)



cplcgZgWmcVWm = 0._dp 
Call CouplingcgZgWmcVWmL(g2,ZZ,cplcgZgWmcVWm)



cplcgWpgZcVWm = 0._dp 
Call CouplingcgWpgZcVWmL(g2,ZZ,cplcgWpgZcVWm)



cplhhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhHmcVWmL(gt1,gt2,g2,ZH,ZP,cplhhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
Call CouplinghhcVWmVWmL(gt1,g2,v1,v2,ZH,cplhhcVWmVWm(gt1))

End Do 


cplAhAhcVWmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhcVWmVWmL(gt1,gt2,g2,ZA,cplAhAhcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplhhhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhcVWmVWmL(gt1,gt2,g2,ZH,cplhhhhcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplHmcHmcVWmVWm = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmcVWmVWmL(gt1,gt2,g2,ZP,cplHmcHmcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplcVWmcVWmVWmVWm1 = 0._dp 
cplcVWmcVWmVWmVWm2 = 0._dp 
cplcVWmcVWmVWmVWm3 = 0._dp 
Call CouplingcVWmcVWmVWmVWmL(g2,cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3)



cplcHmVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingcHmVWmVZL(gt1,g1,g2,v1,v2,ZZ,ZP,cplcHmVWmVZ(gt1))

End Do 


cplcHmVPVWm = 0._dp 
Do gt1 = 1, 2
Call CouplingcHmVPVWmL(gt1,g1,g2,v1,v2,ZZ,ZP,cplcHmVPVWm(gt1))

End Do 


cplAhAhVPVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhVPVZL(gt1,gt2,g1,g2,ZZ,ZA,cplAhAhVPVZ(gt1,gt2))

 End Do 
End Do 


cplhhhhVPVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhVPVZL(gt1,gt2,g1,g2,ZZ,ZH,cplhhhhVPVZ(gt1,gt2))

 End Do 
End Do 


cplHmcHmVPVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVPVZL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVPVZ(gt1,gt2))

 End Do 
End Do 


cplcVWmVPVWmVZ1 = 0._dp 
cplcVWmVPVWmVZ2 = 0._dp 
cplcVWmVPVWmVZ3 = 0._dp 
Call CouplingcVWmVPVWmVZL(g2,ZZ,cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3)



Iname = Iname - 1 
End Subroutine CouplingsForLoopMasses

Subroutine CouplingAhAhUhhL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhUhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res-2*Lam1*v1*ZA(gt1,1)*ZA(gt2,1)
End If 
If ((2.eq.gt3)) Then 
res = res-(Lam3*v2*ZA(gt1,1)*ZA(gt2,1))
End If 
If ((2.eq.gt3)) Then 
res = res-(Lam4*v2*ZA(gt1,1)*ZA(gt2,1))
End If 
If ((3.eq.gt3)) Then 
res = res-(Lam2Dash*v3*ZA(gt1,1)*ZA(gt2,1))
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa1*ZA(gt1,2)*ZA(gt2,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa3*v3*ZA(gt1,2)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(Aa1*ZA(gt1,3)*ZA(gt2,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
res = res+(Aa2*ZA(gt1,3)*ZA(gt2,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
res = res-(Aa3*v3*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(Aa4*v3*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa3*v2*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa1*ZA(gt1,1)*ZA(gt2,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa2*ZA(gt1,1)*ZA(gt2,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa3*v3*ZA(gt1,1)*ZA(gt2,2))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa4*v3*ZA(gt1,1)*ZA(gt2,2))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
res = res-(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(Lam3*v1*ZA(gt1,2)*ZA(gt2,2))
End If 
If ((1.eq.gt3)) Then 
res = res-(Lam4*v1*ZA(gt1,2)*ZA(gt2,2))
End If 
If ((2.eq.gt3)) Then 
res = res-2*Lam2*v2*ZA(gt1,2)*ZA(gt2,2)
End If 
If ((3.eq.gt3)) Then 
res = res-(Lam3Dash*v3*ZA(gt1,2)*ZA(gt2,2))
End If 
If ((1.eq.gt3)) Then 
res = res+(Aa1*ZA(gt1,3)*ZA(gt2,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
res = res-(Aa2*ZA(gt1,3)*ZA(gt2,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
res = res+(Aa3*v3*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(Aa4*v3*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
res = res-(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
res = res+(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res+(Aa3*v1*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa4*v1*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(Aa1*ZA(gt1,1)*ZA(gt2,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
res = res+(Aa2*ZA(gt1,1)*ZA(gt2,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
res = res-(Aa3*v3*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(Aa4*v3*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa3*v2*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res+(Aa4*v2*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(v2*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res+(v2*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(Aa1*ZA(gt1,2)*ZA(gt2,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
res = res-(Aa2*ZA(gt1,2)*ZA(gt2,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
res = res+(Aa3*v3*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(Aa4*v3*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
res = res+(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res+(Aa3*v1*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(Aa4*v1*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res+(v1*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-(v1*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(Lam2Dash*v1*ZA(gt1,3)*ZA(gt2,3))
End If 
If ((1.eq.gt3)) Then 
res = res+(Aa3*v2*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(Aa4*v2*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(Aa3*v1*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(Aa4*v1*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(Lam3Dash*v2*ZA(gt1,3)*ZA(gt2,3))
End If 
If ((2.eq.gt3)) Then 
res = res+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((3.eq.gt3)) Then 
res = res-2*Lam1Dash*v3*ZA(gt1,3)*ZA(gt2,3)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhUhhL  
 
 
Subroutine CouplingAhUhhhhL(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhUhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((3.eq.gt2)) Then 
res = res+(Aa1*ZA(gt1,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt2)) Then 
res = res+(Aa2*ZA(gt1,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt2)) Then 
res = res+(Aa3*v3*ZA(gt1,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res+(Aa4*v3*ZA(gt1,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt2)) Then 
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt2)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res+(Aa1*ZA(gt1,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt2)) Then 
res = res-(Aa2*ZA(gt1,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt2)) Then 
res = res+(Aa3*v3*ZA(gt1,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(Aa4*v3*ZA(gt1,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt2)) Then 
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt2)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res+(Aa3*v2*ZA(gt1,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(Aa4*v2*ZA(gt1,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(Aa1*ZA(gt1,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt2)) Then 
res = res-(Aa2*ZA(gt1,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt2)) Then 
res = res-(Aa3*v3*ZA(gt1,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(Aa4*v3*ZA(gt1,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res+(Conjg(Aa1)*ZA(gt1,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt2)) Then 
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt2)) Then 
res = res+(v3*Conjg(Aa3)*ZA(gt1,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res+(Aa1*ZA(gt1,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt2)) Then 
res = res-(Aa2*ZA(gt1,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt2)) Then 
res = res+(Aa3*v3*ZA(gt1,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res-(Aa4*v3*ZA(gt1,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res-(Conjg(Aa1)*ZA(gt1,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt2)) Then 
res = res+(Conjg(Aa2)*ZA(gt1,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt2)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt1,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt1,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res+(Aa3*v1*ZA(gt1,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(Aa4*v1*ZA(gt1,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(v1*Conjg(Aa3)*ZA(gt1,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZH(gt3,2))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(Aa1*ZA(gt1,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt2)) Then 
res = res-(Aa2*ZA(gt1,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt2)) Then 
res = res-(Aa3*v3*ZA(gt1,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(Aa4*v3*ZA(gt1,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res+(Conjg(Aa1)*ZA(gt1,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt2)) Then 
res = res+(Conjg(Aa2)*ZA(gt1,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt2)) Then 
res = res+(v3*Conjg(Aa3)*ZA(gt1,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt1,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(Aa3*v2*ZA(gt1,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(Aa4*v2*ZA(gt1,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res+(v2*Conjg(Aa3)*ZA(gt1,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res+(v2*Conjg(Aa4)*ZA(gt1,1)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res+(Aa1*ZA(gt1,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt2)) Then 
res = res+(Aa2*ZA(gt1,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt2)) Then 
res = res+(Aa3*v3*ZA(gt1,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res+(Aa4*v3*ZA(gt1,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res-(Conjg(Aa1)*ZA(gt1,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt2)) Then 
res = res-(Conjg(Aa2)*ZA(gt1,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt2)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt1,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res-(v3*Conjg(Aa4)*ZA(gt1,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res+(Aa3*v1*ZA(gt1,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res+(Aa4*v1*ZA(gt1,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(v1*Conjg(Aa3)*ZA(gt1,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt2)) Then 
res = res-(v1*Conjg(Aa4)*ZA(gt1,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res+(Aa3*v2*ZA(gt1,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res-(Aa4*v2*ZA(gt1,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res-(v2*Conjg(Aa3)*ZA(gt1,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res+(v2*Conjg(Aa4)*ZA(gt1,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res+(Aa3*v1*ZA(gt1,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(Aa4*v1*ZA(gt1,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(v1*Conjg(Aa3)*ZA(gt1,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res+(v1*Conjg(Aa4)*ZA(gt1,3)*ZH(gt3,3))/2._dp
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhUhhhhL  
 
 
Subroutine CouplingAhUhhVPL(gt1,gt2,g1,g2,ZZ,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhUhhVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt2)) Then 
res = res-(g1*ZA(gt1,1)*ZZ(1,1))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(g1*ZA(gt1,2)*ZZ(1,1))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res+(g2*ZA(gt1,1)*ZZ(2,1))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res+(g2*ZA(gt1,2)*ZZ(2,1))/2._dp
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhUhhVPL  
 
 
Subroutine CouplingAhUhhVZL(gt1,gt2,g1,g2,ZZ,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhUhhVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt2)) Then 
res = res-(g1*ZA(gt1,1)*ZZ(1,2))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(g1*ZA(gt1,2)*ZZ(1,2))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res+(g2*ZA(gt1,1)*ZZ(2,2))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res+(g2*ZA(gt1,2)*ZZ(2,2))/2._dp
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhUhhVZL  
 
 
Subroutine CouplingcFdFdUhhL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,               & 
& Y1d23,Y2d31,Y2d32,Y2d33,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdUhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d11*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d12*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d13*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d21*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,2)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d22*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,2)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d23*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,2)))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resL = resL-((Y2d31*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,3)))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resL = resL-((Y2d32*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,3)))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resL = resL-((Y2d33*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,3)))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt3)) Then 
resR = resR-((Conjg(Y1d11)*ZDR(gt2,1)*ZDL(gt1,1))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resR = resR-((Conjg(Y1d12)*ZDR(gt2,2)*ZDL(gt1,1))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resR = resR-((Conjg(Y1d13)*ZDR(gt2,3)*ZDL(gt1,1))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resR = resR-((Conjg(Y1d21)*ZDR(gt2,1)*ZDL(gt1,2))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resR = resR-((Conjg(Y1d22)*ZDR(gt2,2)*ZDL(gt1,2))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resR = resR-((Conjg(Y1d23)*ZDR(gt2,3)*ZDL(gt1,2))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resR = resR-((Conjg(Y2d31)*ZDR(gt2,1)*ZDL(gt1,3))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resR = resR-((Conjg(Y2d32)*ZDR(gt2,2)*ZDL(gt1,3))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resR = resR-((Conjg(Y2d33)*ZDR(gt2,3)*ZDL(gt1,3))/sqrt(2._dp))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdUhhL  
 
 
Subroutine CouplingcFeFeUhhL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFeUhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt3)) Then 
resL = resL-((Y1e11*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1e12*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1e21*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,2)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1e22*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,2)))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resL = resL-((Y2e33*Conjg(ZER(gt1,3))*Conjg(ZEL(gt2,3)))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt3)) Then 
resR = resR-((Conjg(Y1e11)*ZER(gt2,1)*ZEL(gt1,1))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resR = resR-((Conjg(Y1e12)*ZER(gt2,2)*ZEL(gt1,1))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resR = resR-((Conjg(Y1e21)*ZER(gt2,1)*ZEL(gt1,2))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resR = resR-((Conjg(Y1e22)*ZER(gt2,2)*ZEL(gt1,2))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resR = resR-((Conjg(Y2e33)*ZER(gt2,3)*ZEL(gt1,3))/sqrt(2._dp))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFeUhhL  
 
 
Subroutine CouplingcFuFuUhhL(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,               & 
& ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuUhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt3)) Then 
resL = resL+(Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,1)))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resL = resL+(Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,1)))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resL = resL+(Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,2)))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resL = resL+(Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,2)))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
resL = resL+(Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZUL(gt2,3)))/sqrt(2._dp)
End If 
resR = 0._dp 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1u11)*ZUR(gt2,1)*ZUL(gt1,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1u12)*ZUR(gt2,2)*ZUL(gt1,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1u21)*ZUR(gt2,1)*ZUL(gt1,2))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1u22)*ZUR(gt2,2)*ZUL(gt1,2))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
resR = resR+(Conjg(Y2u33)*ZUR(gt2,3)*ZUL(gt1,3))/sqrt(2._dp)
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuUhhL  
 
 
Subroutine CouplingFvFvUhhL(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,            & 
& C23,C31,C32,BB11,BB12,BB21,BB22,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvUhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt3)) Then 
resL = resL+(Y1n11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,1)))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resL = resL+(Y1n12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,1)))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resL = resL+(Y1n21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,2)))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resL = resL+(Y1n22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,2)))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resL = resL+(Y1n11*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,4)))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resL = resL+(Y1n21*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,4)))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resL = resL+(Y1n12*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,5)))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resL = resL+(Y1n22*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,5)))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
resL = resL+(Y2n33*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,3)))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
resL = resL+(Y2n33*Conjg(Vv(gt1,3))*Conjg(Vv(gt2,6)))/sqrt(2._dp)
End If 
If ((3.eq.gt3)) Then 
resL = resL-((BB11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,4)))/sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(BB12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(BB21*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(C13*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(C31*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(BB12*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(BB21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(BB22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,5)))/sqrt(2._dp)
End If 
If ((3.eq.gt3)) Then 
resL = resL+(C23*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(C32*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(C13*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(C31*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(C23*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(C32*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6)))/(2._dp*sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n11)*Vv(gt1,4)*Vv(gt2,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n12)*Vv(gt1,5)*Vv(gt2,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n21)*Vv(gt1,4)*Vv(gt2,2))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n22)*Vv(gt1,5)*Vv(gt2,2))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
resR = resR+(Conjg(Y2n33)*Vv(gt1,6)*Vv(gt2,3))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n11)*Vv(gt1,1)*Vv(gt2,4))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n21)*Vv(gt1,2)*Vv(gt2,4))/sqrt(2._dp)
End If 
If ((3.eq.gt3)) Then 
resR = resR-((Conjg(BB11)*Vv(gt1,4)*Vv(gt2,4))/sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(BB12)*Vv(gt1,5)*Vv(gt2,4))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(BB21)*Vv(gt1,5)*Vv(gt2,4))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C13)*Vv(gt1,6)*Vv(gt2,4))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C31)*Vv(gt1,6)*Vv(gt2,4))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n12)*Vv(gt1,1)*Vv(gt2,5))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n22)*Vv(gt1,2)*Vv(gt2,5))/sqrt(2._dp)
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(BB12)*Vv(gt1,4)*Vv(gt2,5))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(BB21)*Vv(gt1,4)*Vv(gt2,5))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(BB22)*Vv(gt1,5)*Vv(gt2,5))/sqrt(2._dp)
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C23)*Vv(gt1,6)*Vv(gt2,5))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C32)*Vv(gt1,6)*Vv(gt2,5))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resR = resR+(Conjg(Y2n33)*Vv(gt1,3)*Vv(gt2,6))/sqrt(2._dp)
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C13)*Vv(gt1,4)*Vv(gt2,6))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C31)*Vv(gt1,4)*Vv(gt2,6))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C23)*Vv(gt1,5)*Vv(gt2,6))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C32)*Vv(gt1,5)*Vv(gt2,6))/(2._dp*sqrt(2._dp))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvUhhL  
 
 
Subroutine CouplingcgWmgWmUhhL(gt3,g2,v1,v2,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g2,v1,v2

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgWmUhh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res-(g2**2*v1*RXiWm)/4._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g2**2*v2*RXiWm)/4._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgWmUhhL  
 
 
Subroutine CouplingcgWpgWpUhhL(gt3,g2,v1,v2,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g2,v1,v2

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgWpUhh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res-(g2**2*v1*RXiWm)/4._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g2**2*v2*RXiWm)/4._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgWpUhhL  
 
 
Subroutine CouplingcgZgZUhhL(gt3,g1,g2,v1,v2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgZgZUhh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res-(g1**2*v1*Conjg(ZZ(1,2))*RXiZ*ZZ(1,2))/8._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(g1*g2*v1*Conjg(ZZ(2,2))*RXiZ*ZZ(1,2))/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g1**2*v2*Conjg(ZZ(1,2))*RXiZ*ZZ(1,2))/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(g1*g2*v2*Conjg(ZZ(2,2))*RXiZ*ZZ(1,2))/8._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(g1**2*v1*RXiZ*ZZ(1,2)**2)/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g1**2*v2*RXiZ*ZZ(1,2)**2)/8._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(g1*g2*v1*Conjg(ZZ(1,2))*RXiZ*ZZ(2,2))/8._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(g2**2*v1*Conjg(ZZ(2,2))*RXiZ*ZZ(2,2))/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(g1*g2*v2*Conjg(ZZ(1,2))*RXiZ*ZZ(2,2))/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g2**2*v2*Conjg(ZZ(2,2))*RXiZ*ZZ(2,2))/8._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(g1*g2*v1*RXiZ*ZZ(1,2)*ZZ(2,2))/4._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(g1*g2*v2*RXiZ*ZZ(1,2)*ZZ(2,2))/4._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(g2**2*v1*RXiZ*ZZ(2,2)**2)/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g2**2*v2*RXiZ*ZZ(2,2)**2)/8._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgZgZUhhL  
 
 
Subroutine CouplingUhhhhhhL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUhhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res-6*Lam1*v1*ZH(gt2,1)*ZH(gt3,1)
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam3*v2*ZH(gt2,1)*ZH(gt3,1))
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam4*v2*ZH(gt2,1)*ZH(gt3,1))
End If 
If ((3.eq.gt1)) Then 
res = res-(Lam2Dash*v3*ZH(gt2,1)*ZH(gt3,1))
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam3*v2*ZH(gt2,2)*ZH(gt3,1))
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam4*v2*ZH(gt2,2)*ZH(gt3,1))
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam3*v1*ZH(gt2,2)*ZH(gt3,1))
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam4*v1*ZH(gt2,2)*ZH(gt3,1))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa1*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa2*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa3*v3*ZH(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v3*ZH(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZH(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam2Dash*v3*ZH(gt2,3)*ZH(gt3,1))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa1*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa2*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa3*v3*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa4*v3*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Lam2Dash*v1*ZH(gt2,3)*ZH(gt3,1))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa3*v2*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v2*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v2*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v2*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam3*v2*ZH(gt2,1)*ZH(gt3,2))
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam4*v2*ZH(gt2,1)*ZH(gt3,2))
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam3*v1*ZH(gt2,1)*ZH(gt3,2))
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam4*v1*ZH(gt2,1)*ZH(gt3,2))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa1*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa2*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa3*v3*ZH(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v3*ZH(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZH(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam3*v1*ZH(gt2,2)*ZH(gt3,2))
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam4*v1*ZH(gt2,2)*ZH(gt3,2))
End If 
If ((2.eq.gt1)) Then 
res = res-6*Lam2*v2*ZH(gt2,2)*ZH(gt3,2)
End If 
If ((3.eq.gt1)) Then 
res = res-(Lam3Dash*v3*ZH(gt2,2)*ZH(gt3,2))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa1*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa2*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa3*v3*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa4*v3*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam3Dash*v3*ZH(gt2,3)*ZH(gt3,2))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa3*v1*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v1*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Lam3Dash*v2*ZH(gt2,3)*ZH(gt3,2))
End If 
If ((3.eq.gt1)) Then 
res = res-(v1*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v1*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam2Dash*v3*ZH(gt2,1)*ZH(gt3,3))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa1*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa2*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa3*v3*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa4*v3*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Lam2Dash*v1*ZH(gt2,1)*ZH(gt3,3))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa3*v2*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v2*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v2*Conjg(Aa3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v2*Conjg(Aa4)*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa1*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa2*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa3*v3*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa4*v3*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam3Dash*v3*ZH(gt2,2)*ZH(gt3,3))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa3*v1*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v1*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Lam3Dash*v2*ZH(gt2,2)*ZH(gt3,3))
End If 
If ((3.eq.gt1)) Then 
res = res-(v1*Conjg(Aa3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v1*Conjg(Aa4)*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam2Dash*v1*ZH(gt2,3)*ZH(gt3,3))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa3*v2*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa4*v2*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(v2*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(v2*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa3*v1*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa4*v1*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam3Dash*v2*ZH(gt2,3)*ZH(gt3,3))
End If 
If ((2.eq.gt1)) Then 
res = res-(v1*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(v1*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-6*Lam1Dash*v3*ZH(gt2,3)*ZH(gt3,3)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUhhhhhhL  
 
 
Subroutine CouplingUhhHmcHmL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,       & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUhhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res-2*Lam1*v1*Conjg(ZP(gt2,1))*ZP(gt3,1)
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam4*v2*Conjg(ZP(gt2,2))*ZP(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam3*v2*Conjg(ZP(gt2,1))*ZP(gt3,1))
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam4*v1*Conjg(ZP(gt2,2))*ZP(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Lam2Dash*v3*Conjg(ZP(gt2,1))*ZP(gt3,1))
End If 
If ((3.eq.gt1)) Then 
res = res-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZP(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-((Conjg(Aa2)*Conjg(ZP(gt2,2))*ZP(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZP(gt3,1))
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZP(gt3,1))
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam4*v2*Conjg(ZP(gt2,1))*ZP(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam3*v1*Conjg(ZP(gt2,2))*ZP(gt3,2))
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam4*v1*Conjg(ZP(gt2,1))*ZP(gt3,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-2*Lam2*v2*Conjg(ZP(gt2,2))*ZP(gt3,2)
End If 
If ((3.eq.gt1)) Then 
res = res-((Aa1*Conjg(ZP(gt2,1))*ZP(gt3,2))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-((Aa2*Conjg(ZP(gt2,1))*ZP(gt3,2))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa3*v3*Conjg(ZP(gt2,1))*ZP(gt3,2))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v3*Conjg(ZP(gt2,1))*ZP(gt3,2))
End If 
If ((3.eq.gt1)) Then 
res = res-(Lam3Dash*v3*Conjg(ZP(gt2,2))*ZP(gt3,2))
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUhhHmcHmL  
 
 
Subroutine CouplingUhhHmcVWmL(gt1,gt2,g2,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUhhHmcVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res+(g2*Conjg(ZP(gt2,1)))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(g2*Conjg(ZP(gt2,2)))/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUhhHmcVWmL  
 
 
Subroutine CouplingUhhVPVZL(gt1,g1,g2,v1,v2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUhhVPVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res+(g1**2*v1*ZZ(1,1)*ZZ(1,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(g1**2*v2*ZZ(1,1)*ZZ(1,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(g1*g2*v1*ZZ(1,2)*ZZ(2,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(g1*g2*v2*ZZ(1,2)*ZZ(2,1))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(g1*g2*v1*ZZ(1,1)*ZZ(2,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(g1*g2*v2*ZZ(1,1)*ZZ(2,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(g2**2*v1*ZZ(2,1)*ZZ(2,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(g2**2*v2*ZZ(2,1)*ZZ(2,2))/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUhhVPVZL  
 
 
Subroutine CouplingUhhcVWmVWmL(gt1,g2,v1,v2,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g2,v1,v2

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUhhcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res+(g2**2*v1)/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(g2**2*v2)/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUhhcVWmVWmL  
 
 
Subroutine CouplingUhhVZVZL(gt1,g1,g2,v1,v2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUhhVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res+(g1**2*v1*ZZ(1,2)**2)/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(g1**2*v2*ZZ(1,2)**2)/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(g1*g2*v1*ZZ(1,2)*ZZ(2,2))
End If 
If ((2.eq.gt1)) Then 
res = res-(g1*g2*v2*ZZ(1,2)*ZZ(2,2))
End If 
If ((1.eq.gt1)) Then 
res = res+(g2**2*v1*ZZ(2,2)**2)/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(g2**2*v2*ZZ(2,2)**2)/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUhhVZVZL  
 
 
Subroutine CouplingAhAhUhhUhhL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,          & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhUhhUhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3).And.(1.eq.gt4)) Then 
res = res-2*Lam1*ZA(gt1,1)*ZA(gt2,1)
End If 
If ((2.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,1))
End If 
If ((2.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,1))
End If 
If ((3.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Lam2Dash*ZA(gt1,1)*ZA(gt2,1))
End If 
If ((3.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Aa3*ZA(gt1,2)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt4).And.(3.eq.gt3)) Then 
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt4).And.(3.eq.gt3)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt4).And.(3.eq.gt3)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt4).And.(3.eq.gt3)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Aa3*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt3).And.(3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((2.eq.gt3).And.(3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1))/2._dp
End If 
If ((3.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,2))/2._dp
End If 
If ((3.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Aa4*ZA(gt1,1)*ZA(gt2,2))/2._dp
End If 
If ((3.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2))/2._dp
End If 
If ((3.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt3).And.(1.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,2))
End If 
If ((1.eq.gt3).And.(1.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,2))
End If 
If ((2.eq.gt3).And.(2.eq.gt4)) Then 
res = res-2*Lam2*ZA(gt1,2)*ZA(gt2,2)
End If 
If ((3.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Lam3Dash*ZA(gt1,2)*ZA(gt2,2))
End If 
If ((1.eq.gt4).And.(3.eq.gt3)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt4).And.(3.eq.gt3)) Then 
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt4).And.(3.eq.gt3)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt4).And.(3.eq.gt3)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt3).And.(3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Aa4*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt3).And.(3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2))/2._dp
End If 
If ((2.eq.gt4).And.(3.eq.gt3)) Then 
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt4).And.(3.eq.gt3)) Then 
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt4).And.(3.eq.gt3)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt4).And.(3.eq.gt3)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Aa3*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3).And.(3.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3).And.(3.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt4).And.(3.eq.gt3)) Then 
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt4).And.(3.eq.gt3)) Then 
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt4).And.(3.eq.gt3)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt4).And.(3.eq.gt3)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3).And.(3.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Aa4*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3).And.(3.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3).And.(3.eq.gt4)) Then 
res = res-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3).And.(1.eq.gt4)) Then 
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,3))
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res+(Aa3*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res+(Aa4*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3))/2._dp
End If 
If ((2.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,3))
End If 
If ((3.eq.gt3).And.(3.eq.gt4)) Then 
res = res-2*Lam1Dash*ZA(gt1,3)*ZA(gt2,3)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhUhhUhhL  
 
 
Subroutine CouplingUhhUhhhhhhL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,          & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUhhUhhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-6*Lam1*ZH(gt3,1)*ZH(gt4,1)
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam4*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam2Dash*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam3*ZH(gt3,2)*ZH(gt4,1))
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam4*ZH(gt3,2)*ZH(gt4,1))
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3*ZH(gt3,2)*ZH(gt4,1))
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam4*ZH(gt3,2)*ZH(gt4,1))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa3*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa4*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Lam2Dash*ZH(gt3,3)*ZH(gt4,1))
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa3*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa4*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam2Dash*ZH(gt3,3)*ZH(gt4,1))
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa3*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa4*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam3*ZH(gt3,1)*ZH(gt4,2))
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam4*ZH(gt3,1)*ZH(gt4,2))
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3*ZH(gt3,1)*ZH(gt4,2))
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam4*ZH(gt3,1)*ZH(gt4,2))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa3*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa4*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(Lam3*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(Lam4*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-6*Lam2*ZH(gt3,2)*ZH(gt4,2)
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam3Dash*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa3*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa4*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Lam3Dash*ZH(gt3,3)*ZH(gt4,2))
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa3*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa4*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam3Dash*ZH(gt3,3)*ZH(gt4,2))
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Lam2Dash*ZH(gt3,1)*ZH(gt4,3))
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa3*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa4*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam2Dash*ZH(gt3,1)*ZH(gt4,3))
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa3*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa4*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa3*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa4*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Lam3Dash*ZH(gt3,2)*ZH(gt4,3))
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa3*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa4*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam3Dash*ZH(gt3,2)*ZH(gt4,3))
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(Lam2Dash*ZH(gt3,3)*ZH(gt4,3))
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Aa3*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Aa4*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Aa3*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Aa4*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3Dash*ZH(gt3,3)*ZH(gt4,3))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-6*Lam1Dash*ZH(gt3,3)*ZH(gt4,3)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUhhUhhhhhhL  
 
 
Subroutine CouplingUhhUhhHmcHmL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,         & 
& Lam3Dash,Aa3,Aa4,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUhhUhhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-2*Lam1*Conjg(ZP(gt3,1))*ZP(gt4,1)
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam4*Conjg(ZP(gt3,2))*ZP(gt4,1))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam4*Conjg(ZP(gt3,2))*ZP(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3*Conjg(ZP(gt3,1))*ZP(gt4,1))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam2Dash*Conjg(ZP(gt3,1))*ZP(gt4,1))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa3)*Conjg(ZP(gt3,2))*ZP(gt4,1))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa4)*Conjg(ZP(gt3,2))*ZP(gt4,1))
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(Lam3*Conjg(ZP(gt3,2))*ZP(gt4,2))
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam4*Conjg(ZP(gt3,1))*ZP(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam4*Conjg(ZP(gt3,1))*ZP(gt4,2))/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-2*Lam2*Conjg(ZP(gt3,2))*ZP(gt4,2)
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa3*Conjg(ZP(gt3,1))*ZP(gt4,2))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa4*Conjg(ZP(gt3,1))*ZP(gt4,2))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam3Dash*Conjg(ZP(gt3,2))*ZP(gt4,2))
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUhhUhhHmcHmL  
 
 
Subroutine CouplingUhhUhhVPVPL(gt1,gt2,g1,g2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUhhUhhVPVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,1)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,1)**2)/2._dp
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(g1*g2*ZZ(1,1)*ZZ(2,1))
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(g1*g2*ZZ(1,1)*ZZ(2,1))
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,1)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,1)**2)/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUhhUhhVPVPL  
 
 
Subroutine CouplingUhhUhhcVWmVWmL(gt1,gt2,g2,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUhhUhhcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+g2**2/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+g2**2/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUhhUhhcVWmVWmL  
 
 
Subroutine CouplingUhhUhhVZVZL(gt1,gt2,g1,g2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUhhUhhVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,2)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,2)**2)/2._dp
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(g1*g2*ZZ(1,2)*ZZ(2,2))
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(g1*g2*ZZ(1,2)*ZZ(2,2))
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,2)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,2)**2)/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUhhUhhVZVZL  
 
 
Subroutine CouplingUAhAhAhL(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhAhAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((3.eq.gt1)) Then 
res = res+(Aa1*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa2*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v3*ZA(gt2,2)*ZA(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v3*ZA(gt2,2)*ZA(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt2,2)*ZA(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa1*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa2*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa3*v3*ZA(gt2,3)*ZA(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa4*v3*ZA(gt2,3)*ZA(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt2,3)*ZA(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v2*ZA(gt2,3)*ZA(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa4*v2*ZA(gt2,3)*ZA(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v2*Conjg(Aa3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v2*Conjg(Aa4)*ZA(gt2,3)*ZA(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa1*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa2*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v3*ZA(gt2,1)*ZA(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v3*ZA(gt2,1)*ZA(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt2,1)*ZA(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(Aa1*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa2*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res+(Aa3*v3*ZA(gt2,3)*ZA(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa4*v3*ZA(gt2,3)*ZA(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt2,3)*ZA(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa3*v1*ZA(gt2,3)*ZA(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v1*ZA(gt2,3)*ZA(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v1*Conjg(Aa3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v1*Conjg(Aa4)*ZA(gt2,3)*ZA(gt3,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa1*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa2*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa3*v3*ZA(gt2,1)*ZA(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa4*v3*ZA(gt2,1)*ZA(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt2,1)*ZA(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v2*ZA(gt2,1)*ZA(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa4*v2*ZA(gt2,1)*ZA(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v2*Conjg(Aa3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v2*Conjg(Aa4)*ZA(gt2,1)*ZA(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(Aa1*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa2*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res+(Aa3*v3*ZA(gt2,2)*ZA(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa4*v3*ZA(gt2,2)*ZA(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt2,2)*ZA(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa3*v1*ZA(gt2,2)*ZA(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v1*ZA(gt2,2)*ZA(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v1*Conjg(Aa3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v1*Conjg(Aa4)*ZA(gt2,2)*ZA(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(Aa3*v2*ZA(gt2,3)*ZA(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(Aa4*v2*ZA(gt2,3)*ZA(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(v2*Conjg(Aa3)*ZA(gt2,3)*ZA(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(v2*Conjg(Aa4)*ZA(gt2,3)*ZA(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa3*v1*ZA(gt2,3)*ZA(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa4*v1*ZA(gt2,3)*ZA(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(v1*Conjg(Aa3)*ZA(gt2,3)*ZA(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(v1*Conjg(Aa4)*ZA(gt2,3)*ZA(gt3,3))/2._dp
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhAhAhL  
 
 
Subroutine CouplingUAhAhhhL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhAhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res-2*Lam1*v1*ZA(gt2,1)*ZH(gt3,1)
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam3*v1*ZA(gt2,2)*ZH(gt3,1))
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam4*v1*ZA(gt2,2)*ZH(gt3,1))
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa1*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa2*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v3*ZA(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v3*ZA(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Conjg(Aa1)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(v3*Conjg(Aa3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZA(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa1*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa2*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa3*v3*ZA(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa4*v3*ZA(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Conjg(Aa1)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res+(v3*Conjg(Aa3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZA(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Lam2Dash*v1*ZA(gt2,3)*ZH(gt3,1))
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v2*ZA(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa4*v2*ZA(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v2*Conjg(Aa3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v2*Conjg(Aa4)*ZA(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam3*v2*ZA(gt2,1)*ZH(gt3,2))
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam4*v2*ZA(gt2,1)*ZH(gt3,2))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa1*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa2*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa3*v3*ZA(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa4*v3*ZA(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-2*Lam2*v2*ZA(gt2,2)*ZH(gt3,2)
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa1*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res+(Aa2*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa3*v3*ZA(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(Aa4*v3*ZA(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZA(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v1*ZA(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa4*v1*ZA(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Lam3Dash*v2*ZA(gt2,3)*ZH(gt3,2))
End If 
If ((3.eq.gt1)) Then 
res = res+(v1*Conjg(Aa3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v1*Conjg(Aa4)*ZA(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Lam2Dash*v3*ZA(gt2,1)*ZH(gt3,3))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa1*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa2*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa3*v3*ZA(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa4*v3*ZA(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZA(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa3*v2*ZA(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa4*v2*ZA(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v2*Conjg(Aa3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v2*Conjg(Aa4)*ZA(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa1*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa2*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa3*v3*ZA(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa4*v3*ZA(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZA(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam3Dash*v3*ZA(gt2,2)*ZH(gt3,3))
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v1*ZA(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v1*ZA(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v1*Conjg(Aa3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v1*Conjg(Aa4)*ZA(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa3*v2*ZA(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(Aa4*v2*ZA(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(v2*Conjg(Aa3)*ZA(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(v2*Conjg(Aa4)*ZA(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa3*v1*ZA(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Aa4*v1*ZA(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(v1*Conjg(Aa3)*ZA(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(v1*Conjg(Aa4)*ZA(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-2*Lam1Dash*v3*ZA(gt2,3)*ZH(gt3,3)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhAhhhL  
 
 
Subroutine CouplingcFdFdUAhL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,               & 
& Y1d23,Y2d31,Y2d32,Y2d33,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdUAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d11*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d12*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d13*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d21*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,2)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d22*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,2)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1d23*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,2)))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resL = resL-((Y2d31*Conjg(ZDR(gt1,1))*Conjg(ZDL(gt2,3)))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resL = resL-((Y2d32*Conjg(ZDR(gt1,2))*Conjg(ZDL(gt2,3)))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resL = resL-((Y2d33*Conjg(ZDR(gt1,3))*Conjg(ZDL(gt2,3)))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1d11)*ZDR(gt2,1)*ZDL(gt1,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1d12)*ZDR(gt2,2)*ZDL(gt1,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1d13)*ZDR(gt2,3)*ZDL(gt1,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1d21)*ZDR(gt2,1)*ZDL(gt1,2))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1d22)*ZDR(gt2,2)*ZDL(gt1,2))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1d23)*ZDR(gt2,3)*ZDL(gt1,2))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
resR = resR+(Conjg(Y2d31)*ZDR(gt2,1)*ZDL(gt1,3))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
resR = resR+(Conjg(Y2d32)*ZDR(gt2,2)*ZDL(gt1,3))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
resR = resR+(Conjg(Y2d33)*ZDR(gt2,3)*ZDL(gt1,3))/sqrt(2._dp)
End If 
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdUAhL  
 
 
Subroutine CouplingcFeFeUAhL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFeUAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt3)) Then 
resL = resL-((Y1e11*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1e12*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1e21*Conjg(ZER(gt1,1))*Conjg(ZEL(gt2,2)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1e22*Conjg(ZER(gt1,2))*Conjg(ZEL(gt2,2)))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resL = resL-((Y2e33*Conjg(ZER(gt1,3))*Conjg(ZEL(gt2,3)))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1e11)*ZER(gt2,1)*ZEL(gt1,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1e12)*ZER(gt2,2)*ZEL(gt1,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1e21)*ZER(gt2,1)*ZEL(gt1,2))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1e22)*ZER(gt2,2)*ZEL(gt1,2))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
resR = resR+(Conjg(Y2e33)*ZER(gt2,3)*ZEL(gt1,3))/sqrt(2._dp)
End If 
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFeUAhL  
 
 
Subroutine CouplingcFuFuUAhL(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,               & 
& ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuUAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt3)) Then 
resL = resL-((Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZUL(gt2,2)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZUL(gt2,2)))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resL = resL-((Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZUL(gt2,3)))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1u11)*ZUR(gt2,1)*ZUL(gt1,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1u12)*ZUR(gt2,2)*ZUL(gt1,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1u21)*ZUR(gt2,1)*ZUL(gt1,2))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1u22)*ZUR(gt2,2)*ZUL(gt1,2))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
resR = resR+(Conjg(Y2u33)*ZUR(gt2,3)*ZUL(gt1,3))/sqrt(2._dp)
End If 
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuUAhL  
 
 
Subroutine CouplingFvFvUAhL(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,            & 
& C23,C31,C32,BB11,BB12,BB21,BB22,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvUAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt3)) Then 
resL = resL-((Y1n11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1n12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,1)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1n21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,2)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1n22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,2)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1n11*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,4)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1n21*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,4)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1n12*Conjg(Vv(gt1,1))*Conjg(Vv(gt2,5)))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resL = resL-((Y1n22*Conjg(Vv(gt1,2))*Conjg(Vv(gt2,5)))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resL = resL-((Y2n33*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,3)))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resL = resL-((Y2n33*Conjg(Vv(gt1,3))*Conjg(Vv(gt2,6)))/sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL-((BB11*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,4)))/sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(BB12*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(BB21*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,4)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL-(C13*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL-(C31*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,4)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(BB12*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(BB21*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,5)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL+(BB22*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,5)))/sqrt(2._dp)
End If 
If ((3.eq.gt3)) Then 
resL = resL-(C23*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL-(C32*Conjg(Vv(gt1,6))*Conjg(Vv(gt2,5)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL-(C13*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL-(C31*Conjg(Vv(gt1,4))*Conjg(Vv(gt2,6)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL-(C23*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6)))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resL = resL-(C32*Conjg(Vv(gt1,5))*Conjg(Vv(gt2,6)))/(2._dp*sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n11)*Vv(gt1,4)*Vv(gt2,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n12)*Vv(gt1,5)*Vv(gt2,1))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n21)*Vv(gt1,4)*Vv(gt2,2))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n22)*Vv(gt1,5)*Vv(gt2,2))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
resR = resR+(Conjg(Y2n33)*Vv(gt1,6)*Vv(gt2,3))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n11)*Vv(gt1,1)*Vv(gt2,4))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n21)*Vv(gt1,2)*Vv(gt2,4))/sqrt(2._dp)
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(BB11)*Vv(gt1,4)*Vv(gt2,4))/sqrt(2._dp)
End If 
If ((3.eq.gt3)) Then 
resR = resR-(Conjg(BB12)*Vv(gt1,5)*Vv(gt2,4))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR-(Conjg(BB21)*Vv(gt1,5)*Vv(gt2,4))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C13)*Vv(gt1,6)*Vv(gt2,4))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C31)*Vv(gt1,6)*Vv(gt2,4))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n12)*Vv(gt1,1)*Vv(gt2,5))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
resR = resR+(Conjg(Y1n22)*Vv(gt1,2)*Vv(gt2,5))/sqrt(2._dp)
End If 
If ((3.eq.gt3)) Then 
resR = resR-(Conjg(BB12)*Vv(gt1,4)*Vv(gt2,5))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR-(Conjg(BB21)*Vv(gt1,4)*Vv(gt2,5))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR-((Conjg(BB22)*Vv(gt1,5)*Vv(gt2,5))/sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C23)*Vv(gt1,6)*Vv(gt2,5))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C32)*Vv(gt1,6)*Vv(gt2,5))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
resR = resR+(Conjg(Y2n33)*Vv(gt1,3)*Vv(gt2,6))/sqrt(2._dp)
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C13)*Vv(gt1,4)*Vv(gt2,6))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C31)*Vv(gt1,4)*Vv(gt2,6))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C23)*Vv(gt1,5)*Vv(gt2,6))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt3)) Then 
resR = resR+(Conjg(C32)*Vv(gt1,5)*Vv(gt2,6))/(2._dp*sqrt(2._dp))
End If 
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvUAhL  
 
 
Subroutine CouplingcgWmgWmUAhL(gt3,g2,v1,v2,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g2,v1,v2

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgWmUAh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res-(g2**2*v1*RXiWm)/4._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g2**2*v2*RXiWm)/4._dp
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgWmUAhL  
 
 
Subroutine CouplingcgWpgWpUAhL(gt3,g2,v1,v2,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g2,v1,v2

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgWpUAh' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res+(g2**2*v1*RXiWm)/4._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(g2**2*v2*RXiWm)/4._dp
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgWpUAhL  
 
 
Subroutine CouplingUAhhhhhL(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((3.eq.gt1)) Then 
res = res+(Aa1*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa2*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v3*ZH(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v3*ZH(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZH(gt2,2)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa1*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa2*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa3*v3*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa4*v3*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v2*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v2*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v2*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v2*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa1*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa2*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v3*ZH(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v3*ZH(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZH(gt2,1)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa1*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa2*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa3*v3*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa4*v3*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(Conjg(Aa1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res+(v3*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v1*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v1*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v1*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v1*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa1*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa2*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa3*v3*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa4*v3*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa1)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(Conjg(Aa2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(v3*Conjg(Aa4)*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v2*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v2*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v2*Conjg(Aa3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v2*Conjg(Aa4)*ZH(gt2,1)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa1*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa2*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa3*v3*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa4*v3*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(Conjg(Aa1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res+(Conjg(Aa2)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
res = res+(v3*Conjg(Aa3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(v3*Conjg(Aa4)*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa3*v1*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v1*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-(v1*Conjg(Aa3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(v1*Conjg(Aa4)*ZH(gt2,2)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa3*v2*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res-(Aa4*v2*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(v2*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(v2*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa3*v1*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Aa4*v1*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(v1*Conjg(Aa3)*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(v1*Conjg(Aa4)*ZH(gt2,3)*ZH(gt3,3))/2._dp
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhhhhhL  
 
 
Subroutine CouplingUAhhhVPL(gt1,gt2,g1,g2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhhhVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res-(g1*ZH(gt2,1)*ZZ(1,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(g1*ZH(gt2,2)*ZZ(1,1))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(g2*ZH(gt2,1)*ZZ(2,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(g2*ZH(gt2,2)*ZZ(2,1))/2._dp
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhhhVPL  
 
 
Subroutine CouplingUAhhhVZL(gt1,gt2,g1,g2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhhhVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res-(g1*ZH(gt2,1)*ZZ(1,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(g1*ZH(gt2,2)*ZZ(1,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
res = res+(g2*ZH(gt2,1)*ZZ(2,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(g2*ZH(gt2,2)*ZZ(2,2))/2._dp
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhhhVZL  
 
 
Subroutine CouplingUAhHmcHmL(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3

Complex(dp), Intent(in) :: Lam4,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res-(Lam4*v2*Conjg(ZP(gt2,2))*ZP(gt3,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(Lam4*v1*Conjg(ZP(gt2,2))*ZP(gt3,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZP(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+(Conjg(Aa2)*Conjg(ZP(gt2,2))*ZP(gt3,1))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
res = res-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZP(gt3,1))
End If 
If ((3.eq.gt1)) Then 
res = res+v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZP(gt3,1)
End If 
If ((1.eq.gt1)) Then 
res = res+(Lam4*v2*Conjg(ZP(gt2,1))*ZP(gt3,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(Lam4*v1*Conjg(ZP(gt2,1))*ZP(gt3,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
res = res+(Aa1*Conjg(ZP(gt2,1))*ZP(gt3,2))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
res = res-((Aa2*Conjg(ZP(gt2,1))*ZP(gt3,2))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
res = res+Aa3*v3*Conjg(ZP(gt2,1))*ZP(gt3,2)
End If 
If ((3.eq.gt1)) Then 
res = res-(Aa4*v3*Conjg(ZP(gt2,1))*ZP(gt3,2))
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhHmcHmL  
 
 
Subroutine CouplingUAhHmcVWmL(gt1,gt2,g2,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhHmcVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res-(g2*Conjg(ZP(gt2,1)))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res-(g2*Conjg(ZP(gt2,2)))/2._dp
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhHmcVWmL  
 
 
Subroutine CouplingUAhUAhAhAhL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,          & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhUAhAhAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-6*Lam1*ZA(gt3,1)*ZA(gt4,1)
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3*ZA(gt3,1)*ZA(gt4,1))
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam4*ZA(gt3,1)*ZA(gt4,1))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam2Dash*ZA(gt3,1)*ZA(gt4,1))
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam3*ZA(gt3,2)*ZA(gt4,1))
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam4*ZA(gt3,2)*ZA(gt4,1))
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3*ZA(gt3,2)*ZA(gt4,1))
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam4*ZA(gt3,2)*ZA(gt4,1))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa3*ZA(gt3,2)*ZA(gt4,1))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa4*ZA(gt3,2)*ZA(gt4,1))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,2)*ZA(gt4,1))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Lam2Dash*ZA(gt3,3)*ZA(gt4,1))
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa3*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa4*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam2Dash*ZA(gt3,3)*ZA(gt4,1))
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa3*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa4*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,3)*ZA(gt4,1))/2._dp
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam3*ZA(gt3,1)*ZA(gt4,2))
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam4*ZA(gt3,1)*ZA(gt4,2))
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3*ZA(gt3,1)*ZA(gt4,2))
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam4*ZA(gt3,1)*ZA(gt4,2))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa3*ZA(gt3,1)*ZA(gt4,2))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa4*ZA(gt3,1)*ZA(gt4,2))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,1)*ZA(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(Lam3*ZA(gt3,2)*ZA(gt4,2))
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(Lam4*ZA(gt3,2)*ZA(gt4,2))
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-6*Lam2*ZA(gt3,2)*ZA(gt4,2)
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam3Dash*ZA(gt3,2)*ZA(gt4,2))
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa3*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa4*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Lam3Dash*ZA(gt3,3)*ZA(gt4,2))
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa3*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa4*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,3)*ZA(gt4,2))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam3Dash*ZA(gt3,3)*ZA(gt4,2))
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Lam2Dash*ZA(gt3,1)*ZA(gt4,3))
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa3*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa4*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam2Dash*ZA(gt3,1)*ZA(gt4,3))
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa3*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa4*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,1)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa3*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa4*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Lam3Dash*ZA(gt3,2)*ZA(gt4,3))
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa3*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa4*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,2)*ZA(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam3Dash*ZA(gt3,2)*ZA(gt4,3))
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(Lam2Dash*ZA(gt3,3)*ZA(gt4,3))
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res+(Aa3*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res+(Aa4*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(Aa3*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(Aa4*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(Conjg(Aa3)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(Conjg(Aa4)*ZA(gt3,3)*ZA(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3Dash*ZA(gt3,3)*ZA(gt4,3))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-6*Lam1Dash*ZA(gt3,3)*ZA(gt4,3)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhUAhAhAhL  
 
 
Subroutine CouplingUAhUAhhhhhL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,          & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhUAhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-2*Lam1*ZH(gt3,1)*ZH(gt4,1)
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam4*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam2Dash*ZH(gt3,1)*ZH(gt4,1))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa3*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa4*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa4)*ZH(gt3,2)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa3*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa4*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa3*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa4*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,1))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa3*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa4*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa4)*ZH(gt3,1)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(Lam3*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(Lam4*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-2*Lam2*ZH(gt3,2)*ZH(gt4,2)
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam3Dash*ZH(gt3,2)*ZH(gt4,2))
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa3*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa4*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa3*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa4*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,2))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa3*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa4*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa3*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa4*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,1)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Aa3*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Aa4*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(3.eq.gt1)) Then 
res = res+(Conjg(Aa4)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Aa3*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Aa4*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(3.eq.gt2)) Then 
res = res+(Conjg(Aa4)*ZH(gt3,2)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(Lam2Dash*ZH(gt3,3)*ZH(gt4,3))
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Aa3*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Aa4*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Aa3*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Aa4*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Conjg(Aa3)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Conjg(Aa4)*ZH(gt3,3)*ZH(gt4,3))/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3Dash*ZH(gt3,3)*ZH(gt4,3))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-2*Lam1Dash*ZH(gt3,3)*ZH(gt4,3)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhUAhhhhhL  
 
 
Subroutine CouplingUAhUAhHmcHmL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,         & 
& Lam3Dash,Aa3,Aa4,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhUAhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-2*Lam1*Conjg(ZP(gt3,1))*ZP(gt4,1)
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam4*Conjg(ZP(gt3,2))*ZP(gt4,1))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam4*Conjg(ZP(gt3,2))*ZP(gt4,1))/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam3*Conjg(ZP(gt3,1))*ZP(gt4,1))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam2Dash*Conjg(ZP(gt3,1))*ZP(gt4,1))
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+Conjg(Aa3)*Conjg(ZP(gt3,2))*ZP(gt4,1)
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+Conjg(Aa4)*Conjg(ZP(gt3,2))*ZP(gt4,1)
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(Lam3*Conjg(ZP(gt3,2))*ZP(gt4,2))
End If 
If ((1.eq.gt2).And.(2.eq.gt1)) Then 
res = res-(Lam4*Conjg(ZP(gt3,1))*ZP(gt4,2))/2._dp
End If 
If ((1.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(Lam4*Conjg(ZP(gt3,1))*ZP(gt4,2))/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-2*Lam2*Conjg(ZP(gt3,2))*ZP(gt4,2)
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+Aa3*Conjg(ZP(gt3,1))*ZP(gt4,2)
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res+Aa4*Conjg(ZP(gt3,1))*ZP(gt4,2)
End If 
If ((3.eq.gt1).And.(3.eq.gt2)) Then 
res = res-(Lam3Dash*Conjg(ZP(gt3,2))*ZP(gt4,2))
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhUAhHmcHmL  
 
 
Subroutine CouplingUAhUAhVPVPL(gt1,gt2,g1,g2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhUAhVPVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,1)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,1)**2)/2._dp
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(g1*g2*ZZ(1,1)*ZZ(2,1))
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(g1*g2*ZZ(1,1)*ZZ(2,1))
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,1)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,1)**2)/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhUAhVPVPL  
 
 
Subroutine CouplingUAhUAhcVWmVWmL(gt1,gt2,g2,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhUAhcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+g2**2/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+g2**2/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhUAhcVWmVWmL  
 
 
Subroutine CouplingUAhUAhVZVZL(gt1,gt2,g1,g2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUAhUAhVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,2)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,2)**2)/2._dp
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res-(g1*g2*ZZ(1,2)*ZZ(2,2))
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res-(g1*g2*ZZ(1,2)*ZZ(2,2))
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,2)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,2)**2)/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUAhUAhVZVZL  
 
 
Subroutine CouplingAhHmcUHmL(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,               & 
& ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3)

Complex(dp), Intent(in) :: Lam4,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhHmcUHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res-(Lam4*v2*Conjg(ZP(gt2,2))*ZA(gt1,1))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(Lam4*v2*Conjg(ZP(gt2,1))*ZA(gt1,1))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(Lam4*v1*Conjg(ZP(gt2,2))*ZA(gt1,2))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(Lam4*v1*Conjg(ZP(gt2,1))*ZA(gt1,2))/2._dp
End If 
If ((1.eq.gt3)) Then 
res = res-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZA(gt1,3))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
res = res+(Conjg(Aa2)*Conjg(ZP(gt2,2))*ZA(gt1,3))/sqrt(2._dp)
End If 
If ((1.eq.gt3)) Then 
res = res-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZA(gt1,3))
End If 
If ((1.eq.gt3)) Then 
res = res+v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZA(gt1,3)
End If 
If ((2.eq.gt3)) Then 
res = res+(Aa1*Conjg(ZP(gt2,1))*ZA(gt1,3))/sqrt(2._dp)
End If 
If ((2.eq.gt3)) Then 
res = res-((Aa2*Conjg(ZP(gt2,1))*ZA(gt1,3))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
res = res+Aa3*v3*Conjg(ZP(gt2,1))*ZA(gt1,3)
End If 
If ((2.eq.gt3)) Then 
res = res-(Aa4*v3*Conjg(ZP(gt2,1))*ZA(gt1,3))
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhHmcUHmL  
 
 
Subroutine CouplingAhcUHmVWmL(gt1,gt2,g2,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZA(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhcUHmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt2)) Then 
res = res-(g2*ZA(gt1,1))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(g2*ZA(gt1,2))/2._dp
End If 
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhcUHmVWmL  
 
 
Subroutine CouplingcFuFdcUHmL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,              & 
& Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZDL,ZDR,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,              & 
& Y1u22,Y2u33,ZDL(3,3),ZDR(3,3),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFdcUHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt3)) Then 
resL = resL-(Y1u11*Conjg(ZUR(gt1,1))*Conjg(ZDL(gt2,1)))
End If 
If ((1.eq.gt3)) Then 
resL = resL-(Y1u12*Conjg(ZUR(gt1,2))*Conjg(ZDL(gt2,1)))
End If 
If ((1.eq.gt3)) Then 
resL = resL-(Y1u21*Conjg(ZUR(gt1,1))*Conjg(ZDL(gt2,2)))
End If 
If ((1.eq.gt3)) Then 
resL = resL-(Y1u22*Conjg(ZUR(gt1,2))*Conjg(ZDL(gt2,2)))
End If 
If ((2.eq.gt3)) Then 
resL = resL-(Y2u33*Conjg(ZUR(gt1,3))*Conjg(ZDL(gt2,3)))
End If 
resR = 0._dp 
If ((1.eq.gt3)) Then 
resR = resR-(Conjg(Y1d11)*ZDR(gt2,1)*ZUL(gt1,1))
End If 
If ((1.eq.gt3)) Then 
resR = resR-(Conjg(Y1d12)*ZDR(gt2,2)*ZUL(gt1,1))
End If 
If ((1.eq.gt3)) Then 
resR = resR-(Conjg(Y1d13)*ZDR(gt2,3)*ZUL(gt1,1))
End If 
If ((1.eq.gt3)) Then 
resR = resR-(Conjg(Y1d21)*ZDR(gt2,1)*ZUL(gt1,2))
End If 
If ((1.eq.gt3)) Then 
resR = resR-(Conjg(Y1d22)*ZDR(gt2,2)*ZUL(gt1,2))
End If 
If ((1.eq.gt3)) Then 
resR = resR-(Conjg(Y1d23)*ZDR(gt2,3)*ZUL(gt1,2))
End If 
If ((2.eq.gt3)) Then 
resR = resR-(Conjg(Y2d31)*ZDR(gt2,1)*ZUL(gt1,3))
End If 
If ((2.eq.gt3)) Then 
resR = resR-(Conjg(Y2d32)*ZDR(gt2,2)*ZUL(gt1,3))
End If 
If ((2.eq.gt3)) Then 
resR = resR-(Conjg(Y2d33)*ZDR(gt2,3)*ZUL(gt1,3))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFdcUHmL  
 
 
Subroutine CouplingFvFecUHmL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZEL,ZER,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZEL(3,3),ZER(3,3),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFecUHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt3)) Then 
resL = resL-(Y1n11*Conjg(ZEL(gt2,1))*Conjg(Vv(gt1,4)))
End If 
If ((1.eq.gt3)) Then 
resL = resL-(Y1n21*Conjg(ZEL(gt2,2))*Conjg(Vv(gt1,4)))
End If 
If ((1.eq.gt3)) Then 
resL = resL-(Y1n12*Conjg(ZEL(gt2,1))*Conjg(Vv(gt1,5)))
End If 
If ((1.eq.gt3)) Then 
resL = resL-(Y1n22*Conjg(ZEL(gt2,2))*Conjg(Vv(gt1,5)))
End If 
If ((2.eq.gt3)) Then 
resL = resL-(Y2n33*Conjg(ZEL(gt2,3))*Conjg(Vv(gt1,6)))
End If 
resR = 0._dp 
If ((1.eq.gt3)) Then 
resR = resR-(Conjg(Y1e11)*ZER(gt2,1)*Vv(gt1,1))
End If 
If ((1.eq.gt3)) Then 
resR = resR-(Conjg(Y1e12)*ZER(gt2,2)*Vv(gt1,1))
End If 
If ((1.eq.gt3)) Then 
resR = resR-(Conjg(Y1e21)*ZER(gt2,1)*Vv(gt1,2))
End If 
If ((1.eq.gt3)) Then 
resR = resR-(Conjg(Y1e22)*ZER(gt2,2)*Vv(gt1,2))
End If 
If ((2.eq.gt3)) Then 
resR = resR-(Conjg(Y2e33)*ZER(gt2,3)*Vv(gt1,3))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFecUHmL  
 
 
Subroutine CouplingcgZgWmcUHmL(gt3,g1,g2,v1,v2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgZgWmcUHm' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res-(g1*g2*v1*Conjg(ZZ(1,2))*RXiZ)/8._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(g2**2*v1*Conjg(ZZ(2,2))*RXiZ)/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g1*g2*v2*Conjg(ZZ(1,2))*RXiZ)/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(g2**2*v2*Conjg(ZZ(2,2))*RXiZ)/8._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(g1*g2*v1*RXiZ*ZZ(1,2))/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g1*g2*v2*RXiZ*ZZ(1,2))/8._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(g2**2*v1*RXiZ*ZZ(2,2))/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(g2**2*v2*RXiZ*ZZ(2,2))/8._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgZgWmcUHmL  
 
 
Subroutine CouplingcgWmgZUHmL(gt3,g1,g2,v1,v2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgZUHm' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res-(g1*g2*v1*Conjg(ZZ(1,2))*RXiWm)/4._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(g2**2*v1*Conjg(ZZ(2,2))*RXiWm)/4._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g1*g2*v2*Conjg(ZZ(1,2))*RXiWm)/4._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g2**2*v2*Conjg(ZZ(2,2))*RXiWm)/4._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgZUHmL  
 
 
Subroutine CouplingcgWpgZcUHmL(gt3,g1,g2,v1,v2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgZcUHm' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res-(g1*g2*v1*RXiWm*ZZ(1,2))/4._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g1*g2*v2*RXiWm*ZZ(1,2))/4._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(g2**2*v1*RXiWm*ZZ(2,2))/4._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g2**2*v2*RXiWm*ZZ(2,2))/4._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgZcUHmL  
 
 
Subroutine CouplingcgZgWpUHmL(gt3,g1,g2,v1,v2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt3
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgZgWpUHm' 
 
If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res-(g1*g2*v1*Conjg(ZZ(1,2))*RXiZ)/8._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(g2**2*v1*Conjg(ZZ(2,2))*RXiZ)/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g1*g2*v2*Conjg(ZZ(1,2))*RXiZ)/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(g2**2*v2*Conjg(ZZ(2,2))*RXiZ)/8._dp
End If 
If ((1.eq.gt3)) Then 
res = res-(g1*g2*v1*RXiZ*ZZ(1,2))/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(g1*g2*v2*RXiZ*ZZ(1,2))/8._dp
End If 
If ((1.eq.gt3)) Then 
res = res+(g2**2*v1*RXiZ*ZZ(2,2))/8._dp
End If 
If ((2.eq.gt3)) Then 
res = res+(g2**2*v2*RXiZ*ZZ(2,2))/8._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgZgWpUHmL  
 
 
Subroutine CouplinghhHmcUHmL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,       & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhHmcUHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3)) Then 
res = res-2*Lam1*v1*Conjg(ZP(gt2,1))*ZH(gt1,1)
End If 
If ((1.eq.gt3)) Then 
res = res-(Lam4*v2*Conjg(ZP(gt2,2))*ZH(gt1,1))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(Lam4*v2*Conjg(ZP(gt2,1))*ZH(gt1,1))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(Lam3*v1*Conjg(ZP(gt2,2))*ZH(gt1,1))
End If 
If ((1.eq.gt3)) Then 
res = res-(Lam3*v2*Conjg(ZP(gt2,1))*ZH(gt1,2))
End If 
If ((1.eq.gt3)) Then 
res = res-(Lam4*v1*Conjg(ZP(gt2,2))*ZH(gt1,2))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res-(Lam4*v1*Conjg(ZP(gt2,1))*ZH(gt1,2))/2._dp
End If 
If ((2.eq.gt3)) Then 
res = res-2*Lam2*v2*Conjg(ZP(gt2,2))*ZH(gt1,2)
End If 
If ((1.eq.gt3)) Then 
res = res-(Lam2Dash*v3*Conjg(ZP(gt2,1))*ZH(gt1,3))
End If 
If ((1.eq.gt3)) Then 
res = res-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZH(gt1,3))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
res = res-((Conjg(Aa2)*Conjg(ZP(gt2,2))*ZH(gt1,3))/sqrt(2._dp))
End If 
If ((1.eq.gt3)) Then 
res = res-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZH(gt1,3))
End If 
If ((1.eq.gt3)) Then 
res = res-(v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZH(gt1,3))
End If 
If ((2.eq.gt3)) Then 
res = res-((Aa1*Conjg(ZP(gt2,1))*ZH(gt1,3))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
res = res-((Aa2*Conjg(ZP(gt2,1))*ZH(gt1,3))/sqrt(2._dp))
End If 
If ((2.eq.gt3)) Then 
res = res-(Aa3*v3*Conjg(ZP(gt2,1))*ZH(gt1,3))
End If 
If ((2.eq.gt3)) Then 
res = res-(Aa4*v3*Conjg(ZP(gt2,1))*ZH(gt1,3))
End If 
If ((2.eq.gt3)) Then 
res = res-(Lam3Dash*v3*Conjg(ZP(gt2,2))*ZH(gt1,3))
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhHmcUHmL  
 
 
Subroutine CouplinghhcUHmVWmL(gt1,gt2,g2,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZH(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhcUHmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt2)) Then 
res = res-(g2*ZH(gt1,1))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(g2*ZH(gt1,2))/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhcUHmVWmL  
 
 
Subroutine CouplingHmcUHmVPL(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcUHmVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt2)) Then 
res = res-(g1*Conjg(ZP(gt1,1))*ZZ(1,1))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(g1*Conjg(ZP(gt1,2))*ZZ(1,1))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res-(g2*Conjg(ZP(gt1,1))*ZZ(2,1))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(g2*Conjg(ZP(gt1,2))*ZZ(2,1))/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcUHmVPL  
 
 
Subroutine CouplingHmcUHmVZL(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcUHmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt2)) Then 
res = res-(g1*Conjg(ZP(gt1,1))*ZZ(1,2))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(g1*Conjg(ZP(gt1,2))*ZZ(1,2))/2._dp
End If 
If ((1.eq.gt2)) Then 
res = res-(g2*Conjg(ZP(gt1,1))*ZZ(2,2))/2._dp
End If 
If ((2.eq.gt2)) Then 
res = res-(g2*Conjg(ZP(gt1,2))*ZZ(2,2))/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcUHmVZL  
 
 
Subroutine CouplingcUHmVPVWmL(gt1,g1,g2,v1,v2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUHmVPVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res+(g1*g2*v1*ZZ(1,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(g1*g2*v2*ZZ(1,1))/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUHmVPVWmL  
 
 
Subroutine CouplingcUHmVWmVZL(gt1,g1,g2,v1,v2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUHmVWmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1)) Then 
res = res+(g1*g2*v1*ZZ(1,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
res = res+(g1*g2*v2*ZZ(1,2))/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUHmVWmVZL  
 
 
Subroutine CouplingAhAhUHmcUHmL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,         & 
& Lam3Dash,Aa3,Aa4,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhUHmcUHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3).And.(1.eq.gt4)) Then 
res = res-2*Lam1*ZA(gt1,1)*ZA(gt2,1)
End If 
If ((2.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,1)*ZA(gt2,1))
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,1))/2._dp
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,2)*ZA(gt2,1))/2._dp
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Lam4*ZA(gt1,1)*ZA(gt2,2))/2._dp
End If 
If ((1.eq.gt3).And.(1.eq.gt4)) Then 
res = res-(Lam3*ZA(gt1,2)*ZA(gt2,2))
End If 
If ((2.eq.gt3).And.(2.eq.gt4)) Then 
res = res-2*Lam2*ZA(gt1,2)*ZA(gt2,2)
End If 
If ((1.eq.gt3).And.(1.eq.gt4)) Then 
res = res-(Lam2Dash*ZA(gt1,3)*ZA(gt2,3))
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res+Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res+Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res+Aa3*ZA(gt1,3)*ZA(gt2,3)
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res+Aa4*ZA(gt1,3)*ZA(gt2,3)
End If 
If ((2.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Lam3Dash*ZA(gt1,3)*ZA(gt2,3))
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhUHmcUHmL  
 
 
Subroutine CouplinghhhhUHmcUHmL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,         & 
& Lam3Dash,Aa3,Aa4,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhUHmcUHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt3).And.(1.eq.gt4)) Then 
res = res-2*Lam1*ZH(gt1,1)*ZH(gt2,1)
End If 
If ((2.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Lam3*ZH(gt1,1)*ZH(gt2,1))
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res-(Lam4*ZH(gt1,2)*ZH(gt2,1))/2._dp
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Lam4*ZH(gt1,2)*ZH(gt2,1))/2._dp
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res-(Lam4*ZH(gt1,1)*ZH(gt2,2))/2._dp
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Lam4*ZH(gt1,1)*ZH(gt2,2))/2._dp
End If 
If ((1.eq.gt3).And.(1.eq.gt4)) Then 
res = res-(Lam3*ZH(gt1,2)*ZH(gt2,2))
End If 
If ((2.eq.gt3).And.(2.eq.gt4)) Then 
res = res-2*Lam2*ZH(gt1,2)*ZH(gt2,2)
End If 
If ((1.eq.gt3).And.(1.eq.gt4)) Then 
res = res-(Lam2Dash*ZH(gt1,3)*ZH(gt2,3))
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3))
End If 
If ((1.eq.gt4).And.(2.eq.gt3)) Then 
res = res-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3))
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Aa3*ZH(gt1,3)*ZH(gt2,3))
End If 
If ((1.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Aa4*ZH(gt1,3)*ZH(gt2,3))
End If 
If ((2.eq.gt3).And.(2.eq.gt4)) Then 
res = res-(Lam3Dash*ZH(gt1,3)*ZH(gt2,3))
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhUHmcUHmL  
 
 
Subroutine CouplingUHmHmcUHmcHmL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUHmHmcUHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt3)) Then 
res = res-4*Lam1*Conjg(ZP(gt2,1))*ZP(gt4,1)
End If 
If ((1.eq.gt1).And.(2.eq.gt3)) Then 
res = res-(Lam3*Conjg(ZP(gt2,2))*ZP(gt4,1))
End If 
If ((1.eq.gt1).And.(2.eq.gt3)) Then 
res = res-(Lam4*Conjg(ZP(gt2,2))*ZP(gt4,1))
End If 
If ((2.eq.gt1).And.(2.eq.gt3)) Then 
res = res-(Lam3*Conjg(ZP(gt2,1))*ZP(gt4,1))
End If 
If ((2.eq.gt1).And.(2.eq.gt3)) Then 
res = res-(Lam4*Conjg(ZP(gt2,1))*ZP(gt4,1))
End If 
If ((1.eq.gt1).And.(1.eq.gt3)) Then 
res = res-(Lam3*Conjg(ZP(gt2,2))*ZP(gt4,2))
End If 
If ((1.eq.gt1).And.(1.eq.gt3)) Then 
res = res-(Lam4*Conjg(ZP(gt2,2))*ZP(gt4,2))
End If 
If ((1.eq.gt3).And.(2.eq.gt1)) Then 
res = res-(Lam3*Conjg(ZP(gt2,1))*ZP(gt4,2))
End If 
If ((1.eq.gt3).And.(2.eq.gt1)) Then 
res = res-(Lam4*Conjg(ZP(gt2,1))*ZP(gt4,2))
End If 
If ((2.eq.gt1).And.(2.eq.gt3)) Then 
res = res-4*Lam2*Conjg(ZP(gt2,2))*ZP(gt4,2)
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUHmHmcUHmcHmL  
 
 
Subroutine CouplingUHmcUHmVPVPL(gt1,gt2,g1,g2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUHmcUHmVPVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,1)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,1)**2)/2._dp
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+g1*g2*ZZ(1,1)*ZZ(2,1)
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+g1*g2*ZZ(1,1)*ZZ(2,1)
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,1)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,1)**2)/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUHmcUHmVPVPL  
 
 
Subroutine CouplingUHmcUHmcVWmVWmL(gt1,gt2,g2,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUHmcUHmcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+g2**2/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+g2**2/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUHmcUHmcVWmVWmL  
 
 
Subroutine CouplingUHmcUHmVZVZL(gt1,gt2,g1,g2,ZZ,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUHmcUHmVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,2)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g1**2*ZZ(1,2)**2)/2._dp
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+g1*g2*ZZ(1,2)*ZZ(2,2)
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+g1*g2*ZZ(1,2)*ZZ(2,2)
End If 
If ((1.eq.gt1).And.(1.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,2)**2)/2._dp
End If 
If ((2.eq.gt1).And.(2.eq.gt2)) Then 
res = res+(g2**2*ZZ(2,2)**2)/2._dp
End If 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUHmcUHmVZVZL  
 
 
Subroutine CouplingcUFdFdAhL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,               & 
& Y1d23,Y2d31,Y2d32,Y2d33,ZA,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFdFdAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-((Y1d11*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
resL = resL-((Y1d21*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1d12*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1d22*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((Y1d13*Conjg(ZDL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((Y1d23*Conjg(ZDL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
resL = resL-((Y2d31*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y2d32*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((Y2d33*Conjg(ZDL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1d11)*ZDR(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1d21)*ZDR(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1d12)*ZDR(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1d22)*ZDR(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1d13)*ZDR(gt2,3)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1d23)*ZDR(gt2,3)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
resR = resR+(Conjg(Y2d31)*ZDR(gt2,1)*ZA(gt3,2))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
resR = resR+(Conjg(Y2d32)*ZDR(gt2,2)*ZA(gt3,2))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
resR = resR+(Conjg(Y2d33)*ZDR(gt2,3)*ZA(gt3,2))/sqrt(2._dp)
End If 
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFdFdAhL  
 
 
Subroutine CouplingcUFdFdhhL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,               & 
& Y1d23,Y2d31,Y2d32,Y2d33,ZH,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFdFdhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-((Y1d11*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
resL = resL-((Y1d21*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1d12*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1d22*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((Y1d13*Conjg(ZDL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((Y1d23*Conjg(ZDL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
resL = resL-((Y2d31*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y2d32*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((Y2d33*Conjg(ZDL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR-((Conjg(Y1d11)*ZDR(gt2,1)*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resR = resR-((Conjg(Y1d21)*ZDR(gt2,1)*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
resR = resR-((Conjg(Y1d12)*ZDR(gt2,2)*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resR = resR-((Conjg(Y1d22)*ZDR(gt2,2)*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
resR = resR-((Conjg(Y1d13)*ZDR(gt2,3)*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resR = resR-((Conjg(Y1d23)*ZDR(gt2,3)*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resR = resR-((Conjg(Y2d31)*ZDR(gt2,1)*ZH(gt3,2))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resR = resR-((Conjg(Y2d32)*ZDR(gt2,2)*ZH(gt3,2))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resR = resR-((Conjg(Y2d33)*ZDR(gt2,3)*ZH(gt3,2))/sqrt(2._dp))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFdFdhhL  
 
 
Subroutine CouplingcUFdFdVGL(gt1,gt2,g3,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3

Complex(dp), Intent(in) :: ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFdFdVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-(g3*Conjg(ZDL(gt2,1)))
End If 
If ((2.eq.gt1)) Then 
resL = resL-(g3*Conjg(ZDL(gt2,2)))
End If 
If ((3.eq.gt1)) Then 
resL = resL-(g3*Conjg(ZDL(gt2,3)))
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR-(g3*ZDR(gt2,1))
End If 
If ((2.eq.gt1)) Then 
resR = resR-(g3*ZDR(gt2,2))
End If 
If ((3.eq.gt1)) Then 
resR = resR-(g3*ZDR(gt2,3))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFdFdVGL  
 
 
Subroutine CouplingcUFdFdVPL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFdFdVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZDL(gt2,1))*ZZ(1,1))/6._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZDL(gt2,2))*ZZ(1,1))/6._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZDL(gt2,3))*ZZ(1,1))/6._dp
End If 
If ((1.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZDL(gt2,1))*ZZ(2,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZDL(gt2,2))*ZZ(2,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZDL(gt2,3))*ZZ(2,1))/2._dp
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR+(g1*ZDR(gt2,1)*ZZ(1,1))/3._dp
End If 
If ((2.eq.gt1)) Then 
resR = resR+(g1*ZDR(gt2,2)*ZZ(1,1))/3._dp
End If 
If ((3.eq.gt1)) Then 
resR = resR+(g1*ZDR(gt2,3)*ZZ(1,1))/3._dp
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFdFdVPL  
 
 
Subroutine CouplingcUFdFdVZL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFdFdVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZDL(gt2,1))*ZZ(1,2))/6._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZDL(gt2,2))*ZZ(1,2))/6._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZDL(gt2,3))*ZZ(1,2))/6._dp
End If 
If ((1.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZDL(gt2,1))*ZZ(2,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZDL(gt2,2))*ZZ(2,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZDL(gt2,3))*ZZ(2,2))/2._dp
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR+(g1*ZDR(gt2,1)*ZZ(1,2))/3._dp
End If 
If ((2.eq.gt1)) Then 
resR = resR+(g1*ZDR(gt2,2)*ZZ(1,2))/3._dp
End If 
If ((3.eq.gt1)) Then 
resR = resR+(g1*ZDR(gt2,3)*ZZ(1,2))/3._dp
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFdFdVZL  
 
 
Subroutine CouplingcUFdFuHmL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,               & 
& Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,              & 
& Y1u22,Y2u33,ZP(2,2),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFdFuHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-(Y1d11*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
End If 
If ((1.eq.gt1)) Then 
resL = resL-(Y1d21*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
End If 
If ((1.eq.gt1)) Then 
resL = resL-(Y2d31*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
End If 
If ((2.eq.gt1)) Then 
resL = resL-(Y1d12*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
End If 
If ((2.eq.gt1)) Then 
resL = resL-(Y1d22*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
End If 
If ((2.eq.gt1)) Then 
resL = resL-(Y2d32*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
End If 
If ((3.eq.gt1)) Then 
resL = resL-(Y1d13*Conjg(ZUL(gt2,1))*Conjg(ZP(gt3,1)))
End If 
If ((3.eq.gt1)) Then 
resL = resL-(Y1d23*Conjg(ZUL(gt2,2))*Conjg(ZP(gt3,1)))
End If 
If ((3.eq.gt1)) Then 
resL = resL-(Y2d33*Conjg(ZUL(gt2,3))*Conjg(ZP(gt3,2)))
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR-(Conjg(Y1u11)*Conjg(ZP(gt3,1))*ZUR(gt2,1))
End If 
If ((2.eq.gt1)) Then 
resR = resR-(Conjg(Y1u21)*Conjg(ZP(gt3,1))*ZUR(gt2,1))
End If 
If ((1.eq.gt1)) Then 
resR = resR-(Conjg(Y1u12)*Conjg(ZP(gt3,1))*ZUR(gt2,2))
End If 
If ((2.eq.gt1)) Then 
resR = resR-(Conjg(Y1u22)*Conjg(ZP(gt3,1))*ZUR(gt2,2))
End If 
If ((3.eq.gt1)) Then 
resR = resR-(Conjg(Y2u33)*Conjg(ZP(gt3,2))*ZUR(gt2,3))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFdFuHmL  
 
 
Subroutine CouplingcUFdFuVWmL(gt1,gt2,g2,ZUL,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZUL(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFdFuVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-((g2*Conjg(ZUL(gt2,1)))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((g2*Conjg(ZUL(gt2,2)))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((g2*Conjg(ZUL(gt2,3)))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFdFuVWmL  
 
 
Subroutine CouplingcUFuFuAhL(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,               & 
& ZA,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFuFuAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-((Y1u11*Conjg(ZUL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
resL = resL-((Y1u21*Conjg(ZUL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1u12*Conjg(ZUL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1u22*Conjg(ZUL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((Y2u33*Conjg(ZUL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1u11)*ZUR(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1u21)*ZUR(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1u12)*ZUR(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1u22)*ZUR(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
resR = resR+(Conjg(Y2u33)*ZUR(gt2,3)*ZA(gt3,2))/sqrt(2._dp)
End If 
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFuFuAhL  
 
 
Subroutine CouplingcUFuFdcHmL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,              & 
& Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,              & 
& Y1u22,Y2u33,ZP(2,2),ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFuFdcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-(Y1u11*Conjg(ZDL(gt2,1))*ZP(gt3,1))
End If 
If ((1.eq.gt1)) Then 
resL = resL-(Y1u21*Conjg(ZDL(gt2,2))*ZP(gt3,1))
End If 
If ((2.eq.gt1)) Then 
resL = resL-(Y1u12*Conjg(ZDL(gt2,1))*ZP(gt3,1))
End If 
If ((2.eq.gt1)) Then 
resL = resL-(Y1u22*Conjg(ZDL(gt2,2))*ZP(gt3,1))
End If 
If ((3.eq.gt1)) Then 
resL = resL-(Y2u33*Conjg(ZDL(gt2,3))*ZP(gt3,2))
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR-(Conjg(Y1d11)*ZDR(gt2,1)*ZP(gt3,1))
End If 
If ((2.eq.gt1)) Then 
resR = resR-(Conjg(Y1d21)*ZDR(gt2,1)*ZP(gt3,1))
End If 
If ((1.eq.gt1)) Then 
resR = resR-(Conjg(Y1d12)*ZDR(gt2,2)*ZP(gt3,1))
End If 
If ((2.eq.gt1)) Then 
resR = resR-(Conjg(Y1d22)*ZDR(gt2,2)*ZP(gt3,1))
End If 
If ((1.eq.gt1)) Then 
resR = resR-(Conjg(Y1d13)*ZDR(gt2,3)*ZP(gt3,1))
End If 
If ((2.eq.gt1)) Then 
resR = resR-(Conjg(Y1d23)*ZDR(gt2,3)*ZP(gt3,1))
End If 
If ((3.eq.gt1)) Then 
resR = resR-(Conjg(Y2d31)*ZDR(gt2,1)*ZP(gt3,2))
End If 
If ((3.eq.gt1)) Then 
resR = resR-(Conjg(Y2d32)*ZDR(gt2,2)*ZP(gt3,2))
End If 
If ((3.eq.gt1)) Then 
resR = resR-(Conjg(Y2d33)*ZDR(gt2,3)*ZP(gt3,2))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFuFdcHmL  
 
 
Subroutine CouplingcUFuFdcVWmL(gt1,gt2,g2,ZDL,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZDL(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFuFdcVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-((g2*Conjg(ZDL(gt2,1)))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((g2*Conjg(ZDL(gt2,2)))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((g2*Conjg(ZDL(gt2,3)))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFuFdcVWmL  
 
 
Subroutine CouplingcUFuFuhhL(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,               & 
& ZH,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFuFuhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL+(Y1u11*Conjg(ZUL(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((1.eq.gt1)) Then 
resL = resL+(Y1u21*Conjg(ZUL(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resL = resL+(Y1u12*Conjg(ZUL(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resL = resL+(Y1u22*Conjg(ZUL(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
resL = resL+(Y2u33*Conjg(ZUL(gt2,3))*ZH(gt3,2))/sqrt(2._dp)
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1u11)*ZUR(gt2,1)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1u21)*ZUR(gt2,1)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1u12)*ZUR(gt2,2)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1u22)*ZUR(gt2,2)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
resR = resR+(Conjg(Y2u33)*ZUR(gt2,3)*ZH(gt3,2))/sqrt(2._dp)
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFuFuhhL  
 
 
Subroutine CouplingcUFuFuVGL(gt1,gt2,g3,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3

Complex(dp), Intent(in) :: ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFuFuVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-(g3*Conjg(ZUL(gt2,1)))
End If 
If ((2.eq.gt1)) Then 
resL = resL-(g3*Conjg(ZUL(gt2,2)))
End If 
If ((3.eq.gt1)) Then 
resL = resL-(g3*Conjg(ZUL(gt2,3)))
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR-(g3*ZUR(gt2,1))
End If 
If ((2.eq.gt1)) Then 
resR = resR-(g3*ZUR(gt2,2))
End If 
If ((3.eq.gt1)) Then 
resR = resR-(g3*ZUR(gt2,3))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFuFuVGL  
 
 
Subroutine CouplingcUFuFuVPL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFuFuVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZUL(gt2,1))*ZZ(1,1))/6._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZUL(gt2,2))*ZZ(1,1))/6._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZUL(gt2,3))*ZZ(1,1))/6._dp
End If 
If ((1.eq.gt1)) Then 
resL = resL-(g2*Conjg(ZUL(gt2,1))*ZZ(2,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL-(g2*Conjg(ZUL(gt2,2))*ZZ(2,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL-(g2*Conjg(ZUL(gt2,3))*ZZ(2,1))/2._dp
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR+(-2*g1*ZUR(gt2,1)*ZZ(1,1))/3._dp
End If 
If ((2.eq.gt1)) Then 
resR = resR+(-2*g1*ZUR(gt2,2)*ZZ(1,1))/3._dp
End If 
If ((3.eq.gt1)) Then 
resR = resR+(-2*g1*ZUR(gt2,3)*ZZ(1,1))/3._dp
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFuFuVPL  
 
 
Subroutine CouplingcUFuFuVZL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFuFuVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZUL(gt2,1))*ZZ(1,2))/6._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZUL(gt2,2))*ZZ(1,2))/6._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL-(g1*Conjg(ZUL(gt2,3))*ZZ(1,2))/6._dp
End If 
If ((1.eq.gt1)) Then 
resL = resL-(g2*Conjg(ZUL(gt2,1))*ZZ(2,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL-(g2*Conjg(ZUL(gt2,2))*ZZ(2,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL-(g2*Conjg(ZUL(gt2,3))*ZZ(2,2))/2._dp
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR+(-2*g1*ZUR(gt2,1)*ZZ(1,2))/3._dp
End If 
If ((2.eq.gt1)) Then 
resR = resR+(-2*g1*ZUR(gt2,2)*ZZ(1,2))/3._dp
End If 
If ((3.eq.gt1)) Then 
resR = resR+(-2*g1*ZUR(gt2,3)*ZZ(1,2))/3._dp
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFuFuVZL  
 
 
Subroutine CouplingcUFeFeAhL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& ZA,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFeFeAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-((Y1e11*Conjg(ZEL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
resL = resL-((Y1e21*Conjg(ZEL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1e12*Conjg(ZEL(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1e22*Conjg(ZEL(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((Y2e33*Conjg(ZEL(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1e11)*ZER(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1e21)*ZER(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1e12)*ZER(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1e22)*ZER(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
resR = resR+(Conjg(Y2e33)*ZER(gt2,3)*ZA(gt3,2))/sqrt(2._dp)
End If 
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFeFeAhL  
 
 
Subroutine CouplingcUFeFehhL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& ZH,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFeFehh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-((Y1e11*Conjg(ZEL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
resL = resL-((Y1e21*Conjg(ZEL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1e12*Conjg(ZEL(gt2,1))*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1e22*Conjg(ZEL(gt2,2))*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((Y2e33*Conjg(ZEL(gt2,3))*ZH(gt3,2))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR-((Conjg(Y1e11)*ZER(gt2,1)*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resR = resR-((Conjg(Y1e21)*ZER(gt2,1)*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
resR = resR-((Conjg(Y1e12)*ZER(gt2,2)*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resR = resR-((Conjg(Y1e22)*ZER(gt2,2)*ZH(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resR = resR-((Conjg(Y2e33)*ZER(gt2,3)*ZH(gt3,2))/sqrt(2._dp))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFeFehhL  
 
 
Subroutine CouplingcUFeFeVPL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFeFeVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL+(g1*Conjg(ZEL(gt2,1))*ZZ(1,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL+(g1*Conjg(ZEL(gt2,2))*ZZ(1,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL+(g1*Conjg(ZEL(gt2,3))*ZZ(1,1))/2._dp
End If 
If ((1.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZEL(gt2,1))*ZZ(2,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZEL(gt2,2))*ZZ(2,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZEL(gt2,3))*ZZ(2,1))/2._dp
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR+g1*ZER(gt2,1)*ZZ(1,1)
End If 
If ((2.eq.gt1)) Then 
resR = resR+g1*ZER(gt2,2)*ZZ(1,1)
End If 
If ((3.eq.gt1)) Then 
resR = resR+g1*ZER(gt2,3)*ZZ(1,1)
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFeFeVPL  
 
 
Subroutine CouplingcUFeFeVZL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFeFeVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL+(g1*Conjg(ZEL(gt2,1))*ZZ(1,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL+(g1*Conjg(ZEL(gt2,2))*ZZ(1,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL+(g1*Conjg(ZEL(gt2,3))*ZZ(1,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZEL(gt2,1))*ZZ(2,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZEL(gt2,2))*ZZ(2,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL+(g2*Conjg(ZEL(gt2,3))*ZZ(2,2))/2._dp
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR+g1*ZER(gt2,1)*ZZ(1,2)
End If 
If ((2.eq.gt1)) Then 
resR = resR+g1*ZER(gt2,2)*ZZ(1,2)
End If 
If ((3.eq.gt1)) Then 
resR = resR+g1*ZER(gt2,3)*ZZ(1,2)
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFeFeVZL  
 
 
Subroutine CouplingcUFeFvHmL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP(2,2),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFeFvHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-(Y1e11*Conjg(Vv(gt2,1))*Conjg(ZP(gt3,1)))
End If 
If ((1.eq.gt1)) Then 
resL = resL-(Y1e21*Conjg(Vv(gt2,2))*Conjg(ZP(gt3,1)))
End If 
If ((2.eq.gt1)) Then 
resL = resL-(Y1e12*Conjg(Vv(gt2,1))*Conjg(ZP(gt3,1)))
End If 
If ((2.eq.gt1)) Then 
resL = resL-(Y1e22*Conjg(Vv(gt2,2))*Conjg(ZP(gt3,1)))
End If 
If ((3.eq.gt1)) Then 
resL = resL-(Y2e33*Conjg(Vv(gt2,3))*Conjg(ZP(gt3,2)))
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR-(Conjg(Y1n11)*Conjg(ZP(gt3,1))*Vv(gt2,4))
End If 
If ((2.eq.gt1)) Then 
resR = resR-(Conjg(Y1n21)*Conjg(ZP(gt3,1))*Vv(gt2,4))
End If 
If ((1.eq.gt1)) Then 
resR = resR-(Conjg(Y1n12)*Conjg(ZP(gt3,1))*Vv(gt2,5))
End If 
If ((2.eq.gt1)) Then 
resR = resR-(Conjg(Y1n22)*Conjg(ZP(gt3,1))*Vv(gt2,5))
End If 
If ((3.eq.gt1)) Then 
resR = resR-(Conjg(Y2n33)*Conjg(ZP(gt3,2))*Vv(gt2,6))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFeFvHmL  
 
 
Subroutine CouplingcUFeFvVWmL(gt1,gt2,g2,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcUFeFvVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-((g2*Conjg(Vv(gt2,1)))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((g2*Conjg(Vv(gt2,2)))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((g2*Conjg(Vv(gt2,3)))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcUFeFvVWmL  
 
 
Subroutine CouplingUFvFvAhL(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,            & 
& C23,C31,C32,BB11,BB12,BB21,BB22,ZA,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUFvFvAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-((Y1n11*Conjg(Vv(gt2,4))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((1.eq.gt1)) Then 
resL = resL-((Y1n12*Conjg(Vv(gt2,5))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1n21*Conjg(Vv(gt2,4))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((Y1n22*Conjg(Vv(gt2,5))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resL = resL-((Y1n11*Conjg(Vv(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resL = resL-((Y1n21*Conjg(Vv(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resL = resL-((Y1n12*Conjg(Vv(gt2,1))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resL = resL-((Y1n22*Conjg(Vv(gt2,2))*ZA(gt3,1))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((Y2n33*Conjg(Vv(gt2,6))*ZA(gt3,2))/sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resL = resL-((Y2n33*Conjg(Vv(gt2,3))*ZA(gt3,2))/sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resL = resL-((BB11*Conjg(Vv(gt2,4))*ZA(gt3,3))/sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resL = resL+(BB12*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resL = resL+(BB21*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resL = resL-(C13*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resL = resL-(C31*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resL = resL+(BB12*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resL = resL+(BB21*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resL = resL+(BB22*Conjg(Vv(gt2,5))*ZA(gt3,3))/sqrt(2._dp)
End If 
If ((5.eq.gt1)) Then 
resL = resL-(C23*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resL = resL-(C32*Conjg(Vv(gt2,6))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resL = resL-(C13*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resL = resL-(C31*Conjg(Vv(gt2,4))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resL = resL-(C23*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resL = resL-(C32*Conjg(Vv(gt2,5))*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
resR = 0._dp 
If ((4.eq.gt1)) Then 
resR = resR+(Conjg(Y1n11)*Vv(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((5.eq.gt1)) Then 
resR = resR+(Conjg(Y1n12)*Vv(gt2,1)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((4.eq.gt1)) Then 
resR = resR+(Conjg(Y1n21)*Vv(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((5.eq.gt1)) Then 
resR = resR+(Conjg(Y1n22)*Vv(gt2,2)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1n11)*Vv(gt2,4)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1n21)*Vv(gt2,4)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1n12)*Vv(gt2,5)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1n22)*Vv(gt2,5)*ZA(gt3,1))/sqrt(2._dp)
End If 
If ((6.eq.gt1)) Then 
resR = resR+(Conjg(Y2n33)*Vv(gt2,3)*ZA(gt3,2))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
resR = resR+(Conjg(Y2n33)*Vv(gt2,6)*ZA(gt3,2))/sqrt(2._dp)
End If 
If ((4.eq.gt1)) Then 
resR = resR+(Conjg(BB11)*Vv(gt2,4)*ZA(gt3,3))/sqrt(2._dp)
End If 
If ((5.eq.gt1)) Then 
resR = resR-(Conjg(BB12)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resR = resR-(Conjg(BB21)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resR = resR+(Conjg(C13)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resR = resR+(Conjg(C31)*Vv(gt2,4)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resR = resR-(Conjg(BB12)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resR = resR-(Conjg(BB21)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resR = resR-((Conjg(BB22)*Vv(gt2,5)*ZA(gt3,3))/sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resR = resR+(Conjg(C23)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resR = resR+(Conjg(C32)*Vv(gt2,5)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resR = resR+(Conjg(C13)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resR = resR+(Conjg(C31)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resR = resR+(Conjg(C23)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resR = resR+(Conjg(C32)*Vv(gt2,6)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
End If 
resL = -(0.,1.)*resL 
 
resR = -(0.,1.)*resR 
 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUFvFvAhL  
 
 
Subroutine CouplingUFvFecHmL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP(2,2),ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUFvFecHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((4.eq.gt1)) Then 
resL = resL-(Y1n11*Conjg(ZEL(gt2,1))*ZP(gt3,1))
End If 
If ((4.eq.gt1)) Then 
resL = resL-(Y1n21*Conjg(ZEL(gt2,2))*ZP(gt3,1))
End If 
If ((5.eq.gt1)) Then 
resL = resL-(Y1n12*Conjg(ZEL(gt2,1))*ZP(gt3,1))
End If 
If ((5.eq.gt1)) Then 
resL = resL-(Y1n22*Conjg(ZEL(gt2,2))*ZP(gt3,1))
End If 
If ((6.eq.gt1)) Then 
resL = resL-(Y2n33*Conjg(ZEL(gt2,3))*ZP(gt3,2))
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR-(Conjg(Y1e11)*ZER(gt2,1)*ZP(gt3,1))
End If 
If ((2.eq.gt1)) Then 
resR = resR-(Conjg(Y1e21)*ZER(gt2,1)*ZP(gt3,1))
End If 
If ((1.eq.gt1)) Then 
resR = resR-(Conjg(Y1e12)*ZER(gt2,2)*ZP(gt3,1))
End If 
If ((2.eq.gt1)) Then 
resR = resR-(Conjg(Y1e22)*ZER(gt2,2)*ZP(gt3,1))
End If 
If ((3.eq.gt1)) Then 
resR = resR-(Conjg(Y2e33)*ZER(gt2,3)*ZP(gt3,2))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUFvFecHmL  
 
 
Subroutine CouplingUFvFecVWmL(gt1,gt2,g2,ZEL,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZEL(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUFvFecVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL-((g2*Conjg(ZEL(gt2,1)))/sqrt(2._dp))
End If 
If ((2.eq.gt1)) Then 
resL = resL-((g2*Conjg(ZEL(gt2,2)))/sqrt(2._dp))
End If 
If ((3.eq.gt1)) Then 
resL = resL-((g2*Conjg(ZEL(gt2,3)))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUFvFecVWmL  
 
 
Subroutine CouplingUFvFvhhL(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,            & 
& C23,C31,C32,BB11,BB12,BB21,BB22,ZH,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUFvFvhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL+(Y1n11*Conjg(Vv(gt2,4))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((1.eq.gt1)) Then 
resL = resL+(Y1n12*Conjg(Vv(gt2,5))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resL = resL+(Y1n21*Conjg(Vv(gt2,4))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resL = resL+(Y1n22*Conjg(Vv(gt2,5))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((4.eq.gt1)) Then 
resL = resL+(Y1n11*Conjg(Vv(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((4.eq.gt1)) Then 
resL = resL+(Y1n21*Conjg(Vv(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((5.eq.gt1)) Then 
resL = resL+(Y1n12*Conjg(Vv(gt2,1))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((5.eq.gt1)) Then 
resL = resL+(Y1n22*Conjg(Vv(gt2,2))*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
resL = resL+(Y2n33*Conjg(Vv(gt2,6))*ZH(gt3,2))/sqrt(2._dp)
End If 
If ((6.eq.gt1)) Then 
resL = resL+(Y2n33*Conjg(Vv(gt2,3))*ZH(gt3,2))/sqrt(2._dp)
End If 
If ((4.eq.gt1)) Then 
resL = resL-((BB11*Conjg(Vv(gt2,4))*ZH(gt3,3))/sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resL = resL+(BB12*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resL = resL+(BB21*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resL = resL+(C13*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resL = resL+(C31*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resL = resL+(BB12*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resL = resL+(BB21*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resL = resL+(BB22*Conjg(Vv(gt2,5))*ZH(gt3,3))/sqrt(2._dp)
End If 
If ((5.eq.gt1)) Then 
resL = resL+(C23*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resL = resL+(C32*Conjg(Vv(gt2,6))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resL = resL+(C13*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resL = resL+(C31*Conjg(Vv(gt2,4))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resL = resL+(C23*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resL = resL+(C32*Conjg(Vv(gt2,5))*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
resR = 0._dp 
If ((4.eq.gt1)) Then 
resR = resR+(Conjg(Y1n11)*Vv(gt2,1)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((5.eq.gt1)) Then 
resR = resR+(Conjg(Y1n12)*Vv(gt2,1)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((4.eq.gt1)) Then 
resR = resR+(Conjg(Y1n21)*Vv(gt2,2)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((5.eq.gt1)) Then 
resR = resR+(Conjg(Y1n22)*Vv(gt2,2)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1n11)*Vv(gt2,4)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1n21)*Vv(gt2,4)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((1.eq.gt1)) Then 
resR = resR+(Conjg(Y1n12)*Vv(gt2,5)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((2.eq.gt1)) Then 
resR = resR+(Conjg(Y1n22)*Vv(gt2,5)*ZH(gt3,1))/sqrt(2._dp)
End If 
If ((6.eq.gt1)) Then 
resR = resR+(Conjg(Y2n33)*Vv(gt2,3)*ZH(gt3,2))/sqrt(2._dp)
End If 
If ((3.eq.gt1)) Then 
resR = resR+(Conjg(Y2n33)*Vv(gt2,6)*ZH(gt3,2))/sqrt(2._dp)
End If 
If ((4.eq.gt1)) Then 
resR = resR-((Conjg(BB11)*Vv(gt2,4)*ZH(gt3,3))/sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resR = resR+(Conjg(BB12)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resR = resR+(Conjg(BB21)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resR = resR+(Conjg(C13)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resR = resR+(Conjg(C31)*Vv(gt2,4)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resR = resR+(Conjg(BB12)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resR = resR+(Conjg(BB21)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resR = resR+(Conjg(BB22)*Vv(gt2,5)*ZH(gt3,3))/sqrt(2._dp)
End If 
If ((6.eq.gt1)) Then 
resR = resR+(Conjg(C23)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((6.eq.gt1)) Then 
resR = resR+(Conjg(C32)*Vv(gt2,5)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resR = resR+(Conjg(C13)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((4.eq.gt1)) Then 
resR = resR+(Conjg(C31)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resR = resR+(Conjg(C23)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((5.eq.gt1)) Then 
resR = resR+(Conjg(C32)*Vv(gt2,6)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUFvFvhhL  
 
 
Subroutine CouplingUFvFvVPL(gt1,gt2,g1,g2,ZZ,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUFvFvVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL+(g1*Conjg(Vv(gt2,1))*ZZ(1,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL+(g1*Conjg(Vv(gt2,2))*ZZ(1,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL+(g1*Conjg(Vv(gt2,3))*ZZ(1,1))/2._dp
End If 
If ((1.eq.gt1)) Then 
resL = resL-(g2*Conjg(Vv(gt2,1))*ZZ(2,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL-(g2*Conjg(Vv(gt2,2))*ZZ(2,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL-(g2*Conjg(Vv(gt2,3))*ZZ(2,1))/2._dp
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR-(g1*Vv(gt2,1)*ZZ(1,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
resR = resR-(g1*Vv(gt2,2)*ZZ(1,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
resR = resR-(g1*Vv(gt2,3)*ZZ(1,1))/2._dp
End If 
If ((1.eq.gt1)) Then 
resR = resR+(g2*Vv(gt2,1)*ZZ(2,1))/2._dp
End If 
If ((2.eq.gt1)) Then 
resR = resR+(g2*Vv(gt2,2)*ZZ(2,1))/2._dp
End If 
If ((3.eq.gt1)) Then 
resR = resR+(g2*Vv(gt2,3)*ZZ(2,1))/2._dp
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUFvFvVPL  
 
 
Subroutine CouplingUFvFvVZL(gt1,gt2,g1,g2,ZZ,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingUFvFvVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt1)) Then 
resL = resL+(g1*Conjg(Vv(gt2,1))*ZZ(1,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL+(g1*Conjg(Vv(gt2,2))*ZZ(1,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL+(g1*Conjg(Vv(gt2,3))*ZZ(1,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
resL = resL-(g2*Conjg(Vv(gt2,1))*ZZ(2,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
resL = resL-(g2*Conjg(Vv(gt2,2))*ZZ(2,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
resL = resL-(g2*Conjg(Vv(gt2,3))*ZZ(2,2))/2._dp
End If 
resR = 0._dp 
If ((1.eq.gt1)) Then 
resR = resR-(g1*Vv(gt2,1)*ZZ(1,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
resR = resR-(g1*Vv(gt2,2)*ZZ(1,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
resR = resR-(g1*Vv(gt2,3)*ZZ(1,2))/2._dp
End If 
If ((1.eq.gt1)) Then 
resR = resR+(g2*Vv(gt2,1)*ZZ(2,2))/2._dp
End If 
If ((2.eq.gt1)) Then 
resR = resR+(g2*Vv(gt2,2)*ZZ(2,2))/2._dp
End If 
If ((3.eq.gt1)) Then 
resR = resR+(g2*Vv(gt2,3)*ZZ(2,2))/2._dp
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingUFvFvVZL  
 
 
Subroutine CouplingcFeUFvHmL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,               & 
& Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Complex(dp), Intent(in) :: Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,ZP(2,2),ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeUFvHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt2)) Then 
resL = resL-(Y1e11*Conjg(ZER(gt1,1))*Conjg(ZP(gt3,1)))
End If 
If ((1.eq.gt2)) Then 
resL = resL-(Y1e12*Conjg(ZER(gt1,2))*Conjg(ZP(gt3,1)))
End If 
If ((2.eq.gt2)) Then 
resL = resL-(Y1e21*Conjg(ZER(gt1,1))*Conjg(ZP(gt3,1)))
End If 
If ((2.eq.gt2)) Then 
resL = resL-(Y1e22*Conjg(ZER(gt1,2))*Conjg(ZP(gt3,1)))
End If 
If ((3.eq.gt2)) Then 
resL = resL-(Y2e33*Conjg(ZER(gt1,3))*Conjg(ZP(gt3,2)))
End If 
resR = 0._dp 
If ((4.eq.gt2)) Then 
resR = resR-(Conjg(Y1n11)*Conjg(ZP(gt3,1))*ZEL(gt1,1))
End If 
If ((5.eq.gt2)) Then 
resR = resR-(Conjg(Y1n12)*Conjg(ZP(gt3,1))*ZEL(gt1,1))
End If 
If ((4.eq.gt2)) Then 
resR = resR-(Conjg(Y1n21)*Conjg(ZP(gt3,1))*ZEL(gt1,2))
End If 
If ((5.eq.gt2)) Then 
resR = resR-(Conjg(Y1n22)*Conjg(ZP(gt3,1))*ZEL(gt1,2))
End If 
If ((6.eq.gt2)) Then 
resR = resR-(Conjg(Y2n33)*Conjg(ZP(gt3,2))*ZEL(gt1,3))
End If 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeUFvHmL  
 
 
Subroutine CouplingcFeUFvVWmL(gt1,gt2,g2,ZEL,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZEL(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeUFvVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
If ((1.eq.gt2)) Then 
resL = resL-((g2*ZEL(gt1,1))/sqrt(2._dp))
End If 
If ((2.eq.gt2)) Then 
resL = resL-((g2*ZEL(gt1,2))/sqrt(2._dp))
End If 
If ((3.eq.gt2)) Then 
resL = resL-((g2*ZEL(gt1,3))/sqrt(2._dp))
End If 
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeUFvVWmL  
 
 
Subroutine CouplingcFdFdVGL(gt1,gt2,g3,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3

Complex(dp), Intent(in) :: ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g3*Conjg(ZDL(gt2,1))*ZDL(gt1,1))
resL = resL-(g3*Conjg(ZDL(gt2,2))*ZDL(gt1,2))
resL = resL-(g3*Conjg(ZDL(gt2,3))*ZDL(gt1,3))
resR = 0._dp 
resR = resR-(g3*Conjg(ZDR(gt1,1))*ZDR(gt2,1))
resR = resR-(g3*Conjg(ZDR(gt1,2))*ZDR(gt2,2))
resR = resR-(g3*Conjg(ZDR(gt1,3))*ZDR(gt2,3))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdVGL  
 
 
Subroutine CouplingcFuFuVGL(gt1,gt2,g3,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g3

Complex(dp), Intent(in) :: ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuVG' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g3*Conjg(ZUL(gt2,1))*ZUL(gt1,1))
resL = resL-(g3*Conjg(ZUL(gt2,2))*ZUL(gt1,2))
resL = resL-(g3*Conjg(ZUL(gt2,3))*ZUL(gt1,3))
resR = 0._dp 
resR = resR-(g3*Conjg(ZUR(gt1,1))*ZUR(gt2,1))
resR = resR-(g3*Conjg(ZUR(gt1,2))*ZUR(gt2,2))
resR = resR-(g3*Conjg(ZUR(gt1,3))*ZUR(gt2,3))
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuVGL  
 
 
Subroutine CouplingcgGgGVGL(g3,res)

Implicit None 

Real(dp), Intent(in) :: g3

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgGgGVG' 
 
res = 0._dp 
res = res+g3
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgGgGVGL  
 
 
Subroutine CouplingVGVGVGL(g3,res)

Implicit None 

Real(dp), Intent(in) :: g3

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingVGVGVG' 
 
res = 0._dp 
res = res+g3
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingVGVGVGL  
 
 
Subroutine CouplingVGVGVGVGL(g3,res1,res2,res3)

Implicit None 

Real(dp), Intent(in) :: g3

Complex(dp), Intent(out) :: res1, res2, res3 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingVGVGVGVG' 
 
res1 = 0._dp 
res1 = res1-16*g3**2
res2 = 0._dp 
res3 = 0._dp 
res3 = res3+16*g3**2
If ((Real(res1,dp).ne.Real(res1,dp)).or.(Real(res2,dp).ne.Real(res2,dp)).or.(Real(res3,dp).ne.Real(res3,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingVGVGVGVGL  
 
 
Subroutine CouplingAhhhVPL(gt1,gt2,g1,g2,ZZ,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*ZA(gt1,1)*ZH(gt2,1)*ZZ(1,1))/2._dp
res = res-(g1*ZA(gt1,2)*ZH(gt2,2)*ZZ(1,1))/2._dp
res = res+(g2*ZA(gt1,1)*ZH(gt2,1)*ZZ(2,1))/2._dp
res = res+(g2*ZA(gt1,2)*ZH(gt2,2)*ZZ(2,1))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhVPL  
 
 
Subroutine CouplingcFdFdVPL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g1*Conjg(ZDL(gt2,1))*ZDL(gt1,1)*ZZ(1,1))/6._dp
resL = resL-(g1*Conjg(ZDL(gt2,2))*ZDL(gt1,2)*ZZ(1,1))/6._dp
resL = resL-(g1*Conjg(ZDL(gt2,3))*ZDL(gt1,3)*ZZ(1,1))/6._dp
resL = resL+(g2*Conjg(ZDL(gt2,1))*ZDL(gt1,1)*ZZ(2,1))/2._dp
resL = resL+(g2*Conjg(ZDL(gt2,2))*ZDL(gt1,2)*ZZ(2,1))/2._dp
resL = resL+(g2*Conjg(ZDL(gt2,3))*ZDL(gt1,3)*ZZ(2,1))/2._dp
resR = 0._dp 
resR = resR+(g1*Conjg(ZDR(gt1,1))*ZDR(gt2,1)*ZZ(1,1))/3._dp
resR = resR+(g1*Conjg(ZDR(gt1,2))*ZDR(gt2,2)*ZZ(1,1))/3._dp
resR = resR+(g1*Conjg(ZDR(gt1,3))*ZDR(gt2,3)*ZZ(1,1))/3._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdVPL  
 
 
Subroutine CouplingcFeFeVPL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFeVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(g1*Conjg(ZEL(gt2,1))*ZEL(gt1,1)*ZZ(1,1))/2._dp
resL = resL+(g1*Conjg(ZEL(gt2,2))*ZEL(gt1,2)*ZZ(1,1))/2._dp
resL = resL+(g1*Conjg(ZEL(gt2,3))*ZEL(gt1,3)*ZZ(1,1))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,1))*ZEL(gt1,1)*ZZ(2,1))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,2))*ZEL(gt1,2)*ZZ(2,1))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,3))*ZEL(gt1,3)*ZZ(2,1))/2._dp
resR = 0._dp 
resR = resR+g1*Conjg(ZER(gt1,1))*ZER(gt2,1)*ZZ(1,1)
resR = resR+g1*Conjg(ZER(gt1,2))*ZER(gt2,2)*ZZ(1,1)
resR = resR+g1*Conjg(ZER(gt1,3))*ZER(gt2,3)*ZZ(1,1)
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFeVPL  
 
 
Subroutine CouplingcFuFuVPL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g1*Conjg(ZUL(gt2,1))*ZUL(gt1,1)*ZZ(1,1))/6._dp
resL = resL-(g1*Conjg(ZUL(gt2,2))*ZUL(gt1,2)*ZZ(1,1))/6._dp
resL = resL-(g1*Conjg(ZUL(gt2,3))*ZUL(gt1,3)*ZZ(1,1))/6._dp
resL = resL-(g2*Conjg(ZUL(gt2,1))*ZUL(gt1,1)*ZZ(2,1))/2._dp
resL = resL-(g2*Conjg(ZUL(gt2,2))*ZUL(gt1,2)*ZZ(2,1))/2._dp
resL = resL-(g2*Conjg(ZUL(gt2,3))*ZUL(gt1,3)*ZZ(2,1))/2._dp
resR = 0._dp 
resR = resR+(-2*g1*Conjg(ZUR(gt1,1))*ZUR(gt2,1)*ZZ(1,1))/3._dp
resR = resR+(-2*g1*Conjg(ZUR(gt1,2))*ZUR(gt2,2)*ZZ(1,1))/3._dp
resR = resR+(-2*g1*Conjg(ZUR(gt1,3))*ZUR(gt2,3)*ZZ(1,1))/3._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuVPL  
 
 
Subroutine CouplingFvFvVPL(gt1,gt2,g1,g2,ZZ,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(g1*Conjg(Vv(gt2,1))*Vv(gt1,1)*ZZ(1,1))/2._dp
resL = resL+(g1*Conjg(Vv(gt2,2))*Vv(gt1,2)*ZZ(1,1))/2._dp
resL = resL+(g1*Conjg(Vv(gt2,3))*Vv(gt1,3)*ZZ(1,1))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,1))*Vv(gt1,1)*ZZ(2,1))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,2))*Vv(gt1,2)*ZZ(2,1))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,3))*Vv(gt1,3)*ZZ(2,1))/2._dp
resR = 0._dp 
resR = resR-(g1*Conjg(Vv(gt1,1))*Vv(gt2,1)*ZZ(1,1))/2._dp
resR = resR-(g1*Conjg(Vv(gt1,2))*Vv(gt2,2)*ZZ(1,1))/2._dp
resR = resR-(g1*Conjg(Vv(gt1,3))*Vv(gt2,3)*ZZ(1,1))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,1))*Vv(gt2,1)*ZZ(2,1))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,2))*Vv(gt2,2)*ZZ(2,1))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,3))*Vv(gt2,3)*ZZ(2,1))/2._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvVPL  
 
 
Subroutine CouplingcgWmgWmVPL(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgWmVP' 
 
res = 0._dp 
res = res-(g2*ZZ(2,1))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgWmVPL  
 
 
Subroutine CouplingcgWpgWpVPL(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgWpVP' 
 
res = 0._dp 
res = res+g2*Conjg(ZZ(2,1))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgWpVPL  
 
 
Subroutine CouplinghhVPVZL(gt1,g1,g2,v1,v2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhVPVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*v1*ZH(gt1,1)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res+(g1**2*v2*ZH(gt1,2)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res-(g1*g2*v1*ZH(gt1,1)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*v2*ZH(gt1,2)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*v1*ZH(gt1,1)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res-(g1*g2*v2*ZH(gt1,2)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res+(g2**2*v1*ZH(gt1,1)*ZZ(2,1)*ZZ(2,2))/2._dp
res = res+(g2**2*v2*ZH(gt1,2)*ZZ(2,1)*ZZ(2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhVPVZL  
 
 
Subroutine CouplingHmcHmVPL(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,1))/2._dp
res = res-(g1*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,1))/2._dp
res = res-(g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(2,1))/2._dp
res = res-(g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(2,1))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmVPL  
 
 
Subroutine CouplingHmcVWmVPL(gt1,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcVWmVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*v1*Conjg(ZP(gt1,1))*ZZ(1,1))/2._dp
res = res+(g1*g2*v2*Conjg(ZP(gt1,2))*ZZ(1,1))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcVWmVPL  
 
 
Subroutine CouplingcVWmVPVWmL(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmVPVWm' 
 
res = 0._dp 
res = res+g2*ZZ(2,1)
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmVPVWmL  
 
 
Subroutine CouplingAhAhVPVPL(gt1,gt2,g1,g2,ZZ,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhVPVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,1)**2)/2._dp
res = res+(g1**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,1)**2)/2._dp
res = res-(g1*g2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,1)*ZZ(2,1))
res = res-(g1*g2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,1)*ZZ(2,1))
res = res+(g2**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(2,1)**2)/2._dp
res = res+(g2**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(2,1)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhVPVPL  
 
 
Subroutine CouplinghhhhVPVPL(gt1,gt2,g1,g2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhVPVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,1)**2)/2._dp
res = res+(g1**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,1)**2)/2._dp
res = res-(g1*g2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,1)*ZZ(2,1))
res = res-(g1*g2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,1)*ZZ(2,1))
res = res+(g2**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(2,1)**2)/2._dp
res = res+(g2**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(2,1)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhVPVPL  
 
 
Subroutine CouplingHmcHmVPVPL(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmVPVP' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,1)**2)/2._dp
res = res+(g1**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,1)**2)/2._dp
res = res+g1*g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,1)*ZZ(2,1)
res = res+g1*g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,1)*ZZ(2,1)
res = res+(g2**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(2,1)**2)/2._dp
res = res+(g2**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(2,1)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmVPVPL  
 
 
Subroutine CouplingcVWmVPVPVWmL(g2,ZZ,res1,res2,res3)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res1, res2, res3 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmVPVPVWm' 
 
res1 = 0._dp 
res1 = res1+g2**2*ZZ(2,1)**2
res2 = 0._dp 
res2 = res2+g2**2*ZZ(2,1)**2
res3 = 0._dp 
res3 = res3-2*g2**2*ZZ(2,1)**2
If ((Real(res1,dp).ne.Real(res1,dp)).or.(Real(res2,dp).ne.Real(res2,dp)).or.(Real(res3,dp).ne.Real(res3,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmVPVPVWmL  
 
 
Subroutine CouplingAhhhVZL(gt1,gt2,g1,g2,ZZ,ZH,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*ZA(gt1,1)*ZH(gt2,1)*ZZ(1,2))/2._dp
res = res-(g1*ZA(gt1,2)*ZH(gt2,2)*ZZ(1,2))/2._dp
res = res+(g2*ZA(gt1,1)*ZH(gt2,1)*ZZ(2,2))/2._dp
res = res+(g2*ZA(gt1,2)*ZH(gt2,2)*ZZ(2,2))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhVZL  
 
 
Subroutine CouplingcFdFdVZL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZDL(3,3),ZDR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFdFdVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g1*Conjg(ZDL(gt2,1))*ZDL(gt1,1)*ZZ(1,2))/6._dp
resL = resL-(g1*Conjg(ZDL(gt2,2))*ZDL(gt1,2)*ZZ(1,2))/6._dp
resL = resL-(g1*Conjg(ZDL(gt2,3))*ZDL(gt1,3)*ZZ(1,2))/6._dp
resL = resL+(g2*Conjg(ZDL(gt2,1))*ZDL(gt1,1)*ZZ(2,2))/2._dp
resL = resL+(g2*Conjg(ZDL(gt2,2))*ZDL(gt1,2)*ZZ(2,2))/2._dp
resL = resL+(g2*Conjg(ZDL(gt2,3))*ZDL(gt1,3)*ZZ(2,2))/2._dp
resR = 0._dp 
resR = resR+(g1*Conjg(ZDR(gt1,1))*ZDR(gt2,1)*ZZ(1,2))/3._dp
resR = resR+(g1*Conjg(ZDR(gt1,2))*ZDR(gt2,2)*ZZ(1,2))/3._dp
resR = resR+(g1*Conjg(ZDR(gt1,3))*ZDR(gt2,3)*ZZ(1,2))/3._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFdFdVZL  
 
 
Subroutine CouplingcFeFeVZL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZEL(3,3),ZER(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFeFeVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(g1*Conjg(ZEL(gt2,1))*ZEL(gt1,1)*ZZ(1,2))/2._dp
resL = resL+(g1*Conjg(ZEL(gt2,2))*ZEL(gt1,2)*ZZ(1,2))/2._dp
resL = resL+(g1*Conjg(ZEL(gt2,3))*ZEL(gt1,3)*ZZ(1,2))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,1))*ZEL(gt1,1)*ZZ(2,2))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,2))*ZEL(gt1,2)*ZZ(2,2))/2._dp
resL = resL+(g2*Conjg(ZEL(gt2,3))*ZEL(gt1,3)*ZZ(2,2))/2._dp
resR = 0._dp 
resR = resR+g1*Conjg(ZER(gt1,1))*ZER(gt2,1)*ZZ(1,2)
resR = resR+g1*Conjg(ZER(gt1,2))*ZER(gt2,2)*ZZ(1,2)
resR = resR+g1*Conjg(ZER(gt1,3))*ZER(gt2,3)*ZZ(1,2)
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFeFeVZL  
 
 
Subroutine CouplingcFuFuVZL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZUL(3,3),ZUR(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFuVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-(g1*Conjg(ZUL(gt2,1))*ZUL(gt1,1)*ZZ(1,2))/6._dp
resL = resL-(g1*Conjg(ZUL(gt2,2))*ZUL(gt1,2)*ZZ(1,2))/6._dp
resL = resL-(g1*Conjg(ZUL(gt2,3))*ZUL(gt1,3)*ZZ(1,2))/6._dp
resL = resL-(g2*Conjg(ZUL(gt2,1))*ZUL(gt1,1)*ZZ(2,2))/2._dp
resL = resL-(g2*Conjg(ZUL(gt2,2))*ZUL(gt1,2)*ZZ(2,2))/2._dp
resL = resL-(g2*Conjg(ZUL(gt2,3))*ZUL(gt1,3)*ZZ(2,2))/2._dp
resR = 0._dp 
resR = resR+(-2*g1*Conjg(ZUR(gt1,1))*ZUR(gt2,1)*ZZ(1,2))/3._dp
resR = resR+(-2*g1*Conjg(ZUR(gt1,2))*ZUR(gt2,2)*ZZ(1,2))/3._dp
resR = resR+(-2*g1*Conjg(ZUR(gt1,3))*ZUR(gt2,3)*ZZ(1,2))/3._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFuVZL  
 
 
Subroutine CouplingFvFvVZL(gt1,gt2,g1,g2,ZZ,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFvVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL+(g1*Conjg(Vv(gt2,1))*Vv(gt1,1)*ZZ(1,2))/2._dp
resL = resL+(g1*Conjg(Vv(gt2,2))*Vv(gt1,2)*ZZ(1,2))/2._dp
resL = resL+(g1*Conjg(Vv(gt2,3))*Vv(gt1,3)*ZZ(1,2))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,1))*Vv(gt1,1)*ZZ(2,2))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,2))*Vv(gt1,2)*ZZ(2,2))/2._dp
resL = resL-(g2*Conjg(Vv(gt2,3))*Vv(gt1,3)*ZZ(2,2))/2._dp
resR = 0._dp 
resR = resR-(g1*Conjg(Vv(gt1,1))*Vv(gt2,1)*ZZ(1,2))/2._dp
resR = resR-(g1*Conjg(Vv(gt1,2))*Vv(gt2,2)*ZZ(1,2))/2._dp
resR = resR-(g1*Conjg(Vv(gt1,3))*Vv(gt2,3)*ZZ(1,2))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,1))*Vv(gt2,1)*ZZ(2,2))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,2))*Vv(gt2,2)*ZZ(2,2))/2._dp
resR = resR+(g2*Conjg(Vv(gt1,3))*Vv(gt2,3)*ZZ(2,2))/2._dp
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFvVZL  
 
 
Subroutine CouplingcgWmgWmVZL(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWmgWmVZ' 
 
res = 0._dp 
res = res-(g2*ZZ(2,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWmgWmVZL  
 
 
Subroutine CouplingcgWpgWpVZL(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgWpVZ' 
 
res = 0._dp 
res = res+g2*Conjg(ZZ(2,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgWpVZL  
 
 
Subroutine CouplinghhVZVZL(gt1,g1,g2,v1,v2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*v1*ZH(gt1,1)*ZZ(1,2)**2)/2._dp
res = res+(g1**2*v2*ZH(gt1,2)*ZZ(1,2)**2)/2._dp
res = res-(g1*g2*v1*ZH(gt1,1)*ZZ(1,2)*ZZ(2,2))
res = res-(g1*g2*v2*ZH(gt1,2)*ZZ(1,2)*ZZ(2,2))
res = res+(g2**2*v1*ZH(gt1,1)*ZZ(2,2)**2)/2._dp
res = res+(g2**2*v2*ZH(gt1,2)*ZZ(2,2)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhVZVZL  
 
 
Subroutine CouplingHmcHmVZL(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g1*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,2))/2._dp
res = res-(g1*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,2))/2._dp
res = res-(g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(2,2))/2._dp
res = res-(g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmVZL  
 
 
Subroutine CouplingHmcVWmVZL(gt1,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcVWmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*v1*Conjg(ZP(gt1,1))*ZZ(1,2))/2._dp
res = res+(g1*g2*v2*Conjg(ZP(gt1,2))*ZZ(1,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcVWmVZL  
 
 
Subroutine CouplingcVWmVWmVZL(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmVWmVZ' 
 
res = 0._dp 
res = res-(g2*ZZ(2,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmVWmVZL  
 
 
Subroutine CouplingAhAhVZVZL(gt1,gt2,g1,g2,ZZ,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,2)**2)/2._dp
res = res+(g1**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,2)**2)/2._dp
res = res-(g1*g2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,2)*ZZ(2,2))
res = res-(g1*g2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,2)*ZZ(2,2))
res = res+(g2**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(2,2)**2)/2._dp
res = res+(g2**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(2,2)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhVZVZL  
 
 
Subroutine CouplinghhhhVZVZL(gt1,gt2,g1,g2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,2)**2)/2._dp
res = res+(g1**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,2)**2)/2._dp
res = res-(g1*g2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,2)*ZZ(2,2))
res = res-(g1*g2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,2)*ZZ(2,2))
res = res+(g2**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(2,2)**2)/2._dp
res = res+(g2**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(2,2)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhVZVZL  
 
 
Subroutine CouplingHmcHmVZVZL(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmVZVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,2)**2)/2._dp
res = res+(g1**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,2)**2)/2._dp
res = res+g1*g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,2)*ZZ(2,2)
res = res+g1*g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,2)*ZZ(2,2)
res = res+(g2**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(2,2)**2)/2._dp
res = res+(g2**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(2,2)**2)/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmVZVZL  
 
 
Subroutine CouplingcVWmVWmVZVZL(g2,ZZ,res1,res2,res3)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res1, res2, res3 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmVWmVZVZ' 
 
res1 = 0._dp 
res1 = res1-2*g2**2*ZZ(2,2)**2
res2 = 0._dp 
res2 = res2+g2**2*ZZ(2,2)**2
res3 = 0._dp 
res3 = res3+g2**2*ZZ(2,2)**2
If ((Real(res1,dp).ne.Real(res1,dp)).or.(Real(res2,dp).ne.Real(res2,dp)).or.(Real(res3,dp).ne.Real(res3,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmVWmVZVZL  
 
 
Subroutine CouplingAhHmcVWmL(gt1,gt2,g2,ZA,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZA(3,3)

Complex(dp), Intent(in) :: ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhHmcVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res-(g2*Conjg(ZP(gt2,1))*ZA(gt1,1))/2._dp
res = res-(g2*Conjg(ZP(gt2,2))*ZA(gt1,2))/2._dp
res = -(0.,1.)*res 
 
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhHmcVWmL  
 
 
Subroutine CouplingcFuFdcVWmL(gt1,gt2,g2,ZDL,ZUL,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZDL(3,3),ZUL(3,3)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcFuFdcVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((g2*Conjg(ZDL(gt2,1))*ZUL(gt1,1))/sqrt(2._dp))
resL = resL-((g2*Conjg(ZDL(gt2,2))*ZUL(gt1,2))/sqrt(2._dp))
resL = resL-((g2*Conjg(ZDL(gt2,3))*ZUL(gt1,3))/sqrt(2._dp))
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcFuFdcVWmL  
 
 
Subroutine CouplingFvFecVWmL(gt1,gt2,g2,ZEL,Vv,resL,resR)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZEL(3,3),Vv(6,6)

Complex(dp), Intent(out) :: resL, resR 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingFvFecVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.6)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

resL = 0._dp 
resL = resL-((g2*Conjg(ZEL(gt2,1))*Vv(gt1,1))/sqrt(2._dp))
resL = resL-((g2*Conjg(ZEL(gt2,2))*Vv(gt1,2))/sqrt(2._dp))
resL = resL-((g2*Conjg(ZEL(gt2,3))*Vv(gt1,3))/sqrt(2._dp))
resR = 0._dp 
If ((Real(resL,dp).ne.Real(resL,dp)).or.(Real(resR,dp).ne.Real(resR,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingFvFecVWmL  
 
 
Subroutine CouplingcgWpgAcVWmL(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgAcVWm' 
 
res = 0._dp 
res = res-(g2*Conjg(ZZ(2,1)))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgAcVWmL  
 
 
Subroutine CouplingcgAgWmcVWmL(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgAgWmcVWm' 
 
res = 0._dp 
res = res+g2*Conjg(ZZ(2,1))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgAgWmcVWmL  
 
 
Subroutine CouplingcgZgWmcVWmL(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgZgWmcVWm' 
 
res = 0._dp 
res = res+g2*Conjg(ZZ(2,2))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgZgWmcVWmL  
 
 
Subroutine CouplingcgWpgZcVWmL(g2,ZZ,res)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcgWpgZcVWm' 
 
res = 0._dp 
res = res-(g2*Conjg(ZZ(2,2)))
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcgWpgZcVWmL  
 
 
Subroutine CouplinghhHmcVWmL(gt1,gt2,g2,ZH,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZH(3,3)

Complex(dp), Intent(in) :: ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhHmcVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2*Conjg(ZP(gt2,1))*ZH(gt1,1))/2._dp
res = res+(g2*Conjg(ZP(gt2,2))*ZH(gt1,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhHmcVWmL  
 
 
Subroutine CouplinghhcVWmVWmL(gt1,g2,v1,v2,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g2,v1,v2,ZH(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2**2*v1*ZH(gt1,1))/2._dp
res = res+(g2**2*v2*ZH(gt1,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhcVWmVWmL  
 
 
Subroutine CouplingAhAhcVWmVWmL(gt1,gt2,g2,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZA(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2**2*ZA(gt1,1)*ZA(gt2,1))/2._dp
res = res+(g2**2*ZA(gt1,2)*ZA(gt2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhcVWmVWmL  
 
 
Subroutine CouplinghhhhcVWmVWmL(gt1,gt2,g2,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2,ZH(3,3)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2**2*ZH(gt1,1)*ZH(gt2,1))/2._dp
res = res+(g2**2*ZH(gt1,2)*ZH(gt2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhcVWmVWmL  
 
 
Subroutine CouplingHmcHmcVWmVWmL(gt1,gt2,g2,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmcVWmVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g2**2*Conjg(ZP(gt1,1))*ZP(gt2,1))/2._dp
res = res+(g2**2*Conjg(ZP(gt1,2))*ZP(gt2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmcVWmVWmL  
 
 
Subroutine CouplingcVWmcVWmVWmVWmL(g2,res1,res2,res3)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(out) :: res1, res2, res3 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmcVWmVWmVWm' 
 
res1 = 0._dp 
res1 = res1+2*g2**2
res2 = 0._dp 
res2 = res2-g2**2
res3 = 0._dp 
res3 = res3-g2**2
If ((Real(res1,dp).ne.Real(res1,dp)).or.(Real(res2,dp).ne.Real(res2,dp)).or.(Real(res3,dp).ne.Real(res3,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmcVWmVWmVWmL  
 
 
Subroutine CouplingcHmVWmVZL(gt1,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcHmVWmVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*v1*ZP(gt1,1)*ZZ(1,2))/2._dp
res = res+(g1*g2*v2*ZP(gt1,2)*ZZ(1,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcHmVWmVZL  
 
 
Subroutine CouplingcHmVPVWmL(gt1,g1,g2,v1,v2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1
Real(dp), Intent(in) :: g1,g2,v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcHmVPVWm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1*g2*v1*ZP(gt1,1)*ZZ(1,1))/2._dp
res = res+(g1*g2*v2*ZP(gt1,2)*ZZ(1,1))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcHmVPVWmL  
 
 
Subroutine CouplingAhAhVPVZL(gt1,gt2,g1,g2,ZZ,ZA,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZA(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhVPVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res+(g1**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res-(g1*g2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*ZA(gt1,1)*ZA(gt2,1)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res-(g1*g2*ZA(gt1,2)*ZA(gt2,2)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res+(g2**2*ZA(gt1,1)*ZA(gt2,1)*ZZ(2,1)*ZZ(2,2))/2._dp
res = res+(g2**2*ZA(gt1,2)*ZA(gt2,2)*ZZ(2,1)*ZZ(2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhVPVZL  
 
 
Subroutine CouplinghhhhVPVZL(gt1,gt2,g1,g2,ZZ,ZH,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2,ZH(3,3)

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhVPVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res+(g1**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res-(g1*g2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res-(g1*g2*ZH(gt1,1)*ZH(gt2,1)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res-(g1*g2*ZH(gt1,2)*ZH(gt2,2)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res+(g2**2*ZH(gt1,1)*ZH(gt2,1)*ZZ(2,1)*ZZ(2,2))/2._dp
res = res+(g2**2*ZH(gt1,2)*ZH(gt2,2)*ZZ(2,1)*ZZ(2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhVPVZL  
 
 
Subroutine CouplingHmcHmVPVZL(gt1,gt2,g1,g2,ZZ,ZP,res)

Implicit None 

Integer, Intent(in) :: gt1,gt2
Real(dp), Intent(in) :: g1,g2

Complex(dp), Intent(in) :: ZZ(2,2),ZP(2,2)

Complex(dp), Intent(out) :: res 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmcHmVPVZ' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

res = 0._dp 
res = res+(g1**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res+(g1**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,1)*ZZ(1,2))/2._dp
res = res+(g1*g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res+(g1*g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,2)*ZZ(2,1))/2._dp
res = res+(g1*g2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res+(g1*g2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(1,1)*ZZ(2,2))/2._dp
res = res+(g2**2*Conjg(ZP(gt1,1))*ZP(gt2,1)*ZZ(2,1)*ZZ(2,2))/2._dp
res = res+(g2**2*Conjg(ZP(gt1,2))*ZP(gt2,2)*ZZ(2,1)*ZZ(2,2))/2._dp
If (Real(res,dp).ne.Real(res,dp)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingHmcHmVPVZL  
 
 
Subroutine CouplingcVWmVPVWmVZL(g2,ZZ,res1,res2,res3)

Implicit None 

Real(dp), Intent(in) :: g2

Complex(dp), Intent(in) :: ZZ(2,2)

Complex(dp), Intent(out) :: res1, res2, res3 
 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingcVWmVPVWmVZ' 
 
res1 = 0._dp 
res1 = res1+g2**2*ZZ(2,1)*ZZ(2,2)
res2 = 0._dp 
res2 = res2-2*g2**2*ZZ(2,1)*ZZ(2,2)
res3 = 0._dp 
res3 = res3+g2**2*ZZ(2,1)*ZZ(2,2)
If ((Real(res1,dp).ne.Real(res1,dp)).or.(Real(res2,dp).ne.Real(res2,dp)).or.(Real(res3,dp).ne.Real(res3,dp))) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Call TerminateProgram 
End If 


Iname = Iname - 1 
 
End Subroutine CouplingcVWmVPVWmVZL  
 
 
Subroutine CouplingsForVectorBosons(g1,g2,ZZ,ZH,ZA,ZDL,ZDR,ZEL,ZER,ZUL,               & 
& ZUR,Vv,v1,v2,ZP,cplAhhhVP,cplcFdFdVPL,cplcFdFdVPR,cplcFeFeVPL,cplcFeFeVPR,             & 
& cplcFuFuVPL,cplcFuFuVPR,cplFvFvVPL,cplFvFvVPR,cplcgWmgWmVP,cplcgWpgWpVP,               & 
& cplhhVPVZ,cplHmcHmVP,cplHmcVWmVP,cplcVWmVPVWm,cplAhAhVPVP,cplhhhhVPVP,cplHmcHmVPVP,    & 
& cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,cplAhhhVZ,cplcFdFdVZL,cplcFdFdVZR,     & 
& cplcFeFeVZL,cplcFeFeVZR,cplcFuFuVZL,cplcFuFuVZR,cplFvFvVZL,cplFvFvVZR,cplcgWmgWmVZ,    & 
& cplcgWpgWpVZ,cplhhVZVZ,cplHmcHmVZ,cplHmcVWmVZ,cplcVWmVWmVZ,cplAhAhVZVZ,cplhhhhVZVZ,    & 
& cplHmcHmVZVZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,cplAhHmcVWm,              & 
& cplcFuFdcVWmL,cplcFuFdcVWmR,cplFvFecVWmL,cplFvFecVWmR,cplcgWpgAcVWm,cplcgAgWmcVWm,     & 
& cplcgZgWmcVWm,cplcgWpgZcVWm,cplhhHmcVWm,cplhhcVWmVWm,cplAhAhcVWmVWm,cplhhhhcVWmVWm,    & 
& cplHmcHmcVWmVWm,cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,              & 
& cplcHmVWmVZ,cplcHmVPVWm,cplAhAhVPVZ,cplhhhhVPVZ,cplHmcHmVPVZ,cplcVWmVPVWmVZ1,          & 
& cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3)

Implicit None 
Real(dp), Intent(in) :: g1,g2,ZH(3,3),ZA(3,3),v1,v2

Complex(dp), Intent(in) :: ZZ(2,2),ZDL(3,3),ZDR(3,3),ZEL(3,3),ZER(3,3),ZUL(3,3),ZUR(3,3),Vv(6,6),ZP(2,2)

Complex(dp), Intent(out) :: cplAhhhVP(3,3),cplcFdFdVPL(3,3),cplcFdFdVPR(3,3),cplcFeFeVPL(3,3),cplcFeFeVPR(3,3),   & 
& cplcFuFuVPL(3,3),cplcFuFuVPR(3,3),cplFvFvVPL(6,6),cplFvFvVPR(6,6),cplcgWmgWmVP,        & 
& cplcgWpgWpVP,cplhhVPVZ(3),cplHmcHmVP(2,2),cplHmcVWmVP(2),cplcVWmVPVWm,cplAhAhVPVP(3,3),& 
& cplhhhhVPVP(3,3),cplHmcHmVPVP(2,2),cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3,    & 
& cplAhhhVZ(3,3),cplcFdFdVZL(3,3),cplcFdFdVZR(3,3),cplcFeFeVZL(3,3),cplcFeFeVZR(3,3),    & 
& cplcFuFuVZL(3,3),cplcFuFuVZR(3,3),cplFvFvVZL(6,6),cplFvFvVZR(6,6),cplcgWmgWmVZ,        & 
& cplcgWpgWpVZ,cplhhVZVZ(3),cplHmcHmVZ(2,2),cplHmcVWmVZ(2),cplcVWmVWmVZ,cplAhAhVZVZ(3,3),& 
& cplhhhhVZVZ(3,3),cplHmcHmVZVZ(2,2),cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3,    & 
& cplAhHmcVWm(3,2),cplcFuFdcVWmL(3,3),cplcFuFdcVWmR(3,3),cplFvFecVWmL(6,3),              & 
& cplFvFecVWmR(6,3),cplcgWpgAcVWm,cplcgAgWmcVWm,cplcgZgWmcVWm,cplcgWpgZcVWm,             & 
& cplhhHmcVWm(3,2),cplhhcVWmVWm(3),cplAhAhcVWmVWm(3,3),cplhhhhcVWmVWm(3,3),              & 
& cplHmcHmcVWmVWm(2,2),cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3,         & 
& cplcHmVWmVZ(2),cplcHmVPVWm(2),cplAhAhVPVZ(3,3),cplhhhhVPVZ(3,3),cplHmcHmVPVZ(2,2),     & 
& cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'CouplingsForVectorBosons'
 
cplAhhhVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVPL(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVP(gt1,gt2))

 End Do 
End Do 


cplcFdFdVPL = 0._dp 
cplcFdFdVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVPL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcFdFdVPL(gt1,gt2),cplcFdFdVPR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVPL = 0._dp 
cplcFeFeVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVPL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcFeFeVPL(gt1,gt2),cplcFeFeVPR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVPL = 0._dp 
cplcFuFuVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVPL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcFuFuVPL(gt1,gt2),cplcFuFuVPR(gt1,gt2))

 End Do 
End Do 


cplFvFvVPL = 0._dp 
cplFvFvVPR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
Call CouplingFvFvVPL(gt1,gt2,g1,g2,ZZ,Vv,cplFvFvVPL(gt1,gt2),cplFvFvVPR(gt1,gt2))

 End Do 
End Do 


cplcgWmgWmVP = 0._dp 
Call CouplingcgWmgWmVPL(g2,ZZ,cplcgWmgWmVP)



cplcgWpgWpVP = 0._dp 
Call CouplingcgWpgWpVPL(g2,ZZ,cplcgWpgWpVP)



cplhhVPVZ = 0._dp 
Do gt1 = 1, 3
Call CouplinghhVPVZL(gt1,g1,g2,v1,v2,ZZ,ZH,cplhhVPVZ(gt1))

End Do 


cplHmcHmVP = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVPL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVP(gt1,gt2))

 End Do 
End Do 


cplHmcVWmVP = 0._dp 
Do gt1 = 1, 2
Call CouplingHmcVWmVPL(gt1,g1,g2,v1,v2,ZZ,ZP,cplHmcVWmVP(gt1))

End Do 


cplcVWmVPVWm = 0._dp 
Call CouplingcVWmVPVWmL(g2,ZZ,cplcVWmVPVWm)



cplAhAhVPVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhVPVPL(gt1,gt2,g1,g2,ZZ,ZA,cplAhAhVPVP(gt1,gt2))

 End Do 
End Do 


cplhhhhVPVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhVPVPL(gt1,gt2,g1,g2,ZZ,ZH,cplhhhhVPVP(gt1,gt2))

 End Do 
End Do 


cplHmcHmVPVP = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVPVPL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVPVP(gt1,gt2))

 End Do 
End Do 


cplcVWmVPVPVWm1 = 0._dp 
cplcVWmVPVPVWm2 = 0._dp 
cplcVWmVPVPVWm3 = 0._dp 
Call CouplingcVWmVPVPVWmL(g2,ZZ,cplcVWmVPVPVWm1,cplcVWmVPVPVWm2,cplcVWmVPVPVWm3)



cplAhhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhhhVZL(gt1,gt2,g1,g2,ZZ,ZH,ZA,cplAhhhVZ(gt1,gt2))

 End Do 
End Do 


cplcFdFdVZL = 0._dp 
cplcFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFdFdVZL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcFdFdVZL(gt1,gt2),cplcFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcFeFeVZL = 0._dp 
cplcFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFeFeVZL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcFeFeVZL(gt1,gt2),cplcFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcFuFuVZL = 0._dp 
cplcFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFuVZL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcFuFuVZL(gt1,gt2),cplcFuFuVZR(gt1,gt2))

 End Do 
End Do 


cplFvFvVZL = 0._dp 
cplFvFvVZR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
Call CouplingFvFvVZL(gt1,gt2,g1,g2,ZZ,Vv,cplFvFvVZL(gt1,gt2),cplFvFvVZR(gt1,gt2))

 End Do 
End Do 


cplcgWmgWmVZ = 0._dp 
Call CouplingcgWmgWmVZL(g2,ZZ,cplcgWmgWmVZ)



cplcgWpgWpVZ = 0._dp 
Call CouplingcgWpgWpVZL(g2,ZZ,cplcgWpgWpVZ)



cplhhVZVZ = 0._dp 
Do gt1 = 1, 3
Call CouplinghhVZVZL(gt1,g1,g2,v1,v2,ZZ,ZH,cplhhVZVZ(gt1))

End Do 


cplHmcHmVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVZL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVZ(gt1,gt2))

 End Do 
End Do 


cplHmcVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingHmcVWmVZL(gt1,g1,g2,v1,v2,ZZ,ZP,cplHmcVWmVZ(gt1))

End Do 


cplcVWmVWmVZ = 0._dp 
Call CouplingcVWmVWmVZL(g2,ZZ,cplcVWmVWmVZ)



cplAhAhVZVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhVZVZL(gt1,gt2,g1,g2,ZZ,ZA,cplAhAhVZVZ(gt1,gt2))

 End Do 
End Do 


cplhhhhVZVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhVZVZL(gt1,gt2,g1,g2,ZZ,ZH,cplhhhhVZVZ(gt1,gt2))

 End Do 
End Do 


cplHmcHmVZVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVZVZL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVZVZ(gt1,gt2))

 End Do 
End Do 


cplcVWmVWmVZVZ1 = 0._dp 
cplcVWmVWmVZVZ2 = 0._dp 
cplcVWmVWmVZVZ3 = 0._dp 
Call CouplingcVWmVWmVZVZL(g2,ZZ,cplcVWmVWmVZVZ1,cplcVWmVWmVZVZ2,cplcVWmVWmVZVZ3)



cplAhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingAhHmcVWmL(gt1,gt2,g2,ZA,ZP,cplAhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplcFuFdcVWmL = 0._dp 
cplcFuFdcVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcFuFdcVWmL(gt1,gt2,g2,ZDL,ZUL,cplcFuFdcVWmL(gt1,gt2),cplcFuFdcVWmR(gt1,gt2))

 End Do 
End Do 


cplFvFecVWmL = 0._dp 
cplFvFecVWmR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 3
Call CouplingFvFecVWmL(gt1,gt2,g2,ZEL,Vv,cplFvFecVWmL(gt1,gt2),cplFvFecVWmR(gt1,gt2))

 End Do 
End Do 


cplcgWpgAcVWm = 0._dp 
Call CouplingcgWpgAcVWmL(g2,ZZ,cplcgWpgAcVWm)



cplcgAgWmcVWm = 0._dp 
Call CouplingcgAgWmcVWmL(g2,ZZ,cplcgAgWmcVWm)



cplcgZgWmcVWm = 0._dp 
Call CouplingcgZgWmcVWmL(g2,ZZ,cplcgZgWmcVWm)



cplcgWpgZcVWm = 0._dp 
Call CouplingcgWpgZcVWmL(g2,ZZ,cplcgWpgZcVWm)



cplhhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplinghhHmcVWmL(gt1,gt2,g2,ZH,ZP,cplhhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
Call CouplinghhcVWmVWmL(gt1,g2,v1,v2,ZH,cplhhcVWmVWm(gt1))

End Do 


cplAhAhcVWmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhcVWmVWmL(gt1,gt2,g2,ZA,cplAhAhcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplhhhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhcVWmVWmL(gt1,gt2,g2,ZH,cplhhhhcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplHmcHmcVWmVWm = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmcVWmVWmL(gt1,gt2,g2,ZP,cplHmcHmcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplcVWmcVWmVWmVWm1 = 0._dp 
cplcVWmcVWmVWmVWm2 = 0._dp 
cplcVWmcVWmVWmVWm3 = 0._dp 
Call CouplingcVWmcVWmVWmVWmL(g2,cplcVWmcVWmVWmVWm1,cplcVWmcVWmVWmVWm2,cplcVWmcVWmVWmVWm3)



cplcHmVWmVZ = 0._dp 
Do gt1 = 1, 2
Call CouplingcHmVWmVZL(gt1,g1,g2,v1,v2,ZZ,ZP,cplcHmVWmVZ(gt1))

End Do 


cplcHmVPVWm = 0._dp 
Do gt1 = 1, 2
Call CouplingcHmVPVWmL(gt1,g1,g2,v1,v2,ZZ,ZP,cplcHmVPVWm(gt1))

End Do 


cplAhAhVPVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhAhVPVZL(gt1,gt2,g1,g2,ZZ,ZA,cplAhAhVPVZ(gt1,gt2))

 End Do 
End Do 


cplhhhhVPVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplinghhhhVPVZL(gt1,gt2,g1,g2,ZZ,ZH,cplhhhhVPVZ(gt1,gt2))

 End Do 
End Do 


cplHmcHmVPVZ = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
Call CouplingHmcHmVPVZL(gt1,gt2,g1,g2,ZZ,ZP,cplHmcHmVPVZ(gt1,gt2))

 End Do 
End Do 


cplcVWmVPVWmVZ1 = 0._dp 
cplcVWmVPVWmVZ2 = 0._dp 
cplcVWmVPVWmVZ3 = 0._dp 
Call CouplingcVWmVPVWmVZL(g2,ZZ,cplcVWmVPVWmVZ1,cplcVWmVPVWmVZ2,cplcVWmVPVWmVZ3)



Iname = Iname - 1 
End Subroutine CouplingsForVectorBosons

Subroutine CouplingsForSMfermions(Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,ZA,ZDL,ZDR,ZH,g3,g1,g2,ZZ,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZUL,            & 
& ZUR,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL,ZER,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,               & 
& Vv,cplcUFdFdAhL,cplcUFdFdAhR,cplcUFdFdhhL,cplcUFdFdhhR,cplcUFdFdVGL,cplcUFdFdVGR,      & 
& cplcUFdFdVPL,cplcUFdFdVPR,cplcUFdFdVZL,cplcUFdFdVZR,cplcUFdFuHmL,cplcUFdFuHmR,         & 
& cplcUFdFuVWmL,cplcUFdFuVWmR,cplcUFuFuAhL,cplcUFuFuAhR,cplcUFuFdcHmL,cplcUFuFdcHmR,     & 
& cplcUFuFdcVWmL,cplcUFuFdcVWmR,cplcUFuFuhhL,cplcUFuFuhhR,cplcUFuFuVGL,cplcUFuFuVGR,     & 
& cplcUFuFuVPL,cplcUFuFuVPR,cplcUFuFuVZL,cplcUFuFuVZR,cplcUFeFeAhL,cplcUFeFeAhR,         & 
& cplcUFeFehhL,cplcUFeFehhR,cplcUFeFeVPL,cplcUFeFeVPR,cplcUFeFeVZL,cplcUFeFeVZR,         & 
& cplcUFeFvHmL,cplcUFeFvHmR,cplcUFeFvVWmL,cplcUFeFvVWmR)

Implicit None 
Real(dp), Intent(in) :: ZA(3,3),ZH(3,3),g3,g1,g2

Complex(dp), Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3),              & 
& ZZ(2,2),Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP(2,2),ZUL(3,3),ZUR(3,3),Y1e11,Y1e12,           & 
& Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3),Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,Vv(6,6)

Complex(dp), Intent(out) :: cplcUFdFdAhL(3,3,3),cplcUFdFdAhR(3,3,3),cplcUFdFdhhL(3,3,3),cplcUFdFdhhR(3,3,3),      & 
& cplcUFdFdVGL(3,3),cplcUFdFdVGR(3,3),cplcUFdFdVPL(3,3),cplcUFdFdVPR(3,3),               & 
& cplcUFdFdVZL(3,3),cplcUFdFdVZR(3,3),cplcUFdFuHmL(3,3,2),cplcUFdFuHmR(3,3,2),           & 
& cplcUFdFuVWmL(3,3),cplcUFdFuVWmR(3,3),cplcUFuFuAhL(3,3,3),cplcUFuFuAhR(3,3,3),         & 
& cplcUFuFdcHmL(3,3,2),cplcUFuFdcHmR(3,3,2),cplcUFuFdcVWmL(3,3),cplcUFuFdcVWmR(3,3),     & 
& cplcUFuFuhhL(3,3,3),cplcUFuFuhhR(3,3,3),cplcUFuFuVGL(3,3),cplcUFuFuVGR(3,3),           & 
& cplcUFuFuVPL(3,3),cplcUFuFuVPR(3,3),cplcUFuFuVZL(3,3),cplcUFuFuVZR(3,3),               & 
& cplcUFeFeAhL(3,3,3),cplcUFeFeAhR(3,3,3),cplcUFeFehhL(3,3,3),cplcUFeFehhR(3,3,3),       & 
& cplcUFeFeVPL(3,3),cplcUFeFeVPR(3,3),cplcUFeFeVZL(3,3),cplcUFeFeVZR(3,3),               & 
& cplcUFeFvHmL(3,6,2),cplcUFeFvHmR(3,6,2),cplcUFeFvVWmL(3,6),cplcUFeFvVWmR(3,6)

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'CouplingsForSMfermions'
 
cplcUFdFdAhL = 0._dp 
cplcUFdFdAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFdFdAhL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,ZA,ZDL,ZDR,cplcUFdFdAhL(gt1,gt2,gt3),cplcUFdFdAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFdFdhhL = 0._dp 
cplcUFdFdhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFdFdhhL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,ZH,ZDL,ZDR,cplcUFdFdhhL(gt1,gt2,gt3),cplcUFdFdhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFdFdVGL = 0._dp 
cplcUFdFdVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFdFdVGL(gt1,gt2,g3,ZDL,ZDR,cplcUFdFdVGL(gt1,gt2),cplcUFdFdVGR(gt1,gt2))

 End Do 
End Do 


cplcUFdFdVPL = 0._dp 
cplcUFdFdVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFdFdVPL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcUFdFdVPL(gt1,gt2),cplcUFdFdVPR(gt1,gt2))

 End Do 
End Do 


cplcUFdFdVZL = 0._dp 
cplcUFdFdVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFdFdVZL(gt1,gt2,g1,g2,ZZ,ZDL,ZDR,cplcUFdFdVZL(gt1,gt2),cplcUFdFdVZR(gt1,gt2))

 End Do 
End Do 


cplcUFdFuHmL = 0._dp 
cplcUFdFuHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcUFdFuHmL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZUL,ZUR,cplcUFdFuHmL(gt1,gt2,gt3)   & 
& ,cplcUFdFuHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFdFuVWmL = 0._dp 
cplcUFdFuVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFdFuVWmL(gt1,gt2,g2,ZUL,cplcUFdFuVWmL(gt1,gt2),cplcUFdFuVWmR(gt1,gt2))

 End Do 
End Do 


cplcUFuFuAhL = 0._dp 
cplcUFuFuAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFuFuAhL(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZA,ZUL,              & 
& ZUR,cplcUFuFuAhL(gt1,gt2,gt3),cplcUFuFuAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFuFdcHmL = 0._dp 
cplcUFuFdcHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
Call CouplingcUFuFdcHmL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,              & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZP,ZDL,ZDR,cplcUFuFdcHmL(gt1,gt2,gt3)  & 
& ,cplcUFuFdcHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFuFdcVWmL = 0._dp 
cplcUFuFdcVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFuFdcVWmL(gt1,gt2,g2,ZDL,cplcUFuFdcVWmL(gt1,gt2),cplcUFuFdcVWmR(gt1,gt2))

 End Do 
End Do 


cplcUFuFuhhL = 0._dp 
cplcUFuFuhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFuFuhhL(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZH,ZUL,              & 
& ZUR,cplcUFuFuhhL(gt1,gt2,gt3),cplcUFuFuhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFuFuVGL = 0._dp 
cplcUFuFuVGR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFuFuVGL(gt1,gt2,g3,ZUL,ZUR,cplcUFuFuVGL(gt1,gt2),cplcUFuFuVGR(gt1,gt2))

 End Do 
End Do 


cplcUFuFuVPL = 0._dp 
cplcUFuFuVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFuFuVPL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcUFuFuVPL(gt1,gt2),cplcUFuFuVPR(gt1,gt2))

 End Do 
End Do 


cplcUFuFuVZL = 0._dp 
cplcUFuFuVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFuFuVZL(gt1,gt2,g1,g2,ZZ,ZUL,ZUR,cplcUFuFuVZL(gt1,gt2),cplcUFuFuVZR(gt1,gt2))

 End Do 
End Do 


cplcUFeFeAhL = 0._dp 
cplcUFeFeAhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFeFeAhL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZA,ZEL,              & 
& ZER,cplcUFeFeAhL(gt1,gt2,gt3),cplcUFeFeAhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFeFehhL = 0._dp 
cplcUFeFehhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcUFeFehhL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZH,ZEL,              & 
& ZER,cplcUFeFehhL(gt1,gt2,gt3),cplcUFeFehhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFeFeVPL = 0._dp 
cplcUFeFeVPR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFeFeVPL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcUFeFeVPL(gt1,gt2),cplcUFeFeVPR(gt1,gt2))

 End Do 
End Do 


cplcUFeFeVZL = 0._dp 
cplcUFeFeVZR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingcUFeFeVZL(gt1,gt2,g1,g2,ZZ,ZEL,ZER,cplcUFeFeVZL(gt1,gt2),cplcUFeFeVZR(gt1,gt2))

 End Do 
End Do 


cplcUFeFvHmL = 0._dp 
cplcUFeFvHmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
  Do gt3 = 1, 2
Call CouplingcUFeFvHmL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,               & 
& Y1n12,Y1n21,Y1n22,Y2n33,ZP,Vv,cplcUFeFvHmL(gt1,gt2,gt3),cplcUFeFvHmR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcUFeFvVWmL = 0._dp 
cplcUFeFvVWmR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 6
Call CouplingcUFeFvVWmL(gt1,gt2,g2,Vv,cplcUFeFvVWmL(gt1,gt2),cplcUFeFvVWmR(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
End Subroutine CouplingsForSMfermions

Subroutine CouplingsForTadpoles(Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,       & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,ZH,g1,g2,ZZ,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,           & 
& Y2d31,Y2d32,Y2d33,ZDL,ZDR,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL,ZER,Y1u11,Y1u12,           & 
& Y1u21,Y1u22,Y2u33,ZUL,ZUR,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,               & 
& BB11,BB12,BB21,BB22,Vv,ZP,cplAhAhUhh,cplAhUhhhh,cplAhUhhVP,cplAhUhhVZ,cplcFdFdUhhL,    & 
& cplcFdFdUhhR,cplcFeFeUhhL,cplcFeFeUhhR,cplcFuFuUhhL,cplcFuFuUhhR,cplFvFvUhhL,          & 
& cplFvFvUhhR,cplcgWmgWmUhh,cplcgWpgWpUhh,cplcgZgZUhh,cplUhhhhhh,cplUhhHmcHm,            & 
& cplUhhHmcVWm,cplUhhVPVZ,cplUhhcVWmVWm,cplUhhVZVZ,cplAhAhUhhUhh,cplUhhUhhhhhh,          & 
& cplUhhUhhHmcHm,cplUhhUhhVPVP,cplUhhUhhcVWmVWm,cplUhhUhhVZVZ)

Implicit None 
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3),ZH(3,3),g1,g2

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4,ZZ(2,2),               & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,ZDL(3,3),ZDR(3,3),               & 
& Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL(3,3),ZER(3,3),Y1u11,Y1u12,Y1u21,Y1u22,               & 
& Y2u33,ZUL(3,3),ZUR(3,3),Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Vv(6,6),ZP(2,2)

Complex(dp), Intent(out) :: cplAhAhUhh(3,3,3),cplAhUhhhh(3,3,3),cplAhUhhVP(3,3),cplAhUhhVZ(3,3),cplcFdFdUhhL(3,3,3),& 
& cplcFdFdUhhR(3,3,3),cplcFeFeUhhL(3,3,3),cplcFeFeUhhR(3,3,3),cplcFuFuUhhL(3,3,3),       & 
& cplcFuFuUhhR(3,3,3),cplFvFvUhhL(6,6,3),cplFvFvUhhR(6,6,3),cplcgWmgWmUhh(3),            & 
& cplcgWpgWpUhh(3),cplcgZgZUhh(3),cplUhhhhhh(3,3,3),cplUhhHmcHm(3,2,2),cplUhhHmcVWm(3,2),& 
& cplUhhVPVZ(3),cplUhhcVWmVWm(3),cplUhhVZVZ(3),cplAhAhUhhUhh(3,3,3,3),cplUhhUhhhhhh(3,3,3,3),& 
& cplUhhUhhHmcHm(3,3,2,2),cplUhhUhhVPVP(3,3),cplUhhUhhcVWmVWm(3,3),cplUhhUhhVZVZ(3,3)

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'CouplingsForTadpoles'
 
cplAhAhUhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhUhhL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,cplAhAhUhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhUhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhUhhhhL(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhUhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhUhhVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhUhhVPL(gt1,gt2,g1,g2,ZZ,ZA,cplAhUhhVP(gt1,gt2))

 End Do 
End Do 


cplAhUhhVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingAhUhhVZL(gt1,gt2,g1,g2,ZZ,ZA,cplAhUhhVZ(gt1,gt2))

 End Do 
End Do 


cplcFdFdUhhL = 0._dp 
cplcFdFdUhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFdFdUhhL(gt1,gt2,gt3,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,               & 
& Y2d31,Y2d32,Y2d33,ZDL,ZDR,cplcFdFdUhhL(gt1,gt2,gt3),cplcFdFdUhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFeFeUhhL = 0._dp 
cplcFeFeUhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFeFeUhhL(gt1,gt2,gt3,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,ZEL,ZER,             & 
& cplcFeFeUhhL(gt1,gt2,gt3),cplcFeFeUhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcFuFuUhhL = 0._dp 
cplcFuFuUhhR = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingcFuFuUhhL(gt1,gt2,gt3,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,ZUL,ZUR,             & 
& cplcFuFuUhhL(gt1,gt2,gt3),cplcFuFuUhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplFvFvUhhL = 0._dp 
cplFvFvUhhR = 0._dp 
Do gt1 = 1, 6
 Do gt2 = 1, 6
  Do gt3 = 1, 3
Call CouplingFvFvUhhL(gt1,gt2,gt3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,              & 
& C31,C32,BB11,BB12,BB21,BB22,Vv,cplFvFvUhhL(gt1,gt2,gt3),cplFvFvUhhR(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplcgWmgWmUhh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgWmgWmUhhL(gt3,g2,v1,v2,cplcgWmgWmUhh(gt3))

End Do 


cplcgWpgWpUhh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgWpgWpUhhL(gt3,g2,v1,v2,cplcgWpgWpUhh(gt3))

End Do 


cplcgZgZUhh = 0._dp 
Do gt3 = 1, 3
Call CouplingcgZgZUhhL(gt3,g1,g2,v1,v2,ZZ,cplcgZgZUhh(gt3))

End Do 


cplUhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingUhhhhhhL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,cplUhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplUhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingUhhHmcHmL(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,             & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZP,cplUhhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplUhhHmcVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
Call CouplingUhhHmcVWmL(gt1,gt2,g2,ZP,cplUhhHmcVWm(gt1,gt2))

 End Do 
End Do 


cplUhhVPVZ = 0._dp 
Do gt1 = 1, 3
Call CouplingUhhVPVZL(gt1,g1,g2,v1,v2,ZZ,cplUhhVPVZ(gt1))

End Do 


cplUhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
Call CouplingUhhcVWmVWmL(gt1,g2,v1,v2,cplUhhcVWmVWm(gt1))

End Do 


cplUhhVZVZ = 0._dp 
Do gt1 = 1, 3
Call CouplingUhhVZVZL(gt1,g1,g2,v1,v2,ZZ,cplUhhVZVZ(gt1))

End Do 


cplAhAhUhhUhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhUhhUhhL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,       & 
& Lam3Dash,Aa3,Aa4,ZA,cplAhAhUhhUhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplUhhUhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingUhhUhhhhhhL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,       & 
& Lam3Dash,Aa3,Aa4,ZH,cplUhhUhhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplUhhUhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingUhhUhhHmcHmL(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,               & 
& Lam3Dash,Aa3,Aa4,ZP,cplUhhUhhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplUhhUhhVPVP = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingUhhUhhVPVPL(gt1,gt2,g1,g2,ZZ,cplUhhUhhVPVP(gt1,gt2))

 End Do 
End Do 


cplUhhUhhcVWmVWm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingUhhUhhcVWmVWmL(gt1,gt2,g2,cplUhhUhhcVWmVWm(gt1,gt2))

 End Do 
End Do 


cplUhhUhhVZVZ = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
Call CouplingUhhUhhVZVZL(gt1,gt2,g1,g2,ZZ,cplUhhUhhVZVZ(gt1,gt2))

 End Do 
End Do 


Iname = Iname - 1 
End Subroutine CouplingsForTadpoles

Subroutine CouplingsColourStructures(Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,Lam1,Lam3,           & 
& Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,ZH,ZP,cplAhAhAh,cplAhAhhh,cplAhhhhh,              & 
& cplAhHmcHm,cplhhhhhh,cplhhHmcHm,cplAhAhAhAh,cplAhAhAhhh,cplAhAhhhhh,cplAhAhHmcHm,      & 
& cplAhhhhhhh,cplAhhhHmcHm,cplhhhhhhhh,cplhhhhHmcHm,cplHmHmcHmcHm)

Implicit None 
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3),ZH(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,ZP(2,2)

Complex(dp), Intent(out) :: cplAhAhAh(3,3,3),cplAhAhhh(3,3,3),cplAhhhhh(3,3,3),cplAhHmcHm(3,2,2),cplhhhhhh(3,3,3),& 
& cplhhHmcHm(3,2,2),cplAhAhAhAh(3,3,3,3),cplAhAhAhhh(3,3,3,3),cplAhAhhhhh(3,3,3,3),      & 
& cplAhAhHmcHm(3,3,2,2),cplAhhhhhhh(3,3,3,3),cplAhhhHmcHm(3,3,2,2),cplhhhhhhhh(3,3,3,3), & 
& cplhhhhHmcHm(3,3,2,2),cplHmHmcHmcHm(2,2,2,2)

Integer :: gt1, gt2, gt3, gt4, ct1, ct2, ct3, ct4

Iname = Iname + 1 
NameOfUnit(Iname) = 'CouplingsColourStructures'
 
cplAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhAhCS(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,cplAhAhAh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhAhhhCS(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhAhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplingAhhhhhCS(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,cplAhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplingAhHmcHmCS(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,ZP,               & 
& cplAhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
Call CouplinghhhhhhCS(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,              & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,cplhhhhhh(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 2
  Do gt3 = 1, 2
Call CouplinghhHmcHmCS(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,             & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZP,cplhhHmcHm(gt1,gt2,gt3))

  End Do 
 End Do 
End Do 


cplAhAhAhAh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhAhAhCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa3,Aa4,ZA,cplAhAhAhAh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhAhAhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhAhhhCS(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,cplAhAhAhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhAhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhAhhhhhCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa3,Aa4,ZH,ZA,cplAhAhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhAhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingAhAhHmcHmCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,       & 
& Aa3,Aa4,ZA,ZP,cplAhAhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplingAhhhhhhhCS(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,cplAhhhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplAhhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingAhhhHmcHmCS(gt1,gt2,gt3,gt4,Lam4,Aa3,Aa4,ZH,ZA,ZP,cplAhhhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplhhhhhhhh = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 3
   Do gt4 = 1, 3
Call CouplinghhhhhhhhCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa3,Aa4,ZH,cplhhhhhhhh(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplhhhhHmcHm = 0._dp 
Do gt1 = 1, 3
 Do gt2 = 1, 3
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplinghhhhHmcHmCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,       & 
& Aa3,Aa4,ZH,ZP,cplhhhhHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


cplHmHmcHmcHm = 0._dp 
Do gt1 = 1, 2
 Do gt2 = 1, 2
  Do gt3 = 1, 2
   Do gt4 = 1, 2
Call CouplingHmHmcHmcHmCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,ZP,cplHmHmcHmcHm(gt1,gt2,gt3,gt4))

   End Do 
  End Do 
 End Do 
End Do 


Iname = Iname - 1 
End Subroutine CouplingsColourStructures

Subroutine CouplingAhAhAhCS(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1+(Aa1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1))/2._dp
res1 = res1+(Aa1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res1 = res1+(Aa3*v2*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res1 = res1+(Aa4*v2*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res1 = res1-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res1 = res1-(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1))/2._dp
res1 = res1+(Aa1*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2))/2._dp
res1 = res1+(Aa1*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res1 = res1-(Aa3*v1*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res1 = res1-(Aa4*v1*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res1 = res1+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res1 = res1+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2))/2._dp
res1 = res1+(Aa1*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res1 = res1+(Aa3*v2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res1 = res1+(Aa4*v2*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res1 = res1-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res1 = res1-(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3))/2._dp
res1 = res1+(Aa1*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res1 = res1-(Aa3*v1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res1 = res1-(Aa4*v1*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res1 = res1+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res1 = res1+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3))/2._dp
res1 = res1+(Aa3*v2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res1 = res1+(Aa4*v2*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res1 = res1-(v2*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res1 = res1-(v2*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res1 = res1-(Aa3*v1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res1 = res1-(Aa4*v1*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res1 = res1+(v1*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res1 = res1+(v1*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3))/2._dp
res1 = -(0.,1.)*res1 
 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhAhCS  
 
 
Subroutine CouplingAhAhhhCS(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1-2*Lam1*v1*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,1)
res1 = res1-(Lam3*v1*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1))
res1 = res1-(Lam4*v1*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1))
res1 = res1+(Aa1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1+(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1+(Aa1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1+(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(Lam2Dash*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))
res1 = res1+(Aa3*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1+(Aa4*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1+(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1+(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(Lam3*v2*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2))
res1 = res1-(Lam4*v2*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2))
res1 = res1-(Aa1*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1+(Aa4*v3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1-2*Lam2*v2*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,2)
res1 = res1-(Aa1*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1+(Aa4*v3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1+(Aa3*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1+(Aa4*v1*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(Lam3Dash*v2*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))
res1 = res1+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1+(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(Lam2Dash*v3*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,3))
res1 = res1-(Aa1*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Aa3*v2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1+(Aa4*v2*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(v2*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1+(v2*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Aa1*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Lam3Dash*v3*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,3))
res1 = res1+(Aa3*v1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v1*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1+(v1*Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(v1*Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Aa3*v2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1+(Aa4*v2*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(v2*Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1+(v2*Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1+(Aa3*v1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v1*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1+(v1*Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(v1*Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-2*Lam1Dash*v3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,3)

 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhhhCS  
 
 
Subroutine CouplingAhhhhhCS(gt1,gt2,gt3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZA,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1+(Aa1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1+(Aa1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa2*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1+(Aa4*v3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1+(Aa3*v2*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(Aa4*v2*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(v2*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1+(v2*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1+(Aa1*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1-(Aa1*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1+(Conjg(Aa1)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1+(v3*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1+(Aa3*v1*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(Aa4*v1*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(v1*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1+(v1*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1+(Aa1*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa2*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1+(Aa3*v3*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1+(Aa4*v3*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Conjg(Aa1)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1+(Aa3*v2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v2*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(v2*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1+(v2*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Aa1*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v3*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1+(Conjg(Aa1)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1+(v3*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1+(v3*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1+(Aa3*v1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v1*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(v1*Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1+(v1*Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Aa3*v2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v2*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1+(v2*Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1+(v2*Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1+(Aa3*v1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1+(Aa4*v1*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(v1*Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(v1*Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = -(0.,1.)*res1 
 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhhhCS  
 
 
Subroutine CouplingAhHmcHmCS(gt1,gt2,gt3,Lam4,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,               & 
& ZA,ZP,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZA(3,3)

Complex(dp), Intent(in) :: Lam4,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1-(Lam4*v2*Conjg(ZP(gt2,2))*ZA(gt1,1)*ZP(gt3,1))/2._dp
res1 = res1+(Lam4*v1*Conjg(ZP(gt2,2))*ZA(gt1,2)*ZP(gt3,1))/2._dp
res1 = res1-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res1 = res1+(Conjg(Aa2)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))/sqrt(2._dp)
res1 = res1-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1))
res1 = res1+v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZA(gt1,3)*ZP(gt3,1)
res1 = res1+(Lam4*v2*Conjg(ZP(gt2,1))*ZA(gt1,1)*ZP(gt3,2))/2._dp
res1 = res1-(Lam4*v1*Conjg(ZP(gt2,1))*ZA(gt1,2)*ZP(gt3,2))/2._dp
res1 = res1+(Aa1*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))/sqrt(2._dp)
res1 = res1-((Aa2*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res1 = res1+Aa3*v3*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2)
res1 = res1-(Aa4*v3*Conjg(ZP(gt2,1))*ZA(gt1,3)*ZP(gt3,2))
res1 = -(0.,1.)*res1 
 


Iname = Iname - 1 
 
End Subroutine CouplingAhHmcHmCS  
 
 
Subroutine CouplinghhhhhhCS(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,        & 
& Lam3Dash,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'Couplinghhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1-6*Lam1*v1*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,1)
res1 = res1-(Lam3*v2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1))
res1 = res1-(Lam4*v2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1))
res1 = res1-(Lam2Dash*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,1))
res1 = res1-(Lam3*v2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1))
res1 = res1-(Lam4*v2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1))
res1 = res1-(Lam3*v1*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1))
res1 = res1-(Lam4*v1*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1))
res1 = res1-(Aa1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1-(Aa4*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1-(Conjg(Aa1)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1))/2._dp
res1 = res1-(Lam2Dash*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,1))
res1 = res1-(Aa1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(Aa4*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(Conjg(Aa1)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(Lam2Dash*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))
res1 = res1-(Aa3*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(Aa4*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(v2*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(v2*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1))/2._dp
res1 = res1-(Lam3*v2*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2))
res1 = res1-(Lam4*v2*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2))
res1 = res1-(Lam3*v1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2))
res1 = res1-(Lam4*v1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2))
res1 = res1-(Aa1*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1-(Aa4*v3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1-(Conjg(Aa1)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2))/2._dp
res1 = res1-(Lam3*v1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2))
res1 = res1-(Lam4*v1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2))
res1 = res1-6*Lam2*v2*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,2)
res1 = res1-(Lam3Dash*v3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,2))
res1 = res1-(Aa1*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(Aa4*v3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(Conjg(Aa1)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(Lam3Dash*v3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,2))
res1 = res1-(Aa3*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(Aa4*v1*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(Lam3Dash*v2*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))
res1 = res1-(v1*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(v1*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2))/2._dp
res1 = res1-(Lam2Dash*v3*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,3))
res1 = res1-(Aa1*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Conjg(Aa1)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Lam2Dash*v1*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))
res1 = res1-(Aa3*v2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v2*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(v2*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(v2*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3))/2._dp
res1 = res1-(Aa1*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa2*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Aa3*v3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Conjg(Aa1)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(Conjg(Aa2)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/(2._dp*sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(v3*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Lam3Dash*v3*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,3))
res1 = res1-(Aa3*v1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v1*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Lam3Dash*v2*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))
res1 = res1-(v1*Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(v1*Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3))/2._dp
res1 = res1-(Lam2Dash*v1*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))
res1 = res1-(Aa3*v2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v2*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(v2*Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(v2*Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(Aa3*v1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(Aa4*v1*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(Lam3Dash*v2*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))
res1 = res1-(v1*Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-(v1*Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3))/2._dp
res1 = res1-6*Lam1Dash*v3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,3)

 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhhhCS  
 
 
Subroutine CouplinghhHmcHmCS(gt1,gt2,gt3,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,       & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,ZP,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3
Real(dp), Intent(in) :: v1,v2,v3,ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa1,Aa2,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1-2*Lam1*v1*Conjg(ZP(gt2,1))*ZH(gt1,1)*ZP(gt3,1)
res1 = res1-(Lam4*v2*Conjg(ZP(gt2,2))*ZH(gt1,1)*ZP(gt3,1))/2._dp
res1 = res1-(Lam3*v2*Conjg(ZP(gt2,1))*ZH(gt1,2)*ZP(gt3,1))
res1 = res1-(Lam4*v1*Conjg(ZP(gt2,2))*ZH(gt1,2)*ZP(gt3,1))/2._dp
res1 = res1-(Lam2Dash*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,1))
res1 = res1-((Conjg(Aa1)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res1 = res1-((Conjg(Aa2)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))/sqrt(2._dp))
res1 = res1-(v3*Conjg(Aa3)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))
res1 = res1-(v3*Conjg(Aa4)*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,1))
res1 = res1-(Lam4*v2*Conjg(ZP(gt2,1))*ZH(gt1,1)*ZP(gt3,2))/2._dp
res1 = res1-(Lam3*v1*Conjg(ZP(gt2,2))*ZH(gt1,1)*ZP(gt3,2))
res1 = res1-(Lam4*v1*Conjg(ZP(gt2,1))*ZH(gt1,2)*ZP(gt3,2))/2._dp
res1 = res1-2*Lam2*v2*Conjg(ZP(gt2,2))*ZH(gt1,2)*ZP(gt3,2)
res1 = res1-((Aa1*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res1 = res1-((Aa2*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))/sqrt(2._dp))
res1 = res1-(Aa3*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))
res1 = res1-(Aa4*v3*Conjg(ZP(gt2,1))*ZH(gt1,3)*ZP(gt3,2))
res1 = res1-(Lam3Dash*v3*Conjg(ZP(gt2,2))*ZH(gt1,3)*ZP(gt3,2))

 


Iname = Iname - 1 
 
End Subroutine CouplinghhHmcHmCS  
 
 
Subroutine CouplingAhAhAhAhCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,           & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZA,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhAhAh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1-6*Lam1*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,1)
res1 = res1-(Lam3*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,1))
res1 = res1-(Lam4*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,1))
res1 = res1-(Lam2Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,1))
res1 = res1-(Lam3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,1))
res1 = res1-(Lam4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,1))
res1 = res1-(Lam3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,1))
res1 = res1-(Lam4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,1))
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,1))/2._dp
res1 = res1-(Lam2Dash*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,1))
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res1 = res1-(Lam2Dash*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))
res1 = res1+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res1 = res1+(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,1))/2._dp
res1 = res1-(Lam3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,2))
res1 = res1-(Lam4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,2))
res1 = res1-(Lam3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,2))
res1 = res1-(Lam4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,2))
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,2))/2._dp
res1 = res1-(Lam3*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,2))
res1 = res1-(Lam4*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,2))
res1 = res1-6*Lam2*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,2)
res1 = res1-(Lam3Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,2))
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res1 = res1-(Lam3Dash*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,2))
res1 = res1+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res1 = res1+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))/2._dp
res1 = res1-(Lam3Dash*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,2))
res1 = res1-(Lam2Dash*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,1)*ZA(gt4,3))
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res1 = res1-(Lam2Dash*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))
res1 = res1+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res1 = res1+(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZA(gt4,3))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res1 = res1-(Lam3Dash*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,2)*ZA(gt4,3))
res1 = res1+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res1 = res1+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))/2._dp
res1 = res1-(Lam3Dash*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,2)*ZA(gt4,3))
res1 = res1-(Lam2Dash*ZA(gt1,1)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))
res1 = res1+(Aa3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res1 = res1+(Aa4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res1 = res1+(Aa3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res1 = res1+(Aa4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))/2._dp
res1 = res1-(Lam3Dash*ZA(gt1,2)*ZA(gt2,2)*ZA(gt3,3)*ZA(gt4,3))
res1 = res1-6*Lam1Dash*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,3)*ZA(gt4,3)

 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhAhAhCS  
 
 
Subroutine CouplingAhAhAhhhCS(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Aa3,Aa4

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhAhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1-(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1-(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZA(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZA(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Aa3*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1+(Aa3*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZA(gt3,3)*ZH(gt4,3))/2._dp
res1 = -(0.,1.)*res1 
 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhAhhhCS  
 
 
Subroutine CouplingAhAhhhhhCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,           & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZH,ZA,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1-2*Lam1*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,1)*ZH(gt4,1)
res1 = res1-(Lam3*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res1 = res1-(Lam4*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res1 = res1-(Lam2Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,1))
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1-(Lam3*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res1 = res1-(Lam4*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res1 = res1-2*Lam2*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,2)*ZH(gt4,2)
res1 = res1-(Lam3Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,2))
res1 = res1-(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1+(Aa3*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Aa3*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Aa4*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZA(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Aa3*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Aa4*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Lam2Dash*ZA(gt1,1)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))
res1 = res1-(Aa3*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,2)*ZA(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Aa3*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,1)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Lam3Dash*ZA(gt1,2)*ZA(gt2,2)*ZH(gt3,3)*ZH(gt4,3))
res1 = res1-2*Lam1Dash*ZA(gt1,3)*ZA(gt2,3)*ZH(gt3,3)*ZH(gt4,3)

 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhhhhhCS  
 
 
Subroutine CouplingAhAhHmcHmCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,          & 
& Lam3Dash,Aa3,Aa4,ZA,ZP,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZA(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhAhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1-2*Lam1*Conjg(ZP(gt3,1))*ZA(gt1,1)*ZA(gt2,1)*ZP(gt4,1)
res1 = res1-(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,2)*ZA(gt2,1)*ZP(gt4,1))/2._dp
res1 = res1-(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,1)*ZA(gt2,2)*ZP(gt4,1))/2._dp
res1 = res1-(Lam3*Conjg(ZP(gt3,1))*ZA(gt1,2)*ZA(gt2,2)*ZP(gt4,1))
res1 = res1-(Lam2Dash*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1))
res1 = res1+Conjg(Aa3)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1)
res1 = res1+Conjg(Aa4)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,1)
res1 = res1-(Lam3*Conjg(ZP(gt3,2))*ZA(gt1,1)*ZA(gt2,1)*ZP(gt4,2))
res1 = res1-(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,2)*ZA(gt2,1)*ZP(gt4,2))/2._dp
res1 = res1-(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,1)*ZA(gt2,2)*ZP(gt4,2))/2._dp
res1 = res1-2*Lam2*Conjg(ZP(gt3,2))*ZA(gt1,2)*ZA(gt2,2)*ZP(gt4,2)
res1 = res1+Aa3*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2)
res1 = res1+Aa4*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2)
res1 = res1-(Lam3Dash*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZA(gt2,3)*ZP(gt4,2))

 


Iname = Iname - 1 
 
End Subroutine CouplingAhAhHmcHmCS  
 
 
Subroutine CouplingAhhhhhhhCS(gt1,gt2,gt3,gt4,Aa3,Aa4,ZH,ZA,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Aa3,Aa4

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1+(Aa3*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Aa3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Aa4*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Aa3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Aa4*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1+(Aa3*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1+(Aa4*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1+(Aa3*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Aa3*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1+(Aa3*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1+(Aa4*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZA(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Aa3*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa3)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1+(Conjg(Aa4)*ZA(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = -(0.,1.)*res1 
 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhhhhhCS  
 
 
Subroutine CouplingAhhhHmcHmCS(gt1,gt2,gt3,gt4,Lam4,Aa3,Aa4,ZH,ZA,ZP,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3),ZA(3,3)

Complex(dp), Intent(in) :: Lam4,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingAhhhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1+(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,2)*ZH(gt2,1)*ZP(gt4,1))/2._dp
res1 = res1-(Lam4*Conjg(ZP(gt3,2))*ZA(gt1,1)*ZH(gt2,2)*ZP(gt4,1))/2._dp
res1 = res1-(Conjg(Aa3)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res1 = res1+Conjg(Aa4)*Conjg(ZP(gt3,2))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,1)
res1 = res1-(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,2)*ZH(gt2,1)*ZP(gt4,2))/2._dp
res1 = res1+(Lam4*Conjg(ZP(gt3,1))*ZA(gt1,1)*ZH(gt2,2)*ZP(gt4,2))/2._dp
res1 = res1+Aa3*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,2)
res1 = res1-(Aa4*Conjg(ZP(gt3,1))*ZA(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
res1 = -(0.,1.)*res1 
 


Iname = Iname - 1 
 
End Subroutine CouplingAhhhHmcHmCS  
 
 
Subroutine CouplinghhhhhhhhCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam1Dash,           & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,ZH,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'Couplinghhhhhhhh' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1-6*Lam1*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,1)
res1 = res1-(Lam3*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res1 = res1-(Lam4*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,1))
res1 = res1-(Lam2Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,1))
res1 = res1-(Lam3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,1))
res1 = res1-(Lam4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,1))
res1 = res1-(Lam3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,1))
res1 = res1-(Lam4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,1))
res1 = res1-(Aa3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1-(Aa4*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,1))/2._dp
res1 = res1-(Lam2Dash*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,1))
res1 = res1-(Aa3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Aa4*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Lam2Dash*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))
res1 = res1-(Aa3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Aa4*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,1))/2._dp
res1 = res1-(Lam3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,2))
res1 = res1-(Lam4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,2))
res1 = res1-(Lam3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,2))
res1 = res1-(Lam4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,2))
res1 = res1-(Aa3*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1-(Aa4*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,2))/2._dp
res1 = res1-(Lam3*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res1 = res1-(Lam4*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,2))
res1 = res1-6*Lam2*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,2)
res1 = res1-(Lam3Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,2))
res1 = res1-(Aa3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Aa4*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Lam3Dash*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,2))
res1 = res1-(Aa3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Aa4*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))/2._dp
res1 = res1-(Lam3Dash*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,2))
res1 = res1-(Lam2Dash*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,1)*ZH(gt4,3))
res1 = res1-(Aa3*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Lam2Dash*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))
res1 = res1-(Aa3*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,1)*ZH(gt4,3))/2._dp
res1 = res1-(Aa3*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,3)*ZH(gt2,1)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Lam3Dash*ZH(gt1,3)*ZH(gt2,2)*ZH(gt3,2)*ZH(gt4,3))
res1 = res1-(Aa3*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))/2._dp
res1 = res1-(Lam3Dash*ZH(gt1,2)*ZH(gt2,3)*ZH(gt3,2)*ZH(gt4,3))
res1 = res1-(Lam2Dash*ZH(gt1,1)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))
res1 = res1-(Aa3*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,2)*ZH(gt2,1)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Aa3*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Aa4*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa3)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Conjg(Aa4)*ZH(gt1,1)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))/2._dp
res1 = res1-(Lam3Dash*ZH(gt1,2)*ZH(gt2,2)*ZH(gt3,3)*ZH(gt4,3))
res1 = res1-6*Lam1Dash*ZH(gt1,3)*ZH(gt2,3)*ZH(gt3,3)*ZH(gt4,3)

 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhhhhhCS  
 
 
Subroutine CouplinghhhhHmcHmCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,Lam2Dash,          & 
& Lam3Dash,Aa3,Aa4,ZH,ZP,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Real(dp), Intent(in) :: ZH(3,3)

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Aa3,Aa4,ZP(2,2)

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplinghhhhHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.3)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1-2*Lam1*Conjg(ZP(gt3,1))*ZH(gt1,1)*ZH(gt2,1)*ZP(gt4,1)
res1 = res1-(Lam4*Conjg(ZP(gt3,2))*ZH(gt1,2)*ZH(gt2,1)*ZP(gt4,1))/2._dp
res1 = res1-(Lam4*Conjg(ZP(gt3,2))*ZH(gt1,1)*ZH(gt2,2)*ZP(gt4,1))/2._dp
res1 = res1-(Lam3*Conjg(ZP(gt3,1))*ZH(gt1,2)*ZH(gt2,2)*ZP(gt4,1))
res1 = res1-(Lam2Dash*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res1 = res1-(Conjg(Aa3)*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res1 = res1-(Conjg(Aa4)*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,1))
res1 = res1-(Lam3*Conjg(ZP(gt3,2))*ZH(gt1,1)*ZH(gt2,1)*ZP(gt4,2))
res1 = res1-(Lam4*Conjg(ZP(gt3,1))*ZH(gt1,2)*ZH(gt2,1)*ZP(gt4,2))/2._dp
res1 = res1-(Lam4*Conjg(ZP(gt3,1))*ZH(gt1,1)*ZH(gt2,2)*ZP(gt4,2))/2._dp
res1 = res1-2*Lam2*Conjg(ZP(gt3,2))*ZH(gt1,2)*ZH(gt2,2)*ZP(gt4,2)
res1 = res1-(Aa3*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
res1 = res1-(Aa4*Conjg(ZP(gt3,1))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))
res1 = res1-(Lam3Dash*Conjg(ZP(gt3,2))*ZH(gt1,3)*ZH(gt2,3)*ZP(gt4,2))

 


Iname = Iname - 1 
 
End Subroutine CouplinghhhhHmcHmCS  
 
 
Subroutine CouplingHmHmcHmcHmCS(gt1,gt2,gt3,gt4,Lam1,Lam3,Lam4,Lam2,ZP,res1)

Implicit None 

Integer, Intent(in) :: gt1,gt2,gt3,gt4
Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,ZP(2,2)

Complex(dp), Intent(out) :: res1 
Integer :: j1,j2,j3,j4,j5,j6, j7, j8, j9, j10, j11, j12 
Iname = Iname +1 
NameOfUnit(Iname) = 'CouplingHmHmcHmcHm' 
 
If ((gt1.Lt.1).Or.(gt1.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt1 out of range', gt1 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt1 out of range', gt1 
  Call TerminateProgram 
End If 

If ((gt2.Lt.1).Or.(gt2.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt2 out of range', gt2 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt2 out of range', gt2 
  Call TerminateProgram 
End If 

If ((gt3.Lt.1).Or.(gt3.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt3 out of range', gt3 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt3 out of range', gt3 
  Call TerminateProgram 
End If 

If ((gt4.Lt.1).Or.(gt4.Gt.2)) Then 
  Write (ErrCan,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (ErrCan,*) 'index gt4 out of range', gt4 
  Write (*,*) 'Problem in Subroutine ',NameOfUnit(Iname) 
  Write (*,*) 'index gt4 out of range', gt4 
  Call TerminateProgram 
End If 

res1 = 0._dp 
res1 = res1-4*Lam1*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,1)
res1 = res1-(Lam3*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,2)*ZP(gt4,1))
res1 = res1-(Lam4*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,2)*ZP(gt4,1))
res1 = res1-(Lam3*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,1))
res1 = res1-(Lam4*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,1))
res1 = res1-(Lam3*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,2))
res1 = res1-(Lam4*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,1))*ZP(gt3,1)*ZP(gt4,2))
res1 = res1-(Lam3*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,1)*ZP(gt4,2))
res1 = res1-(Lam4*Conjg(ZP(gt1,1))*Conjg(ZP(gt2,2))*ZP(gt3,1)*ZP(gt4,2))
res1 = res1-4*Lam2*Conjg(ZP(gt1,2))*Conjg(ZP(gt2,2))*ZP(gt3,2)*ZP(gt4,2)

 


Iname = Iname - 1 
 
End Subroutine CouplingHmHmcHmcHmCS  
 
 
End Module Couplings_BGLNCS 
