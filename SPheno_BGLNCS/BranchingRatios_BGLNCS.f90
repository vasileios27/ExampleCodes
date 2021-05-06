! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 11:16 on 6.5.2021   
! ----------------------------------------------------------------------  
 
 
Module BranchingRatios_BGLNCS 
 
Use Control 
Use Settings 
Use Couplings_BGLNCS 
Use Model_Data_BGLNCS 
Use LoopCouplings_BGLNCS 
Use Fv3Decays_BGLNCS 
Use Fu3Decays_BGLNCS 
Use Fe3Decays_BGLNCS 
Use Fd3Decays_BGLNCS 
Use TreeLevelDecays_BGLNCS 


 Contains 
 
Subroutine CalculateBR(CTBD,fac3,epsI,deltaM,kont,MAh,MAh2,MFd,MFd2,MFe,              & 
& MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,               & 
& ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,             & 
& Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          & 
& Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,               & 
& Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,gPFu,gTFu,               & 
& BRFu,gPFe,gTFe,BRFe,gPFd,gTFd,BRFd,gPFv,gTFv,BRFv,gPhh,gThh,BRhh,gPAh,gTAh,            & 
& BRAh,gPHm,gTHm,BRHm,gPVZ,gTVZ,BRVZ,gPVWm,gTVWm,BRVWm)

Real(dp), Intent(in) :: epsI, deltaM, fac3 
Integer, Intent(inout) :: kont 
Logical, Intent(in) :: CTBD 
Real(dp),Intent(inout) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp),Intent(inout) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mub,Mu3

Real(dp),Intent(in) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(6),MFv2(6),           & 
& Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(in) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),Vv(6,6),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp),Intent(inout) :: v1,v2,v3

Real(dp),Intent(inout) :: gPFu(3,267),gTFu(3),BRFu(3,267),gPFe(3,273),gTFe(3),BRFe(3,273),gPFd(3,267),          & 
& gTFd(3),BRFd(3,267),gPFv(6,447),gTFv(6),BRFv(6,447),gPhh(3,73),gThh(3),BRhh(3,73),     & 
& gPAh(3,70),gTAh(3),BRAh(3,70),gPHm(2,39),gTHm(2),BRHm(2,39),gPVZ(1,60),gTVZ,           & 
& BRVZ(1,60),gPVWm(1,37),gTVWm,BRVWm(1,37)

Complex(dp) :: cplHiggsPP(3),cplHiggsGG(3),cplPseudoHiggsPP(3),cplPseudoHiggsGG(3),cplHiggsZZvirt(3),& 
& cplHiggsWWvirt(3)

Real(dp) :: gFvFvcFdFd(6,6,3,3),gFvFvcFeFe(6,6,3,3),gFvFvcFuFu(6,6,3,3),gFvFvFvFv(6,6,6,6),       & 
& gFvFecFdFu(6,3,3,3),gFuFucFdFd(3,3,3,3),gFuFucFeFe(3,3,3,3),gFuFucFuFu(3,3,3,3),       & 
& gFuFuFvFv(3,3,6,6),gFuFdcFeFv(3,3,3,6),gFeFecFdFd(3,3,3,3),gFeFecFeFe(3,3,3,3),        & 
& gFeFecFuFu(3,3,3,3),gFeFeFvFv(3,3,6,6),gFeFvcFuFd(3,6,3,3),gFdFdcFdFd(3,3,3,3),        & 
& gFdFdcFeFe(3,3,3,3),gFdFdcFuFu(3,3,3,3),gFdFdFvFv(3,3,6,6),gFdFuFvFe(3,3,6,3)

Complex(dp) :: coup 
Real(dp) :: vev 
Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateBR'
 
Write(*,*) "Calculating branching ratios and decay widths" 
gTVWm = gamW 
gTVZ = gamZ 
gPFu = 0._dp 
gTFu = 0._dp 
BRFu = 0._dp 
Call FuTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,              & 
& ZH,ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,               & 
& Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,           & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPFu(:,1:24),gTFu,BRFu(:,1:24))

Do i1=1,3
gTFu(i1) =Sum(gPFu(i1,:)) 
If (gTFu(i1).Gt.0._dp) BRFu(i1,: ) =gPFu(i1,:)/gTFu(i1) 
End Do 
 

gPFe = 0._dp 
gTFe = 0._dp 
BRFe = 0._dp 
Call FeTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,              & 
& ZH,ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,               & 
& Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,           & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPFe(:,1:30),gTFe,BRFe(:,1:30))

Do i1=1,3
gTFe(i1) =Sum(gPFe(i1,:)) 
If (gTFe(i1).Gt.0._dp) BRFe(i1,: ) =gPFe(i1,:)/gTFe(i1) 
End Do 
 

gPFd = 0._dp 
gTFd = 0._dp 
BRFd = 0._dp 
Call FdTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,              & 
& ZH,ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,               & 
& Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,           & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPFd(:,1:24),gTFd,BRFd(:,1:24))

Do i1=1,3
gTFd(i1) =Sum(gPFd(i1,:)) 
If (gTFd(i1).Gt.0._dp) BRFd(i1,: ) =gPFd(i1,:)/gTFd(i1) 
End Do 
 

gPFv = 0._dp 
gTFv = 0._dp 
BRFv = 0._dp 
Call FvTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,              & 
& ZH,ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,               & 
& Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,           & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPFv(:,1:42),gTFv,BRFv(:,1:42))

Do i1=1,6
gTFv(i1) =Sum(gPFv(i1,:)) 
If (gTFv(i1).Gt.0._dp) BRFv(i1,: ) =gPFv(i1,:)/gTFv(i1) 
End Do 
 

gPhh = 0._dp 
gThh = 0._dp 
BRhh = 0._dp 
Call hhTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,              & 
& ZH,ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,               & 
& Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,           & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPhh,gThh,BRhh)

Do i1=1,3
gThh(i1) =Sum(gPhh(i1,:)) 
If (gThh(i1).Gt.0._dp) BRhh(i1,: ) =gPhh(i1,:)/gThh(i1) 
End Do 
 

gPAh = 0._dp 
gTAh = 0._dp 
BRAh = 0._dp 
Call AhTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,              & 
& ZH,ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,               & 
& Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,           & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPAh,gTAh,BRAh)

Do i1=1,3
gTAh(i1) =Sum(gPAh(i1,:)) 
If (gTAh(i1).Gt.0._dp) BRAh(i1,: ) =gPAh(i1,:)/gTAh(i1) 
End Do 
 

! Set Goldstone Widhts 
gTAh(1)=gTVZ


gPHm = 0._dp 
gTHm = 0._dp 
BRHm = 0._dp 
Call HmTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,              & 
& ZH,ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,               & 
& Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,           & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPHm,gTHm,BRHm)

Do i1=1,2
gTHm(i1) =Sum(gPHm(i1,:)) 
If (gTHm(i1).Gt.0._dp) BRHm(i1,: ) =gPHm(i1,:)/gTHm(i1) 
End Do 
 

! Set Goldstone Widhts 
gTHm(1)=gTVWm


gPVZ = 0._dp 
gTVZ = 0._dp 
BRVZ = 0._dp 
Call VZTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,           & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,              & 
& ZH,ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,               & 
& Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,           & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPVZ,gTVZ,BRVZ)

Do i1=1,1
gTVZ =Sum(gPVZ(i1,:)) 
If (gTVZ.Gt.0._dp) BRVZ(i1,: ) =gPVZ(i1,:)/gTVZ 
End Do 
 

! Set Goldstone Widhts 
gTAh(1)=gTVZ


gPVWm = 0._dp 
gTVWm = 0._dp 
BRVWm = 0._dp 
Call VWmTwoBodyDecay(-1,DeltaM,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,               & 
& MFv2,Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,               & 
& Vv,ZA,ZH,ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,             & 
& Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,             & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,             & 
& BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gPVWm,gTVWm,BRVWm)

Do i1=1,1
gTVWm =Sum(gPVWm(i1,:)) 
If (gTVWm.Gt.0._dp) BRVWm(i1,: ) =gPVWm(i1,:)/gTVWm 
End Do 
 

! Set Goldstone Widhts 
gTHm(1)=gTVWm


If (.Not.CTBD) Then 
If ((Enable3BDecaysF).and.(Calc3BodyDecay_Fu)) Then 
If (MaxVal(gTFu).Lt.MaxVal(fac3*Abs(MFu))) Then 
Call FuThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFuFucFdFd,          & 
& gFuFucFeFe,gFuFucFuFu,gFuFuFvFv,gFuFdcFeFv,epsI,deltaM,.False.,gTFu,gPFu(:,25:267)     & 
& ,BRFu(:,25:267))

Else 
Call FuThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFuFucFdFd,          & 
& gFuFucFeFe,gFuFucFuFu,gFuFuFvFv,gFuFdcFeFv,epsI,deltaM,.True.,gTFu,gPFu(:,25:267)      & 
& ,BRFu(:,25:267))

End If 
 
End If 
Else 
Call FuThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFuFucFdFd,          & 
& gFuFucFeFe,gFuFucFuFu,gFuFuFvFv,gFuFdcFeFv,epsI,deltaM,.False.,gTFu,gPFu(:,25:267)     & 
& ,BRFu(:,25:267))

End If 
Do i1=1,3
gTFu(i1) =Sum(gPFu(i1,:)) 
If (gTFu(i1).Gt.0._dp) BRFu(i1,: ) =gPFu(i1,:)/gTFu(i1) 
End Do 
 

If (.Not.CTBD) Then 
If ((Enable3BDecaysF).and.(Calc3BodyDecay_Fe)) Then 
If (MaxVal(gTFe).Lt.MaxVal(fac3*Abs(MFe))) Then 
Call FeThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFeFecFdFd,          & 
& gFeFecFeFe,gFeFecFuFu,gFeFeFvFv,gFeFvcFuFd,epsI,deltaM,.False.,gTFe,gPFe(:,31:273)     & 
& ,BRFe(:,31:273))

Else 
Call FeThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFeFecFdFd,          & 
& gFeFecFeFe,gFeFecFuFu,gFeFeFvFv,gFeFvcFuFd,epsI,deltaM,.True.,gTFe,gPFe(:,31:273)      & 
& ,BRFe(:,31:273))

End If 
 
End If 
Else 
Call FeThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFeFecFdFd,          & 
& gFeFecFeFe,gFeFecFuFu,gFeFeFvFv,gFeFvcFuFd,epsI,deltaM,.False.,gTFe,gPFe(:,31:273)     & 
& ,BRFe(:,31:273))

End If 
Do i1=1,3
gTFe(i1) =Sum(gPFe(i1,:)) 
If (gTFe(i1).Gt.0._dp) BRFe(i1,: ) =gPFe(i1,:)/gTFe(i1) 
End Do 
 

If (.Not.CTBD) Then 
If ((Enable3BDecaysF).and.(Calc3BodyDecay_Fd)) Then 
If (MaxVal(gTFd).Lt.MaxVal(fac3*Abs(MFd))) Then 
Call FdThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFdFdcFdFd,          & 
& gFdFdcFeFe,gFdFdcFuFu,gFdFdFvFv,gFdFuFvFe,epsI,deltaM,.False.,gTFd,gPFd(:,25:267)      & 
& ,BRFd(:,25:267))

Else 
Call FdThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFdFdcFdFd,          & 
& gFdFdcFeFe,gFdFdcFuFu,gFdFdFvFv,gFdFuFvFe,epsI,deltaM,.True.,gTFd,gPFd(:,25:267)       & 
& ,BRFd(:,25:267))

End If 
 
End If 
Else 
Call FdThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFdFdcFdFd,          & 
& gFdFdcFeFe,gFdFdcFuFu,gFdFdFvFv,gFdFuFvFe,epsI,deltaM,.False.,gTFd,gPFd(:,25:267)      & 
& ,BRFd(:,25:267))

End If 
Do i1=1,3
gTFd(i1) =Sum(gPFd(i1,:)) 
If (gTFd(i1).Gt.0._dp) BRFd(i1,: ) =gPFd(i1,:)/gTFd(i1) 
End Do 
 

If (.Not.CTBD) Then 
If ((Enable3BDecaysF).and.(Calc3BodyDecay_Fv)) Then 
If (MaxVal(gTFv).Lt.MaxVal(fac3*Abs(MFv))) Then 
Call FvThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFvFvcFdFd,          & 
& gFvFvcFeFe,gFvFvcFuFu,gFvFvFvFv,gFvFecFdFu,epsI,deltaM,.False.,gTFv,gPFv(:,43:447)     & 
& ,BRFv(:,43:447))

Else 
Call FvThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFvFvcFdFd,          & 
& gFvFvcFeFe,gFvFvcFuFu,gFvFvFvFv,gFvFecFdFu,epsI,deltaM,.True.,gTFv,gPFv(:,43:447)      & 
& ,BRFv(:,43:447))

End If 
 
End If 
Else 
Call FvThreeBodyDecay(-1,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gTAh,gThh,gTHm,gTVWm,gTVZ,gFvFvcFdFd,          & 
& gFvFvcFeFe,gFvFvcFuFu,gFvFvFvFv,gFvFecFdFu,epsI,deltaM,.False.,gTFv,gPFv(:,43:447)     & 
& ,BRFv(:,43:447))

End If 
Do i1=1,6
gTFv(i1) =Sum(gPFv(i1,:)) 
If (gTFv(i1).Gt.0._dp) BRFv(i1,: ) =gPFv(i1,:)/gTFv(i1) 
End Do 
 

! No 3-body decays for hh  
! No 3-body decays for Ah  
! No 3-body decays for Hm  
! No 3-body decays for VZ  
! No 3-body decays for VWm  
Iname = Iname - 1 
 
End Subroutine CalculateBR 
End Module BranchingRatios_BGLNCS 
 