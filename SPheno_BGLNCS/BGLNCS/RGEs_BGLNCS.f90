! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 21:59 on 7.5.2021   
! ----------------------------------------------------------------------  
 
 
Module RGEs_BGLNCS 
 
Use Control 
Use Settings 
Use Model_Data_BGLNCS 
Use Mathematics 
 
Logical,Private,Save::OnlyDiagonal

Contains 


Subroutine GToParameters96(g,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,          & 
& Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,          & 
& Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,               & 
& Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3)

Implicit None 
Real(dp), Intent(in) :: g(96) 
Real(dp),Intent(out) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp),Intent(out) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Aa1,Aa2,Mub,Mu3

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'GToParameters96' 
 
g1= g(1) 
g2= g(2) 
g3= g(3) 
Lam1= Cmplx(g(4),g(5),dp) 
Lam3= Cmplx(g(6),g(7),dp) 
Lam4= Cmplx(g(8),g(9),dp) 
Lam2= Cmplx(g(10),g(11),dp) 
Lam1Dash= Cmplx(g(12),g(13),dp) 
Lam2Dash= Cmplx(g(14),g(15),dp) 
Lam3Dash= Cmplx(g(16),g(17),dp) 
Aa3= Cmplx(g(18),g(19),dp) 
Aa4= Cmplx(g(20),g(21),dp) 
Y1d11= Cmplx(g(22),g(23),dp) 
Y1d12= Cmplx(g(24),g(25),dp) 
Y1d13= Cmplx(g(26),g(27),dp) 
Y1d21= Cmplx(g(28),g(29),dp) 
Y1d22= Cmplx(g(30),g(31),dp) 
Y1d23= Cmplx(g(32),g(33),dp) 
Y2d31= Cmplx(g(34),g(35),dp) 
Y2d32= Cmplx(g(36),g(37),dp) 
Y2d33= Cmplx(g(38),g(39),dp) 
Y1u11= Cmplx(g(40),g(41),dp) 
Y1u12= Cmplx(g(42),g(43),dp) 
Y1u21= Cmplx(g(44),g(45),dp) 
Y1u22= Cmplx(g(46),g(47),dp) 
Y2u33= Cmplx(g(48),g(49),dp) 
Y1e11= Cmplx(g(50),g(51),dp) 
Y1e12= Cmplx(g(52),g(53),dp) 
Y1e21= Cmplx(g(54),g(55),dp) 
Y1e22= Cmplx(g(56),g(57),dp) 
Y2e33= Cmplx(g(58),g(59),dp) 
Y1n11= Cmplx(g(60),g(61),dp) 
Y1n12= Cmplx(g(62),g(63),dp) 
Y1n21= Cmplx(g(64),g(65),dp) 
Y1n22= Cmplx(g(66),g(67),dp) 
Y2n33= Cmplx(g(68),g(69),dp) 
C13= Cmplx(g(70),g(71),dp) 
C23= Cmplx(g(72),g(73),dp) 
C31= Cmplx(g(74),g(75),dp) 
C32= Cmplx(g(76),g(77),dp) 
BB11= Cmplx(g(78),g(79),dp) 
BB12= Cmplx(g(80),g(81),dp) 
BB21= Cmplx(g(82),g(83),dp) 
BB22= Cmplx(g(84),g(85),dp) 
Aa1= Cmplx(g(86),g(87),dp) 
Aa2= Cmplx(g(88),g(89),dp) 
Mu1= g(90) 
Mu2= g(91) 
MuDash= g(92) 
Mub= Cmplx(g(93),g(94),dp) 
Mu3= Cmplx(g(95),g(96),dp) 
Do i1=1,96 
If (g(i1).ne.g(i1)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Write(*,*) "At position ", i1 
 Call TerminateProgram 
End if 
End do 
Iname = Iname - 1 
 
End Subroutine GToParameters96

Subroutine ParametersToG96(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,          & 
& Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,               & 
& Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,g)

Implicit None 
Real(dp), Intent(out) :: g(96) 
Real(dp), Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Aa1,Aa2,Mub,Mu3

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'ParametersToG96' 
 
g(1) = g1  
g(2) = g2  
g(3) = g3  
g(4) = Real(Lam1,dp)  
g(5) = Aimag(Lam1)  
g(6) = Real(Lam3,dp)  
g(7) = Aimag(Lam3)  
g(8) = Real(Lam4,dp)  
g(9) = Aimag(Lam4)  
g(10) = Real(Lam2,dp)  
g(11) = Aimag(Lam2)  
g(12) = Real(Lam1Dash,dp)  
g(13) = Aimag(Lam1Dash)  
g(14) = Real(Lam2Dash,dp)  
g(15) = Aimag(Lam2Dash)  
g(16) = Real(Lam3Dash,dp)  
g(17) = Aimag(Lam3Dash)  
g(18) = Real(Aa3,dp)  
g(19) = Aimag(Aa3)  
g(20) = Real(Aa4,dp)  
g(21) = Aimag(Aa4)  
g(22) = Real(Y1d11,dp)  
g(23) = Aimag(Y1d11)  
g(24) = Real(Y1d12,dp)  
g(25) = Aimag(Y1d12)  
g(26) = Real(Y1d13,dp)  
g(27) = Aimag(Y1d13)  
g(28) = Real(Y1d21,dp)  
g(29) = Aimag(Y1d21)  
g(30) = Real(Y1d22,dp)  
g(31) = Aimag(Y1d22)  
g(32) = Real(Y1d23,dp)  
g(33) = Aimag(Y1d23)  
g(34) = Real(Y2d31,dp)  
g(35) = Aimag(Y2d31)  
g(36) = Real(Y2d32,dp)  
g(37) = Aimag(Y2d32)  
g(38) = Real(Y2d33,dp)  
g(39) = Aimag(Y2d33)  
g(40) = Real(Y1u11,dp)  
g(41) = Aimag(Y1u11)  
g(42) = Real(Y1u12,dp)  
g(43) = Aimag(Y1u12)  
g(44) = Real(Y1u21,dp)  
g(45) = Aimag(Y1u21)  
g(46) = Real(Y1u22,dp)  
g(47) = Aimag(Y1u22)  
g(48) = Real(Y2u33,dp)  
g(49) = Aimag(Y2u33)  
g(50) = Real(Y1e11,dp)  
g(51) = Aimag(Y1e11)  
g(52) = Real(Y1e12,dp)  
g(53) = Aimag(Y1e12)  
g(54) = Real(Y1e21,dp)  
g(55) = Aimag(Y1e21)  
g(56) = Real(Y1e22,dp)  
g(57) = Aimag(Y1e22)  
g(58) = Real(Y2e33,dp)  
g(59) = Aimag(Y2e33)  
g(60) = Real(Y1n11,dp)  
g(61) = Aimag(Y1n11)  
g(62) = Real(Y1n12,dp)  
g(63) = Aimag(Y1n12)  
g(64) = Real(Y1n21,dp)  
g(65) = Aimag(Y1n21)  
g(66) = Real(Y1n22,dp)  
g(67) = Aimag(Y1n22)  
g(68) = Real(Y2n33,dp)  
g(69) = Aimag(Y2n33)  
g(70) = Real(C13,dp)  
g(71) = Aimag(C13)  
g(72) = Real(C23,dp)  
g(73) = Aimag(C23)  
g(74) = Real(C31,dp)  
g(75) = Aimag(C31)  
g(76) = Real(C32,dp)  
g(77) = Aimag(C32)  
g(78) = Real(BB11,dp)  
g(79) = Aimag(BB11)  
g(80) = Real(BB12,dp)  
g(81) = Aimag(BB12)  
g(82) = Real(BB21,dp)  
g(83) = Aimag(BB21)  
g(84) = Real(BB22,dp)  
g(85) = Aimag(BB22)  
g(86) = Real(Aa1,dp)  
g(87) = Aimag(Aa1)  
g(88) = Real(Aa2,dp)  
g(89) = Aimag(Aa2)  
g(90) = Mu1  
g(91) = Mu2  
g(92) = MuDash  
g(93) = Real(Mub,dp)  
g(94) = Aimag(Mub)  
g(95) = Real(Mu3,dp)  
g(96) = Aimag(Mu3)  
Iname = Iname - 1 
 
End Subroutine ParametersToG96

Subroutine rge96(len, T, GY, F) 
Implicit None 
Integer, Intent(in) :: len 
Real(dp), Intent(in) :: T, GY(len) 
Real(dp), Intent(out) :: F(len) 
Integer :: i1,i2,i3,i4 
Integer :: j1,j2,j3,j4,j5,j6,j7 
Real(dp) :: q 
Real(dp) :: g1,betag11,betag12,Dg1,g2,betag21,betag22,Dg2,g3,betag31,betag32,         & 
& Dg3,Mu1,betaMu11,betaMu12,DMu1,Mu2,betaMu21,betaMu22,DMu2,MuDash,betaMuDash1,          & 
& betaMuDash2,DMuDash
Complex(dp) :: Lam1,betaLam11,betaLam12,DLam1,Lam3,betaLam31,betaLam32,               & 
& DLam3,Lam4,betaLam41,betaLam42,DLam4,Lam2,betaLam21,betaLam22,DLam2,Lam1Dash,          & 
& betaLam1Dash1,betaLam1Dash2,DLam1Dash,Lam2Dash,betaLam2Dash1,betaLam2Dash2,            & 
& DLam2Dash,Lam3Dash,betaLam3Dash1,betaLam3Dash2,DLam3Dash,Aa3,betaAa31,betaAa32,        & 
& DAa3,Aa4,betaAa41,betaAa42,DAa4,Y1d11,betaY1d111,betaY1d112,DY1d11,Y1d12,              & 
& betaY1d121,betaY1d122,DY1d12,Y1d13,betaY1d131,betaY1d132,DY1d13,Y1d21,betaY1d211,      & 
& betaY1d212,DY1d21,Y1d22,betaY1d221,betaY1d222,DY1d22,Y1d23,betaY1d231,betaY1d232,      & 
& DY1d23,Y2d31,betaY2d311,betaY2d312,DY2d31,Y2d32,betaY2d321,betaY2d322,DY2d32,          & 
& Y2d33,betaY2d331,betaY2d332,DY2d33,Y1u11,betaY1u111,betaY1u112,DY1u11,Y1u12,           & 
& betaY1u121,betaY1u122,DY1u12,Y1u21,betaY1u211,betaY1u212,DY1u21,Y1u22,betaY1u221,      & 
& betaY1u222,DY1u22,Y2u33,betaY2u331,betaY2u332,DY2u33,Y1e11,betaY1e111,betaY1e112,      & 
& DY1e11,Y1e12,betaY1e121,betaY1e122,DY1e12,Y1e21,betaY1e211,betaY1e212,DY1e21,          & 
& Y1e22,betaY1e221,betaY1e222,DY1e22,Y2e33,betaY2e331,betaY2e332,DY2e33,Y1n11,           & 
& betaY1n111,betaY1n112,DY1n11,Y1n12,betaY1n121,betaY1n122,DY1n12,Y1n21,betaY1n211,      & 
& betaY1n212,DY1n21,Y1n22,betaY1n221,betaY1n222,DY1n22,Y2n33,betaY2n331,betaY2n332,      & 
& DY2n33,C13,betaC131,betaC132,DC13,C23,betaC231,betaC232,DC23,C31,betaC311,             & 
& betaC312,DC31,C32,betaC321,betaC322,DC32,BB11,betaBB111,betaBB112,DBB11,               & 
& BB12,betaBB121,betaBB122,DBB12,BB21,betaBB211,betaBB212,DBB21,BB22,betaBB221,          & 
& betaBB222,DBB22,Aa1,betaAa11,betaAa12,DAa1,Aa2,betaAa21,betaAa22,DAa2,Mub,             & 
& betaMub1,betaMub2,DMub,Mu3,betaMu31,betaMu32,DMu3
Iname = Iname +1 
NameOfUnit(Iname) = 'rge96' 
 
OnlyDiagonal = .Not.GenerationMixing 
q = t 
 
Call GToParameters96(gy,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,          & 
& Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,               & 
& Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3)



If (TwoLoopRGE) Then 
End If 
 
 
!-------------------- 
! g1 
!-------------------- 
 
betag11  = 0

 
 
If (TwoLoopRGE) Then 
betag12 = 0

 
Dg1 = oo16pi2*( betag11 + oo16pi2 * betag12 ) 

 
Else 
Dg1 = oo16pi2* betag11 
End If 
 
 
!-------------------- 
! g2 
!-------------------- 
 
betag21  = 0

 
 
If (TwoLoopRGE) Then 
betag22 = 0

 
Dg2 = oo16pi2*( betag21 + oo16pi2 * betag22 ) 

 
Else 
Dg2 = oo16pi2* betag21 
End If 
 
 
!-------------------- 
! g3 
!-------------------- 
 
betag31  = 0

 
 
If (TwoLoopRGE) Then 
betag32 = 0

 
Dg3 = oo16pi2*( betag31 + oo16pi2 * betag32 ) 

 
Else 
Dg3 = oo16pi2* betag31 
End If 
 
 
!-------------------- 
! Lam1 
!-------------------- 
 
betaLam11  = 0

 
 
If (TwoLoopRGE) Then 
betaLam12 = 0

 
DLam1 = oo16pi2*( betaLam11 + oo16pi2 * betaLam12 ) 

 
Else 
DLam1 = oo16pi2* betaLam11 
End If 
 
 
Call Chop(DLam1) 

!-------------------- 
! Lam3 
!-------------------- 
 
betaLam31  = 0

 
 
If (TwoLoopRGE) Then 
betaLam32 = 0

 
DLam3 = oo16pi2*( betaLam31 + oo16pi2 * betaLam32 ) 

 
Else 
DLam3 = oo16pi2* betaLam31 
End If 
 
 
Call Chop(DLam3) 

!-------------------- 
! Lam4 
!-------------------- 
 
betaLam41  = 0

 
 
If (TwoLoopRGE) Then 
betaLam42 = 0

 
DLam4 = oo16pi2*( betaLam41 + oo16pi2 * betaLam42 ) 

 
Else 
DLam4 = oo16pi2* betaLam41 
End If 
 
 
Call Chop(DLam4) 

!-------------------- 
! Lam2 
!-------------------- 
 
betaLam21  = 0

 
 
If (TwoLoopRGE) Then 
betaLam22 = 0

 
DLam2 = oo16pi2*( betaLam21 + oo16pi2 * betaLam22 ) 

 
Else 
DLam2 = oo16pi2* betaLam21 
End If 
 
 
Call Chop(DLam2) 

!-------------------- 
! Lam1Dash 
!-------------------- 
 
betaLam1Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam1Dash2 = 0

 
DLam1Dash = oo16pi2*( betaLam1Dash1 + oo16pi2 * betaLam1Dash2 ) 

 
Else 
DLam1Dash = oo16pi2* betaLam1Dash1 
End If 
 
 
Call Chop(DLam1Dash) 

!-------------------- 
! Lam2Dash 
!-------------------- 
 
betaLam2Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam2Dash2 = 0

 
DLam2Dash = oo16pi2*( betaLam2Dash1 + oo16pi2 * betaLam2Dash2 ) 

 
Else 
DLam2Dash = oo16pi2* betaLam2Dash1 
End If 
 
 
Call Chop(DLam2Dash) 

!-------------------- 
! Lam3Dash 
!-------------------- 
 
betaLam3Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam3Dash2 = 0

 
DLam3Dash = oo16pi2*( betaLam3Dash1 + oo16pi2 * betaLam3Dash2 ) 

 
Else 
DLam3Dash = oo16pi2* betaLam3Dash1 
End If 
 
 
Call Chop(DLam3Dash) 

!-------------------- 
! Aa3 
!-------------------- 
 
betaAa31  = 0

 
 
If (TwoLoopRGE) Then 
betaAa32 = 0

 
DAa3 = oo16pi2*( betaAa31 + oo16pi2 * betaAa32 ) 

 
Else 
DAa3 = oo16pi2* betaAa31 
End If 
 
 
Call Chop(DAa3) 

!-------------------- 
! Aa4 
!-------------------- 
 
betaAa41  = 0

 
 
If (TwoLoopRGE) Then 
betaAa42 = 0

 
DAa4 = oo16pi2*( betaAa41 + oo16pi2 * betaAa42 ) 

 
Else 
DAa4 = oo16pi2* betaAa41 
End If 
 
 
Call Chop(DAa4) 

!-------------------- 
! Y1d11 
!-------------------- 
 
betaY1d111  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d112 = 0

 
DY1d11 = oo16pi2*( betaY1d111 + oo16pi2 * betaY1d112 ) 

 
Else 
DY1d11 = oo16pi2* betaY1d111 
End If 
 
 
Call Chop(DY1d11) 

!-------------------- 
! Y1d12 
!-------------------- 
 
betaY1d121  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d122 = 0

 
DY1d12 = oo16pi2*( betaY1d121 + oo16pi2 * betaY1d122 ) 

 
Else 
DY1d12 = oo16pi2* betaY1d121 
End If 
 
 
Call Chop(DY1d12) 

!-------------------- 
! Y1d13 
!-------------------- 
 
betaY1d131  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d132 = 0

 
DY1d13 = oo16pi2*( betaY1d131 + oo16pi2 * betaY1d132 ) 

 
Else 
DY1d13 = oo16pi2* betaY1d131 
End If 
 
 
Call Chop(DY1d13) 

!-------------------- 
! Y1d21 
!-------------------- 
 
betaY1d211  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d212 = 0

 
DY1d21 = oo16pi2*( betaY1d211 + oo16pi2 * betaY1d212 ) 

 
Else 
DY1d21 = oo16pi2* betaY1d211 
End If 
 
 
Call Chop(DY1d21) 

!-------------------- 
! Y1d22 
!-------------------- 
 
betaY1d221  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d222 = 0

 
DY1d22 = oo16pi2*( betaY1d221 + oo16pi2 * betaY1d222 ) 

 
Else 
DY1d22 = oo16pi2* betaY1d221 
End If 
 
 
Call Chop(DY1d22) 

!-------------------- 
! Y1d23 
!-------------------- 
 
betaY1d231  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d232 = 0

 
DY1d23 = oo16pi2*( betaY1d231 + oo16pi2 * betaY1d232 ) 

 
Else 
DY1d23 = oo16pi2* betaY1d231 
End If 
 
 
Call Chop(DY1d23) 

!-------------------- 
! Y2d31 
!-------------------- 
 
betaY2d311  = 0

 
 
If (TwoLoopRGE) Then 
betaY2d312 = 0

 
DY2d31 = oo16pi2*( betaY2d311 + oo16pi2 * betaY2d312 ) 

 
Else 
DY2d31 = oo16pi2* betaY2d311 
End If 
 
 
Call Chop(DY2d31) 

!-------------------- 
! Y2d32 
!-------------------- 
 
betaY2d321  = 0

 
 
If (TwoLoopRGE) Then 
betaY2d322 = 0

 
DY2d32 = oo16pi2*( betaY2d321 + oo16pi2 * betaY2d322 ) 

 
Else 
DY2d32 = oo16pi2* betaY2d321 
End If 
 
 
Call Chop(DY2d32) 

!-------------------- 
! Y2d33 
!-------------------- 
 
betaY2d331  = 0

 
 
If (TwoLoopRGE) Then 
betaY2d332 = 0

 
DY2d33 = oo16pi2*( betaY2d331 + oo16pi2 * betaY2d332 ) 

 
Else 
DY2d33 = oo16pi2* betaY2d331 
End If 
 
 
Call Chop(DY2d33) 

!-------------------- 
! Y1u11 
!-------------------- 
 
betaY1u111  = 0

 
 
If (TwoLoopRGE) Then 
betaY1u112 = 0

 
DY1u11 = oo16pi2*( betaY1u111 + oo16pi2 * betaY1u112 ) 

 
Else 
DY1u11 = oo16pi2* betaY1u111 
End If 
 
 
Call Chop(DY1u11) 

!-------------------- 
! Y1u12 
!-------------------- 
 
betaY1u121  = 0

 
 
If (TwoLoopRGE) Then 
betaY1u122 = 0

 
DY1u12 = oo16pi2*( betaY1u121 + oo16pi2 * betaY1u122 ) 

 
Else 
DY1u12 = oo16pi2* betaY1u121 
End If 
 
 
Call Chop(DY1u12) 

!-------------------- 
! Y1u21 
!-------------------- 
 
betaY1u211  = 0

 
 
If (TwoLoopRGE) Then 
betaY1u212 = 0

 
DY1u21 = oo16pi2*( betaY1u211 + oo16pi2 * betaY1u212 ) 

 
Else 
DY1u21 = oo16pi2* betaY1u211 
End If 
 
 
Call Chop(DY1u21) 

!-------------------- 
! Y1u22 
!-------------------- 
 
betaY1u221  = 0

 
 
If (TwoLoopRGE) Then 
betaY1u222 = 0

 
DY1u22 = oo16pi2*( betaY1u221 + oo16pi2 * betaY1u222 ) 

 
Else 
DY1u22 = oo16pi2* betaY1u221 
End If 
 
 
Call Chop(DY1u22) 

!-------------------- 
! Y2u33 
!-------------------- 
 
betaY2u331  = 0

 
 
If (TwoLoopRGE) Then 
betaY2u332 = 0

 
DY2u33 = oo16pi2*( betaY2u331 + oo16pi2 * betaY2u332 ) 

 
Else 
DY2u33 = oo16pi2* betaY2u331 
End If 
 
 
Call Chop(DY2u33) 

!-------------------- 
! Y1e11 
!-------------------- 
 
betaY1e111  = 0

 
 
If (TwoLoopRGE) Then 
betaY1e112 = 0

 
DY1e11 = oo16pi2*( betaY1e111 + oo16pi2 * betaY1e112 ) 

 
Else 
DY1e11 = oo16pi2* betaY1e111 
End If 
 
 
Call Chop(DY1e11) 

!-------------------- 
! Y1e12 
!-------------------- 
 
betaY1e121  = 0

 
 
If (TwoLoopRGE) Then 
betaY1e122 = 0

 
DY1e12 = oo16pi2*( betaY1e121 + oo16pi2 * betaY1e122 ) 

 
Else 
DY1e12 = oo16pi2* betaY1e121 
End If 
 
 
Call Chop(DY1e12) 

!-------------------- 
! Y1e21 
!-------------------- 
 
betaY1e211  = 0

 
 
If (TwoLoopRGE) Then 
betaY1e212 = 0

 
DY1e21 = oo16pi2*( betaY1e211 + oo16pi2 * betaY1e212 ) 

 
Else 
DY1e21 = oo16pi2* betaY1e211 
End If 
 
 
Call Chop(DY1e21) 

!-------------------- 
! Y1e22 
!-------------------- 
 
betaY1e221  = 0

 
 
If (TwoLoopRGE) Then 
betaY1e222 = 0

 
DY1e22 = oo16pi2*( betaY1e221 + oo16pi2 * betaY1e222 ) 

 
Else 
DY1e22 = oo16pi2* betaY1e221 
End If 
 
 
Call Chop(DY1e22) 

!-------------------- 
! Y2e33 
!-------------------- 
 
betaY2e331  = 0

 
 
If (TwoLoopRGE) Then 
betaY2e332 = 0

 
DY2e33 = oo16pi2*( betaY2e331 + oo16pi2 * betaY2e332 ) 

 
Else 
DY2e33 = oo16pi2* betaY2e331 
End If 
 
 
Call Chop(DY2e33) 

!-------------------- 
! Y1n11 
!-------------------- 
 
betaY1n111  = 0

 
 
If (TwoLoopRGE) Then 
betaY1n112 = 0

 
DY1n11 = oo16pi2*( betaY1n111 + oo16pi2 * betaY1n112 ) 

 
Else 
DY1n11 = oo16pi2* betaY1n111 
End If 
 
 
Call Chop(DY1n11) 

!-------------------- 
! Y1n12 
!-------------------- 
 
betaY1n121  = 0

 
 
If (TwoLoopRGE) Then 
betaY1n122 = 0

 
DY1n12 = oo16pi2*( betaY1n121 + oo16pi2 * betaY1n122 ) 

 
Else 
DY1n12 = oo16pi2* betaY1n121 
End If 
 
 
Call Chop(DY1n12) 

!-------------------- 
! Y1n21 
!-------------------- 
 
betaY1n211  = 0

 
 
If (TwoLoopRGE) Then 
betaY1n212 = 0

 
DY1n21 = oo16pi2*( betaY1n211 + oo16pi2 * betaY1n212 ) 

 
Else 
DY1n21 = oo16pi2* betaY1n211 
End If 
 
 
Call Chop(DY1n21) 

!-------------------- 
! Y1n22 
!-------------------- 
 
betaY1n221  = 0

 
 
If (TwoLoopRGE) Then 
betaY1n222 = 0

 
DY1n22 = oo16pi2*( betaY1n221 + oo16pi2 * betaY1n222 ) 

 
Else 
DY1n22 = oo16pi2* betaY1n221 
End If 
 
 
Call Chop(DY1n22) 

!-------------------- 
! Y2n33 
!-------------------- 
 
betaY2n331  = 0

 
 
If (TwoLoopRGE) Then 
betaY2n332 = 0

 
DY2n33 = oo16pi2*( betaY2n331 + oo16pi2 * betaY2n332 ) 

 
Else 
DY2n33 = oo16pi2* betaY2n331 
End If 
 
 
Call Chop(DY2n33) 

!-------------------- 
! C13 
!-------------------- 
 
betaC131  = 0

 
 
If (TwoLoopRGE) Then 
betaC132 = 0

 
DC13 = oo16pi2*( betaC131 + oo16pi2 * betaC132 ) 

 
Else 
DC13 = oo16pi2* betaC131 
End If 
 
 
Call Chop(DC13) 

!-------------------- 
! C23 
!-------------------- 
 
betaC231  = 0

 
 
If (TwoLoopRGE) Then 
betaC232 = 0

 
DC23 = oo16pi2*( betaC231 + oo16pi2 * betaC232 ) 

 
Else 
DC23 = oo16pi2* betaC231 
End If 
 
 
Call Chop(DC23) 

!-------------------- 
! C31 
!-------------------- 
 
betaC311  = 0

 
 
If (TwoLoopRGE) Then 
betaC312 = 0

 
DC31 = oo16pi2*( betaC311 + oo16pi2 * betaC312 ) 

 
Else 
DC31 = oo16pi2* betaC311 
End If 
 
 
Call Chop(DC31) 

!-------------------- 
! C32 
!-------------------- 
 
betaC321  = 0

 
 
If (TwoLoopRGE) Then 
betaC322 = 0

 
DC32 = oo16pi2*( betaC321 + oo16pi2 * betaC322 ) 

 
Else 
DC32 = oo16pi2* betaC321 
End If 
 
 
Call Chop(DC32) 

!-------------------- 
! BB11 
!-------------------- 
 
betaBB111  = 0

 
 
If (TwoLoopRGE) Then 
betaBB112 = 0

 
DBB11 = oo16pi2*( betaBB111 + oo16pi2 * betaBB112 ) 

 
Else 
DBB11 = oo16pi2* betaBB111 
End If 
 
 
Call Chop(DBB11) 

!-------------------- 
! BB12 
!-------------------- 
 
betaBB121  = 0

 
 
If (TwoLoopRGE) Then 
betaBB122 = 0

 
DBB12 = oo16pi2*( betaBB121 + oo16pi2 * betaBB122 ) 

 
Else 
DBB12 = oo16pi2* betaBB121 
End If 
 
 
Call Chop(DBB12) 

!-------------------- 
! BB21 
!-------------------- 
 
betaBB211  = 0

 
 
If (TwoLoopRGE) Then 
betaBB212 = 0

 
DBB21 = oo16pi2*( betaBB211 + oo16pi2 * betaBB212 ) 

 
Else 
DBB21 = oo16pi2* betaBB211 
End If 
 
 
Call Chop(DBB21) 

!-------------------- 
! BB22 
!-------------------- 
 
betaBB221  = 0

 
 
If (TwoLoopRGE) Then 
betaBB222 = 0

 
DBB22 = oo16pi2*( betaBB221 + oo16pi2 * betaBB222 ) 

 
Else 
DBB22 = oo16pi2* betaBB221 
End If 
 
 
Call Chop(DBB22) 

!-------------------- 
! Aa1 
!-------------------- 
 
betaAa11  = 0

 
 
If (TwoLoopRGE) Then 
betaAa12 = 0

 
DAa1 = oo16pi2*( betaAa11 + oo16pi2 * betaAa12 ) 

 
Else 
DAa1 = oo16pi2* betaAa11 
End If 
 
 
Call Chop(DAa1) 

!-------------------- 
! Aa2 
!-------------------- 
 
betaAa21  = 0

 
 
If (TwoLoopRGE) Then 
betaAa22 = 0

 
DAa2 = oo16pi2*( betaAa21 + oo16pi2 * betaAa22 ) 

 
Else 
DAa2 = oo16pi2* betaAa21 
End If 
 
 
Call Chop(DAa2) 

!-------------------- 
! Mu1 
!-------------------- 
 
betaMu11  = 0

 
 
If (TwoLoopRGE) Then 
betaMu12 = 0

 
DMu1 = oo16pi2*( betaMu11 + oo16pi2 * betaMu12 ) 

 
Else 
DMu1 = oo16pi2* betaMu11 
End If 
 
 
!-------------------- 
! Mu2 
!-------------------- 
 
betaMu21  = 0

 
 
If (TwoLoopRGE) Then 
betaMu22 = 0

 
DMu2 = oo16pi2*( betaMu21 + oo16pi2 * betaMu22 ) 

 
Else 
DMu2 = oo16pi2* betaMu21 
End If 
 
 
!-------------------- 
! MuDash 
!-------------------- 
 
betaMuDash1  = 0

 
 
If (TwoLoopRGE) Then 
betaMuDash2 = 0

 
DMuDash = oo16pi2*( betaMuDash1 + oo16pi2 * betaMuDash2 ) 

 
Else 
DMuDash = oo16pi2* betaMuDash1 
End If 
 
 
!-------------------- 
! Mub 
!-------------------- 
 
betaMub1  = 0

 
 
If (TwoLoopRGE) Then 
betaMub2 = 0

 
DMub = oo16pi2*( betaMub1 + oo16pi2 * betaMub2 ) 

 
Else 
DMub = oo16pi2* betaMub1 
End If 
 
 
Call Chop(DMub) 

!-------------------- 
! Mu3 
!-------------------- 
 
betaMu31  = 0

 
 
If (TwoLoopRGE) Then 
betaMu32 = 0

 
DMu3 = oo16pi2*( betaMu31 + oo16pi2 * betaMu32 ) 

 
Else 
DMu3 = oo16pi2* betaMu31 
End If 
 
 
Call Chop(DMu3) 

Call ParametersToG96(Dg1,Dg2,Dg3,DLam1,DLam3,DLam4,DLam2,DLam1Dash,DLam2Dash,         & 
& DLam3Dash,DAa3,DAa4,DY1d11,DY1d12,DY1d13,DY1d21,DY1d22,DY1d23,DY2d31,DY2d32,           & 
& DY2d33,DY1u11,DY1u12,DY1u21,DY1u22,DY2u33,DY1e11,DY1e12,DY1e21,DY1e22,DY2e33,          & 
& DY1n11,DY1n12,DY1n21,DY1n22,DY2n33,DC13,DC23,DC31,DC32,DBB11,DBB12,DBB21,              & 
& DBB22,DAa1,DAa2,DMu1,DMu2,DMuDash,DMub,DMu3,f)

Iname = Iname - 1 
 
End Subroutine rge96  

Subroutine GToParameters99(g,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,          & 
& Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,          & 
& Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,               & 
& Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,            & 
& Mu3,v1,v2,v3)

Implicit None 
Real(dp), Intent(in) :: g(99) 
Real(dp),Intent(out) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp),Intent(out) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Aa1,Aa2,Mub,Mu3

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'GToParameters99' 
 
g1= g(1) 
g2= g(2) 
g3= g(3) 
Lam1= Cmplx(g(4),g(5),dp) 
Lam3= Cmplx(g(6),g(7),dp) 
Lam4= Cmplx(g(8),g(9),dp) 
Lam2= Cmplx(g(10),g(11),dp) 
Lam1Dash= Cmplx(g(12),g(13),dp) 
Lam2Dash= Cmplx(g(14),g(15),dp) 
Lam3Dash= Cmplx(g(16),g(17),dp) 
Aa3= Cmplx(g(18),g(19),dp) 
Aa4= Cmplx(g(20),g(21),dp) 
Y1d11= Cmplx(g(22),g(23),dp) 
Y1d12= Cmplx(g(24),g(25),dp) 
Y1d13= Cmplx(g(26),g(27),dp) 
Y1d21= Cmplx(g(28),g(29),dp) 
Y1d22= Cmplx(g(30),g(31),dp) 
Y1d23= Cmplx(g(32),g(33),dp) 
Y2d31= Cmplx(g(34),g(35),dp) 
Y2d32= Cmplx(g(36),g(37),dp) 
Y2d33= Cmplx(g(38),g(39),dp) 
Y1u11= Cmplx(g(40),g(41),dp) 
Y1u12= Cmplx(g(42),g(43),dp) 
Y1u21= Cmplx(g(44),g(45),dp) 
Y1u22= Cmplx(g(46),g(47),dp) 
Y2u33= Cmplx(g(48),g(49),dp) 
Y1e11= Cmplx(g(50),g(51),dp) 
Y1e12= Cmplx(g(52),g(53),dp) 
Y1e21= Cmplx(g(54),g(55),dp) 
Y1e22= Cmplx(g(56),g(57),dp) 
Y2e33= Cmplx(g(58),g(59),dp) 
Y1n11= Cmplx(g(60),g(61),dp) 
Y1n12= Cmplx(g(62),g(63),dp) 
Y1n21= Cmplx(g(64),g(65),dp) 
Y1n22= Cmplx(g(66),g(67),dp) 
Y2n33= Cmplx(g(68),g(69),dp) 
C13= Cmplx(g(70),g(71),dp) 
C23= Cmplx(g(72),g(73),dp) 
C31= Cmplx(g(74),g(75),dp) 
C32= Cmplx(g(76),g(77),dp) 
BB11= Cmplx(g(78),g(79),dp) 
BB12= Cmplx(g(80),g(81),dp) 
BB21= Cmplx(g(82),g(83),dp) 
BB22= Cmplx(g(84),g(85),dp) 
Aa1= Cmplx(g(86),g(87),dp) 
Aa2= Cmplx(g(88),g(89),dp) 
Mu1= g(90) 
Mu2= g(91) 
MuDash= g(92) 
Mub= Cmplx(g(93),g(94),dp) 
Mu3= Cmplx(g(95),g(96),dp) 
v1= g(97) 
v2= g(98) 
v3= g(99) 
Do i1=1,99 
If (g(i1).ne.g(i1)) Then 
 Write(*,*) "NaN appearing in ",NameOfUnit(Iname) 
 Write(*,*) "At position ", i1 
 Call TerminateProgram 
End if 
End do 
Iname = Iname - 1 
 
End Subroutine GToParameters99

Subroutine ParametersToG99(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            & 
& Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,          & 
& Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,               & 
& Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,            & 
& Mu3,v1,v2,v3,g)

Implicit None 
Real(dp), Intent(out) :: g(99) 
Real(dp), Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Aa1,Aa2,Mub,Mu3

Integer i1, i2, i3, i4, SumI 
 
Iname = Iname +1 
NameOfUnit(Iname) = 'ParametersToG99' 
 
g(1) = g1  
g(2) = g2  
g(3) = g3  
g(4) = Real(Lam1,dp)  
g(5) = Aimag(Lam1)  
g(6) = Real(Lam3,dp)  
g(7) = Aimag(Lam3)  
g(8) = Real(Lam4,dp)  
g(9) = Aimag(Lam4)  
g(10) = Real(Lam2,dp)  
g(11) = Aimag(Lam2)  
g(12) = Real(Lam1Dash,dp)  
g(13) = Aimag(Lam1Dash)  
g(14) = Real(Lam2Dash,dp)  
g(15) = Aimag(Lam2Dash)  
g(16) = Real(Lam3Dash,dp)  
g(17) = Aimag(Lam3Dash)  
g(18) = Real(Aa3,dp)  
g(19) = Aimag(Aa3)  
g(20) = Real(Aa4,dp)  
g(21) = Aimag(Aa4)  
g(22) = Real(Y1d11,dp)  
g(23) = Aimag(Y1d11)  
g(24) = Real(Y1d12,dp)  
g(25) = Aimag(Y1d12)  
g(26) = Real(Y1d13,dp)  
g(27) = Aimag(Y1d13)  
g(28) = Real(Y1d21,dp)  
g(29) = Aimag(Y1d21)  
g(30) = Real(Y1d22,dp)  
g(31) = Aimag(Y1d22)  
g(32) = Real(Y1d23,dp)  
g(33) = Aimag(Y1d23)  
g(34) = Real(Y2d31,dp)  
g(35) = Aimag(Y2d31)  
g(36) = Real(Y2d32,dp)  
g(37) = Aimag(Y2d32)  
g(38) = Real(Y2d33,dp)  
g(39) = Aimag(Y2d33)  
g(40) = Real(Y1u11,dp)  
g(41) = Aimag(Y1u11)  
g(42) = Real(Y1u12,dp)  
g(43) = Aimag(Y1u12)  
g(44) = Real(Y1u21,dp)  
g(45) = Aimag(Y1u21)  
g(46) = Real(Y1u22,dp)  
g(47) = Aimag(Y1u22)  
g(48) = Real(Y2u33,dp)  
g(49) = Aimag(Y2u33)  
g(50) = Real(Y1e11,dp)  
g(51) = Aimag(Y1e11)  
g(52) = Real(Y1e12,dp)  
g(53) = Aimag(Y1e12)  
g(54) = Real(Y1e21,dp)  
g(55) = Aimag(Y1e21)  
g(56) = Real(Y1e22,dp)  
g(57) = Aimag(Y1e22)  
g(58) = Real(Y2e33,dp)  
g(59) = Aimag(Y2e33)  
g(60) = Real(Y1n11,dp)  
g(61) = Aimag(Y1n11)  
g(62) = Real(Y1n12,dp)  
g(63) = Aimag(Y1n12)  
g(64) = Real(Y1n21,dp)  
g(65) = Aimag(Y1n21)  
g(66) = Real(Y1n22,dp)  
g(67) = Aimag(Y1n22)  
g(68) = Real(Y2n33,dp)  
g(69) = Aimag(Y2n33)  
g(70) = Real(C13,dp)  
g(71) = Aimag(C13)  
g(72) = Real(C23,dp)  
g(73) = Aimag(C23)  
g(74) = Real(C31,dp)  
g(75) = Aimag(C31)  
g(76) = Real(C32,dp)  
g(77) = Aimag(C32)  
g(78) = Real(BB11,dp)  
g(79) = Aimag(BB11)  
g(80) = Real(BB12,dp)  
g(81) = Aimag(BB12)  
g(82) = Real(BB21,dp)  
g(83) = Aimag(BB21)  
g(84) = Real(BB22,dp)  
g(85) = Aimag(BB22)  
g(86) = Real(Aa1,dp)  
g(87) = Aimag(Aa1)  
g(88) = Real(Aa2,dp)  
g(89) = Aimag(Aa2)  
g(90) = Mu1  
g(91) = Mu2  
g(92) = MuDash  
g(93) = Real(Mub,dp)  
g(94) = Aimag(Mub)  
g(95) = Real(Mu3,dp)  
g(96) = Aimag(Mu3)  
g(97) = v1  
g(98) = v2  
g(99) = v3  
Iname = Iname - 1 
 
End Subroutine ParametersToG99

Subroutine rge99(len, T, GY, F) 
Implicit None 
Integer, Intent(in) :: len 
Real(dp), Intent(in) :: T, GY(len) 
Real(dp), Intent(out) :: F(len) 
Integer :: i1,i2,i3,i4 
Integer :: j1,j2,j3,j4,j5,j6,j7 
Real(dp) :: q 
Real(dp) :: g1,betag11,betag12,Dg1,g2,betag21,betag22,Dg2,g3,betag31,betag32,         & 
& Dg3,Mu1,betaMu11,betaMu12,DMu1,Mu2,betaMu21,betaMu22,DMu2,MuDash,betaMuDash1,          & 
& betaMuDash2,DMuDash,v1,betav11,betav12,Dv1,v2,betav21,betav22,Dv2,v3,betav31,          & 
& betav32,Dv3
Complex(dp) :: Lam1,betaLam11,betaLam12,DLam1,Lam3,betaLam31,betaLam32,               & 
& DLam3,Lam4,betaLam41,betaLam42,DLam4,Lam2,betaLam21,betaLam22,DLam2,Lam1Dash,          & 
& betaLam1Dash1,betaLam1Dash2,DLam1Dash,Lam2Dash,betaLam2Dash1,betaLam2Dash2,            & 
& DLam2Dash,Lam3Dash,betaLam3Dash1,betaLam3Dash2,DLam3Dash,Aa3,betaAa31,betaAa32,        & 
& DAa3,Aa4,betaAa41,betaAa42,DAa4,Y1d11,betaY1d111,betaY1d112,DY1d11,Y1d12,              & 
& betaY1d121,betaY1d122,DY1d12,Y1d13,betaY1d131,betaY1d132,DY1d13,Y1d21,betaY1d211,      & 
& betaY1d212,DY1d21,Y1d22,betaY1d221,betaY1d222,DY1d22,Y1d23,betaY1d231,betaY1d232,      & 
& DY1d23,Y2d31,betaY2d311,betaY2d312,DY2d31,Y2d32,betaY2d321,betaY2d322,DY2d32,          & 
& Y2d33,betaY2d331,betaY2d332,DY2d33,Y1u11,betaY1u111,betaY1u112,DY1u11,Y1u12,           & 
& betaY1u121,betaY1u122,DY1u12,Y1u21,betaY1u211,betaY1u212,DY1u21,Y1u22,betaY1u221,      & 
& betaY1u222,DY1u22,Y2u33,betaY2u331,betaY2u332,DY2u33,Y1e11,betaY1e111,betaY1e112,      & 
& DY1e11,Y1e12,betaY1e121,betaY1e122,DY1e12,Y1e21,betaY1e211,betaY1e212,DY1e21,          & 
& Y1e22,betaY1e221,betaY1e222,DY1e22,Y2e33,betaY2e331,betaY2e332,DY2e33,Y1n11,           & 
& betaY1n111,betaY1n112,DY1n11,Y1n12,betaY1n121,betaY1n122,DY1n12,Y1n21,betaY1n211,      & 
& betaY1n212,DY1n21,Y1n22,betaY1n221,betaY1n222,DY1n22,Y2n33,betaY2n331,betaY2n332,      & 
& DY2n33,C13,betaC131,betaC132,DC13,C23,betaC231,betaC232,DC23,C31,betaC311,             & 
& betaC312,DC31,C32,betaC321,betaC322,DC32,BB11,betaBB111,betaBB112,DBB11,               & 
& BB12,betaBB121,betaBB122,DBB12,BB21,betaBB211,betaBB212,DBB21,BB22,betaBB221,          & 
& betaBB222,DBB22,Aa1,betaAa11,betaAa12,DAa1,Aa2,betaAa21,betaAa22,DAa2,Mub,             & 
& betaMub1,betaMub2,DMub,Mu3,betaMu31,betaMu32,DMu3
Iname = Iname +1 
NameOfUnit(Iname) = 'rge99' 
 
OnlyDiagonal = .Not.GenerationMixing 
q = t 
 
Call GToParameters99(gy,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,          & 
& Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,               & 
& Y1n22,Y2n33,C13,C23,C31,C32,BB11,BB12,BB21,BB22,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,            & 
& Mu3,v1,v2,v3)



If (TwoLoopRGE) Then 
End If 
 
 
!-------------------- 
! g1 
!-------------------- 
 
betag11  = 0

 
 
If (TwoLoopRGE) Then 
betag12 = 0

 
Dg1 = oo16pi2*( betag11 + oo16pi2 * betag12 ) 

 
Else 
Dg1 = oo16pi2* betag11 
End If 
 
 
!-------------------- 
! g2 
!-------------------- 
 
betag21  = 0

 
 
If (TwoLoopRGE) Then 
betag22 = 0

 
Dg2 = oo16pi2*( betag21 + oo16pi2 * betag22 ) 

 
Else 
Dg2 = oo16pi2* betag21 
End If 
 
 
!-------------------- 
! g3 
!-------------------- 
 
betag31  = 0

 
 
If (TwoLoopRGE) Then 
betag32 = 0

 
Dg3 = oo16pi2*( betag31 + oo16pi2 * betag32 ) 

 
Else 
Dg3 = oo16pi2* betag31 
End If 
 
 
!-------------------- 
! Lam1 
!-------------------- 
 
betaLam11  = 0

 
 
If (TwoLoopRGE) Then 
betaLam12 = 0

 
DLam1 = oo16pi2*( betaLam11 + oo16pi2 * betaLam12 ) 

 
Else 
DLam1 = oo16pi2* betaLam11 
End If 
 
 
Call Chop(DLam1) 

!-------------------- 
! Lam3 
!-------------------- 
 
betaLam31  = 0

 
 
If (TwoLoopRGE) Then 
betaLam32 = 0

 
DLam3 = oo16pi2*( betaLam31 + oo16pi2 * betaLam32 ) 

 
Else 
DLam3 = oo16pi2* betaLam31 
End If 
 
 
Call Chop(DLam3) 

!-------------------- 
! Lam4 
!-------------------- 
 
betaLam41  = 0

 
 
If (TwoLoopRGE) Then 
betaLam42 = 0

 
DLam4 = oo16pi2*( betaLam41 + oo16pi2 * betaLam42 ) 

 
Else 
DLam4 = oo16pi2* betaLam41 
End If 
 
 
Call Chop(DLam4) 

!-------------------- 
! Lam2 
!-------------------- 
 
betaLam21  = 0

 
 
If (TwoLoopRGE) Then 
betaLam22 = 0

 
DLam2 = oo16pi2*( betaLam21 + oo16pi2 * betaLam22 ) 

 
Else 
DLam2 = oo16pi2* betaLam21 
End If 
 
 
Call Chop(DLam2) 

!-------------------- 
! Lam1Dash 
!-------------------- 
 
betaLam1Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam1Dash2 = 0

 
DLam1Dash = oo16pi2*( betaLam1Dash1 + oo16pi2 * betaLam1Dash2 ) 

 
Else 
DLam1Dash = oo16pi2* betaLam1Dash1 
End If 
 
 
Call Chop(DLam1Dash) 

!-------------------- 
! Lam2Dash 
!-------------------- 
 
betaLam2Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam2Dash2 = 0

 
DLam2Dash = oo16pi2*( betaLam2Dash1 + oo16pi2 * betaLam2Dash2 ) 

 
Else 
DLam2Dash = oo16pi2* betaLam2Dash1 
End If 
 
 
Call Chop(DLam2Dash) 

!-------------------- 
! Lam3Dash 
!-------------------- 
 
betaLam3Dash1  = 0

 
 
If (TwoLoopRGE) Then 
betaLam3Dash2 = 0

 
DLam3Dash = oo16pi2*( betaLam3Dash1 + oo16pi2 * betaLam3Dash2 ) 

 
Else 
DLam3Dash = oo16pi2* betaLam3Dash1 
End If 
 
 
Call Chop(DLam3Dash) 

!-------------------- 
! Aa3 
!-------------------- 
 
betaAa31  = 0

 
 
If (TwoLoopRGE) Then 
betaAa32 = 0

 
DAa3 = oo16pi2*( betaAa31 + oo16pi2 * betaAa32 ) 

 
Else 
DAa3 = oo16pi2* betaAa31 
End If 
 
 
Call Chop(DAa3) 

!-------------------- 
! Aa4 
!-------------------- 
 
betaAa41  = 0

 
 
If (TwoLoopRGE) Then 
betaAa42 = 0

 
DAa4 = oo16pi2*( betaAa41 + oo16pi2 * betaAa42 ) 

 
Else 
DAa4 = oo16pi2* betaAa41 
End If 
 
 
Call Chop(DAa4) 

!-------------------- 
! Y1d11 
!-------------------- 
 
betaY1d111  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d112 = 0

 
DY1d11 = oo16pi2*( betaY1d111 + oo16pi2 * betaY1d112 ) 

 
Else 
DY1d11 = oo16pi2* betaY1d111 
End If 
 
 
Call Chop(DY1d11) 

!-------------------- 
! Y1d12 
!-------------------- 
 
betaY1d121  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d122 = 0

 
DY1d12 = oo16pi2*( betaY1d121 + oo16pi2 * betaY1d122 ) 

 
Else 
DY1d12 = oo16pi2* betaY1d121 
End If 
 
 
Call Chop(DY1d12) 

!-------------------- 
! Y1d13 
!-------------------- 
 
betaY1d131  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d132 = 0

 
DY1d13 = oo16pi2*( betaY1d131 + oo16pi2 * betaY1d132 ) 

 
Else 
DY1d13 = oo16pi2* betaY1d131 
End If 
 
 
Call Chop(DY1d13) 

!-------------------- 
! Y1d21 
!-------------------- 
 
betaY1d211  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d212 = 0

 
DY1d21 = oo16pi2*( betaY1d211 + oo16pi2 * betaY1d212 ) 

 
Else 
DY1d21 = oo16pi2* betaY1d211 
End If 
 
 
Call Chop(DY1d21) 

!-------------------- 
! Y1d22 
!-------------------- 
 
betaY1d221  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d222 = 0

 
DY1d22 = oo16pi2*( betaY1d221 + oo16pi2 * betaY1d222 ) 

 
Else 
DY1d22 = oo16pi2* betaY1d221 
End If 
 
 
Call Chop(DY1d22) 

!-------------------- 
! Y1d23 
!-------------------- 
 
betaY1d231  = 0

 
 
If (TwoLoopRGE) Then 
betaY1d232 = 0

 
DY1d23 = oo16pi2*( betaY1d231 + oo16pi2 * betaY1d232 ) 

 
Else 
DY1d23 = oo16pi2* betaY1d231 
End If 
 
 
Call Chop(DY1d23) 

!-------------------- 
! Y2d31 
!-------------------- 
 
betaY2d311  = 0

 
 
If (TwoLoopRGE) Then 
betaY2d312 = 0

 
DY2d31 = oo16pi2*( betaY2d311 + oo16pi2 * betaY2d312 ) 

 
Else 
DY2d31 = oo16pi2* betaY2d311 
End If 
 
 
Call Chop(DY2d31) 

!-------------------- 
! Y2d32 
!-------------------- 
 
betaY2d321  = 0

 
 
If (TwoLoopRGE) Then 
betaY2d322 = 0

 
DY2d32 = oo16pi2*( betaY2d321 + oo16pi2 * betaY2d322 ) 

 
Else 
DY2d32 = oo16pi2* betaY2d321 
End If 
 
 
Call Chop(DY2d32) 

!-------------------- 
! Y2d33 
!-------------------- 
 
betaY2d331  = 0

 
 
If (TwoLoopRGE) Then 
betaY2d332 = 0

 
DY2d33 = oo16pi2*( betaY2d331 + oo16pi2 * betaY2d332 ) 

 
Else 
DY2d33 = oo16pi2* betaY2d331 
End If 
 
 
Call Chop(DY2d33) 

!-------------------- 
! Y1u11 
!-------------------- 
 
betaY1u111  = 0

 
 
If (TwoLoopRGE) Then 
betaY1u112 = 0

 
DY1u11 = oo16pi2*( betaY1u111 + oo16pi2 * betaY1u112 ) 

 
Else 
DY1u11 = oo16pi2* betaY1u111 
End If 
 
 
Call Chop(DY1u11) 

!-------------------- 
! Y1u12 
!-------------------- 
 
betaY1u121  = 0

 
 
If (TwoLoopRGE) Then 
betaY1u122 = 0

 
DY1u12 = oo16pi2*( betaY1u121 + oo16pi2 * betaY1u122 ) 

 
Else 
DY1u12 = oo16pi2* betaY1u121 
End If 
 
 
Call Chop(DY1u12) 

!-------------------- 
! Y1u21 
!-------------------- 
 
betaY1u211  = 0

 
 
If (TwoLoopRGE) Then 
betaY1u212 = 0

 
DY1u21 = oo16pi2*( betaY1u211 + oo16pi2 * betaY1u212 ) 

 
Else 
DY1u21 = oo16pi2* betaY1u211 
End If 
 
 
Call Chop(DY1u21) 

!-------------------- 
! Y1u22 
!-------------------- 
 
betaY1u221  = 0

 
 
If (TwoLoopRGE) Then 
betaY1u222 = 0

 
DY1u22 = oo16pi2*( betaY1u221 + oo16pi2 * betaY1u222 ) 

 
Else 
DY1u22 = oo16pi2* betaY1u221 
End If 
 
 
Call Chop(DY1u22) 

!-------------------- 
! Y2u33 
!-------------------- 
 
betaY2u331  = 0

 
 
If (TwoLoopRGE) Then 
betaY2u332 = 0

 
DY2u33 = oo16pi2*( betaY2u331 + oo16pi2 * betaY2u332 ) 

 
Else 
DY2u33 = oo16pi2* betaY2u331 
End If 
 
 
Call Chop(DY2u33) 

!-------------------- 
! Y1e11 
!-------------------- 
 
betaY1e111  = 0

 
 
If (TwoLoopRGE) Then 
betaY1e112 = 0

 
DY1e11 = oo16pi2*( betaY1e111 + oo16pi2 * betaY1e112 ) 

 
Else 
DY1e11 = oo16pi2* betaY1e111 
End If 
 
 
Call Chop(DY1e11) 

!-------------------- 
! Y1e12 
!-------------------- 
 
betaY1e121  = 0

 
 
If (TwoLoopRGE) Then 
betaY1e122 = 0

 
DY1e12 = oo16pi2*( betaY1e121 + oo16pi2 * betaY1e122 ) 

 
Else 
DY1e12 = oo16pi2* betaY1e121 
End If 
 
 
Call Chop(DY1e12) 

!-------------------- 
! Y1e21 
!-------------------- 
 
betaY1e211  = 0

 
 
If (TwoLoopRGE) Then 
betaY1e212 = 0

 
DY1e21 = oo16pi2*( betaY1e211 + oo16pi2 * betaY1e212 ) 

 
Else 
DY1e21 = oo16pi2* betaY1e211 
End If 
 
 
Call Chop(DY1e21) 

!-------------------- 
! Y1e22 
!-------------------- 
 
betaY1e221  = 0

 
 
If (TwoLoopRGE) Then 
betaY1e222 = 0

 
DY1e22 = oo16pi2*( betaY1e221 + oo16pi2 * betaY1e222 ) 

 
Else 
DY1e22 = oo16pi2* betaY1e221 
End If 
 
 
Call Chop(DY1e22) 

!-------------------- 
! Y2e33 
!-------------------- 
 
betaY2e331  = 0

 
 
If (TwoLoopRGE) Then 
betaY2e332 = 0

 
DY2e33 = oo16pi2*( betaY2e331 + oo16pi2 * betaY2e332 ) 

 
Else 
DY2e33 = oo16pi2* betaY2e331 
End If 
 
 
Call Chop(DY2e33) 

!-------------------- 
! Y1n11 
!-------------------- 
 
betaY1n111  = 0

 
 
If (TwoLoopRGE) Then 
betaY1n112 = 0

 
DY1n11 = oo16pi2*( betaY1n111 + oo16pi2 * betaY1n112 ) 

 
Else 
DY1n11 = oo16pi2* betaY1n111 
End If 
 
 
Call Chop(DY1n11) 

!-------------------- 
! Y1n12 
!-------------------- 
 
betaY1n121  = 0

 
 
If (TwoLoopRGE) Then 
betaY1n122 = 0

 
DY1n12 = oo16pi2*( betaY1n121 + oo16pi2 * betaY1n122 ) 

 
Else 
DY1n12 = oo16pi2* betaY1n121 
End If 
 
 
Call Chop(DY1n12) 

!-------------------- 
! Y1n21 
!-------------------- 
 
betaY1n211  = 0

 
 
If (TwoLoopRGE) Then 
betaY1n212 = 0

 
DY1n21 = oo16pi2*( betaY1n211 + oo16pi2 * betaY1n212 ) 

 
Else 
DY1n21 = oo16pi2* betaY1n211 
End If 
 
 
Call Chop(DY1n21) 

!-------------------- 
! Y1n22 
!-------------------- 
 
betaY1n221  = 0

 
 
If (TwoLoopRGE) Then 
betaY1n222 = 0

 
DY1n22 = oo16pi2*( betaY1n221 + oo16pi2 * betaY1n222 ) 

 
Else 
DY1n22 = oo16pi2* betaY1n221 
End If 
 
 
Call Chop(DY1n22) 

!-------------------- 
! Y2n33 
!-------------------- 
 
betaY2n331  = 0

 
 
If (TwoLoopRGE) Then 
betaY2n332 = 0

 
DY2n33 = oo16pi2*( betaY2n331 + oo16pi2 * betaY2n332 ) 

 
Else 
DY2n33 = oo16pi2* betaY2n331 
End If 
 
 
Call Chop(DY2n33) 

!-------------------- 
! C13 
!-------------------- 
 
betaC131  = 0

 
 
If (TwoLoopRGE) Then 
betaC132 = 0

 
DC13 = oo16pi2*( betaC131 + oo16pi2 * betaC132 ) 

 
Else 
DC13 = oo16pi2* betaC131 
End If 
 
 
Call Chop(DC13) 

!-------------------- 
! C23 
!-------------------- 
 
betaC231  = 0

 
 
If (TwoLoopRGE) Then 
betaC232 = 0

 
DC23 = oo16pi2*( betaC231 + oo16pi2 * betaC232 ) 

 
Else 
DC23 = oo16pi2* betaC231 
End If 
 
 
Call Chop(DC23) 

!-------------------- 
! C31 
!-------------------- 
 
betaC311  = 0

 
 
If (TwoLoopRGE) Then 
betaC312 = 0

 
DC31 = oo16pi2*( betaC311 + oo16pi2 * betaC312 ) 

 
Else 
DC31 = oo16pi2* betaC311 
End If 
 
 
Call Chop(DC31) 

!-------------------- 
! C32 
!-------------------- 
 
betaC321  = 0

 
 
If (TwoLoopRGE) Then 
betaC322 = 0

 
DC32 = oo16pi2*( betaC321 + oo16pi2 * betaC322 ) 

 
Else 
DC32 = oo16pi2* betaC321 
End If 
 
 
Call Chop(DC32) 

!-------------------- 
! BB11 
!-------------------- 
 
betaBB111  = 0

 
 
If (TwoLoopRGE) Then 
betaBB112 = 0

 
DBB11 = oo16pi2*( betaBB111 + oo16pi2 * betaBB112 ) 

 
Else 
DBB11 = oo16pi2* betaBB111 
End If 
 
 
Call Chop(DBB11) 

!-------------------- 
! BB12 
!-------------------- 
 
betaBB121  = 0

 
 
If (TwoLoopRGE) Then 
betaBB122 = 0

 
DBB12 = oo16pi2*( betaBB121 + oo16pi2 * betaBB122 ) 

 
Else 
DBB12 = oo16pi2* betaBB121 
End If 
 
 
Call Chop(DBB12) 

!-------------------- 
! BB21 
!-------------------- 
 
betaBB211  = 0

 
 
If (TwoLoopRGE) Then 
betaBB212 = 0

 
DBB21 = oo16pi2*( betaBB211 + oo16pi2 * betaBB212 ) 

 
Else 
DBB21 = oo16pi2* betaBB211 
End If 
 
 
Call Chop(DBB21) 

!-------------------- 
! BB22 
!-------------------- 
 
betaBB221  = 0

 
 
If (TwoLoopRGE) Then 
betaBB222 = 0

 
DBB22 = oo16pi2*( betaBB221 + oo16pi2 * betaBB222 ) 

 
Else 
DBB22 = oo16pi2* betaBB221 
End If 
 
 
Call Chop(DBB22) 

!-------------------- 
! Aa1 
!-------------------- 
 
betaAa11  = 0

 
 
If (TwoLoopRGE) Then 
betaAa12 = 0

 
DAa1 = oo16pi2*( betaAa11 + oo16pi2 * betaAa12 ) 

 
Else 
DAa1 = oo16pi2* betaAa11 
End If 
 
 
Call Chop(DAa1) 

!-------------------- 
! Aa2 
!-------------------- 
 
betaAa21  = 0

 
 
If (TwoLoopRGE) Then 
betaAa22 = 0

 
DAa2 = oo16pi2*( betaAa21 + oo16pi2 * betaAa22 ) 

 
Else 
DAa2 = oo16pi2* betaAa21 
End If 
 
 
Call Chop(DAa2) 

!-------------------- 
! Mu1 
!-------------------- 
 
betaMu11  = 0

 
 
If (TwoLoopRGE) Then 
betaMu12 = 0

 
DMu1 = oo16pi2*( betaMu11 + oo16pi2 * betaMu12 ) 

 
Else 
DMu1 = oo16pi2* betaMu11 
End If 
 
 
!-------------------- 
! Mu2 
!-------------------- 
 
betaMu21  = 0

 
 
If (TwoLoopRGE) Then 
betaMu22 = 0

 
DMu2 = oo16pi2*( betaMu21 + oo16pi2 * betaMu22 ) 

 
Else 
DMu2 = oo16pi2* betaMu21 
End If 
 
 
!-------------------- 
! MuDash 
!-------------------- 
 
betaMuDash1  = 0

 
 
If (TwoLoopRGE) Then 
betaMuDash2 = 0

 
DMuDash = oo16pi2*( betaMuDash1 + oo16pi2 * betaMuDash2 ) 

 
Else 
DMuDash = oo16pi2* betaMuDash1 
End If 
 
 
!-------------------- 
! Mub 
!-------------------- 
 
betaMub1  = 0

 
 
If (TwoLoopRGE) Then 
betaMub2 = 0

 
DMub = oo16pi2*( betaMub1 + oo16pi2 * betaMub2 ) 

 
Else 
DMub = oo16pi2* betaMub1 
End If 
 
 
Call Chop(DMub) 

!-------------------- 
! Mu3 
!-------------------- 
 
betaMu31  = 0

 
 
If (TwoLoopRGE) Then 
betaMu32 = 0

 
DMu3 = oo16pi2*( betaMu31 + oo16pi2 * betaMu32 ) 

 
Else 
DMu3 = oo16pi2* betaMu31 
End If 
 
 
Call Chop(DMu3) 

!-------------------- 
! v1 
!-------------------- 
 
betav11  = 0

 
 
If (TwoLoopRGE) Then 
betav12 = 0

 
Dv1 = oo16pi2*( betav11 + oo16pi2 * betav12 ) 

 
Else 
Dv1 = oo16pi2* betav11 
End If 
 
 
!-------------------- 
! v2 
!-------------------- 
 
betav21  = 0

 
 
If (TwoLoopRGE) Then 
betav22 = 0

 
Dv2 = oo16pi2*( betav21 + oo16pi2 * betav22 ) 

 
Else 
Dv2 = oo16pi2* betav21 
End If 
 
 
!-------------------- 
! v3 
!-------------------- 
 
betav31  = 0

 
 
If (TwoLoopRGE) Then 
betav32 = 0

 
Dv3 = oo16pi2*( betav31 + oo16pi2 * betav32 ) 

 
Else 
Dv3 = oo16pi2* betav31 
End If 
 
 
Call ParametersToG99(Dg1,Dg2,Dg3,DLam1,DLam3,DLam4,DLam2,DLam1Dash,DLam2Dash,         & 
& DLam3Dash,DAa3,DAa4,DY1d11,DY1d12,DY1d13,DY1d21,DY1d22,DY1d23,DY2d31,DY2d32,           & 
& DY2d33,DY1u11,DY1u12,DY1u21,DY1u22,DY2u33,DY1e11,DY1e12,DY1e21,DY1e22,DY2e33,          & 
& DY1n11,DY1n12,DY1n21,DY1n22,DY2n33,DC13,DC23,DC31,DC32,DBB11,DBB12,DBB21,              & 
& DBB22,DAa1,DAa2,DMu1,DMu2,DMuDash,DMub,DMu3,Dv1,Dv2,Dv3,f)

Iname = Iname - 1 
 
End Subroutine rge99  

End Module RGEs_BGLNCS 
 
