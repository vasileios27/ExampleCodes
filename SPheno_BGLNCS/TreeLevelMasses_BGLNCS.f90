! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 11:15 on 6.5.2021   
! ----------------------------------------------------------------------  
 
 
Module TreeLevelMasses_BGLNCS 
 
Use Control 
Use Mathematics 
Use MathematicsQP 
Use Settings 
Use Model_Data_BGLNCS 

 
Logical :: SignOfMassChanged =.False.  
Logical :: SignOfMuChanged =.False.  
Contains 
 
Subroutine TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,               & 
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               & 
& ZP,ZW,ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,             & 
& Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,             & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,             & 
& BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,GenerationMixing,kont)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mub,Mu3

Real(dp),Intent(out) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(6),MFv2(6),           & 
& Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(out) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),Vv(6,6),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp),Intent(in) :: v1,v2,v3

Logical, Intent(in) :: GenerationMixing 
Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,kontSave 
Iname = Iname + 1 
NameOfUnit(Iname) = 'TreeMassesBGLNCS'
 
kont = 0 
Call CalculateVPVZ(g1,g2,v1,v2,ZZ,MVZ,MVZ2,kont)

Call CalculateVWm(g2,v1,v2,ZW,MVWm,MVWm2,kont)

Call CalculateMhh(Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,MuDash,Lam1Dash,Lam2Dash,               & 
& Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,Mhh,Mhh2,kont)

kontSave = kont 
Call CalculateMAh(g1,g2,Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,MuDash,Lam1Dash,Lam2Dash,         & 
& Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZZ,ZA,MAh,MAh2,kont)

kont = kontSave 
kontSave = kont 
Call CalculateMHm(g2,Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Mu3,               & 
& Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZP,MHm,MHm2,kont)

kont = kontSave 
Call CalculateMFd(Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,              & 
& v1,v2,ZDL,ZDR,MFd,kont)

MFd2 = MFd**2 
Call CalculateMFu(Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,v1,v2,ZUL,ZUR,MFu,kont)

MFu2 = MFu**2 
Call CalculateMFe(Y1e11,Y1e22,Y1e21,Y2e33,v1,v2,ZEL,ZER,MFe,kont)

MFe2 = MFe**2 
Call CalculateMFv(Y1n11,Y1n22,Y1n12,Y2n33,BB11,C23,C32,v1,v2,v3,Vv,MFv,kont)

MFv2 = MFv**2 

 
 Call SortGoldstones(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,            & 
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,ZZ,kont)

If (SignOfMassChanged) Then  
 If (.Not.IgnoreNegativeMasses) Then 
  Write(*,*) " Stopping calculation because of negative mass squared." 
  Call TerminateProgram 
 Else 
  SignOfMassChanged= .False. 
  kont=0  
 End If 
End If 
If (SignOfMuChanged) Then 
 If (.Not.IgnoreMuSignFlip) Then 
  Write(*,*) " Stopping calculation because of negative mass squared in tadpoles." 
  Call TerminateProgram 
 Else 
  SignOfMuChanged= .False. 
  kont=0 
 End If 
End If 

 ! -------------------------------- 
! Setting Goldstone masses 
! -------------------------------- 
 
MAh(1)=MVZ*sqrt(RXiZ)
MAh2(1)=MVZ2*RXiZ
MHm(1)=MVWm*sqrt(RXiWm)
MHm2(1)=MVWm2*RXiWm
v = Sqrt(v1**2 + v2**2)
TW = ACos(Abs(ZZ(1,1)))
Iname = Iname - 1 
 
End Subroutine  TreeMasses 
 
 
Subroutine RunningFermionMasses(MFeIN,MFe2IN,MFdIN,MFd2IN,MFuIN,MFu2IN,               & 
& v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              & 
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,kont)

Implicit None 
 
Integer, Intent(inout) :: kont 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mub,Mu3

Real(dp),Intent(in) :: v1,v2,v3

Real(dp),Intent(inout) :: MFeIN(3),MFe2IN(3),MFdIN(3),MFd2IN(3),MFuIN(3),MFu2IN(3)

Real(dp) :: MFe(3),MFe2(3),MFd(3),MFd2(3),MFu(3),MFu2(3)

Call CalculateMFe(Y1e11,Y1e22,Y1e21,Y2e33,v1,v2,ZEL,ZER,MFe,kont)

MFe2 = MFe**2 
MFeIN(1:2) = MFe(1:2) 
MFe2IN(1:2) = MFe2(1:2) 
Call CalculateMFd(Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,              & 
& v1,v2,ZDL,ZDR,MFd,kont)

MFd2 = MFd**2 
MFdIN(1:2) = MFd(1:2) 
MFd2IN(1:2) = MFd2(1:2) 
Call CalculateMFu(Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,v1,v2,ZUL,ZUR,MFu,kont)

MFu2 = MFu**2 
MFuIN(1:2) = MFu(1:2) 
MFu2IN(1:2) = MFu2(1:2) 
End Subroutine RunningFermionMasses 

Subroutine TreeMassesEffPot(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,             & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,              & 
& ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,          & 
& Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,             & 
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,             & 
& BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,GenerationMixing,kont)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mub,Mu3

Real(dp),Intent(out) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(6),MFv2(6),           & 
& Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(out) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),Vv(6,6),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp),Intent(in) :: v1,v2,v3

Logical, Intent(in) :: GenerationMixing 
Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,kontSave 
Iname = Iname + 1 
NameOfUnit(Iname) = 'TreeMassesBGLNCS'
 
kont = 0 
Call CalculateVPVZEffPot(g1,g2,v1,v2,ZZ,MVZ,MVZ2,kont)

Call CalculateVWmEffPot(g2,v1,v2,ZW,MVWm,MVWm2,kont)

Call CalculateMhhEffPot(Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,MuDash,Lam1Dash,Lam2Dash,         & 
& Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,Mhh,Mhh2,kont)

kontSave = kont 
Call CalculateMAhEffPot(g1,g2,Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,MuDash,Lam1Dash,            & 
& Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZZ,ZA,MAh,MAh2,kont)

kont = kontSave 
kontSave = kont 
Call CalculateMHmEffPot(g2,Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,             & 
& Mu3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZP,MHm,MHm2,kont)

kont = kontSave 
Call CalculateMFdEffPot(Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,              & 
& Y2d33,v1,v2,ZDL,ZDR,MFd,kont)

MFd2 = MFd**2 
Call CalculateMFuEffPot(Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,v1,v2,ZUL,ZUR,MFu,kont)

MFu2 = MFu**2 
Call CalculateMFeEffPot(Y1e11,Y1e22,Y1e21,Y2e33,v1,v2,ZEL,ZER,MFe,kont)

MFe2 = MFe**2 
Call CalculateMFvEffPot(Y1n11,Y1n22,Y1n12,Y2n33,BB11,C23,C32,v1,v2,v3,Vv,MFv,kont)

MFv2 = MFv**2 

 
 If (SignOfMassChanged) Then  
 If (.Not.IgnoreNegativeMasses) Then 
  Write(*,*) " Stopping calculation because of negative mass squared." 
  Call TerminateProgram 
 Else 
  SignOfMassChanged= .False. 
  kont=0  
 End If 
End If 
If (SignOfMuChanged) Then 
 If (.Not.IgnoreMuSignFlip) Then 
  Write(*,*) " Stopping calculation because of negative mass squared in tadpoles." 
  Call TerminateProgram 
 Else 
  SignOfMuChanged= .False. 
  kont=0 
 End If 
End If 
Iname = Iname - 1 
 
End Subroutine  TreeMassesEffPot 
 
 
Subroutine CalculateMhh(Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,MuDash,Lam1Dash,Lam2Dash,         & 
& Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,Mhh,Mhh2,kont)

Real(dp), Intent(in) :: Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4, pos 
Real(dp), Intent(out) :: Mhh(3), Mhh2(3) 
Real(dp), Intent(out) :: ZH(3,3) 
 
Real(dp) :: mat(3,3)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMhh'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+Mu1
mat(1,1) = mat(1,1)+3*Lam1*v1**2
mat(1,1) = mat(1,1)+(Lam3*v2**2)/2._dp
mat(1,1) = mat(1,1)+(Lam4*v2**2)/2._dp
mat(1,1) = mat(1,1)+(Lam2Dash*v3**2)/2._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+Mu3/2._dp
mat(1,2) = mat(1,2)+Lam3*v1*v2
mat(1,2) = mat(1,2)+Lam4*v1*v2
mat(1,2) = mat(1,2)+(Aa1*v3)/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(Aa2*v3)/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(Aa3*v3**2)/4._dp
mat(1,2) = mat(1,2)+(Aa4*v3**2)/4._dp
mat(1,2) = mat(1,2)+(v3*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(v3*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa3))/4._dp
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa4))/4._dp
mat(1,2) = mat(1,2)+Conjg(Mu3)/2._dp
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(Aa1*v2)/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+(Aa2*v2)/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+Lam2Dash*v1*v3
mat(1,3) = mat(1,3)+(Aa3*v2*v3)/2._dp
mat(1,3) = mat(1,3)+(Aa4*v2*v3)/2._dp
mat(1,3) = mat(1,3)+(v2*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+(v2*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+(v2*v3*Conjg(Aa3))/2._dp
mat(1,3) = mat(1,3)+(v2*v3*Conjg(Aa4))/2._dp
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+Mu2
mat(2,2) = mat(2,2)+(Lam3*v1**2)/2._dp
mat(2,2) = mat(2,2)+(Lam4*v1**2)/2._dp
mat(2,2) = mat(2,2)+3*Lam2*v2**2
mat(2,2) = mat(2,2)+(Lam3Dash*v3**2)/2._dp
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)+(Aa1*v1)/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(Aa2*v1)/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(Aa3*v1*v3)/2._dp
mat(2,3) = mat(2,3)+(Aa4*v1*v3)/2._dp
mat(2,3) = mat(2,3)+Lam3Dash*v2*v3
mat(2,3) = mat(2,3)+(v1*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(v1*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(v1*v3*Conjg(Aa3))/2._dp
mat(2,3) = mat(2,3)+(v1*v3*Conjg(Aa4))/2._dp
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+Mub/2._dp
mat(3,3) = mat(3,3)+MuDash
mat(3,3) = mat(3,3)+(Lam2Dash*v1**2)/2._dp
mat(3,3) = mat(3,3)+(Aa3*v1*v2)/2._dp
mat(3,3) = mat(3,3)+(Aa4*v1*v2)/2._dp
mat(3,3) = mat(3,3)+(Lam3Dash*v2**2)/2._dp
mat(3,3) = mat(3,3)+3*Lam1Dash*v3**2
mat(3,3) = mat(3,3)+(v1*v2*Conjg(Aa3))/2._dp
mat(3,3) = mat(3,3)+(v1*v2*Conjg(Aa4))/2._dp
mat(3,3) = mat(3,3)+Conjg(Mub)/2._dp

 
 Do i1=2,3
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
Call EigenSystem(mat,Mhh2,ZH,ierr,test) 
 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,3
  If (Abs(Mhh2(i1)).Le.MaxMassNumericalZero) Mhh2(i1) = 1.E-10_dp 
  If (Mhh2(i1).ne.Mhh2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Mhh2(i1).Ge.0._dp) Then 
  Mhh(i1)=Sqrt(Mhh2(i1) ) 
  Else 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,Mhh2(i1) 
    End If 
  Mhh(i1) = 1._dp 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,Mhh2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,Mhh2(i1) 
  Mhh2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
Iname = Iname - 1 
 
End Subroutine CalculateMhh 

Subroutine CalculateMAh(g1,g2,Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,MuDash,Lam1Dash,            & 
& Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZZ,ZA,MAh,MAh2,kont)

Real(dp), Intent(in) :: g1,g2,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4,ZZ(2,2)

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4, pos 
Real(dp), Intent(out) :: MAh(3), MAh2(3) 
Real(dp), Intent(out) :: ZA(3,3) 
 
Real(dp) :: mat(3,3)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMAh'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+Mu1
mat(1,1) = mat(1,1)+Lam1*v1**2
mat(1,1) = mat(1,1)+(Lam3*v2**2)/2._dp
mat(1,1) = mat(1,1)+(Lam4*v2**2)/2._dp
mat(1,1) = mat(1,1)+(Lam2Dash*v3**2)/2._dp
mat(1,1) = mat(1,1)+(g1**2*v1**2*Conjg(ZZ(1,2))*RXiZ*ZZ(1,2))/4._dp
mat(1,1) = mat(1,1)-(g1*g2*v1**2*Conjg(ZZ(2,2))*RXiZ*ZZ(1,2))/4._dp
mat(1,1) = mat(1,1)-(g1*g2*v1**2*Conjg(ZZ(1,2))*RXiZ*ZZ(2,2))/4._dp
mat(1,1) = mat(1,1)+(g2**2*v1**2*Conjg(ZZ(2,2))*RXiZ*ZZ(2,2))/4._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+Mu3/2._dp
mat(1,2) = mat(1,2)+(Aa1*v3)/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(Aa2*v3)/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(Aa3*v3**2)/4._dp
mat(1,2) = mat(1,2)+(Aa4*v3**2)/4._dp
mat(1,2) = mat(1,2)+(v3*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(v3*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa3))/4._dp
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa4))/4._dp
mat(1,2) = mat(1,2)+Conjg(Mu3)/2._dp
mat(1,2) = mat(1,2)+(g1**2*v1*v2*Conjg(ZZ(1,2))*RXiZ*ZZ(1,2))/4._dp
mat(1,2) = mat(1,2)-(g1*g2*v1*v2*Conjg(ZZ(2,2))*RXiZ*ZZ(1,2))/4._dp
mat(1,2) = mat(1,2)-(g1*g2*v1*v2*Conjg(ZZ(1,2))*RXiZ*ZZ(2,2))/4._dp
mat(1,2) = mat(1,2)+(g2**2*v1*v2*Conjg(ZZ(2,2))*RXiZ*ZZ(2,2))/4._dp
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(Aa1*v2)/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)-(Aa2*v2)/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+(Aa3*v2*v3)/2._dp
mat(1,3) = mat(1,3)-(Aa4*v2*v3)/2._dp
mat(1,3) = mat(1,3)+(v2*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)-(v2*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+(v2*v3*Conjg(Aa3))/2._dp
mat(1,3) = mat(1,3)-(v2*v3*Conjg(Aa4))/2._dp
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+Mu2
mat(2,2) = mat(2,2)+(Lam3*v1**2)/2._dp
mat(2,2) = mat(2,2)+(Lam4*v1**2)/2._dp
mat(2,2) = mat(2,2)+Lam2*v2**2
mat(2,2) = mat(2,2)+(Lam3Dash*v3**2)/2._dp
mat(2,2) = mat(2,2)+(g1**2*v2**2*Conjg(ZZ(1,2))*RXiZ*ZZ(1,2))/4._dp
mat(2,2) = mat(2,2)-(g1*g2*v2**2*Conjg(ZZ(2,2))*RXiZ*ZZ(1,2))/4._dp
mat(2,2) = mat(2,2)-(g1*g2*v2**2*Conjg(ZZ(1,2))*RXiZ*ZZ(2,2))/4._dp
mat(2,2) = mat(2,2)+(g2**2*v2**2*Conjg(ZZ(2,2))*RXiZ*ZZ(2,2))/4._dp
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)-(Aa1*v1)/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(Aa2*v1)/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)-(Aa3*v1*v3)/2._dp
mat(2,3) = mat(2,3)+(Aa4*v1*v3)/2._dp
mat(2,3) = mat(2,3)-(v1*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(v1*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)-(v1*v3*Conjg(Aa3))/2._dp
mat(2,3) = mat(2,3)+(v1*v3*Conjg(Aa4))/2._dp
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)-1._dp*(Mub)/2._dp
mat(3,3) = mat(3,3)+MuDash
mat(3,3) = mat(3,3)+(Lam2Dash*v1**2)/2._dp
mat(3,3) = mat(3,3)-(Aa3*v1*v2)/2._dp
mat(3,3) = mat(3,3)-(Aa4*v1*v2)/2._dp
mat(3,3) = mat(3,3)+(Lam3Dash*v2**2)/2._dp
mat(3,3) = mat(3,3)+Lam1Dash*v3**2
mat(3,3) = mat(3,3)-(v1*v2*Conjg(Aa3))/2._dp
mat(3,3) = mat(3,3)-(v1*v2*Conjg(Aa4))/2._dp
mat(3,3) = mat(3,3)-Conjg(Mub)/2._dp

 
 Do i1=2,3
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
Call EigenSystem(mat,MAh2,ZA,ierr,test) 
 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,3
  If (Abs(MAh2(i1)).Le.MaxMassNumericalZero) MAh2(i1) = 1.E-10_dp 
  If (MAh2(i1).ne.MAh2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (MAh2(i1).Ge.0._dp) Then 
  MAh(i1)=Sqrt(MAh2(i1) ) 
  Else 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,MAh2(i1) 
    End If 
  MAh(i1) = 1._dp 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,MAh2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,MAh2(i1) 
  MAh2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
Iname = Iname - 1 
 
End Subroutine CalculateMAh 

Subroutine CalculateMHm(g2,Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,             & 
& Mu3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZP,MHm,MHm2,kont)

Real(dp), Intent(in) :: g2,Mu1,Mu2,v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Mu3,Aa1,Aa2,Aa3,Aa4

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4, pos 
Real(dp), Intent(out) :: MHm(2), MHm2(2) 
Complex(dp), Intent(out) :: ZP(2,2) 
 
Complex(dp) :: mat(2,2)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMHm'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+Mu1
mat(1,1) = mat(1,1)+Lam1*v1**2
mat(1,1) = mat(1,1)+(Lam3*v2**2)/2._dp
mat(1,1) = mat(1,1)+(Lam2Dash*v3**2)/2._dp
mat(1,1) = mat(1,1)+(g2**2*v1**2*RXiWm)/4._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+Mu3
mat(1,2) = mat(1,2)+(Lam4*v1*v2)/2._dp
mat(1,2) = mat(1,2)+(v3*Conjg(Aa1))/sqrt(2._dp)
mat(1,2) = mat(1,2)+(v3*Conjg(Aa2))/sqrt(2._dp)
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa3))/2._dp
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa4))/2._dp
mat(1,2) = mat(1,2)+(g2**2*v1*v2*RXiWm)/4._dp
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+Mu2
mat(2,2) = mat(2,2)+(Lam3*v1**2)/2._dp
mat(2,2) = mat(2,2)+Lam2*v2**2
mat(2,2) = mat(2,2)+(Lam3Dash*v3**2)/2._dp
mat(2,2) = mat(2,2)+(g2**2*v2**2*RXiWm)/4._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = Conjg(mat(i2,i1)) 
  End do 
End do 

 
Call EigenSystem(mat,MHm2,ZP,ierr,test) 
 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Abs(MHm2(i1)).Le.MaxMassNumericalZero) MHm2(i1) = 1.E-10_dp 
  If (MHm2(i1).ne.MHm2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (MHm2(i1).Ge.0._dp) Then 
  MHm(i1)=Sqrt(MHm2(i1) ) 
  Else 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,MHm2(i1) 
    End If 
  MHm(i1) = 1._dp 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,MHm2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,MHm2(i1) 
  MHm2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
Iname = Iname - 1 
 
End Subroutine CalculateMHm 

Subroutine CalculateMFd(Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,              & 
& Y2d33,v1,v2,ZDL,ZDR,MFd,kont)

Real(dp),Intent(in) :: v1,v2

Complex(dp),Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFd(3) 
 Complex(dp), Intent(out) :: ZDL(3,3), ZDR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZDL2(3,3), ZDR2(3,3) 
 
 Real(dp) :: ZDL1(3,3), ZDR1(3,3), test(2), MFd2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFd'
 
MFd = 0._dp 
ZDL = 0._dp 
ZDR = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(v1*Y1d11)/sqrt(2._dp)
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+(v1*Y1d12)/sqrt(2._dp)
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(v1*Y1d13)/sqrt(2._dp)
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)+(v1*Y1d21)/sqrt(2._dp)
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(v1*Y1d22)/sqrt(2._dp)
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)+(v1*Y1d23)/sqrt(2._dp)
mat(3,1) = 0._dp 
mat(3,1) = mat(3,1)+(v2*Y2d31)/sqrt(2._dp)
mat(3,2) = 0._dp 
mat(3,2) = mat(3,2)+(v2*Y2d32)/sqrt(2._dp)
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+(v2*Y2d33)/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2,ZDR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFd2,ZDL1,ierr,test) 
                  
                  
ZDL2 = ZDL1 
Else 
Call EigenSystem(mat2,MFd2,ZDL2,ierr,test) 
 
 
End If 
ZDL2 = Conjg(ZDL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZDL2),mat),Transpose( Conjg(ZDR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZDR2(i1,:) = phaseM *ZDR2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZDR2(i1,i1)).gt.0._dp) Then 
phaseM = ZDR2(i1,i1) / Abs(ZDR2(i1,i1)) 
ZDR2(i1,:) = Conjg(phaseM) *ZDR2(i1,:) 
 ZDL2(i1,:) = phaseM *ZDL2(i1,:) 
 End if 
  If (MFd2(i1).ne.MFd2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFd2(i1)).Le.MaxMassNumericalZero) MFd2(i1) = Abs(MFd2(i1))+1.E-10_dp 
  If (MFd2(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,MFd2(i1) 
      Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(*,*) 'a mass squarred is negative: ',i1,MFd2(i1) 
      Call TerminateProgram 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,MFd2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,MFd2(i1) 
  MFd2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFd, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFd = Sqrt( MFd2 ) 
ZDL = ZDL2 
ZDR = ZDR2 
Iname = Iname - 1 
 
End Subroutine CalculateMFd 

Subroutine CalculateMFu(Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,v1,v2,ZUL,ZUR,MFu,kont)

Real(dp),Intent(in) :: v1,v2

Complex(dp),Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFu(3) 
 Complex(dp), Intent(out) :: ZUL(3,3), ZUR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZUL2(3,3), ZUR2(3,3) 
 
 Real(dp) :: ZUL1(3,3), ZUR1(3,3), test(2), MFu2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFu'
 
MFu = 0._dp 
ZUL = 0._dp 
ZUR = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)-((v1*Y1u11)/sqrt(2._dp))
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)-((v1*Y1u12)/sqrt(2._dp))
mat(1,3) = 0._dp 
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)-((v1*Y1u21)/sqrt(2._dp))
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)-((v1*Y1u22)/sqrt(2._dp))
mat(2,3) = 0._dp 
mat(3,1) = 0._dp 
mat(3,2) = 0._dp 
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)-((v2*Y2u33)/sqrt(2._dp))

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2,ZUR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFu2,ZUL1,ierr,test) 
                  
                  
ZUL2 = ZUL1 
Else 
Call EigenSystem(mat2,MFu2,ZUL2,ierr,test) 
 
 
End If 
ZUL2 = Conjg(ZUL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZUL2),mat),Transpose( Conjg(ZUR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZUR2(i1,:) = phaseM *ZUR2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZUR2(i1,i1)).gt.0._dp) Then 
phaseM = ZUR2(i1,i1) / Abs(ZUR2(i1,i1)) 
ZUR2(i1,:) = Conjg(phaseM) *ZUR2(i1,:) 
 ZUL2(i1,:) = phaseM *ZUL2(i1,:) 
 End if 
  If (MFu2(i1).ne.MFu2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFu2(i1)).Le.MaxMassNumericalZero) MFu2(i1) = Abs(MFu2(i1))+1.E-10_dp 
  If (MFu2(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,MFu2(i1) 
      Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(*,*) 'a mass squarred is negative: ',i1,MFu2(i1) 
      Call TerminateProgram 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,MFu2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,MFu2(i1) 
  MFu2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFu, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFu = Sqrt( MFu2 ) 
ZUL = ZUL2 
ZUR = ZUR2 
Iname = Iname - 1 
 
End Subroutine CalculateMFu 

Subroutine CalculateMFe(Y1e11,Y1e22,Y1e21,Y2e33,v1,v2,ZEL,ZER,MFe,kont)

Real(dp),Intent(in) :: v1,v2

Complex(dp),Intent(in) :: Y1e11,Y1e22,Y1e21,Y2e33

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFe(3) 
 Complex(dp), Intent(out) :: ZEL(3,3), ZER(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZEL2(3,3), ZER2(3,3) 
 
 Real(dp) :: ZEL1(3,3), ZER1(3,3), test(2), MFe2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFe'
 
MFe = 0._dp 
ZEL = 0._dp 
ZER = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(v1*Y1e11)/sqrt(2._dp)
mat(1,2) = 0._dp 
mat(1,3) = 0._dp 
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)+(v2*Y1e21)/sqrt(2._dp)
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(v1*Y1e22)/sqrt(2._dp)
mat(2,3) = 0._dp 
mat(3,1) = 0._dp 
mat(3,2) = 0._dp 
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+(v2*Y2e33)/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2,ZER1,ierr,test) 
ZER2 = ZER1 
Else 
Call EigenSystem(mat2,MFe2,ZER2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFe2,ZEL1,ierr,test) 
                  
                  
ZEL2 = ZEL1 
Else 
Call EigenSystem(mat2,MFe2,ZEL2,ierr,test) 
 
 
End If 
ZEL2 = Conjg(ZEL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZEL2),mat),Transpose( Conjg(ZER2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZER2(i1,:) = phaseM *ZER2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZER2(i1,i1)).gt.0._dp) Then 
phaseM = ZER2(i1,i1) / Abs(ZER2(i1,i1)) 
ZER2(i1,:) = Conjg(phaseM) *ZER2(i1,:) 
 ZEL2(i1,:) = phaseM *ZEL2(i1,:) 
 End if 
  If (MFe2(i1).ne.MFe2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFe2(i1)).Le.MaxMassNumericalZero) MFe2(i1) = Abs(MFe2(i1))+1.E-10_dp 
  If (MFe2(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,MFe2(i1) 
      Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(*,*) 'a mass squarred is negative: ',i1,MFe2(i1) 
      Call TerminateProgram 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,MFe2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,MFe2(i1) 
  MFe2(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFe, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFe = Sqrt( MFe2 ) 
ZEL = ZEL2 
ZER = ZER2 
Iname = Iname - 1 
 
End Subroutine CalculateMFe 

Subroutine CalculateMFv(Y1n11,Y1n22,Y1n12,Y2n33,BB11,C23,C32,v1,v2,v3,Vv,MFv,kont)

Real(dp) ,Intent(in) :: v1,v2,v3

Complex(dp) ,Intent(in) :: Y1n11,Y1n22,Y1n12,Y2n33,BB11,C23,C32

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr, pos 
Integer :: j1,j2,j3,j4 
Logical :: SecondDiagonalisationNeeded 
Real(dp), Intent(out) :: MFv(6) 
Complex(dp), Intent(out) ::  Vv(6,6) 
                              
Complex(dp) :: mat(6,6), mat2(6,6), phaseM, E6(6) 

Real(dp) :: Vva(6,6), test(2), eig(6) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFv'
 
mat(1,1) = 0._dp 
mat(1,2) = 0._dp 
mat(1,3) = 0._dp 
mat(1,4) = 0._dp 
mat(1,4) = mat(1,4)+(v1*Y1n11)/sqrt(2._dp)
mat(1,5) = 0._dp 
mat(1,5) = mat(1,5)+(v2*Y1n12)/sqrt(2._dp)
mat(1,6) = 0._dp 
mat(2,2) = 0._dp 
mat(2,3) = 0._dp 
mat(2,4) = 0._dp 
mat(2,5) = 0._dp 
mat(2,5) = mat(2,5)+(v1*Y1n22)/sqrt(2._dp)
mat(2,6) = 0._dp 
mat(3,3) = 0._dp 
mat(3,4) = 0._dp 
mat(3,5) = 0._dp 
mat(3,6) = 0._dp 
mat(3,6) = mat(3,6)+(v2*Y2n33)/sqrt(2._dp)
mat(4,4) = 0._dp 
mat(4,4) = mat(4,4)+BB11
mat(4,5) = 0._dp 
mat(4,6) = 0._dp 
mat(5,5) = 0._dp 
mat(5,6) = 0._dp 
mat(5,6) = mat(5,6)+(C23*v3)/(2._dp*sqrt(2._dp))
mat(5,6) = mat(5,6)+(C32*v3)/(2._dp*sqrt(2._dp))
mat(6,6) = 0._dp 

 
 Do i1=2,6
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
If (Maxval(Abs(Aimag(mat))).Eq.0._dp) Then 
Call EigenSystem(Real(mat,dp),Eig,Vva,ierr,test) 
 
   Do i1=1,6
   If ((Eig(i1).Lt.0._dp).or.(Abs(eig(i1)).lt.1E-15)) Then 
    MFv(i1) = - Eig(i1) 
    Vv(i1,:) = (0._dp,1._dp)*Vva(i1,:) 
   Else 
    MFv(i1) = Eig(i1) 
    Vv(i1,:) = Vva(i1,:)
    End If 
   End Do 
 
Do i1=1,5
  Do i2=i1+1,6
    If (MFv(i1).Gt.MFv(i2)) Then 
      Eig(1) = MFv(i1) 
      MFv(i1) = MFv(i2) 
      MFv(i2) =  Eig(1) 
      E6 = Vv(i1,:) 
      Vv(i1,:) = Vv(i2,:) 
      Vv(i2,:) = E6
    End If 
   End Do 
End Do 
 
Else 
 
mat2 = Matmul( Transpose(Conjg( mat) ), mat ) 
Call Eigensystem(mat2, Eig, Vv, ierr, test) 
mat2 = Matmul( Conjg(Vv), Matmul( mat, Transpose( Conjg(Vv)))) 
! Special efforts are needed for matrices like the Higgsinos one 
SecondDiagonalisationNeeded = .False. 
Do i1=1,6-1
If (MaxVal(Abs(mat2(i1,(i1+1):6))).gt.Abs(mat2(i1,i1))) SecondDiagonalisationNeeded = .True. 

  If (Eig(i1).ne.Eig(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If ((Abs(Eig(i1)).Le.MaxMassNumericalZero).and.(Eig(i1).lt.0._dp)) Eig(i1) = Abs(Eig(i1))+1.E-10_dp 
  If (Eig(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,Eig(i1) 
      Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(*,*) 'a mass squarred is negative: ',i1,Eig(i1) 
      Call TerminateProgram 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,Eig(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,Eig(i1) 
  Eig(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End do 
If (SecondDiagonalisationNeeded) Then 
Call EigenSystem(Real(mat2,dp),Eig,Vva,ierr,test) 
 
     Vv = MatMul(Vv,Vva)
  Do i1=1,6
   If ((Eig(i1).Lt.0._dp).or.(Abs(eig(i1)).lt.1E-15)) Then 
    MFv(i1) = - Eig(i1) 
    Vv(i1,:) = (0._dp,1._dp)*Vva(i1,:) 
   Else 
    MFv(i1) = Eig(i1) 
    Vv(i1,:) = Vva(i1,:)
    End If 
   End Do 
 
Else 
Do i1=1,6
  If (Eig(i1).ne.Eig(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
  phaseM = Sqrt( mat2(i1,i1) / Abs(mat2(i1,i1))) 
  Vv(i1,:)= phaseM * Vv(i1,:) 
End if 
  If ((Abs(Eig(i1)).Le.MaxMassNumericalZero).and.(Eig(i1).lt.0._dp)) Eig(i1) = Abs(Eig(i1))+1.E-10_dp 
  If (Eig(i1).Le.0._dp) Then 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,Eig(i1) 
      Write(*,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(*,*) 'a mass squarred is negative: ',i1,Eig(i1) 
      Call TerminateProgram 
    End If 
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,Eig(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,Eig(i1) 
  Eig(i1) = 1._dp 
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
MFv = Sqrt( Eig ) 
 
End if ! Second diagonalisation 
End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFv, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Iname = Iname - 1 
 
End Subroutine CalculateMFv 

Subroutine CalculateVPVZ(g1,g2,v1,v2,ZZ,MVZ,MVZ2,kont)

Real(dp), Intent(in) :: g1,g2,v1,v2

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MVZ, MVZ2
Real(dp) :: VPVZ2(2),VPVZ(2)  

Complex(dp), Intent(out) :: ZZ(2,2) 
 
Complex(dp) :: mat(2,2)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateVPVZ'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(g1**2*v1**2)/4._dp
mat(1,1) = mat(1,1)+(g1**2*v2**2)/4._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)-(g1*g2*v1**2)/4._dp
mat(1,2) = mat(1,2)-(g1*g2*v2**2)/4._dp
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(g2**2*v1**2)/4._dp
mat(2,2) = mat(2,2)+(g2**2*v2**2)/4._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = Conjg(mat(i2,i1)) 
  End do 
End do 

 
Call EigenSystem(mat,VPVZ2,ZZ,ierr,test) 
 
 
ZZ = Transpose(ZZ) 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Abs(VPVZ2(i1)).Le.1.E-10_dp*(Maxval(VPVZ2))) VPVZ2(i1) = 1.E-10_dp 
  If (VPVZ2(i1).ne.VPVZ2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (VPVZ2(i1).Ge.0._dp) Then 
  VPVZ(i1) =Sqrt(VPVZ2(i1) ) 
  Else 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,VPVZ2(i1) 
    End If 
  VPVZ(i1)= 1._dp 
  VPVZ2(i1)= 1._dp  
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,VPVZ2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,VPVZ2(i1) 
  VPVZ(i1)= 1._dp 
  VPVZ2(i1) = 1._dp  
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
MVZ = VPVZ(2) 
MVZ2 = VPVZ2(2) 

 Iname = Iname - 1 
 
End Subroutine CalculateVPVZ 

Subroutine CalculateVWm(g2,v1,v2,ZW,MVWm,MVWm2,kont)

Real(dp), Intent(in) :: g2,v1,v2

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MVWm, MVWm2
Real(dp) :: VWm2(2),VWm(2)  

Complex(dp), Intent(out) :: ZW(2,2) 
 
Complex(dp) :: mat(2,2)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateVWm'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(g2**2*v1**2)/4._dp
mat(1,1) = mat(1,1)+(g2**2*v2**2)/4._dp
mat(1,2) = 0._dp 
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(g2**2*v1**2)/4._dp
mat(2,2) = mat(2,2)+(g2**2*v2**2)/4._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = Conjg(mat(i2,i1)) 
  End do 
End do 

 
Call EigenSystem(mat,VWm2,ZW,ierr,test) 
 
 
ZW = Transpose(ZW) 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Abs(VWm2(i1)).Le.1.E-10_dp*(Maxval(VWm2))) VWm2(i1) = 1.E-10_dp 
  If (VWm2(i1).ne.VWm2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (VWm2(i1).Ge.0._dp) Then 
  VWm(i1) =Sqrt(VWm2(i1) ) 
  Else 
    If (ErrorLevel.Ge.0) Then 
      Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
      Write(10,*) 'a mass squarred is negative: ',i1,VWm2(i1) 
    End If 
  VWm(i1)= 1._dp 
  VWm2(i1)= 1._dp  
     Write(ErrCan,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(ErrCan,*) 'in the calculation of the masses' 
     Write(ErrCan,*) 'occurred a negative mass squared!' 
     Write(ErrCan,*) i1,VWm2(i1) 
     Write(*,*) 'Warning from routine '//NameOfUnit(Iname) 
     Write(*,*) 'in the calculation of the masses' 
     Write(*,*) 'occurred a negative mass squared!' 
     Write(*,*) i1,VWm2(i1) 
  VWm(i1)= 1._dp 
  VWm2(i1) = 1._dp  
   SignOfMassChanged = .True. 
! kont = -104 
 End if 
End Do 
 
MVWm = VWm(1) 
MVWm2 = VWm2(1) 

 Iname = Iname - 1 
 
End Subroutine CalculateVWm 

Subroutine CalculateMhhEffPot(Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,MuDash,Lam1Dash,            & 
& Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZH,Mhh,Mhh2,kont)

Real(dp), Intent(in) :: Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4, pos 
Real(dp), Intent(out) :: Mhh(3), Mhh2(3) 
Real(dp), Intent(out) :: ZH(3,3) 
 
Real(dp) :: mat(3,3)  

Real(dp) :: Mhh2temp(3), Q2 
Real(dp) :: ZHtemp(3,3),ZHtemp2(3,3) 
 
Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMhh'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+Mu1
mat(1,1) = mat(1,1)+3*Lam1*v1**2
mat(1,1) = mat(1,1)+(Lam3*v2**2)/2._dp
mat(1,1) = mat(1,1)+(Lam4*v2**2)/2._dp
mat(1,1) = mat(1,1)+(Lam2Dash*v3**2)/2._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+Mu3/2._dp
mat(1,2) = mat(1,2)+Lam3*v1*v2
mat(1,2) = mat(1,2)+Lam4*v1*v2
mat(1,2) = mat(1,2)+(Aa1*v3)/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(Aa2*v3)/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(Aa3*v3**2)/4._dp
mat(1,2) = mat(1,2)+(Aa4*v3**2)/4._dp
mat(1,2) = mat(1,2)+(v3*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(v3*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa3))/4._dp
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa4))/4._dp
mat(1,2) = mat(1,2)+Conjg(Mu3)/2._dp
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(Aa1*v2)/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+(Aa2*v2)/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+Lam2Dash*v1*v3
mat(1,3) = mat(1,3)+(Aa3*v2*v3)/2._dp
mat(1,3) = mat(1,3)+(Aa4*v2*v3)/2._dp
mat(1,3) = mat(1,3)+(v2*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+(v2*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+(v2*v3*Conjg(Aa3))/2._dp
mat(1,3) = mat(1,3)+(v2*v3*Conjg(Aa4))/2._dp
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+Mu2
mat(2,2) = mat(2,2)+(Lam3*v1**2)/2._dp
mat(2,2) = mat(2,2)+(Lam4*v1**2)/2._dp
mat(2,2) = mat(2,2)+3*Lam2*v2**2
mat(2,2) = mat(2,2)+(Lam3Dash*v3**2)/2._dp
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)+(Aa1*v1)/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(Aa2*v1)/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(Aa3*v1*v3)/2._dp
mat(2,3) = mat(2,3)+(Aa4*v1*v3)/2._dp
mat(2,3) = mat(2,3)+Lam3Dash*v2*v3
mat(2,3) = mat(2,3)+(v1*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(v1*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(v1*v3*Conjg(Aa3))/2._dp
mat(2,3) = mat(2,3)+(v1*v3*Conjg(Aa4))/2._dp
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+Mub/2._dp
mat(3,3) = mat(3,3)+MuDash
mat(3,3) = mat(3,3)+(Lam2Dash*v1**2)/2._dp
mat(3,3) = mat(3,3)+(Aa3*v1*v2)/2._dp
mat(3,3) = mat(3,3)+(Aa4*v1*v2)/2._dp
mat(3,3) = mat(3,3)+(Lam3Dash*v2**2)/2._dp
mat(3,3) = mat(3,3)+3*Lam1Dash*v3**2
mat(3,3) = mat(3,3)+(v1*v2*Conjg(Aa3))/2._dp
mat(3,3) = mat(3,3)+(v1*v2*Conjg(Aa4))/2._dp
mat(3,3) = mat(3,3)+Conjg(Mub)/2._dp

 
 Do i1=2,3
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
Call EigenSystem(mat,Mhh2,ZH,ierr,test) 
 
 
! Fix phases
Do i1=1,3
  pos=Maxloc(Abs(ZH(i1,:)),1)
  If (Real(ZH(i1,pos),dp).lt.0._dp) Then
    ZH(i1,:)=-ZH(i1,:)
  End if
End do
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,3
  If (Mhh2(i1).ne.Mhh2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Mhh2(i1).Ge.0._dp) Then 
  Mhh(i1)=Sqrt(Mhh2(i1) ) 
  Else 
  Mhh(i1) = 1._dp 
! kont = -104 
 End if 
End Do 
Iname = Iname - 1 
 
End Subroutine CalculateMhhEffPot 

Subroutine CalculateMAhEffPot(g1,g2,Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,MuDash,               & 
& Lam1Dash,Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZZ,ZA,MAh,MAh2,kont)

Real(dp), Intent(in) :: g1,g2,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Mub,Mu3,Aa1,Aa2,Aa3,Aa4,ZZ(2,2)

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr, pos 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MAh(3), MAh2(3) 
Real(dp), Intent(out) :: ZA(3,3) 
 
Real(dp) :: ZAFIX(3,3) 
 
Real(dp) :: mat(3,3)  

Real(dp) ::  test(2), Q2 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMAh'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+Mu1
mat(1,1) = mat(1,1)+Lam1*v1**2
mat(1,1) = mat(1,1)+(Lam3*v2**2)/2._dp
mat(1,1) = mat(1,1)+(Lam4*v2**2)/2._dp
mat(1,1) = mat(1,1)+(Lam2Dash*v3**2)/2._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+Mu3/2._dp
mat(1,2) = mat(1,2)+(Aa1*v3)/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(Aa2*v3)/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(Aa3*v3**2)/4._dp
mat(1,2) = mat(1,2)+(Aa4*v3**2)/4._dp
mat(1,2) = mat(1,2)+(v3*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(v3*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa3))/4._dp
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa4))/4._dp
mat(1,2) = mat(1,2)+Conjg(Mu3)/2._dp
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(Aa1*v2)/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)-(Aa2*v2)/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+(Aa3*v2*v3)/2._dp
mat(1,3) = mat(1,3)-(Aa4*v2*v3)/2._dp
mat(1,3) = mat(1,3)+(v2*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)-(v2*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(1,3) = mat(1,3)+(v2*v3*Conjg(Aa3))/2._dp
mat(1,3) = mat(1,3)-(v2*v3*Conjg(Aa4))/2._dp
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+Mu2
mat(2,2) = mat(2,2)+(Lam3*v1**2)/2._dp
mat(2,2) = mat(2,2)+(Lam4*v1**2)/2._dp
mat(2,2) = mat(2,2)+Lam2*v2**2
mat(2,2) = mat(2,2)+(Lam3Dash*v3**2)/2._dp
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)-(Aa1*v1)/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(Aa2*v1)/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)-(Aa3*v1*v3)/2._dp
mat(2,3) = mat(2,3)+(Aa4*v1*v3)/2._dp
mat(2,3) = mat(2,3)-(v1*Conjg(Aa1))/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)+(v1*Conjg(Aa2))/(2._dp*sqrt(2._dp))
mat(2,3) = mat(2,3)-(v1*v3*Conjg(Aa3))/2._dp
mat(2,3) = mat(2,3)+(v1*v3*Conjg(Aa4))/2._dp
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)-1._dp*(Mub)/2._dp
mat(3,3) = mat(3,3)+MuDash
mat(3,3) = mat(3,3)+(Lam2Dash*v1**2)/2._dp
mat(3,3) = mat(3,3)-(Aa3*v1*v2)/2._dp
mat(3,3) = mat(3,3)-(Aa4*v1*v2)/2._dp
mat(3,3) = mat(3,3)+(Lam3Dash*v2**2)/2._dp
mat(3,3) = mat(3,3)+Lam1Dash*v3**2
mat(3,3) = mat(3,3)-(v1*v2*Conjg(Aa3))/2._dp
mat(3,3) = mat(3,3)-(v1*v2*Conjg(Aa4))/2._dp
mat(3,3) = mat(3,3)-Conjg(Mub)/2._dp

 
 Do i1=2,3
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
Call EigenSystem(mat,MAh2,ZA,ierr,test) 
 
 
! Fix phases
Do i1=1,3
  pos=Maxloc(Abs(ZA(i1,:)),1)
  If (Real(ZA(i1,pos),dp).lt.0._dp) Then
    ZA(i1,:)=-ZA(i1,:)
  End if
End do
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,3
  If (MAh2(i1).ne.MAh2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (MAh2(i1).Ge.0._dp) Then 
  MAh(i1)=Sqrt(MAh2(i1) ) 
  Else 
  MAh(i1) = 1._dp 
! kont = -104 
 End if 
End Do 
Iname = Iname - 1 
 
End Subroutine CalculateMAhEffPot 

Subroutine CalculateMHmEffPot(g2,Mu1,Mu2,Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,       & 
& Mu3,Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZP,MHm,MHm2,kont)

Real(dp), Intent(in) :: g2,Mu1,Mu2,v1,v2,v3

Complex(dp), Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam2Dash,Lam3Dash,Mu3,Aa1,Aa2,Aa3,Aa4

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr, pos 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MHm(2), MHm2(2) 
Complex(dp), Intent(out) :: ZP(2,2) 
 
Complex(dp) :: ZPFIX(2,2) 
 
Complex(dp) :: mat(2,2)  

Real(dp) ::  test(2), Q2 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMHm'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+Mu1
mat(1,1) = mat(1,1)+Lam1*v1**2
mat(1,1) = mat(1,1)+(Lam3*v2**2)/2._dp
mat(1,1) = mat(1,1)+(Lam2Dash*v3**2)/2._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+Mu3
mat(1,2) = mat(1,2)+(Lam4*v1*v2)/2._dp
mat(1,2) = mat(1,2)+(v3*Conjg(Aa1))/sqrt(2._dp)
mat(1,2) = mat(1,2)+(v3*Conjg(Aa2))/sqrt(2._dp)
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa3))/2._dp
mat(1,2) = mat(1,2)+(v3**2*Conjg(Aa4))/2._dp
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+Mu2
mat(2,2) = mat(2,2)+(Lam3*v1**2)/2._dp
mat(2,2) = mat(2,2)+Lam2*v2**2
mat(2,2) = mat(2,2)+(Lam3Dash*v3**2)/2._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = Conjg(mat(i2,i1)) 
  End do 
End do 

 
Call EigenSystem(mat,MHm2,ZP,ierr,test) 
 
 
! Fix phases
Do i1=1,2
  pos=Maxloc(Abs(ZP(i1,:)),1)
  If (Real(ZP(i1,pos),dp).lt.0._dp) Then
    ZP(i1,:)=-ZP(i1,:)
  End if
End do
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (MHm2(i1).ne.MHm2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (MHm2(i1).Ge.0._dp) Then 
  MHm(i1)=Sqrt(MHm2(i1) ) 
  Else 
  MHm(i1) = 1._dp 
! kont = -104 
 End if 
End Do 
Iname = Iname - 1 
 
End Subroutine CalculateMHmEffPot 

Subroutine CalculateMFdEffPot(Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,              & 
& Y2d32,Y2d33,v1,v2,ZDL,ZDR,MFd,kont)

Real(dp),Intent(in) :: v1,v2

Complex(dp),Intent(in) :: Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFd(3) 
 Complex(dp), Intent(out) :: ZDL(3,3), ZDR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZDL2(3,3), ZDR2(3,3) 
 
 Real(dp) :: ZDL1(3,3), ZDR1(3,3), test(2), MFd2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFd'
 
MFd = 0._dp 
ZDL = 0._dp 
ZDR = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(v1*Y1d11)/sqrt(2._dp)
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)+(v1*Y1d12)/sqrt(2._dp)
mat(1,3) = 0._dp 
mat(1,3) = mat(1,3)+(v1*Y1d13)/sqrt(2._dp)
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)+(v1*Y1d21)/sqrt(2._dp)
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(v1*Y1d22)/sqrt(2._dp)
mat(2,3) = 0._dp 
mat(2,3) = mat(2,3)+(v1*Y1d23)/sqrt(2._dp)
mat(3,1) = 0._dp 
mat(3,1) = mat(3,1)+(v2*Y2d31)/sqrt(2._dp)
mat(3,2) = 0._dp 
mat(3,2) = mat(3,2)+(v2*Y2d32)/sqrt(2._dp)
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+(v2*Y2d33)/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFd2,ZDR1,ierr,test) 
ZDR2 = ZDR1 
Else 
Call EigenSystem(mat2,MFd2,ZDR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFd2,ZDL1,ierr,test) 
                  
                  
ZDL2 = ZDL1 
Else 
Call EigenSystem(mat2,MFd2,ZDL2,ierr,test) 
 
 
End If 
ZDL2 = Conjg(ZDL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZDL2),mat),Transpose( Conjg(ZDR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZDR2(i1,:) = phaseM *ZDR2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZDR2(i1,i1)).gt.0._dp) Then 
phaseM = ZDR2(i1,i1) / Abs(ZDR2(i1,i1)) 
ZDR2(i1,:) = Conjg(phaseM) *ZDR2(i1,:) 
 ZDL2(i1,:) = phaseM *ZDL2(i1,:) 
 End if 
  If (MFd2(i1).ne.MFd2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFd2(i1)).Le.MaxMassNumericalZero) MFd2(i1) = Abs(MFd2(i1))+1.E-10_dp 
  If (MFd2(i1).Le.0._dp) Then 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFd, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFd = Sqrt( MFd2 ) 
ZDL = ZDL2 
ZDR = ZDR2 
Iname = Iname - 1 
 
End Subroutine CalculateMFdEffPot 

Subroutine CalculateMFuEffPot(Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,v1,v2,ZUL,ZUR,MFu,kont)

Real(dp),Intent(in) :: v1,v2

Complex(dp),Intent(in) :: Y1u11,Y1u12,Y1u21,Y1u22,Y2u33

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFu(3) 
 Complex(dp), Intent(out) :: ZUL(3,3), ZUR(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZUL2(3,3), ZUR2(3,3) 
 
 Real(dp) :: ZUL1(3,3), ZUR1(3,3), test(2), MFu2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFu'
 
MFu = 0._dp 
ZUL = 0._dp 
ZUR = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)-((v1*Y1u11)/sqrt(2._dp))
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)-((v1*Y1u12)/sqrt(2._dp))
mat(1,3) = 0._dp 
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)-((v1*Y1u21)/sqrt(2._dp))
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)-((v1*Y1u22)/sqrt(2._dp))
mat(2,3) = 0._dp 
mat(3,1) = 0._dp 
mat(3,2) = 0._dp 
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)-((v2*Y2u33)/sqrt(2._dp))

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFu2,ZUR1,ierr,test) 
ZUR2 = ZUR1 
Else 
Call EigenSystem(mat2,MFu2,ZUR2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFu2,ZUL1,ierr,test) 
                  
                  
ZUL2 = ZUL1 
Else 
Call EigenSystem(mat2,MFu2,ZUL2,ierr,test) 
 
 
End If 
ZUL2 = Conjg(ZUL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZUL2),mat),Transpose( Conjg(ZUR2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZUR2(i1,:) = phaseM *ZUR2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZUR2(i1,i1)).gt.0._dp) Then 
phaseM = ZUR2(i1,i1) / Abs(ZUR2(i1,i1)) 
ZUR2(i1,:) = Conjg(phaseM) *ZUR2(i1,:) 
 ZUL2(i1,:) = phaseM *ZUL2(i1,:) 
 End if 
  If (MFu2(i1).ne.MFu2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFu2(i1)).Le.MaxMassNumericalZero) MFu2(i1) = Abs(MFu2(i1))+1.E-10_dp 
  If (MFu2(i1).Le.0._dp) Then 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFu, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFu = Sqrt( MFu2 ) 
ZUL = ZUL2 
ZUR = ZUR2 
Iname = Iname - 1 
 
End Subroutine CalculateMFuEffPot 

Subroutine CalculateMFeEffPot(Y1e11,Y1e22,Y1e21,Y2e33,v1,v2,ZEL,ZER,MFe,kont)

Real(dp),Intent(in) :: v1,v2

Complex(dp),Intent(in) :: Y1e11,Y1e22,Y1e21,Y2e33

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MFe(3) 
 Complex(dp), Intent(out) :: ZEL(3,3), ZER(3,3) 
 
 Complex(dp) :: mat(3,3)=0._dp, mat2(3,3)=0._dp, phaseM 

Complex(dp) :: ZEL2(3,3), ZER2(3,3) 
 
 Real(dp) :: ZEL1(3,3), ZER1(3,3), test(2), MFe2(3) 
 
 Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFe'
 
MFe = 0._dp 
ZEL = 0._dp 
ZER = 0._dp 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(v1*Y1e11)/sqrt(2._dp)
mat(1,2) = 0._dp 
mat(1,3) = 0._dp 
mat(2,1) = 0._dp 
mat(2,1) = mat(2,1)+(v2*Y1e21)/sqrt(2._dp)
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(v1*Y1e22)/sqrt(2._dp)
mat(2,3) = 0._dp 
mat(3,1) = 0._dp 
mat(3,2) = 0._dp 
mat(3,3) = 0._dp 
mat(3,3) = mat(3,3)+(v2*Y2e33)/sqrt(2._dp)

 
mat2 = Matmul(Transpose(Conjg(mat)),mat) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem(Real(mat2,dp),MFe2,ZER1,ierr,test) 
ZER2 = ZER1 
Else 
Call EigenSystem(mat2,MFe2,ZER2,ierr,test) 
 End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(mat,Transpose(Conjg(mat))) 
If (Maxval(Abs(Aimag(mat2))).Eq.0._dp) Then 
Call EigenSystem (Real(mat2,dp),MFe2,ZEL1,ierr,test) 
                  
                  
ZEL2 = ZEL1 
Else 
Call EigenSystem(mat2,MFe2,ZEL2,ierr,test) 
 
 
End If 
ZEL2 = Conjg(ZEL2) 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
mat2 = Matmul(Matmul( Conjg(ZEL2),mat),Transpose( Conjg(ZER2))) 
Do i1=1,3
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
phaseM = mat2(i1,i1) / Abs(mat2(i1,i1)) 
ZER2(i1,:) = phaseM *ZER2(i1,:) 
 End if 
End Do 
 
Do i1=1,3
If (Abs(ZER2(i1,i1)).gt.0._dp) Then 
phaseM = ZER2(i1,i1) / Abs(ZER2(i1,i1)) 
ZER2(i1,:) = Conjg(phaseM) *ZER2(i1,:) 
 ZEL2(i1,:) = phaseM *ZEL2(i1,:) 
 End if 
  If (MFe2(i1).ne.MFe2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (Abs(MFe2(i1)).Le.MaxMassNumericalZero) MFe2(i1) = Abs(MFe2(i1))+1.E-10_dp 
  If (MFe2(i1).Le.0._dp) Then 
! kont = -104 
 End if 
End Do 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFe, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


MFe = Sqrt( MFe2 ) 
ZEL = ZEL2 
ZER = ZER2 
Iname = Iname - 1 
 
End Subroutine CalculateMFeEffPot 

Subroutine CalculateMFvEffPot(Y1n11,Y1n22,Y1n12,Y2n33,BB11,C23,C32,v1,v2,             & 
& v3,Vv,MFv,kont)

Real(dp) ,Intent(in) :: v1,v2,v3

Complex(dp) ,Intent(in) :: Y1n11,Y1n22,Y1n12,Y2n33,BB11,C23,C32

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr, pos 
Integer :: j1,j2,j3,j4 
Logical :: SecondDiagonalisationNeeded 
Real(dp), Intent(out) :: MFv(6) 
Complex(dp), Intent(out) ::  Vv(6,6) 
                              
Complex(dp) :: mat(6,6), mat2(6,6), phaseM, E6(6) 

Real(dp) :: Vva(6,6), test(2), eig(6) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateMFv'
 
mat(1,1) = 0._dp 
mat(1,2) = 0._dp 
mat(1,3) = 0._dp 
mat(1,4) = 0._dp 
mat(1,4) = mat(1,4)+(v1*Y1n11)/sqrt(2._dp)
mat(1,5) = 0._dp 
mat(1,5) = mat(1,5)+(v2*Y1n12)/sqrt(2._dp)
mat(1,6) = 0._dp 
mat(2,2) = 0._dp 
mat(2,3) = 0._dp 
mat(2,4) = 0._dp 
mat(2,5) = 0._dp 
mat(2,5) = mat(2,5)+(v1*Y1n22)/sqrt(2._dp)
mat(2,6) = 0._dp 
mat(3,3) = 0._dp 
mat(3,4) = 0._dp 
mat(3,5) = 0._dp 
mat(3,6) = 0._dp 
mat(3,6) = mat(3,6)+(v2*Y2n33)/sqrt(2._dp)
mat(4,4) = 0._dp 
mat(4,4) = mat(4,4)+BB11
mat(4,5) = 0._dp 
mat(4,6) = 0._dp 
mat(5,5) = 0._dp 
mat(5,6) = 0._dp 
mat(5,6) = mat(5,6)+(C23*v3)/(2._dp*sqrt(2._dp))
mat(5,6) = mat(5,6)+(C32*v3)/(2._dp*sqrt(2._dp))
mat(6,6) = 0._dp 

 
 Do i1=2,6
  Do i2 = 1, i1-1 
  mat(i1,i2) = mat(i2,i1) 
  End do 
End do 

 
If (Maxval(Abs(Aimag(mat))).Eq.0._dp) Then 
Call EigenSystem(Real(mat,dp),Eig,Vva,ierr,test) 
 
   Do i1=1,6
   If ((Eig(i1).Lt.0._dp).or.(Abs(eig(i1)).lt.1E-15)) Then 
    MFv(i1) = - Eig(i1) 
    Vv(i1,:) = (0._dp,1._dp)*Vva(i1,:) 
   Else 
    MFv(i1) = Eig(i1) 
    Vv(i1,:) = Vva(i1,:)
    End If 
   End Do 
 
  Do i1=1,6
   pos=Maxloc(Abs(Vv(i1,:)),1) 
   If (Abs(Real(Vv(i1,pos),dp)).gt.Abs(Aimag(Vv(i1,pos)))) Then 
      If (Real(Vv(i1,pos),dp).lt.0._dp) Then 
        Vv(i1,:)=-Vv(i1,:) 
       End If 
    Else 
      If (Aimag(Vv(i1,pos)).lt.0._dp) Then 
        Vv(i1,:)=-Vv(i1,:) 
      End If 
    End If 
 End Do 
 
Do i1=1,5
  Do i2=i1+1,6
    If (MFv(i1).Gt.MFv(i2)) Then 
      Eig(1) = MFv(i1) 
      MFv(i1) = MFv(i2) 
      MFv(i2) =  Eig(1) 
      E6 = Vv(i1,:) 
      Vv(i1,:) = Vv(i2,:) 
      Vv(i2,:) = E6
    End If 
   End Do 
End Do 
 
Else 
 
mat2 = Matmul( Transpose(Conjg( mat) ), mat ) 
Call Eigensystem(mat2, Eig, Vv, ierr, test) 
mat2 = Matmul( Conjg(Vv), Matmul( mat, Transpose( Conjg(Vv)))) 
! Special efforts are needed for matrices like the Higgsinos one 
SecondDiagonalisationNeeded = .False. 
Do i1=1,6-1
If (MaxVal(Abs(mat2(i1,(i1+1):6))).gt.Abs(mat2(i1,i1))) SecondDiagonalisationNeeded = .True. 

  If (Eig(i1).ne.Eig(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If ((Abs(Eig(i1)).Le.MaxMassNumericalZero).and.(Eig(i1).lt.0._dp)) Eig(i1) = Abs(Eig(i1))+1.E-10_dp 
  If (Eig(i1).Le.0._dp) Then 
! kont = -104 
 End if 
End do 
If (SecondDiagonalisationNeeded) Then 
Call EigenSystem(Real(mat2,dp),Eig,Vva,ierr,test) 
 
     Vv = MatMul(Vv,Vva)
  Do i1=1,6
   If ((Eig(i1).Lt.0._dp).or.(Abs(eig(i1)).lt.1E-15)) Then 
    MFv(i1) = - Eig(i1) 
    Vv(i1,:) = (0._dp,1._dp)*Vva(i1,:) 
   Else 
    MFv(i1) = Eig(i1) 
    Vv(i1,:) = Vva(i1,:)
    End If 
   End Do 
 
Else 
Do i1=1,6
  If (Eig(i1).ne.Eig(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
If (Abs(mat2(i1,i1)).gt.0._dp) Then 
  phaseM = Sqrt( mat2(i1,i1) / Abs(mat2(i1,i1))) 
  Vv(i1,:)= phaseM * Vv(i1,:) 
End if 
  If ((Abs(Eig(i1)).Le.MaxMassNumericalZero).and.(Eig(i1).lt.0._dp)) Eig(i1) = Abs(Eig(i1))+1.E-10_dp 
  If (Eig(i1).Le.0._dp) Then 
! kont = -104 
 End if 
End Do 
MFv = Sqrt( Eig ) 
 
End if ! Second diagonalisation 
End If 
 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If (ierr.Ne.0.) Then 
  Write(ErrCan,*) 'Warning from Subroutine CalculateMFv, ierr =',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Iname = Iname - 1 
 
End Subroutine CalculateMFvEffPot 

Subroutine CalculateVPVZEffPot(g1,g2,v1,v2,ZZ,MVZ,MVZ2,kont)

Real(dp), Intent(in) :: g1,g2,v1,v2

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MVZ, MVZ2
Real(dp) :: VPVZ2(2),VPVZ(2)  

Complex(dp), Intent(out) :: ZZ(2,2) 
 
Complex(dp) :: mat(2,2)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateVPVZ'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(g1**2*v1Fix**2)/4._dp
mat(1,1) = mat(1,1)+(g1**2*v2Fix**2)/4._dp
mat(1,2) = 0._dp 
mat(1,2) = mat(1,2)-(g1*g2*v1Fix**2)/4._dp
mat(1,2) = mat(1,2)-(g1*g2*v2Fix**2)/4._dp
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(g2**2*v1Fix**2)/4._dp
mat(2,2) = mat(2,2)+(g2**2*v2Fix**2)/4._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = Conjg(mat(i2,i1)) 
  End do 
End do 

 
Call EigenSystem(mat,VPVZ2,ZZ,ierr,test) 
 
 
ZZ = Transpose(ZZ) 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Abs(VPVZ2(i1)).Le.1.E-10_dp*(Maxval(VPVZ2))) VPVZ2(i1) = 1.E-10_dp 
  If (VPVZ2(i1).ne.VPVZ2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (VPVZ2(i1).Ge.0._dp) Then 
  VPVZ(i1) =Sqrt(VPVZ2(i1) ) 
  Else 
  VPVZ(i1)= 1._dp 
  VPVZ(i1)= 1._dp 
! kont = -104 
 End if 
End Do 
 
MVZ = VPVZ(2) 
MVZ2 = VPVZ2(2) 

 Iname = Iname - 1 
 
End Subroutine CalculateVPVZEffPot 

Subroutine CalculateVWmEffPot(g2,v1,v2,ZW,MVWm,MVWm2,kont)

Real(dp), Intent(in) :: g2,v1,v2

Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4, ierr 
Integer :: j1,j2,j3,j4 
Real(dp), Intent(out) :: MVWm, MVWm2
Real(dp) :: VWm2(2),VWm(2)  

Complex(dp), Intent(out) :: ZW(2,2) 
 
Complex(dp) :: mat(2,2)  

Real(dp) ::  test(2) 

Iname = Iname + 1 
NameOfUnit(Iname) = 'CalculateVWm'
 
mat(1,1) = 0._dp 
mat(1,1) = mat(1,1)+(g2**2*v1Fix**2)/4._dp
mat(1,1) = mat(1,1)+(g2**2*v2Fix**2)/4._dp
mat(1,2) = 0._dp 
mat(2,2) = 0._dp 
mat(2,2) = mat(2,2)+(g2**2*v1Fix**2)/4._dp
mat(2,2) = mat(2,2)+(g2**2*v2Fix**2)/4._dp

 
 Do i1=2,2
  Do i2 = 1, i1-1 
  mat(i1,i2) = Conjg(mat(i2,i1)) 
  End do 
End do 

 
Call EigenSystem(mat,VWm2,ZW,ierr,test) 
 
 
ZW = Transpose(ZW) 
If ((ierr.Eq.-8).Or.(ierr.Eq.-9)) Then 
  Write(ErrCan,*) "Possible numerical problem in "//NameOfUnit(Iname) 
  If (ErrorLevel.Eq.2) Then 
  Write(*,*) "Possible numerical problem in "//NameOfUnit(Iname) 
    Call TerminateProgram 
  End If 
  ierr = 0 
End If 
 
If ((ierr.Ne.0.).And.(ErrorLevel.Ge.-1)) Then 
  Write(10,*) 'Warning from Subroutine '//NameOfUnit(Iname) 
  Write(10,*) 'Diagonalization failed, ierr : ',ierr 
  kont = ierr 
  Iname = Iname - 1 
  Return 
End If 


Do i1=1,2
  If (Abs(VWm2(i1)).Le.1.E-10_dp*(Maxval(VWm2))) VWm2(i1) = 1.E-10_dp 
  If (VWm2(i1).ne.VWm2(i1)) Then 
      Write(*,*) 'NaN appearing in '//NameOfUnit(Iname) 
      Call TerminateProgram 
    End If 
  If (VWm2(i1).Ge.0._dp) Then 
  VWm(i1) =Sqrt(VWm2(i1) ) 
  Else 
  VWm(i1)= 1._dp 
  VWm(i1)= 1._dp 
! kont = -104 
 End if 
End Do 
 
MVWm = VWm(1) 
MVWm2 = VWm2(1) 

 Iname = Iname - 1 
 
End Subroutine CalculateVWmEffPot 

Subroutine TreeMassesSM(MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,MVWm,MVWm2,               & 
& MVZ,MVZ2,ZDR,ZER,ZUR,ZDL,ZEL,ZUL,Vv,ZW,ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,            & 
& Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,           & 
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,               & 
& Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,GenerationMixing,kont)

Implicit None 
 
Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mub,Mu3

Real(dp),Intent(out) :: MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(6),MFv2(6),MVWm,MVWm2,MVZ,MVZ2

Complex(dp),Intent(out) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),Vv(6,6),ZW(2,2),ZZ(2,2)

Real(dp),Intent(in) :: v1,v2,v3

Logical, Intent(in) :: GenerationMixing 
Integer, Intent(inout) :: kont 
Integer :: i1,i2,i3,i4,j1,j2,j3,kontSave 
Iname = Iname + 1 
NameOfUnit(Iname) = 'TreeMassesBGLNCS'
 
kont = 0 
Call CalculateMFd(Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,              & 
& v1,v2,ZDL,ZDR,MFd,kont)

MFd2 = MFd**2 
Call CalculateMFu(Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,v1,v2,ZUL,ZUR,MFu,kont)

MFu2 = MFu**2 
Call CalculateMFe(Y1e11,Y1e22,Y1e21,Y2e33,v1,v2,ZEL,ZER,MFe,kont)

MFe2 = MFe**2 
Call CalculateMFv(Y1n11,Y1n22,Y1n12,Y2n33,BB11,C23,C32,v1,v2,v3,Vv,MFv,kont)

MFv2 = MFv**2 

 
 Call CalculateVPVZ(g1,g2,v1,v2,ZZ,MVZ,MVZ2,kont)

Call CalculateVWm(g2,v1,v2,ZW,MVWm,MVWm2,kont)

Iname = Iname - 1 
 
End Subroutine  TreeMassesSM 
 
 
Subroutine SortGoldstones(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,               & 
& Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,              & 
& ZH,ZP,ZW,ZZ,kont)

Real(dp),Intent(inout) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(6),MFv2(6),           & 
& Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(inout) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),Vv(6,6),ZP(2,2),ZW(2,2),ZZ(2,2)

Integer, Intent(inout) :: kont 
Integer :: i1, i2, pos 
Real(dp) :: MAhtemp(3) 
Complex(dp) :: ZAhtemp(3,3) 
Real(dp) :: MHmtemp(2) 
Complex(dp) :: ZHmtemp(2,2) 


pos = MinLoc(Abs(MAh2-MVZ2*RXiZ),1) 
If (pos.ne.1) Then 
  MAhtemp = MAh2 
  ZAhtemp = ZA 
  MAh2(1) = MAhtemp(pos) 
  ZA(1,:) = ZAhtemp(pos,:) 
  MAh2(pos) = MAhtemp(1) 
  ZA(pos,:) = ZAhtemp(1,:) 
End if 

 ! Reorder the physical states by their mass 
Do i1=2,3
 pos = Minloc(MAh2(i1:3),1) + i1 -1  
If (pos.ne.i1) Then 
  MAhtemp = MAh2 
  ZAhtemp = ZA 
  MAh2(i1) = MAhtemp(pos) 
  ZA(i1,:) = ZAhtemp(pos,:) 
  MAh2(pos) = MAhtemp(i1) 
  ZA(pos,:) = ZAhtemp(i1,:) 
End if 
End do 
MAh = sqrt(MAh2) 

 
 
pos = MinLoc(Abs(MHm2-MVWm2*RXiWm),1) 
If (pos.ne.1) Then 
  MHmtemp = MHm2 
  ZHmtemp = ZP 
  MHm2(1) = MHmtemp(pos) 
  ZP(1,:) = ZHmtemp(pos,:) 
  MHm2(pos) = MHmtemp(1) 
  ZP(pos,:) = ZHmtemp(1,:) 
End if 

 ! Reorder the physical states by their mass 
Do i1=2,2
 pos = Minloc(MHm2(i1:2),1) + i1 -1  
If (pos.ne.i1) Then 
  MHmtemp = MHm2 
  ZHmtemp = ZP 
  MHm2(i1) = MHmtemp(pos) 
  ZP(i1,:) = ZHmtemp(pos,:) 
  MHm2(pos) = MHmtemp(i1) 
  ZP(pos,:) = ZHmtemp(i1,:) 
End if 
End do 
MHm = sqrt(MHm2) 

 
 
End subroutine SortGoldstones 


End Module TreeLevelMasses_BGLNCS 
 
