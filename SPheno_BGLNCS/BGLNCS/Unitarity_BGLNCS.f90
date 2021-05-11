! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 22:02 on 7.5.2021   
! ----------------------------------------------------------------------  
 
 
Module Unitarity_BGLNCS 
 
Use Control 
Use Settings 
Use LoopFunctions 
Use Mathematics 
Use Model_Data_BGLNCS 
Use RGEs_BGLNCS 
Use LoopMasses_BGLNCS 
Use TreeLevelMasses_BGLNCS 
Use Couplings_BGLNCS 
Use Tadpoles_BGLNCS 
 Use StandardModel 
 
Logical :: IncludeGoldstoneContributions=.true. 
Logical :: IncludeGoldstoneExternal=.true. 
Logical :: AddR=.true. 
Real(dp) :: cut_elements = 0.0001_dp 
Real(dp) :: cut_amplitudes = 0.01_dp 
Logical :: Ignore_poles=.false. 
 
Contains 

Subroutine ScatteringEigenvalues(v1input,v2input,v3input,g1input,g2input,             & 
& g3input,Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,           & 
& Lam3Dashinput,Aa3input,Aa4input,Y1d11input,Y1d12input,Y1d13input,Y1d21input,           & 
& Y1d22input,Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,Y1u12input,          & 
& Y1u21input,Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,Y1e22input,          & 
& Y2e33input,Y1n11input,Y1n12input,Y1n21input,Y1n22input,Y2n33input,C13input,            & 
& C23input,C31input,C32input,BB11input,BB12input,BB21input,BB22input,Aa1input,           & 
& Aa2input,Mu1input,Mu2input,MuDashinput,Mubinput,Mu3input,delta0,kont)

Implicit None 
Integer, Intent(inout) :: kont 
Integer :: ierr 
Real(dp) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Aa1,Aa2,Mub,Mu3

Real(dp) :: v1,v2,v3

Real(dp) :: gD(99) 
Real(dp) :: tz,dt,q,q2,mudim 
Real(dp), Intent(in) :: delta0 
Integer :: iter 
Complex(dp) :: scatter_matrix(55,55) 
Complex(dp) :: rot_matrix(55,55) 
Real(dp) :: eigenvalues_matrix(55), test(2), unitarity_s, scattering_eigenvalue, step_size
Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput,v1input,v2input,v3input

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    & 
& Aa3input,Aa4input,Y1d11input,Y1d12input,Y1d13input,Y1d21input,Y1d22input,              & 
& Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,Y1u12input,Y1u21input,          & 
& Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,Y1e22input,Y2e33input,          & 
& Y1n11input,Y1n12input,Y1n21input,Y1n22input,Y2n33input,C13input,C23input,              & 
& C31input,C32input,BB11input,BB12input,BB21input,BB22input,Aa1input,Aa2input,           & 
& Mubinput,Mu3input

max_scattering_eigenvalue = 0._dp 

g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam1 = Lam1input 
Lam3 = Lam3input 
Lam4 = Lam4input 
Lam2 = Lam2input 
Lam1Dash = Lam1Dashinput 
Lam2Dash = Lam2Dashinput 
Lam3Dash = Lam3Dashinput 
Aa3 = Aa3input 
Aa4 = Aa4input 
Y1d11 = Y1d11input 
Y1d12 = Y1d12input 
Y1d13 = Y1d13input 
Y1d21 = Y1d21input 
Y1d22 = Y1d22input 
Y1d23 = Y1d23input 
Y2d31 = Y2d31input 
Y2d32 = Y2d32input 
Y2d33 = Y2d33input 
Y1u11 = Y1u11input 
Y1u12 = Y1u12input 
Y1u21 = Y1u21input 
Y1u22 = Y1u22input 
Y2u33 = Y2u33input 
Y1e11 = Y1e11input 
Y1e12 = Y1e12input 
Y1e21 = Y1e21input 
Y1e22 = Y1e22input 
Y2e33 = Y2e33input 
Y1n11 = Y1n11input 
Y1n12 = Y1n12input 
Y1n21 = Y1n21input 
Y1n22 = Y1n22input 
Y2n33 = Y2n33input 
C13 = C13input 
C23 = C23input 
C31 = C31input 
C32 = C32input 
BB11 = BB11input 
BB12 = BB12input 
BB21 = BB21input 
BB22 = BB22input 
Aa1 = Aa1input 
Aa2 = Aa2input 
Mu1 = Mu1input 
Mu2 = Mu2input 
MuDash = MuDashinput 
Mub = Mubinput 
Mu3 = Mu3input 
v1 = v1input 
v2 = v2input 
v3 = v3input 
scatter_matrix=0._dp 
scatter_matrix(1,41) =-2._dp*(Lam1)
scatter_matrix(2,42) =-2._dp*(Lam1)
scatter_matrix(3,43) =-1._dp*(Lam3) - Lam4
scatter_matrix(4,44) =-1._dp*(Lam3)
scatter_matrix(4,47) =-1._dp*(Lam4)
scatter_matrix(5,38) =-2*Conjg(Aa4)
scatter_matrix(5,45) =-1._dp*(Lam2Dash)
scatter_matrix(6,6) =-4._dp*(Lam1)
scatter_matrix(6,16) =-2._dp*(Lam1)
scatter_matrix(6,25) =-1._dp*(Lam3) - Lam4
scatter_matrix(6,33) =-1._dp*(Lam3)
scatter_matrix(6,40) =-1._dp*(Lam2Dash)
scatter_matrix(7,15) =-2._dp*(Lam1)
scatter_matrix(7,32) =-1._dp*(Lam4)
scatter_matrix(8,23) =-1._dp*(Lam3) - Lam4
scatter_matrix(8,31) =-1._dp*(Lam4)
scatter_matrix(8,35) =-(sqrt(2._dp)*Conjg(Aa4))
scatter_matrix(8,55) =-(sqrt(2._dp)*Conjg(Aa3))
scatter_matrix(9,30) =-1._dp*(Lam3)
scatter_matrix(10,36) =-1._dp*(Lam2Dash)
scatter_matrix(10,52) =-2*Conjg(Aa3)
scatter_matrix(11,46) =-2._dp*(Lam1)
scatter_matrix(12,44) =-1._dp*(Lam4)
scatter_matrix(12,47) =-1._dp*(Lam3)
scatter_matrix(13,48) =-1._dp*(Lam3) - Lam4
scatter_matrix(14,39) =-2*Conjg(Aa4)
scatter_matrix(14,49) =-1._dp*(Lam2Dash)
scatter_matrix(15,7) =-2._dp*(Lam1)
scatter_matrix(15,26) =-1._dp*(Lam4)
scatter_matrix(16,6) =-2._dp*(Lam1)
scatter_matrix(16,16) =-4._dp*(Lam1)
scatter_matrix(16,25) =-1._dp*(Lam3)
scatter_matrix(16,33) =-1._dp*(Lam3) - Lam4
scatter_matrix(16,40) =-1._dp*(Lam2Dash)
scatter_matrix(17,24) =-1._dp*(Lam3)
scatter_matrix(18,23) =-1._dp*(Lam4)
scatter_matrix(18,31) =-1._dp*(Lam3) - Lam4
scatter_matrix(18,35) =-(sqrt(2._dp)*Conjg(Aa4))
scatter_matrix(18,55) =-(sqrt(2._dp)*Conjg(Aa3))
scatter_matrix(19,37) =-1._dp*(Lam2Dash)
scatter_matrix(19,54) =-2*Conjg(Aa3)
scatter_matrix(20,50) =-2._dp*(Lam2)
scatter_matrix(21,51) =-2._dp*(Lam2)
scatter_matrix(22,36) =-2._dp*(Aa3)
scatter_matrix(22,52) =-1._dp*(Lam3Dash)
scatter_matrix(23,8) =-1._dp*(Lam3) - Lam4
scatter_matrix(23,18) =-1._dp*(Lam4)
scatter_matrix(23,35) =-(sqrt(2._dp)*Aa3)
scatter_matrix(23,55) =-(sqrt(2._dp)*Aa4)
scatter_matrix(24,17) =-1._dp*(Lam3)
scatter_matrix(25,6) =-1._dp*(Lam3) - Lam4
scatter_matrix(25,16) =-1._dp*(Lam3)
scatter_matrix(25,25) =-4._dp*(Lam2)
scatter_matrix(25,33) =-2._dp*(Lam2)
scatter_matrix(25,40) =-1._dp*(Lam3Dash)
scatter_matrix(26,15) =-1._dp*(Lam4)
scatter_matrix(26,32) =-2._dp*(Lam2)
scatter_matrix(27,38) =-1._dp*(Lam3Dash)
scatter_matrix(27,45) =-2._dp*(Aa4)
scatter_matrix(28,53) =-2._dp*(Lam2)
scatter_matrix(29,37) =-2._dp*(Aa3)
scatter_matrix(29,54) =-1._dp*(Lam3Dash)
scatter_matrix(30,9) =-1._dp*(Lam3)
scatter_matrix(31,8) =-1._dp*(Lam4)
scatter_matrix(31,18) =-1._dp*(Lam3) - Lam4
scatter_matrix(31,35) =-(sqrt(2._dp)*Aa3)
scatter_matrix(31,55) =-(sqrt(2._dp)*Aa4)
scatter_matrix(32,7) =-1._dp*(Lam4)
scatter_matrix(32,26) =-2._dp*(Lam2)
scatter_matrix(33,6) =-1._dp*(Lam3)
scatter_matrix(33,16) =-1._dp*(Lam3) - Lam4
scatter_matrix(33,25) =-2._dp*(Lam2)
scatter_matrix(33,33) =-4._dp*(Lam2)
scatter_matrix(33,40) =-1._dp*(Lam3Dash)
scatter_matrix(34,39) =-1._dp*(Lam3Dash)
scatter_matrix(34,49) =-2._dp*(Aa4)
scatter_matrix(35,8) =-(sqrt(2._dp)*Conjg(Aa4))
scatter_matrix(35,18) =-(sqrt(2._dp)*Conjg(Aa4))
scatter_matrix(35,23) =-(sqrt(2._dp)*Aa3)
scatter_matrix(35,31) =-(sqrt(2._dp)*Aa3)
scatter_matrix(35,55) =-2._dp*(Lam1Dash)
scatter_matrix(36,10) =-1._dp*(Lam2Dash)
scatter_matrix(36,22) =-2._dp*(Aa3)
scatter_matrix(37,19) =-1._dp*(Lam2Dash)
scatter_matrix(37,29) =-2._dp*(Aa3)
scatter_matrix(38,5) =-2*Conjg(Aa4)
scatter_matrix(38,27) =-1._dp*(Lam3Dash)
scatter_matrix(39,14) =-2*Conjg(Aa4)
scatter_matrix(39,34) =-1._dp*(Lam3Dash)
scatter_matrix(40,6) =-1._dp*(Lam2Dash)
scatter_matrix(40,16) =-1._dp*(Lam2Dash)
scatter_matrix(40,25) =-1._dp*(Lam3Dash)
scatter_matrix(40,33) =-1._dp*(Lam3Dash)
scatter_matrix(40,40) =-4._dp*(Lam1Dash)
scatter_matrix(41,1) =-2._dp*(Lam1)
scatter_matrix(42,2) =-2._dp*(Lam1)
scatter_matrix(43,3) =-1._dp*(Lam3) - Lam4
scatter_matrix(44,4) =-1._dp*(Lam3)
scatter_matrix(44,12) =-1._dp*(Lam4)
scatter_matrix(45,5) =-1._dp*(Lam2Dash)
scatter_matrix(45,27) =-2._dp*(Aa4)
scatter_matrix(46,11) =-2._dp*(Lam1)
scatter_matrix(47,4) =-1._dp*(Lam4)
scatter_matrix(47,12) =-1._dp*(Lam3)
scatter_matrix(48,13) =-1._dp*(Lam3) - Lam4
scatter_matrix(49,14) =-1._dp*(Lam2Dash)
scatter_matrix(49,34) =-2._dp*(Aa4)
scatter_matrix(50,20) =-2._dp*(Lam2)
scatter_matrix(51,21) =-2._dp*(Lam2)
scatter_matrix(52,10) =-2*Conjg(Aa3)
scatter_matrix(52,22) =-1._dp*(Lam3Dash)
scatter_matrix(53,28) =-2._dp*(Lam2)
scatter_matrix(54,19) =-2*Conjg(Aa3)
scatter_matrix(54,29) =-1._dp*(Lam3Dash)
scatter_matrix(55,8) =-(sqrt(2._dp)*Conjg(Aa3))
scatter_matrix(55,18) =-(sqrt(2._dp)*Conjg(Aa3))
scatter_matrix(55,23) =-(sqrt(2._dp)*Aa4)
scatter_matrix(55,31) =-(sqrt(2._dp)*Aa4)
scatter_matrix(55,35) =-2._dp*(Lam1Dash)
Call EigenSystem( oo16pi*scatter_matrix,eigenvalues_matrix,rot_matrix,ierr,test) 

scattering_eigenvalue=MaxVal(Abs(eigenvalues_matrix)) 
max_scattering_eigenvalue=scattering_eigenvalue 
If (max_scattering_eigenvalue.gt.0.5_dp) TreeUnitarity=0._dp 
End Subroutine ScatteringEigenvalues

Subroutine ScatteringEigenvaluesWithTrilinears(MAhinput,MAh2input,MFdinput,           & 
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,Mhhinput,           & 
& Mhh2input,MHminput,MHm2input,MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,          & 
& ZDRinput,ZERinput,ZURinput,vinput,ZDLinput,ZELinput,ZULinput,Vvinput,ZAinput,          & 
& ZHinput,ZPinput,ZWinput,ZZinput,v1input,v2input,v3input,g1input,g2input,               & 
& g3input,Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,           & 
& Lam3Dashinput,Aa3input,Aa4input,Y1d11input,Y1d12input,Y1d13input,Y1d21input,           & 
& Y1d22input,Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,Y1u12input,          & 
& Y1u21input,Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,Y1e22input,          & 
& Y2e33input,Y1n11input,Y1n12input,Y1n21input,Y1n22input,Y2n33input,C13input,            & 
& C23input,C31input,C32input,BB11input,BB12input,BB21input,BB22input,Aa1input,           & 
& Aa2input,Mu1input,Mu2input,MuDashinput,Mubinput,Mu3input,delta0,kont)

Implicit None 
Integer, Intent(inout) :: kont 
Integer :: ierr 
Logical :: Pole_Present, SPole_Present, TPole_Present, UPole_Present 
Integer :: RemoveTUpoles(99) 
Real(dp) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,C13,C23,C31,C32,BB11,            & 
& BB12,BB21,BB22,Aa1,Aa2,Mub,Mu3

Real(dp) :: v1,v2,v3

Complex(dp) :: cplAhAhAh(3,3,3),cplAhAhhh(3,3,3),cplAhhhhh(3,3,3),cplAhHmcHm(3,2,2),cplhhhhhh(3,3,3),& 
& cplhhHmcHm(3,2,2),cplAhAhAhAh(3,3,3,3),cplAhAhAhhh(3,3,3,3),cplAhAhhhhh(3,3,3,3),      & 
& cplAhAhHmcHm(3,3,2,2),cplAhhhhhhh(3,3,3,3),cplAhhhHmcHm(3,3,2,2),cplhhhhhhhh(3,3,3,3), & 
& cplhhhhHmcHm(3,3,2,2),cplHmHmcHmcHm(2,2,2,2)

Real(dp) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(6),MFv2(6),           & 
& Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),Vv(6,6),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput,v1input,v2input,v3input

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    & 
& Aa3input,Aa4input,Y1d11input,Y1d12input,Y1d13input,Y1d21input,Y1d22input,              & 
& Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,Y1u12input,Y1u21input,          & 
& Y1u22input,Y2u33input,Y1e11input,Y1e12input,Y1e21input,Y1e22input,Y2e33input,          & 
& Y1n11input,Y1n12input,Y1n21input,Y1n22input,Y2n33input,C13input,C23input,              & 
& C31input,C32input,BB11input,BB12input,BB21input,BB22input,Aa1input,Aa2input,           & 
& Mubinput,Mu3input

Real(dp),Intent(in) :: MAhinput(3),MAh2input(3),MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),           & 
& MFuinput(3),MFu2input(3),MFvinput(6),MFv2input(6),Mhhinput(3),Mhh2input(3),            & 
& MHminput(2),MHm2input(2),MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,              & 
& vinput,ZAinput(3,3),ZHinput(3,3)

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  & 
& Vvinput(6,6),ZPinput(2,2),ZWinput(2,2),ZZinput(2,2)

Complex(dp) :: scatter_matrix1(25,25) 
Complex(dp) :: scatter_matrix1B(25,25) 
Complex(dp) :: rot_matrix1(25,25) 
Real(dp) :: eigenvalues_matrix1(25)
Complex(dp) :: scatter_matrix2(12,12) 
Complex(dp) :: scatter_matrix2B(12,12) 
Complex(dp) :: rot_matrix2(12,12) 
Real(dp) :: eigenvalues_matrix2(12)
Complex(dp) :: scatter_matrix3(3,3) 
Complex(dp) :: scatter_matrix3B(3,3) 
Complex(dp) :: rot_matrix3(3,3) 
Real(dp) :: eigenvalues_matrix3(3)
Real(dp) :: step_size,scattering_eigenvalue_trilinears, unitarity_s, test(2) 
Real(dp) :: gD(99) 
Real(dp) :: tz,dt,q,q2,mudim, max_element_removed  
Real(dp), Intent(in) :: delta0 
Integer :: iter, i, il,ir, count 
g1 = g1input 
g2 = g2input 
g3 = g3input 
Lam1 = Lam1input 
Lam3 = Lam3input 
Lam4 = Lam4input 
Lam2 = Lam2input 
Lam1Dash = Lam1Dashinput 
Lam2Dash = Lam2Dashinput 
Lam3Dash = Lam3Dashinput 
Aa3 = Aa3input 
Aa4 = Aa4input 
Y1d11 = Y1d11input 
Y1d12 = Y1d12input 
Y1d13 = Y1d13input 
Y1d21 = Y1d21input 
Y1d22 = Y1d22input 
Y1d23 = Y1d23input 
Y2d31 = Y2d31input 
Y2d32 = Y2d32input 
Y2d33 = Y2d33input 
Y1u11 = Y1u11input 
Y1u12 = Y1u12input 
Y1u21 = Y1u21input 
Y1u22 = Y1u22input 
Y2u33 = Y2u33input 
Y1e11 = Y1e11input 
Y1e12 = Y1e12input 
Y1e21 = Y1e21input 
Y1e22 = Y1e22input 
Y2e33 = Y2e33input 
Y1n11 = Y1n11input 
Y1n12 = Y1n12input 
Y1n21 = Y1n21input 
Y1n22 = Y1n22input 
Y2n33 = Y2n33input 
C13 = C13input 
C23 = C23input 
C31 = C31input 
C32 = C32input 
BB11 = BB11input 
BB12 = BB12input 
BB21 = BB21input 
BB22 = BB22input 
Aa1 = Aa1input 
Aa2 = Aa2input 
Mu1 = Mu1input 
Mu2 = Mu2input 
MuDash = MuDashinput 
Mub = Mubinput 
Mu3 = Mu3input 
v1 = v1input 
v2 = v2input 
v3 = v3input 
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,MHm,            & 
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,ZZ,               & 
& v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              & 
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               & 
& Y1u22,Y2u33,Y1e11,Y1e12,Y1e21,Y1e22,Y2e33,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,               & 
& C13,C23,C31,C32,BB11,BB12,BB21,BB22,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,.True.,kont)

Call CouplingsColourStructures(Aa1,Aa2,Aa3,Aa4,v1,v2,v3,ZA,Lam1,Lam3,Lam4,            & 
& Lam2,Lam1Dash,Lam2Dash,Lam3Dash,ZH,ZP,cplAhAhAh,cplAhAhhh,cplAhhhhh,cplAhHmcHm,        & 
& cplhhhhhh,cplhhHmcHm,cplAhAhAhAh,cplAhAhAhhh,cplAhAhhhhh,cplAhAhHmcHm,cplAhhhhhhh,     & 
& cplAhhhHmcHm,cplhhhhhhhh,cplhhhhHmcHm,cplHmHmcHmcHm)

max_scattering_eigenvalue_trilinears = 0._dp 
If (unitarity_steps.eq.1) Then  
  step_size = 0._dp
Else  
 If (unitarity_steps.gt.0) Then 
  step_size = ((Abs(unitarity_s_max)) -(abs(unitarity_s_min)))/(1._dp*(Abs(unitarity_steps)-1)) 
 Else 
  step_size = (log(Abs(unitarity_s_max)) -log(abs(unitarity_s_min)))/(1._dp*(Abs(unitarity_steps)-1)) 
 End if 
End if 
Do iter=0,Abs(unitarity_steps)-1 
If (unitarity_steps.lt.0) Then 
  unitarity_s=exp(log(unitarity_s_min) + iter*step_size)**2 
Else 
  unitarity_s=(unitarity_s_min + iter*step_size)**2 
End if 
!! 1. sub-matrix  
Pole_Present = .false. 
max_element_removed = 0._dp 
RemoveTUpoles = 0 
scatter_matrix1=0._dp 
If (IncludeGoldstoneExternal) scatter_matrix1(1,1) = a0_AhAh_AhAh_00(unitarity_s,1,1,1,1,1,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,2) = a0_AhAh_AhAh_00(unitarity_s,1,1,1,2,1,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,3) = a0_AhAh_AhAh_00(unitarity_s,1,1,1,3,1,3) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,4) = a0_AhAh_AhAh_00(unitarity_s,1,1,2,2,1,4) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,5) = a0_AhAh_AhAh_00(unitarity_s,1,1,2,3,1,5) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,6) = a0_AhAh_AhAh_00(unitarity_s,1,1,3,3,1,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,7) = a0_AhAh_Ahhh_00(unitarity_s,1,1,1,1,1,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,8) = a0_AhAh_Ahhh_00(unitarity_s,1,1,1,2,1,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,9) = a0_AhAh_Ahhh_00(unitarity_s,1,1,1,3,1,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,10) = a0_AhAh_Ahhh_00(unitarity_s,1,1,2,1,1,10) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,11) = a0_AhAh_Ahhh_00(unitarity_s,1,1,2,2,1,11) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,12) = a0_AhAh_Ahhh_00(unitarity_s,1,1,2,3,1,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,13) = a0_AhAh_Ahhh_00(unitarity_s,1,1,3,1,1,13) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,14) = a0_AhAh_Ahhh_00(unitarity_s,1,1,3,2,1,14) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,15) = a0_AhAh_Ahhh_00(unitarity_s,1,1,3,3,1,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,16) = a0_AhAh_hhhh_00(unitarity_s,1,1,1,1,1,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,17) = a0_AhAh_hhhh_00(unitarity_s,1,1,1,2,1,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,18) = a0_AhAh_hhhh_00(unitarity_s,1,1,1,3,1,18) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,19) = a0_AhAh_hhhh_00(unitarity_s,1,1,2,2,1,19) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,20) = a0_AhAh_hhhh_00(unitarity_s,1,1,2,3,1,20) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,21) = a0_AhAh_hhhh_00(unitarity_s,1,1,3,3,1,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,22) = a0_AhAh_HmHmc_00(unitarity_s,1,1,1,1,1,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,23) = a0_AhAh_HmHmc_00(unitarity_s,1,1,1,2,1,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,24) = a0_AhAh_HmHmc_00(unitarity_s,1,1,2,1,1,24) 
If (IncludeGoldstoneExternal) scatter_matrix1(1,25) = a0_AhAh_HmHmc_00(unitarity_s,1,1,2,2,1,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,1) = a0_AhAh_AhAh_00(unitarity_s,1,2,1,1,2,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,2) = a0_AhAh_AhAh_00(unitarity_s,1,2,1,2,2,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,3) = a0_AhAh_AhAh_00(unitarity_s,1,2,1,3,2,3) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,4) = a0_AhAh_AhAh_00(unitarity_s,1,2,2,2,2,4) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,5) = a0_AhAh_AhAh_00(unitarity_s,1,2,2,3,2,5) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,6) = a0_AhAh_AhAh_00(unitarity_s,1,2,3,3,2,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,7) = a0_AhAh_Ahhh_00(unitarity_s,1,2,1,1,2,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,8) = a0_AhAh_Ahhh_00(unitarity_s,1,2,1,2,2,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,9) = a0_AhAh_Ahhh_00(unitarity_s,1,2,1,3,2,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,10) = a0_AhAh_Ahhh_00(unitarity_s,1,2,2,1,2,10) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,11) = a0_AhAh_Ahhh_00(unitarity_s,1,2,2,2,2,11) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,12) = a0_AhAh_Ahhh_00(unitarity_s,1,2,2,3,2,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,13) = a0_AhAh_Ahhh_00(unitarity_s,1,2,3,1,2,13) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,14) = a0_AhAh_Ahhh_00(unitarity_s,1,2,3,2,2,14) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,15) = a0_AhAh_Ahhh_00(unitarity_s,1,2,3,3,2,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,16) = a0_AhAh_hhhh_00(unitarity_s,1,2,1,1,2,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,17) = a0_AhAh_hhhh_00(unitarity_s,1,2,1,2,2,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,18) = a0_AhAh_hhhh_00(unitarity_s,1,2,1,3,2,18) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,19) = a0_AhAh_hhhh_00(unitarity_s,1,2,2,2,2,19) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,20) = a0_AhAh_hhhh_00(unitarity_s,1,2,2,3,2,20) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,21) = a0_AhAh_hhhh_00(unitarity_s,1,2,3,3,2,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,22) = a0_AhAh_HmHmc_00(unitarity_s,1,2,1,1,2,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,23) = a0_AhAh_HmHmc_00(unitarity_s,1,2,1,2,2,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,24) = a0_AhAh_HmHmc_00(unitarity_s,1,2,2,1,2,24) 
If (IncludeGoldstoneExternal) scatter_matrix1(2,25) = a0_AhAh_HmHmc_00(unitarity_s,1,2,2,2,2,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,1) = a0_AhAh_AhAh_00(unitarity_s,1,3,1,1,3,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,2) = a0_AhAh_AhAh_00(unitarity_s,1,3,1,2,3,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,3) = a0_AhAh_AhAh_00(unitarity_s,1,3,1,3,3,3) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,4) = a0_AhAh_AhAh_00(unitarity_s,1,3,2,2,3,4) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,5) = a0_AhAh_AhAh_00(unitarity_s,1,3,2,3,3,5) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,6) = a0_AhAh_AhAh_00(unitarity_s,1,3,3,3,3,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,7) = a0_AhAh_Ahhh_00(unitarity_s,1,3,1,1,3,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,8) = a0_AhAh_Ahhh_00(unitarity_s,1,3,1,2,3,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,9) = a0_AhAh_Ahhh_00(unitarity_s,1,3,1,3,3,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,10) = a0_AhAh_Ahhh_00(unitarity_s,1,3,2,1,3,10) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,11) = a0_AhAh_Ahhh_00(unitarity_s,1,3,2,2,3,11) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,12) = a0_AhAh_Ahhh_00(unitarity_s,1,3,2,3,3,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,13) = a0_AhAh_Ahhh_00(unitarity_s,1,3,3,1,3,13) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,14) = a0_AhAh_Ahhh_00(unitarity_s,1,3,3,2,3,14) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,15) = a0_AhAh_Ahhh_00(unitarity_s,1,3,3,3,3,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,16) = a0_AhAh_hhhh_00(unitarity_s,1,3,1,1,3,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,17) = a0_AhAh_hhhh_00(unitarity_s,1,3,1,2,3,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,18) = a0_AhAh_hhhh_00(unitarity_s,1,3,1,3,3,18) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,19) = a0_AhAh_hhhh_00(unitarity_s,1,3,2,2,3,19) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,20) = a0_AhAh_hhhh_00(unitarity_s,1,3,2,3,3,20) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,21) = a0_AhAh_hhhh_00(unitarity_s,1,3,3,3,3,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,22) = a0_AhAh_HmHmc_00(unitarity_s,1,3,1,1,3,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,23) = a0_AhAh_HmHmc_00(unitarity_s,1,3,1,2,3,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,24) = a0_AhAh_HmHmc_00(unitarity_s,1,3,2,1,3,24) 
If (IncludeGoldstoneExternal) scatter_matrix1(3,25) = a0_AhAh_HmHmc_00(unitarity_s,1,3,2,2,3,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,1) = a0_AhAh_AhAh_00(unitarity_s,2,2,1,1,4,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,2) = a0_AhAh_AhAh_00(unitarity_s,2,2,1,2,4,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,3) = a0_AhAh_AhAh_00(unitarity_s,2,2,1,3,4,3) 
scatter_matrix1(4,4) = a0_AhAh_AhAh_00(unitarity_s,2,2,2,2,4,4) 
scatter_matrix1(4,5) = a0_AhAh_AhAh_00(unitarity_s,2,2,2,3,4,5) 
scatter_matrix1(4,6) = a0_AhAh_AhAh_00(unitarity_s,2,2,3,3,4,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,7) = a0_AhAh_Ahhh_00(unitarity_s,2,2,1,1,4,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,8) = a0_AhAh_Ahhh_00(unitarity_s,2,2,1,2,4,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,9) = a0_AhAh_Ahhh_00(unitarity_s,2,2,1,3,4,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,10) = a0_AhAh_Ahhh_00(unitarity_s,2,2,2,1,4,10) 
scatter_matrix1(4,11) = a0_AhAh_Ahhh_00(unitarity_s,2,2,2,2,4,11) 
scatter_matrix1(4,12) = a0_AhAh_Ahhh_00(unitarity_s,2,2,2,3,4,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,13) = a0_AhAh_Ahhh_00(unitarity_s,2,2,3,1,4,13) 
scatter_matrix1(4,14) = a0_AhAh_Ahhh_00(unitarity_s,2,2,3,2,4,14) 
scatter_matrix1(4,15) = a0_AhAh_Ahhh_00(unitarity_s,2,2,3,3,4,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,16) = a0_AhAh_hhhh_00(unitarity_s,2,2,1,1,4,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,17) = a0_AhAh_hhhh_00(unitarity_s,2,2,1,2,4,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,18) = a0_AhAh_hhhh_00(unitarity_s,2,2,1,3,4,18) 
scatter_matrix1(4,19) = a0_AhAh_hhhh_00(unitarity_s,2,2,2,2,4,19) 
scatter_matrix1(4,20) = a0_AhAh_hhhh_00(unitarity_s,2,2,2,3,4,20) 
scatter_matrix1(4,21) = a0_AhAh_hhhh_00(unitarity_s,2,2,3,3,4,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,22) = a0_AhAh_HmHmc_00(unitarity_s,2,2,1,1,4,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,23) = a0_AhAh_HmHmc_00(unitarity_s,2,2,1,2,4,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(4,24) = a0_AhAh_HmHmc_00(unitarity_s,2,2,2,1,4,24) 
scatter_matrix1(4,25) = a0_AhAh_HmHmc_00(unitarity_s,2,2,2,2,4,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,1) = a0_AhAh_AhAh_00(unitarity_s,2,3,1,1,5,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,2) = a0_AhAh_AhAh_00(unitarity_s,2,3,1,2,5,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,3) = a0_AhAh_AhAh_00(unitarity_s,2,3,1,3,5,3) 
scatter_matrix1(5,4) = a0_AhAh_AhAh_00(unitarity_s,2,3,2,2,5,4) 
scatter_matrix1(5,5) = a0_AhAh_AhAh_00(unitarity_s,2,3,2,3,5,5) 
scatter_matrix1(5,6) = a0_AhAh_AhAh_00(unitarity_s,2,3,3,3,5,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,7) = a0_AhAh_Ahhh_00(unitarity_s,2,3,1,1,5,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,8) = a0_AhAh_Ahhh_00(unitarity_s,2,3,1,2,5,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,9) = a0_AhAh_Ahhh_00(unitarity_s,2,3,1,3,5,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,10) = a0_AhAh_Ahhh_00(unitarity_s,2,3,2,1,5,10) 
scatter_matrix1(5,11) = a0_AhAh_Ahhh_00(unitarity_s,2,3,2,2,5,11) 
scatter_matrix1(5,12) = a0_AhAh_Ahhh_00(unitarity_s,2,3,2,3,5,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,13) = a0_AhAh_Ahhh_00(unitarity_s,2,3,3,1,5,13) 
scatter_matrix1(5,14) = a0_AhAh_Ahhh_00(unitarity_s,2,3,3,2,5,14) 
scatter_matrix1(5,15) = a0_AhAh_Ahhh_00(unitarity_s,2,3,3,3,5,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,16) = a0_AhAh_hhhh_00(unitarity_s,2,3,1,1,5,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,17) = a0_AhAh_hhhh_00(unitarity_s,2,3,1,2,5,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,18) = a0_AhAh_hhhh_00(unitarity_s,2,3,1,3,5,18) 
scatter_matrix1(5,19) = a0_AhAh_hhhh_00(unitarity_s,2,3,2,2,5,19) 
scatter_matrix1(5,20) = a0_AhAh_hhhh_00(unitarity_s,2,3,2,3,5,20) 
scatter_matrix1(5,21) = a0_AhAh_hhhh_00(unitarity_s,2,3,3,3,5,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,22) = a0_AhAh_HmHmc_00(unitarity_s,2,3,1,1,5,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,23) = a0_AhAh_HmHmc_00(unitarity_s,2,3,1,2,5,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(5,24) = a0_AhAh_HmHmc_00(unitarity_s,2,3,2,1,5,24) 
scatter_matrix1(5,25) = a0_AhAh_HmHmc_00(unitarity_s,2,3,2,2,5,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,1) = a0_AhAh_AhAh_00(unitarity_s,3,3,1,1,6,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,2) = a0_AhAh_AhAh_00(unitarity_s,3,3,1,2,6,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,3) = a0_AhAh_AhAh_00(unitarity_s,3,3,1,3,6,3) 
scatter_matrix1(6,4) = a0_AhAh_AhAh_00(unitarity_s,3,3,2,2,6,4) 
scatter_matrix1(6,5) = a0_AhAh_AhAh_00(unitarity_s,3,3,2,3,6,5) 
scatter_matrix1(6,6) = a0_AhAh_AhAh_00(unitarity_s,3,3,3,3,6,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,7) = a0_AhAh_Ahhh_00(unitarity_s,3,3,1,1,6,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,8) = a0_AhAh_Ahhh_00(unitarity_s,3,3,1,2,6,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,9) = a0_AhAh_Ahhh_00(unitarity_s,3,3,1,3,6,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,10) = a0_AhAh_Ahhh_00(unitarity_s,3,3,2,1,6,10) 
scatter_matrix1(6,11) = a0_AhAh_Ahhh_00(unitarity_s,3,3,2,2,6,11) 
scatter_matrix1(6,12) = a0_AhAh_Ahhh_00(unitarity_s,3,3,2,3,6,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,13) = a0_AhAh_Ahhh_00(unitarity_s,3,3,3,1,6,13) 
scatter_matrix1(6,14) = a0_AhAh_Ahhh_00(unitarity_s,3,3,3,2,6,14) 
scatter_matrix1(6,15) = a0_AhAh_Ahhh_00(unitarity_s,3,3,3,3,6,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,16) = a0_AhAh_hhhh_00(unitarity_s,3,3,1,1,6,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,17) = a0_AhAh_hhhh_00(unitarity_s,3,3,1,2,6,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,18) = a0_AhAh_hhhh_00(unitarity_s,3,3,1,3,6,18) 
scatter_matrix1(6,19) = a0_AhAh_hhhh_00(unitarity_s,3,3,2,2,6,19) 
scatter_matrix1(6,20) = a0_AhAh_hhhh_00(unitarity_s,3,3,2,3,6,20) 
scatter_matrix1(6,21) = a0_AhAh_hhhh_00(unitarity_s,3,3,3,3,6,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,22) = a0_AhAh_HmHmc_00(unitarity_s,3,3,1,1,6,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,23) = a0_AhAh_HmHmc_00(unitarity_s,3,3,1,2,6,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(6,24) = a0_AhAh_HmHmc_00(unitarity_s,3,3,2,1,6,24) 
scatter_matrix1(6,25) = a0_AhAh_HmHmc_00(unitarity_s,3,3,2,2,6,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,1) = a0_Ahhh_AhAh_00(unitarity_s,1,1,1,1,7,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,2) = a0_Ahhh_AhAh_00(unitarity_s,1,1,1,2,7,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,3) = a0_Ahhh_AhAh_00(unitarity_s,1,1,1,3,7,3) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,4) = a0_Ahhh_AhAh_00(unitarity_s,1,1,2,2,7,4) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,5) = a0_Ahhh_AhAh_00(unitarity_s,1,1,2,3,7,5) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,6) = a0_Ahhh_AhAh_00(unitarity_s,1,1,3,3,7,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,7) = a0_Ahhh_Ahhh_00(unitarity_s,1,1,1,1,7,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,8) = a0_Ahhh_Ahhh_00(unitarity_s,1,1,1,2,7,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,9) = a0_Ahhh_Ahhh_00(unitarity_s,1,1,1,3,7,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,10) = a0_Ahhh_Ahhh_00(unitarity_s,1,1,2,1,7,10) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,11) = a0_Ahhh_Ahhh_00(unitarity_s,1,1,2,2,7,11) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,12) = a0_Ahhh_Ahhh_00(unitarity_s,1,1,2,3,7,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,13) = a0_Ahhh_Ahhh_00(unitarity_s,1,1,3,1,7,13) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,14) = a0_Ahhh_Ahhh_00(unitarity_s,1,1,3,2,7,14) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,15) = a0_Ahhh_Ahhh_00(unitarity_s,1,1,3,3,7,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,16) = a0_Ahhh_hhhh_00(unitarity_s,1,1,1,1,7,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,17) = a0_Ahhh_hhhh_00(unitarity_s,1,1,1,2,7,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,18) = a0_Ahhh_hhhh_00(unitarity_s,1,1,1,3,7,18) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,19) = a0_Ahhh_hhhh_00(unitarity_s,1,1,2,2,7,19) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,20) = a0_Ahhh_hhhh_00(unitarity_s,1,1,2,3,7,20) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,21) = a0_Ahhh_hhhh_00(unitarity_s,1,1,3,3,7,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,22) = a0_Ahhh_HmHmc_00(unitarity_s,1,1,1,1,7,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,23) = a0_Ahhh_HmHmc_00(unitarity_s,1,1,1,2,7,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,24) = a0_Ahhh_HmHmc_00(unitarity_s,1,1,2,1,7,24) 
If (IncludeGoldstoneExternal) scatter_matrix1(7,25) = a0_Ahhh_HmHmc_00(unitarity_s,1,1,2,2,7,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,1) = a0_Ahhh_AhAh_00(unitarity_s,1,2,1,1,8,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,2) = a0_Ahhh_AhAh_00(unitarity_s,1,2,1,2,8,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,3) = a0_Ahhh_AhAh_00(unitarity_s,1,2,1,3,8,3) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,4) = a0_Ahhh_AhAh_00(unitarity_s,1,2,2,2,8,4) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,5) = a0_Ahhh_AhAh_00(unitarity_s,1,2,2,3,8,5) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,6) = a0_Ahhh_AhAh_00(unitarity_s,1,2,3,3,8,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,7) = a0_Ahhh_Ahhh_00(unitarity_s,1,2,1,1,8,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,8) = a0_Ahhh_Ahhh_00(unitarity_s,1,2,1,2,8,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,9) = a0_Ahhh_Ahhh_00(unitarity_s,1,2,1,3,8,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,10) = a0_Ahhh_Ahhh_00(unitarity_s,1,2,2,1,8,10) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,11) = a0_Ahhh_Ahhh_00(unitarity_s,1,2,2,2,8,11) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,12) = a0_Ahhh_Ahhh_00(unitarity_s,1,2,2,3,8,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,13) = a0_Ahhh_Ahhh_00(unitarity_s,1,2,3,1,8,13) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,14) = a0_Ahhh_Ahhh_00(unitarity_s,1,2,3,2,8,14) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,15) = a0_Ahhh_Ahhh_00(unitarity_s,1,2,3,3,8,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,16) = a0_Ahhh_hhhh_00(unitarity_s,1,2,1,1,8,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,17) = a0_Ahhh_hhhh_00(unitarity_s,1,2,1,2,8,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,18) = a0_Ahhh_hhhh_00(unitarity_s,1,2,1,3,8,18) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,19) = a0_Ahhh_hhhh_00(unitarity_s,1,2,2,2,8,19) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,20) = a0_Ahhh_hhhh_00(unitarity_s,1,2,2,3,8,20) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,21) = a0_Ahhh_hhhh_00(unitarity_s,1,2,3,3,8,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,22) = a0_Ahhh_HmHmc_00(unitarity_s,1,2,1,1,8,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,23) = a0_Ahhh_HmHmc_00(unitarity_s,1,2,1,2,8,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,24) = a0_Ahhh_HmHmc_00(unitarity_s,1,2,2,1,8,24) 
If (IncludeGoldstoneExternal) scatter_matrix1(8,25) = a0_Ahhh_HmHmc_00(unitarity_s,1,2,2,2,8,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,1) = a0_Ahhh_AhAh_00(unitarity_s,1,3,1,1,9,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,2) = a0_Ahhh_AhAh_00(unitarity_s,1,3,1,2,9,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,3) = a0_Ahhh_AhAh_00(unitarity_s,1,3,1,3,9,3) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,4) = a0_Ahhh_AhAh_00(unitarity_s,1,3,2,2,9,4) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,5) = a0_Ahhh_AhAh_00(unitarity_s,1,3,2,3,9,5) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,6) = a0_Ahhh_AhAh_00(unitarity_s,1,3,3,3,9,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,7) = a0_Ahhh_Ahhh_00(unitarity_s,1,3,1,1,9,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,8) = a0_Ahhh_Ahhh_00(unitarity_s,1,3,1,2,9,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,9) = a0_Ahhh_Ahhh_00(unitarity_s,1,3,1,3,9,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,10) = a0_Ahhh_Ahhh_00(unitarity_s,1,3,2,1,9,10) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,11) = a0_Ahhh_Ahhh_00(unitarity_s,1,3,2,2,9,11) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,12) = a0_Ahhh_Ahhh_00(unitarity_s,1,3,2,3,9,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,13) = a0_Ahhh_Ahhh_00(unitarity_s,1,3,3,1,9,13) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,14) = a0_Ahhh_Ahhh_00(unitarity_s,1,3,3,2,9,14) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,15) = a0_Ahhh_Ahhh_00(unitarity_s,1,3,3,3,9,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,16) = a0_Ahhh_hhhh_00(unitarity_s,1,3,1,1,9,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,17) = a0_Ahhh_hhhh_00(unitarity_s,1,3,1,2,9,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,18) = a0_Ahhh_hhhh_00(unitarity_s,1,3,1,3,9,18) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,19) = a0_Ahhh_hhhh_00(unitarity_s,1,3,2,2,9,19) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,20) = a0_Ahhh_hhhh_00(unitarity_s,1,3,2,3,9,20) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,21) = a0_Ahhh_hhhh_00(unitarity_s,1,3,3,3,9,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,22) = a0_Ahhh_HmHmc_00(unitarity_s,1,3,1,1,9,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,23) = a0_Ahhh_HmHmc_00(unitarity_s,1,3,1,2,9,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,24) = a0_Ahhh_HmHmc_00(unitarity_s,1,3,2,1,9,24) 
If (IncludeGoldstoneExternal) scatter_matrix1(9,25) = a0_Ahhh_HmHmc_00(unitarity_s,1,3,2,2,9,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(10,1) = a0_Ahhh_AhAh_00(unitarity_s,2,1,1,1,10,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(10,2) = a0_Ahhh_AhAh_00(unitarity_s,2,1,1,2,10,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(10,3) = a0_Ahhh_AhAh_00(unitarity_s,2,1,1,3,10,3) 
scatter_matrix1(10,4) = a0_Ahhh_AhAh_00(unitarity_s,2,1,2,2,10,4) 
scatter_matrix1(10,5) = a0_Ahhh_AhAh_00(unitarity_s,2,1,2,3,10,5) 
scatter_matrix1(10,6) = a0_Ahhh_AhAh_00(unitarity_s,2,1,3,3,10,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(10,7) = a0_Ahhh_Ahhh_00(unitarity_s,2,1,1,1,10,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(10,8) = a0_Ahhh_Ahhh_00(unitarity_s,2,1,1,2,10,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(10,9) = a0_Ahhh_Ahhh_00(unitarity_s,2,1,1,3,10,9) 
scatter_matrix1(10,10) = a0_Ahhh_Ahhh_00(unitarity_s,2,1,2,1,10,10) 
scatter_matrix1(10,11) = a0_Ahhh_Ahhh_00(unitarity_s,2,1,2,2,10,11) 
scatter_matrix1(10,12) = a0_Ahhh_Ahhh_00(unitarity_s,2,1,2,3,10,12) 
scatter_matrix1(10,13) = a0_Ahhh_Ahhh_00(unitarity_s,2,1,3,1,10,13) 
scatter_matrix1(10,14) = a0_Ahhh_Ahhh_00(unitarity_s,2,1,3,2,10,14) 
scatter_matrix1(10,15) = a0_Ahhh_Ahhh_00(unitarity_s,2,1,3,3,10,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(10,16) = a0_Ahhh_hhhh_00(unitarity_s,2,1,1,1,10,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(10,17) = a0_Ahhh_hhhh_00(unitarity_s,2,1,1,2,10,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(10,18) = a0_Ahhh_hhhh_00(unitarity_s,2,1,1,3,10,18) 
scatter_matrix1(10,19) = a0_Ahhh_hhhh_00(unitarity_s,2,1,2,2,10,19) 
scatter_matrix1(10,20) = a0_Ahhh_hhhh_00(unitarity_s,2,1,2,3,10,20) 
scatter_matrix1(10,21) = a0_Ahhh_hhhh_00(unitarity_s,2,1,3,3,10,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(10,22) = a0_Ahhh_HmHmc_00(unitarity_s,2,1,1,1,10,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(10,23) = a0_Ahhh_HmHmc_00(unitarity_s,2,1,1,2,10,23) 
scatter_matrix1(10,24) = a0_Ahhh_HmHmc_00(unitarity_s,2,1,2,1,10,24) 
scatter_matrix1(10,25) = a0_Ahhh_HmHmc_00(unitarity_s,2,1,2,2,10,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(11,1) = a0_Ahhh_AhAh_00(unitarity_s,2,2,1,1,11,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(11,2) = a0_Ahhh_AhAh_00(unitarity_s,2,2,1,2,11,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(11,3) = a0_Ahhh_AhAh_00(unitarity_s,2,2,1,3,11,3) 
scatter_matrix1(11,4) = a0_Ahhh_AhAh_00(unitarity_s,2,2,2,2,11,4) 
scatter_matrix1(11,5) = a0_Ahhh_AhAh_00(unitarity_s,2,2,2,3,11,5) 
scatter_matrix1(11,6) = a0_Ahhh_AhAh_00(unitarity_s,2,2,3,3,11,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(11,7) = a0_Ahhh_Ahhh_00(unitarity_s,2,2,1,1,11,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(11,8) = a0_Ahhh_Ahhh_00(unitarity_s,2,2,1,2,11,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(11,9) = a0_Ahhh_Ahhh_00(unitarity_s,2,2,1,3,11,9) 
scatter_matrix1(11,10) = a0_Ahhh_Ahhh_00(unitarity_s,2,2,2,1,11,10) 
scatter_matrix1(11,11) = a0_Ahhh_Ahhh_00(unitarity_s,2,2,2,2,11,11) 
scatter_matrix1(11,12) = a0_Ahhh_Ahhh_00(unitarity_s,2,2,2,3,11,12) 
scatter_matrix1(11,13) = a0_Ahhh_Ahhh_00(unitarity_s,2,2,3,1,11,13) 
scatter_matrix1(11,14) = a0_Ahhh_Ahhh_00(unitarity_s,2,2,3,2,11,14) 
scatter_matrix1(11,15) = a0_Ahhh_Ahhh_00(unitarity_s,2,2,3,3,11,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(11,16) = a0_Ahhh_hhhh_00(unitarity_s,2,2,1,1,11,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(11,17) = a0_Ahhh_hhhh_00(unitarity_s,2,2,1,2,11,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(11,18) = a0_Ahhh_hhhh_00(unitarity_s,2,2,1,3,11,18) 
scatter_matrix1(11,19) = a0_Ahhh_hhhh_00(unitarity_s,2,2,2,2,11,19) 
scatter_matrix1(11,20) = a0_Ahhh_hhhh_00(unitarity_s,2,2,2,3,11,20) 
scatter_matrix1(11,21) = a0_Ahhh_hhhh_00(unitarity_s,2,2,3,3,11,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(11,22) = a0_Ahhh_HmHmc_00(unitarity_s,2,2,1,1,11,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(11,23) = a0_Ahhh_HmHmc_00(unitarity_s,2,2,1,2,11,23) 
scatter_matrix1(11,24) = a0_Ahhh_HmHmc_00(unitarity_s,2,2,2,1,11,24) 
scatter_matrix1(11,25) = a0_Ahhh_HmHmc_00(unitarity_s,2,2,2,2,11,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(12,1) = a0_Ahhh_AhAh_00(unitarity_s,2,3,1,1,12,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(12,2) = a0_Ahhh_AhAh_00(unitarity_s,2,3,1,2,12,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(12,3) = a0_Ahhh_AhAh_00(unitarity_s,2,3,1,3,12,3) 
scatter_matrix1(12,4) = a0_Ahhh_AhAh_00(unitarity_s,2,3,2,2,12,4) 
scatter_matrix1(12,5) = a0_Ahhh_AhAh_00(unitarity_s,2,3,2,3,12,5) 
scatter_matrix1(12,6) = a0_Ahhh_AhAh_00(unitarity_s,2,3,3,3,12,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(12,7) = a0_Ahhh_Ahhh_00(unitarity_s,2,3,1,1,12,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(12,8) = a0_Ahhh_Ahhh_00(unitarity_s,2,3,1,2,12,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(12,9) = a0_Ahhh_Ahhh_00(unitarity_s,2,3,1,3,12,9) 
scatter_matrix1(12,10) = a0_Ahhh_Ahhh_00(unitarity_s,2,3,2,1,12,10) 
scatter_matrix1(12,11) = a0_Ahhh_Ahhh_00(unitarity_s,2,3,2,2,12,11) 
scatter_matrix1(12,12) = a0_Ahhh_Ahhh_00(unitarity_s,2,3,2,3,12,12) 
scatter_matrix1(12,13) = a0_Ahhh_Ahhh_00(unitarity_s,2,3,3,1,12,13) 
scatter_matrix1(12,14) = a0_Ahhh_Ahhh_00(unitarity_s,2,3,3,2,12,14) 
scatter_matrix1(12,15) = a0_Ahhh_Ahhh_00(unitarity_s,2,3,3,3,12,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(12,16) = a0_Ahhh_hhhh_00(unitarity_s,2,3,1,1,12,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(12,17) = a0_Ahhh_hhhh_00(unitarity_s,2,3,1,2,12,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(12,18) = a0_Ahhh_hhhh_00(unitarity_s,2,3,1,3,12,18) 
scatter_matrix1(12,19) = a0_Ahhh_hhhh_00(unitarity_s,2,3,2,2,12,19) 
scatter_matrix1(12,20) = a0_Ahhh_hhhh_00(unitarity_s,2,3,2,3,12,20) 
scatter_matrix1(12,21) = a0_Ahhh_hhhh_00(unitarity_s,2,3,3,3,12,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(12,22) = a0_Ahhh_HmHmc_00(unitarity_s,2,3,1,1,12,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(12,23) = a0_Ahhh_HmHmc_00(unitarity_s,2,3,1,2,12,23) 
scatter_matrix1(12,24) = a0_Ahhh_HmHmc_00(unitarity_s,2,3,2,1,12,24) 
scatter_matrix1(12,25) = a0_Ahhh_HmHmc_00(unitarity_s,2,3,2,2,12,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(13,1) = a0_Ahhh_AhAh_00(unitarity_s,3,1,1,1,13,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(13,2) = a0_Ahhh_AhAh_00(unitarity_s,3,1,1,2,13,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(13,3) = a0_Ahhh_AhAh_00(unitarity_s,3,1,1,3,13,3) 
scatter_matrix1(13,4) = a0_Ahhh_AhAh_00(unitarity_s,3,1,2,2,13,4) 
scatter_matrix1(13,5) = a0_Ahhh_AhAh_00(unitarity_s,3,1,2,3,13,5) 
scatter_matrix1(13,6) = a0_Ahhh_AhAh_00(unitarity_s,3,1,3,3,13,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(13,7) = a0_Ahhh_Ahhh_00(unitarity_s,3,1,1,1,13,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(13,8) = a0_Ahhh_Ahhh_00(unitarity_s,3,1,1,2,13,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(13,9) = a0_Ahhh_Ahhh_00(unitarity_s,3,1,1,3,13,9) 
scatter_matrix1(13,10) = a0_Ahhh_Ahhh_00(unitarity_s,3,1,2,1,13,10) 
scatter_matrix1(13,11) = a0_Ahhh_Ahhh_00(unitarity_s,3,1,2,2,13,11) 
scatter_matrix1(13,12) = a0_Ahhh_Ahhh_00(unitarity_s,3,1,2,3,13,12) 
scatter_matrix1(13,13) = a0_Ahhh_Ahhh_00(unitarity_s,3,1,3,1,13,13) 
scatter_matrix1(13,14) = a0_Ahhh_Ahhh_00(unitarity_s,3,1,3,2,13,14) 
scatter_matrix1(13,15) = a0_Ahhh_Ahhh_00(unitarity_s,3,1,3,3,13,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(13,16) = a0_Ahhh_hhhh_00(unitarity_s,3,1,1,1,13,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(13,17) = a0_Ahhh_hhhh_00(unitarity_s,3,1,1,2,13,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(13,18) = a0_Ahhh_hhhh_00(unitarity_s,3,1,1,3,13,18) 
scatter_matrix1(13,19) = a0_Ahhh_hhhh_00(unitarity_s,3,1,2,2,13,19) 
scatter_matrix1(13,20) = a0_Ahhh_hhhh_00(unitarity_s,3,1,2,3,13,20) 
scatter_matrix1(13,21) = a0_Ahhh_hhhh_00(unitarity_s,3,1,3,3,13,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(13,22) = a0_Ahhh_HmHmc_00(unitarity_s,3,1,1,1,13,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(13,23) = a0_Ahhh_HmHmc_00(unitarity_s,3,1,1,2,13,23) 
scatter_matrix1(13,24) = a0_Ahhh_HmHmc_00(unitarity_s,3,1,2,1,13,24) 
scatter_matrix1(13,25) = a0_Ahhh_HmHmc_00(unitarity_s,3,1,2,2,13,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(14,1) = a0_Ahhh_AhAh_00(unitarity_s,3,2,1,1,14,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(14,2) = a0_Ahhh_AhAh_00(unitarity_s,3,2,1,2,14,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(14,3) = a0_Ahhh_AhAh_00(unitarity_s,3,2,1,3,14,3) 
scatter_matrix1(14,4) = a0_Ahhh_AhAh_00(unitarity_s,3,2,2,2,14,4) 
scatter_matrix1(14,5) = a0_Ahhh_AhAh_00(unitarity_s,3,2,2,3,14,5) 
scatter_matrix1(14,6) = a0_Ahhh_AhAh_00(unitarity_s,3,2,3,3,14,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(14,7) = a0_Ahhh_Ahhh_00(unitarity_s,3,2,1,1,14,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(14,8) = a0_Ahhh_Ahhh_00(unitarity_s,3,2,1,2,14,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(14,9) = a0_Ahhh_Ahhh_00(unitarity_s,3,2,1,3,14,9) 
scatter_matrix1(14,10) = a0_Ahhh_Ahhh_00(unitarity_s,3,2,2,1,14,10) 
scatter_matrix1(14,11) = a0_Ahhh_Ahhh_00(unitarity_s,3,2,2,2,14,11) 
scatter_matrix1(14,12) = a0_Ahhh_Ahhh_00(unitarity_s,3,2,2,3,14,12) 
scatter_matrix1(14,13) = a0_Ahhh_Ahhh_00(unitarity_s,3,2,3,1,14,13) 
scatter_matrix1(14,14) = a0_Ahhh_Ahhh_00(unitarity_s,3,2,3,2,14,14) 
scatter_matrix1(14,15) = a0_Ahhh_Ahhh_00(unitarity_s,3,2,3,3,14,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(14,16) = a0_Ahhh_hhhh_00(unitarity_s,3,2,1,1,14,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(14,17) = a0_Ahhh_hhhh_00(unitarity_s,3,2,1,2,14,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(14,18) = a0_Ahhh_hhhh_00(unitarity_s,3,2,1,3,14,18) 
scatter_matrix1(14,19) = a0_Ahhh_hhhh_00(unitarity_s,3,2,2,2,14,19) 
scatter_matrix1(14,20) = a0_Ahhh_hhhh_00(unitarity_s,3,2,2,3,14,20) 
scatter_matrix1(14,21) = a0_Ahhh_hhhh_00(unitarity_s,3,2,3,3,14,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(14,22) = a0_Ahhh_HmHmc_00(unitarity_s,3,2,1,1,14,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(14,23) = a0_Ahhh_HmHmc_00(unitarity_s,3,2,1,2,14,23) 
scatter_matrix1(14,24) = a0_Ahhh_HmHmc_00(unitarity_s,3,2,2,1,14,24) 
scatter_matrix1(14,25) = a0_Ahhh_HmHmc_00(unitarity_s,3,2,2,2,14,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(15,1) = a0_Ahhh_AhAh_00(unitarity_s,3,3,1,1,15,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(15,2) = a0_Ahhh_AhAh_00(unitarity_s,3,3,1,2,15,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(15,3) = a0_Ahhh_AhAh_00(unitarity_s,3,3,1,3,15,3) 
scatter_matrix1(15,4) = a0_Ahhh_AhAh_00(unitarity_s,3,3,2,2,15,4) 
scatter_matrix1(15,5) = a0_Ahhh_AhAh_00(unitarity_s,3,3,2,3,15,5) 
scatter_matrix1(15,6) = a0_Ahhh_AhAh_00(unitarity_s,3,3,3,3,15,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(15,7) = a0_Ahhh_Ahhh_00(unitarity_s,3,3,1,1,15,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(15,8) = a0_Ahhh_Ahhh_00(unitarity_s,3,3,1,2,15,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(15,9) = a0_Ahhh_Ahhh_00(unitarity_s,3,3,1,3,15,9) 
scatter_matrix1(15,10) = a0_Ahhh_Ahhh_00(unitarity_s,3,3,2,1,15,10) 
scatter_matrix1(15,11) = a0_Ahhh_Ahhh_00(unitarity_s,3,3,2,2,15,11) 
scatter_matrix1(15,12) = a0_Ahhh_Ahhh_00(unitarity_s,3,3,2,3,15,12) 
scatter_matrix1(15,13) = a0_Ahhh_Ahhh_00(unitarity_s,3,3,3,1,15,13) 
scatter_matrix1(15,14) = a0_Ahhh_Ahhh_00(unitarity_s,3,3,3,2,15,14) 
scatter_matrix1(15,15) = a0_Ahhh_Ahhh_00(unitarity_s,3,3,3,3,15,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(15,16) = a0_Ahhh_hhhh_00(unitarity_s,3,3,1,1,15,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(15,17) = a0_Ahhh_hhhh_00(unitarity_s,3,3,1,2,15,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(15,18) = a0_Ahhh_hhhh_00(unitarity_s,3,3,1,3,15,18) 
scatter_matrix1(15,19) = a0_Ahhh_hhhh_00(unitarity_s,3,3,2,2,15,19) 
scatter_matrix1(15,20) = a0_Ahhh_hhhh_00(unitarity_s,3,3,2,3,15,20) 
scatter_matrix1(15,21) = a0_Ahhh_hhhh_00(unitarity_s,3,3,3,3,15,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(15,22) = a0_Ahhh_HmHmc_00(unitarity_s,3,3,1,1,15,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(15,23) = a0_Ahhh_HmHmc_00(unitarity_s,3,3,1,2,15,23) 
scatter_matrix1(15,24) = a0_Ahhh_HmHmc_00(unitarity_s,3,3,2,1,15,24) 
scatter_matrix1(15,25) = a0_Ahhh_HmHmc_00(unitarity_s,3,3,2,2,15,25) 
scatter_matrix1(16,1) = a0_hhhh_AhAh_00(unitarity_s,1,1,1,1,16,1) 
scatter_matrix1(16,2) = a0_hhhh_AhAh_00(unitarity_s,1,1,1,2,16,2) 
scatter_matrix1(16,3) = a0_hhhh_AhAh_00(unitarity_s,1,1,1,3,16,3) 
scatter_matrix1(16,4) = a0_hhhh_AhAh_00(unitarity_s,1,1,2,2,16,4) 
scatter_matrix1(16,5) = a0_hhhh_AhAh_00(unitarity_s,1,1,2,3,16,5) 
scatter_matrix1(16,6) = a0_hhhh_AhAh_00(unitarity_s,1,1,3,3,16,6) 
scatter_matrix1(16,7) = a0_hhhh_Ahhh_00(unitarity_s,1,1,1,1,16,7) 
scatter_matrix1(16,8) = a0_hhhh_Ahhh_00(unitarity_s,1,1,1,2,16,8) 
scatter_matrix1(16,9) = a0_hhhh_Ahhh_00(unitarity_s,1,1,1,3,16,9) 
scatter_matrix1(16,10) = a0_hhhh_Ahhh_00(unitarity_s,1,1,2,1,16,10) 
scatter_matrix1(16,11) = a0_hhhh_Ahhh_00(unitarity_s,1,1,2,2,16,11) 
scatter_matrix1(16,12) = a0_hhhh_Ahhh_00(unitarity_s,1,1,2,3,16,12) 
scatter_matrix1(16,13) = a0_hhhh_Ahhh_00(unitarity_s,1,1,3,1,16,13) 
scatter_matrix1(16,14) = a0_hhhh_Ahhh_00(unitarity_s,1,1,3,2,16,14) 
scatter_matrix1(16,15) = a0_hhhh_Ahhh_00(unitarity_s,1,1,3,3,16,15) 
scatter_matrix1(16,16) = a0_hhhh_hhhh_00(unitarity_s,1,1,1,1,16,16) 
scatter_matrix1(16,17) = a0_hhhh_hhhh_00(unitarity_s,1,1,1,2,16,17) 
scatter_matrix1(16,18) = a0_hhhh_hhhh_00(unitarity_s,1,1,1,3,16,18) 
scatter_matrix1(16,19) = a0_hhhh_hhhh_00(unitarity_s,1,1,2,2,16,19) 
scatter_matrix1(16,20) = a0_hhhh_hhhh_00(unitarity_s,1,1,2,3,16,20) 
scatter_matrix1(16,21) = a0_hhhh_hhhh_00(unitarity_s,1,1,3,3,16,21) 
scatter_matrix1(16,22) = a0_hhhh_HmHmc_00(unitarity_s,1,1,1,1,16,22) 
scatter_matrix1(16,23) = a0_hhhh_HmHmc_00(unitarity_s,1,1,1,2,16,23) 
scatter_matrix1(16,24) = a0_hhhh_HmHmc_00(unitarity_s,1,1,2,1,16,24) 
scatter_matrix1(16,25) = a0_hhhh_HmHmc_00(unitarity_s,1,1,2,2,16,25) 
scatter_matrix1(17,1) = a0_hhhh_AhAh_00(unitarity_s,1,2,1,1,17,1) 
scatter_matrix1(17,2) = a0_hhhh_AhAh_00(unitarity_s,1,2,1,2,17,2) 
scatter_matrix1(17,3) = a0_hhhh_AhAh_00(unitarity_s,1,2,1,3,17,3) 
scatter_matrix1(17,4) = a0_hhhh_AhAh_00(unitarity_s,1,2,2,2,17,4) 
scatter_matrix1(17,5) = a0_hhhh_AhAh_00(unitarity_s,1,2,2,3,17,5) 
scatter_matrix1(17,6) = a0_hhhh_AhAh_00(unitarity_s,1,2,3,3,17,6) 
scatter_matrix1(17,7) = a0_hhhh_Ahhh_00(unitarity_s,1,2,1,1,17,7) 
scatter_matrix1(17,8) = a0_hhhh_Ahhh_00(unitarity_s,1,2,1,2,17,8) 
scatter_matrix1(17,9) = a0_hhhh_Ahhh_00(unitarity_s,1,2,1,3,17,9) 
scatter_matrix1(17,10) = a0_hhhh_Ahhh_00(unitarity_s,1,2,2,1,17,10) 
scatter_matrix1(17,11) = a0_hhhh_Ahhh_00(unitarity_s,1,2,2,2,17,11) 
scatter_matrix1(17,12) = a0_hhhh_Ahhh_00(unitarity_s,1,2,2,3,17,12) 
scatter_matrix1(17,13) = a0_hhhh_Ahhh_00(unitarity_s,1,2,3,1,17,13) 
scatter_matrix1(17,14) = a0_hhhh_Ahhh_00(unitarity_s,1,2,3,2,17,14) 
scatter_matrix1(17,15) = a0_hhhh_Ahhh_00(unitarity_s,1,2,3,3,17,15) 
scatter_matrix1(17,16) = a0_hhhh_hhhh_00(unitarity_s,1,2,1,1,17,16) 
scatter_matrix1(17,17) = a0_hhhh_hhhh_00(unitarity_s,1,2,1,2,17,17) 
scatter_matrix1(17,18) = a0_hhhh_hhhh_00(unitarity_s,1,2,1,3,17,18) 
scatter_matrix1(17,19) = a0_hhhh_hhhh_00(unitarity_s,1,2,2,2,17,19) 
scatter_matrix1(17,20) = a0_hhhh_hhhh_00(unitarity_s,1,2,2,3,17,20) 
scatter_matrix1(17,21) = a0_hhhh_hhhh_00(unitarity_s,1,2,3,3,17,21) 
scatter_matrix1(17,22) = a0_hhhh_HmHmc_00(unitarity_s,1,2,1,1,17,22) 
scatter_matrix1(17,23) = a0_hhhh_HmHmc_00(unitarity_s,1,2,1,2,17,23) 
scatter_matrix1(17,24) = a0_hhhh_HmHmc_00(unitarity_s,1,2,2,1,17,24) 
scatter_matrix1(17,25) = a0_hhhh_HmHmc_00(unitarity_s,1,2,2,2,17,25) 
scatter_matrix1(18,1) = a0_hhhh_AhAh_00(unitarity_s,1,3,1,1,18,1) 
scatter_matrix1(18,2) = a0_hhhh_AhAh_00(unitarity_s,1,3,1,2,18,2) 
scatter_matrix1(18,3) = a0_hhhh_AhAh_00(unitarity_s,1,3,1,3,18,3) 
scatter_matrix1(18,4) = a0_hhhh_AhAh_00(unitarity_s,1,3,2,2,18,4) 
scatter_matrix1(18,5) = a0_hhhh_AhAh_00(unitarity_s,1,3,2,3,18,5) 
scatter_matrix1(18,6) = a0_hhhh_AhAh_00(unitarity_s,1,3,3,3,18,6) 
scatter_matrix1(18,7) = a0_hhhh_Ahhh_00(unitarity_s,1,3,1,1,18,7) 
scatter_matrix1(18,8) = a0_hhhh_Ahhh_00(unitarity_s,1,3,1,2,18,8) 
scatter_matrix1(18,9) = a0_hhhh_Ahhh_00(unitarity_s,1,3,1,3,18,9) 
scatter_matrix1(18,10) = a0_hhhh_Ahhh_00(unitarity_s,1,3,2,1,18,10) 
scatter_matrix1(18,11) = a0_hhhh_Ahhh_00(unitarity_s,1,3,2,2,18,11) 
scatter_matrix1(18,12) = a0_hhhh_Ahhh_00(unitarity_s,1,3,2,3,18,12) 
scatter_matrix1(18,13) = a0_hhhh_Ahhh_00(unitarity_s,1,3,3,1,18,13) 
scatter_matrix1(18,14) = a0_hhhh_Ahhh_00(unitarity_s,1,3,3,2,18,14) 
scatter_matrix1(18,15) = a0_hhhh_Ahhh_00(unitarity_s,1,3,3,3,18,15) 
scatter_matrix1(18,16) = a0_hhhh_hhhh_00(unitarity_s,1,3,1,1,18,16) 
scatter_matrix1(18,17) = a0_hhhh_hhhh_00(unitarity_s,1,3,1,2,18,17) 
scatter_matrix1(18,18) = a0_hhhh_hhhh_00(unitarity_s,1,3,1,3,18,18) 
scatter_matrix1(18,19) = a0_hhhh_hhhh_00(unitarity_s,1,3,2,2,18,19) 
scatter_matrix1(18,20) = a0_hhhh_hhhh_00(unitarity_s,1,3,2,3,18,20) 
scatter_matrix1(18,21) = a0_hhhh_hhhh_00(unitarity_s,1,3,3,3,18,21) 
scatter_matrix1(18,22) = a0_hhhh_HmHmc_00(unitarity_s,1,3,1,1,18,22) 
scatter_matrix1(18,23) = a0_hhhh_HmHmc_00(unitarity_s,1,3,1,2,18,23) 
scatter_matrix1(18,24) = a0_hhhh_HmHmc_00(unitarity_s,1,3,2,1,18,24) 
scatter_matrix1(18,25) = a0_hhhh_HmHmc_00(unitarity_s,1,3,2,2,18,25) 
scatter_matrix1(19,1) = a0_hhhh_AhAh_00(unitarity_s,2,2,1,1,19,1) 
scatter_matrix1(19,2) = a0_hhhh_AhAh_00(unitarity_s,2,2,1,2,19,2) 
scatter_matrix1(19,3) = a0_hhhh_AhAh_00(unitarity_s,2,2,1,3,19,3) 
scatter_matrix1(19,4) = a0_hhhh_AhAh_00(unitarity_s,2,2,2,2,19,4) 
scatter_matrix1(19,5) = a0_hhhh_AhAh_00(unitarity_s,2,2,2,3,19,5) 
scatter_matrix1(19,6) = a0_hhhh_AhAh_00(unitarity_s,2,2,3,3,19,6) 
scatter_matrix1(19,7) = a0_hhhh_Ahhh_00(unitarity_s,2,2,1,1,19,7) 
scatter_matrix1(19,8) = a0_hhhh_Ahhh_00(unitarity_s,2,2,1,2,19,8) 
scatter_matrix1(19,9) = a0_hhhh_Ahhh_00(unitarity_s,2,2,1,3,19,9) 
scatter_matrix1(19,10) = a0_hhhh_Ahhh_00(unitarity_s,2,2,2,1,19,10) 
scatter_matrix1(19,11) = a0_hhhh_Ahhh_00(unitarity_s,2,2,2,2,19,11) 
scatter_matrix1(19,12) = a0_hhhh_Ahhh_00(unitarity_s,2,2,2,3,19,12) 
scatter_matrix1(19,13) = a0_hhhh_Ahhh_00(unitarity_s,2,2,3,1,19,13) 
scatter_matrix1(19,14) = a0_hhhh_Ahhh_00(unitarity_s,2,2,3,2,19,14) 
scatter_matrix1(19,15) = a0_hhhh_Ahhh_00(unitarity_s,2,2,3,3,19,15) 
scatter_matrix1(19,16) = a0_hhhh_hhhh_00(unitarity_s,2,2,1,1,19,16) 
scatter_matrix1(19,17) = a0_hhhh_hhhh_00(unitarity_s,2,2,1,2,19,17) 
scatter_matrix1(19,18) = a0_hhhh_hhhh_00(unitarity_s,2,2,1,3,19,18) 
scatter_matrix1(19,19) = a0_hhhh_hhhh_00(unitarity_s,2,2,2,2,19,19) 
scatter_matrix1(19,20) = a0_hhhh_hhhh_00(unitarity_s,2,2,2,3,19,20) 
scatter_matrix1(19,21) = a0_hhhh_hhhh_00(unitarity_s,2,2,3,3,19,21) 
scatter_matrix1(19,22) = a0_hhhh_HmHmc_00(unitarity_s,2,2,1,1,19,22) 
scatter_matrix1(19,23) = a0_hhhh_HmHmc_00(unitarity_s,2,2,1,2,19,23) 
scatter_matrix1(19,24) = a0_hhhh_HmHmc_00(unitarity_s,2,2,2,1,19,24) 
scatter_matrix1(19,25) = a0_hhhh_HmHmc_00(unitarity_s,2,2,2,2,19,25) 
scatter_matrix1(20,1) = a0_hhhh_AhAh_00(unitarity_s,2,3,1,1,20,1) 
scatter_matrix1(20,2) = a0_hhhh_AhAh_00(unitarity_s,2,3,1,2,20,2) 
scatter_matrix1(20,3) = a0_hhhh_AhAh_00(unitarity_s,2,3,1,3,20,3) 
scatter_matrix1(20,4) = a0_hhhh_AhAh_00(unitarity_s,2,3,2,2,20,4) 
scatter_matrix1(20,5) = a0_hhhh_AhAh_00(unitarity_s,2,3,2,3,20,5) 
scatter_matrix1(20,6) = a0_hhhh_AhAh_00(unitarity_s,2,3,3,3,20,6) 
scatter_matrix1(20,7) = a0_hhhh_Ahhh_00(unitarity_s,2,3,1,1,20,7) 
scatter_matrix1(20,8) = a0_hhhh_Ahhh_00(unitarity_s,2,3,1,2,20,8) 
scatter_matrix1(20,9) = a0_hhhh_Ahhh_00(unitarity_s,2,3,1,3,20,9) 
scatter_matrix1(20,10) = a0_hhhh_Ahhh_00(unitarity_s,2,3,2,1,20,10) 
scatter_matrix1(20,11) = a0_hhhh_Ahhh_00(unitarity_s,2,3,2,2,20,11) 
scatter_matrix1(20,12) = a0_hhhh_Ahhh_00(unitarity_s,2,3,2,3,20,12) 
scatter_matrix1(20,13) = a0_hhhh_Ahhh_00(unitarity_s,2,3,3,1,20,13) 
scatter_matrix1(20,14) = a0_hhhh_Ahhh_00(unitarity_s,2,3,3,2,20,14) 
scatter_matrix1(20,15) = a0_hhhh_Ahhh_00(unitarity_s,2,3,3,3,20,15) 
scatter_matrix1(20,16) = a0_hhhh_hhhh_00(unitarity_s,2,3,1,1,20,16) 
scatter_matrix1(20,17) = a0_hhhh_hhhh_00(unitarity_s,2,3,1,2,20,17) 
scatter_matrix1(20,18) = a0_hhhh_hhhh_00(unitarity_s,2,3,1,3,20,18) 
scatter_matrix1(20,19) = a0_hhhh_hhhh_00(unitarity_s,2,3,2,2,20,19) 
scatter_matrix1(20,20) = a0_hhhh_hhhh_00(unitarity_s,2,3,2,3,20,20) 
scatter_matrix1(20,21) = a0_hhhh_hhhh_00(unitarity_s,2,3,3,3,20,21) 
scatter_matrix1(20,22) = a0_hhhh_HmHmc_00(unitarity_s,2,3,1,1,20,22) 
scatter_matrix1(20,23) = a0_hhhh_HmHmc_00(unitarity_s,2,3,1,2,20,23) 
scatter_matrix1(20,24) = a0_hhhh_HmHmc_00(unitarity_s,2,3,2,1,20,24) 
scatter_matrix1(20,25) = a0_hhhh_HmHmc_00(unitarity_s,2,3,2,2,20,25) 
scatter_matrix1(21,1) = a0_hhhh_AhAh_00(unitarity_s,3,3,1,1,21,1) 
scatter_matrix1(21,2) = a0_hhhh_AhAh_00(unitarity_s,3,3,1,2,21,2) 
scatter_matrix1(21,3) = a0_hhhh_AhAh_00(unitarity_s,3,3,1,3,21,3) 
scatter_matrix1(21,4) = a0_hhhh_AhAh_00(unitarity_s,3,3,2,2,21,4) 
scatter_matrix1(21,5) = a0_hhhh_AhAh_00(unitarity_s,3,3,2,3,21,5) 
scatter_matrix1(21,6) = a0_hhhh_AhAh_00(unitarity_s,3,3,3,3,21,6) 
scatter_matrix1(21,7) = a0_hhhh_Ahhh_00(unitarity_s,3,3,1,1,21,7) 
scatter_matrix1(21,8) = a0_hhhh_Ahhh_00(unitarity_s,3,3,1,2,21,8) 
scatter_matrix1(21,9) = a0_hhhh_Ahhh_00(unitarity_s,3,3,1,3,21,9) 
scatter_matrix1(21,10) = a0_hhhh_Ahhh_00(unitarity_s,3,3,2,1,21,10) 
scatter_matrix1(21,11) = a0_hhhh_Ahhh_00(unitarity_s,3,3,2,2,21,11) 
scatter_matrix1(21,12) = a0_hhhh_Ahhh_00(unitarity_s,3,3,2,3,21,12) 
scatter_matrix1(21,13) = a0_hhhh_Ahhh_00(unitarity_s,3,3,3,1,21,13) 
scatter_matrix1(21,14) = a0_hhhh_Ahhh_00(unitarity_s,3,3,3,2,21,14) 
scatter_matrix1(21,15) = a0_hhhh_Ahhh_00(unitarity_s,3,3,3,3,21,15) 
scatter_matrix1(21,16) = a0_hhhh_hhhh_00(unitarity_s,3,3,1,1,21,16) 
scatter_matrix1(21,17) = a0_hhhh_hhhh_00(unitarity_s,3,3,1,2,21,17) 
scatter_matrix1(21,18) = a0_hhhh_hhhh_00(unitarity_s,3,3,1,3,21,18) 
scatter_matrix1(21,19) = a0_hhhh_hhhh_00(unitarity_s,3,3,2,2,21,19) 
scatter_matrix1(21,20) = a0_hhhh_hhhh_00(unitarity_s,3,3,2,3,21,20) 
scatter_matrix1(21,21) = a0_hhhh_hhhh_00(unitarity_s,3,3,3,3,21,21) 
scatter_matrix1(21,22) = a0_hhhh_HmHmc_00(unitarity_s,3,3,1,1,21,22) 
scatter_matrix1(21,23) = a0_hhhh_HmHmc_00(unitarity_s,3,3,1,2,21,23) 
scatter_matrix1(21,24) = a0_hhhh_HmHmc_00(unitarity_s,3,3,2,1,21,24) 
scatter_matrix1(21,25) = a0_hhhh_HmHmc_00(unitarity_s,3,3,2,2,21,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,1) = a0_HmHmc_AhAh_00(unitarity_s,1,1,1,1,22,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,2) = a0_HmHmc_AhAh_00(unitarity_s,1,1,1,2,22,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,3) = a0_HmHmc_AhAh_00(unitarity_s,1,1,1,3,22,3) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,4) = a0_HmHmc_AhAh_00(unitarity_s,1,1,2,2,22,4) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,5) = a0_HmHmc_AhAh_00(unitarity_s,1,1,2,3,22,5) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,6) = a0_HmHmc_AhAh_00(unitarity_s,1,1,3,3,22,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,7) = a0_HmHmc_Ahhh_00(unitarity_s,1,1,1,1,22,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,8) = a0_HmHmc_Ahhh_00(unitarity_s,1,1,1,2,22,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,9) = a0_HmHmc_Ahhh_00(unitarity_s,1,1,1,3,22,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,10) = a0_HmHmc_Ahhh_00(unitarity_s,1,1,2,1,22,10) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,11) = a0_HmHmc_Ahhh_00(unitarity_s,1,1,2,2,22,11) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,12) = a0_HmHmc_Ahhh_00(unitarity_s,1,1,2,3,22,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,13) = a0_HmHmc_Ahhh_00(unitarity_s,1,1,3,1,22,13) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,14) = a0_HmHmc_Ahhh_00(unitarity_s,1,1,3,2,22,14) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,15) = a0_HmHmc_Ahhh_00(unitarity_s,1,1,3,3,22,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,16) = a0_HmHmc_hhhh_00(unitarity_s,1,1,1,1,22,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,17) = a0_HmHmc_hhhh_00(unitarity_s,1,1,1,2,22,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,18) = a0_HmHmc_hhhh_00(unitarity_s,1,1,1,3,22,18) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,19) = a0_HmHmc_hhhh_00(unitarity_s,1,1,2,2,22,19) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,20) = a0_HmHmc_hhhh_00(unitarity_s,1,1,2,3,22,20) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,21) = a0_HmHmc_hhhh_00(unitarity_s,1,1,3,3,22,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,22) = a0_HmHmc_HmHmc_00(unitarity_s,1,1,1,1,22,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,23) = a0_HmHmc_HmHmc_00(unitarity_s,1,1,1,2,22,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,24) = a0_HmHmc_HmHmc_00(unitarity_s,1,1,2,1,22,24) 
If (IncludeGoldstoneExternal) scatter_matrix1(22,25) = a0_HmHmc_HmHmc_00(unitarity_s,1,1,2,2,22,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,1) = a0_HmHmc_AhAh_00(unitarity_s,1,2,1,1,23,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,2) = a0_HmHmc_AhAh_00(unitarity_s,1,2,1,2,23,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,3) = a0_HmHmc_AhAh_00(unitarity_s,1,2,1,3,23,3) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,4) = a0_HmHmc_AhAh_00(unitarity_s,1,2,2,2,23,4) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,5) = a0_HmHmc_AhAh_00(unitarity_s,1,2,2,3,23,5) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,6) = a0_HmHmc_AhAh_00(unitarity_s,1,2,3,3,23,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,7) = a0_HmHmc_Ahhh_00(unitarity_s,1,2,1,1,23,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,8) = a0_HmHmc_Ahhh_00(unitarity_s,1,2,1,2,23,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,9) = a0_HmHmc_Ahhh_00(unitarity_s,1,2,1,3,23,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,10) = a0_HmHmc_Ahhh_00(unitarity_s,1,2,2,1,23,10) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,11) = a0_HmHmc_Ahhh_00(unitarity_s,1,2,2,2,23,11) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,12) = a0_HmHmc_Ahhh_00(unitarity_s,1,2,2,3,23,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,13) = a0_HmHmc_Ahhh_00(unitarity_s,1,2,3,1,23,13) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,14) = a0_HmHmc_Ahhh_00(unitarity_s,1,2,3,2,23,14) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,15) = a0_HmHmc_Ahhh_00(unitarity_s,1,2,3,3,23,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,16) = a0_HmHmc_hhhh_00(unitarity_s,1,2,1,1,23,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,17) = a0_HmHmc_hhhh_00(unitarity_s,1,2,1,2,23,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,18) = a0_HmHmc_hhhh_00(unitarity_s,1,2,1,3,23,18) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,19) = a0_HmHmc_hhhh_00(unitarity_s,1,2,2,2,23,19) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,20) = a0_HmHmc_hhhh_00(unitarity_s,1,2,2,3,23,20) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,21) = a0_HmHmc_hhhh_00(unitarity_s,1,2,3,3,23,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,22) = a0_HmHmc_HmHmc_00(unitarity_s,1,2,1,1,23,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,23) = a0_HmHmc_HmHmc_00(unitarity_s,1,2,1,2,23,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,24) = a0_HmHmc_HmHmc_00(unitarity_s,1,2,2,1,23,24) 
If (IncludeGoldstoneExternal) scatter_matrix1(23,25) = a0_HmHmc_HmHmc_00(unitarity_s,1,2,2,2,23,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,1) = a0_HmHmc_AhAh_00(unitarity_s,2,1,1,1,24,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,2) = a0_HmHmc_AhAh_00(unitarity_s,2,1,1,2,24,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,3) = a0_HmHmc_AhAh_00(unitarity_s,2,1,1,3,24,3) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,4) = a0_HmHmc_AhAh_00(unitarity_s,2,1,2,2,24,4) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,5) = a0_HmHmc_AhAh_00(unitarity_s,2,1,2,3,24,5) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,6) = a0_HmHmc_AhAh_00(unitarity_s,2,1,3,3,24,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,7) = a0_HmHmc_Ahhh_00(unitarity_s,2,1,1,1,24,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,8) = a0_HmHmc_Ahhh_00(unitarity_s,2,1,1,2,24,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,9) = a0_HmHmc_Ahhh_00(unitarity_s,2,1,1,3,24,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,10) = a0_HmHmc_Ahhh_00(unitarity_s,2,1,2,1,24,10) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,11) = a0_HmHmc_Ahhh_00(unitarity_s,2,1,2,2,24,11) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,12) = a0_HmHmc_Ahhh_00(unitarity_s,2,1,2,3,24,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,13) = a0_HmHmc_Ahhh_00(unitarity_s,2,1,3,1,24,13) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,14) = a0_HmHmc_Ahhh_00(unitarity_s,2,1,3,2,24,14) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,15) = a0_HmHmc_Ahhh_00(unitarity_s,2,1,3,3,24,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,16) = a0_HmHmc_hhhh_00(unitarity_s,2,1,1,1,24,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,17) = a0_HmHmc_hhhh_00(unitarity_s,2,1,1,2,24,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,18) = a0_HmHmc_hhhh_00(unitarity_s,2,1,1,3,24,18) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,19) = a0_HmHmc_hhhh_00(unitarity_s,2,1,2,2,24,19) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,20) = a0_HmHmc_hhhh_00(unitarity_s,2,1,2,3,24,20) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,21) = a0_HmHmc_hhhh_00(unitarity_s,2,1,3,3,24,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,22) = a0_HmHmc_HmHmc_00(unitarity_s,2,1,1,1,24,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,23) = a0_HmHmc_HmHmc_00(unitarity_s,2,1,1,2,24,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,24) = a0_HmHmc_HmHmc_00(unitarity_s,2,1,2,1,24,24) 
If (IncludeGoldstoneExternal) scatter_matrix1(24,25) = a0_HmHmc_HmHmc_00(unitarity_s,2,1,2,2,24,25) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,1) = a0_HmHmc_AhAh_00(unitarity_s,2,2,1,1,25,1) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,2) = a0_HmHmc_AhAh_00(unitarity_s,2,2,1,2,25,2) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,3) = a0_HmHmc_AhAh_00(unitarity_s,2,2,1,3,25,3) 
scatter_matrix1(25,4) = a0_HmHmc_AhAh_00(unitarity_s,2,2,2,2,25,4) 
scatter_matrix1(25,5) = a0_HmHmc_AhAh_00(unitarity_s,2,2,2,3,25,5) 
scatter_matrix1(25,6) = a0_HmHmc_AhAh_00(unitarity_s,2,2,3,3,25,6) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,7) = a0_HmHmc_Ahhh_00(unitarity_s,2,2,1,1,25,7) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,8) = a0_HmHmc_Ahhh_00(unitarity_s,2,2,1,2,25,8) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,9) = a0_HmHmc_Ahhh_00(unitarity_s,2,2,1,3,25,9) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,10) = a0_HmHmc_Ahhh_00(unitarity_s,2,2,2,1,25,10) 
scatter_matrix1(25,11) = a0_HmHmc_Ahhh_00(unitarity_s,2,2,2,2,25,11) 
scatter_matrix1(25,12) = a0_HmHmc_Ahhh_00(unitarity_s,2,2,2,3,25,12) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,13) = a0_HmHmc_Ahhh_00(unitarity_s,2,2,3,1,25,13) 
scatter_matrix1(25,14) = a0_HmHmc_Ahhh_00(unitarity_s,2,2,3,2,25,14) 
scatter_matrix1(25,15) = a0_HmHmc_Ahhh_00(unitarity_s,2,2,3,3,25,15) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,16) = a0_HmHmc_hhhh_00(unitarity_s,2,2,1,1,25,16) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,17) = a0_HmHmc_hhhh_00(unitarity_s,2,2,1,2,25,17) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,18) = a0_HmHmc_hhhh_00(unitarity_s,2,2,1,3,25,18) 
scatter_matrix1(25,19) = a0_HmHmc_hhhh_00(unitarity_s,2,2,2,2,25,19) 
scatter_matrix1(25,20) = a0_HmHmc_hhhh_00(unitarity_s,2,2,2,3,25,20) 
scatter_matrix1(25,21) = a0_HmHmc_hhhh_00(unitarity_s,2,2,3,3,25,21) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,22) = a0_HmHmc_HmHmc_00(unitarity_s,2,2,1,1,25,22) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,23) = a0_HmHmc_HmHmc_00(unitarity_s,2,2,1,2,25,23) 
If (IncludeGoldstoneExternal) scatter_matrix1(25,24) = a0_HmHmc_HmHmc_00(unitarity_s,2,2,2,1,25,24) 
scatter_matrix1(25,25) = a0_HmHmc_HmHmc_00(unitarity_s,2,2,2,2,25,25) 


Select CASE (TUcutLevel)  
CASE (2) 
  scatter_matrix1B = scatter_matrix1
Do i=1,25 
  If (RemoveTUpoles(i).eq.1) Then
   scatter_matrix1(i,:) = 0._dp 
   scatter_matrix1(:,i) = 0._dp 
    If (AddR) scatter_matrix1(i,i) = -1111._dp ! to get a fixed order of the eigenvalues 
   scatter_matrix1B(:,i) = 0._dp 
  Else 
   scatter_matrix1B(i,:) = 0._dp 
  End If 
End Do 
CASE (1) 
If ((Abs(max_element_removed)/MaxVal(Abs(scatter_matrix1))).gt.cut_elements) Then 
 ! Write(*,*)  (Abs(max_element_removed)/MaxVal(Abs(scatter_matrix1)))  
End if 
End Select 
If (.not.pole_present) Then 
Call EigenSystem(scatter_matrix1,eigenvalues_matrix1,rot_matrix1,ierr,test)
 If ((TUcutLevel.eq.2).and.(AddR)) Then ! Calculate 'R' 
  scatter_matrix1B = MatMul(scatter_matrix1B,Conjg(Transpose(rot_matrix1))) 
   Do i=1,25 
    If (eigenvalues_matrix1 (i).lt.-1000._dp) Then
     eigenvalues_matrix1(i) = 0._dp 
    Else 
     eigenvalues_matrix1(i) = sqrt(eigenvalues_matrix1(i)**2+  sum(scatter_matrix1B(i,:)**2) )
    End If
   End Do 
 End If 
scattering_eigenvalue_trilinears=MaxVal(Abs(eigenvalues_matrix1)) 
Else 
  scattering_eigenvalue_trilinears = 0._dp 
End if 
If (scattering_eigenvalue_trilinears.gt.max_scattering_eigenvalue_trilinears) Then 
   max_scattering_eigenvalue_trilinears=scattering_eigenvalue_trilinears 
   unitarity_s_best=sqrt(unitarity_s)
End if 
 
!! 2. sub-matrix  
Pole_Present = .false. 
max_element_removed = 0._dp 
RemoveTUpoles = 0 
scatter_matrix2=0._dp 
If (IncludeGoldstoneExternal) scatter_matrix2(1,1) = a0_AhHmc_AhHm_00(unitarity_s,1,1,1,1,1,1) 
If (IncludeGoldstoneExternal) scatter_matrix2(1,2) = a0_AhHmc_AhHm_00(unitarity_s,1,1,1,2,1,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(1,3) = a0_AhHmc_AhHm_00(unitarity_s,1,1,2,1,1,3) 
If (IncludeGoldstoneExternal) scatter_matrix2(1,4) = a0_AhHmc_AhHm_00(unitarity_s,1,1,2,2,1,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(1,5) = a0_AhHmc_AhHm_00(unitarity_s,1,1,3,1,1,5) 
If (IncludeGoldstoneExternal) scatter_matrix2(1,6) = a0_AhHmc_AhHm_00(unitarity_s,1,1,3,2,1,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(1,7) = a0_AhHmc_hhHm_00(unitarity_s,1,1,1,1,1,7) 
If (IncludeGoldstoneExternal) scatter_matrix2(1,8) = a0_AhHmc_hhHm_00(unitarity_s,1,1,1,2,1,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(1,9) = a0_AhHmc_hhHm_00(unitarity_s,1,1,2,1,1,9) 
If (IncludeGoldstoneExternal) scatter_matrix2(1,10) = a0_AhHmc_hhHm_00(unitarity_s,1,1,2,2,1,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(1,11) = a0_AhHmc_hhHm_00(unitarity_s,1,1,3,1,1,11) 
If (IncludeGoldstoneExternal) scatter_matrix2(1,12) = a0_AhHmc_hhHm_00(unitarity_s,1,1,3,2,1,12) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,1) = a0_AhHmc_AhHm_00(unitarity_s,1,2,1,1,2,1) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,2) = a0_AhHmc_AhHm_00(unitarity_s,1,2,1,2,2,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,3) = a0_AhHmc_AhHm_00(unitarity_s,1,2,2,1,2,3) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,4) = a0_AhHmc_AhHm_00(unitarity_s,1,2,2,2,2,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,5) = a0_AhHmc_AhHm_00(unitarity_s,1,2,3,1,2,5) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,6) = a0_AhHmc_AhHm_00(unitarity_s,1,2,3,2,2,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,7) = a0_AhHmc_hhHm_00(unitarity_s,1,2,1,1,2,7) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,8) = a0_AhHmc_hhHm_00(unitarity_s,1,2,1,2,2,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,9) = a0_AhHmc_hhHm_00(unitarity_s,1,2,2,1,2,9) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,10) = a0_AhHmc_hhHm_00(unitarity_s,1,2,2,2,2,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,11) = a0_AhHmc_hhHm_00(unitarity_s,1,2,3,1,2,11) 
If (IncludeGoldstoneExternal) scatter_matrix2(2,12) = a0_AhHmc_hhHm_00(unitarity_s,1,2,3,2,2,12) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,1) = a0_AhHmc_AhHm_00(unitarity_s,2,1,1,1,3,1) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,2) = a0_AhHmc_AhHm_00(unitarity_s,2,1,1,2,3,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,3) = a0_AhHmc_AhHm_00(unitarity_s,2,1,2,1,3,3) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,4) = a0_AhHmc_AhHm_00(unitarity_s,2,1,2,2,3,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,5) = a0_AhHmc_AhHm_00(unitarity_s,2,1,3,1,3,5) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,6) = a0_AhHmc_AhHm_00(unitarity_s,2,1,3,2,3,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,7) = a0_AhHmc_hhHm_00(unitarity_s,2,1,1,1,3,7) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,8) = a0_AhHmc_hhHm_00(unitarity_s,2,1,1,2,3,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,9) = a0_AhHmc_hhHm_00(unitarity_s,2,1,2,1,3,9) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,10) = a0_AhHmc_hhHm_00(unitarity_s,2,1,2,2,3,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,11) = a0_AhHmc_hhHm_00(unitarity_s,2,1,3,1,3,11) 
If (IncludeGoldstoneExternal) scatter_matrix2(3,12) = a0_AhHmc_hhHm_00(unitarity_s,2,1,3,2,3,12) 
If (IncludeGoldstoneExternal) scatter_matrix2(4,1) = a0_AhHmc_AhHm_00(unitarity_s,2,2,1,1,4,1) 
If (IncludeGoldstoneExternal) scatter_matrix2(4,2) = a0_AhHmc_AhHm_00(unitarity_s,2,2,1,2,4,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(4,3) = a0_AhHmc_AhHm_00(unitarity_s,2,2,2,1,4,3) 
scatter_matrix2(4,4) = a0_AhHmc_AhHm_00(unitarity_s,2,2,2,2,4,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(4,5) = a0_AhHmc_AhHm_00(unitarity_s,2,2,3,1,4,5) 
scatter_matrix2(4,6) = a0_AhHmc_AhHm_00(unitarity_s,2,2,3,2,4,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(4,7) = a0_AhHmc_hhHm_00(unitarity_s,2,2,1,1,4,7) 
If (IncludeGoldstoneExternal) scatter_matrix2(4,8) = a0_AhHmc_hhHm_00(unitarity_s,2,2,1,2,4,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(4,9) = a0_AhHmc_hhHm_00(unitarity_s,2,2,2,1,4,9) 
scatter_matrix2(4,10) = a0_AhHmc_hhHm_00(unitarity_s,2,2,2,2,4,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(4,11) = a0_AhHmc_hhHm_00(unitarity_s,2,2,3,1,4,11) 
scatter_matrix2(4,12) = a0_AhHmc_hhHm_00(unitarity_s,2,2,3,2,4,12) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,1) = a0_AhHmc_AhHm_00(unitarity_s,3,1,1,1,5,1) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,2) = a0_AhHmc_AhHm_00(unitarity_s,3,1,1,2,5,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,3) = a0_AhHmc_AhHm_00(unitarity_s,3,1,2,1,5,3) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,4) = a0_AhHmc_AhHm_00(unitarity_s,3,1,2,2,5,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,5) = a0_AhHmc_AhHm_00(unitarity_s,3,1,3,1,5,5) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,6) = a0_AhHmc_AhHm_00(unitarity_s,3,1,3,2,5,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,7) = a0_AhHmc_hhHm_00(unitarity_s,3,1,1,1,5,7) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,8) = a0_AhHmc_hhHm_00(unitarity_s,3,1,1,2,5,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,9) = a0_AhHmc_hhHm_00(unitarity_s,3,1,2,1,5,9) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,10) = a0_AhHmc_hhHm_00(unitarity_s,3,1,2,2,5,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,11) = a0_AhHmc_hhHm_00(unitarity_s,3,1,3,1,5,11) 
If (IncludeGoldstoneExternal) scatter_matrix2(5,12) = a0_AhHmc_hhHm_00(unitarity_s,3,1,3,2,5,12) 
If (IncludeGoldstoneExternal) scatter_matrix2(6,1) = a0_AhHmc_AhHm_00(unitarity_s,3,2,1,1,6,1) 
If (IncludeGoldstoneExternal) scatter_matrix2(6,2) = a0_AhHmc_AhHm_00(unitarity_s,3,2,1,2,6,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(6,3) = a0_AhHmc_AhHm_00(unitarity_s,3,2,2,1,6,3) 
scatter_matrix2(6,4) = a0_AhHmc_AhHm_00(unitarity_s,3,2,2,2,6,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(6,5) = a0_AhHmc_AhHm_00(unitarity_s,3,2,3,1,6,5) 
scatter_matrix2(6,6) = a0_AhHmc_AhHm_00(unitarity_s,3,2,3,2,6,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(6,7) = a0_AhHmc_hhHm_00(unitarity_s,3,2,1,1,6,7) 
If (IncludeGoldstoneExternal) scatter_matrix2(6,8) = a0_AhHmc_hhHm_00(unitarity_s,3,2,1,2,6,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(6,9) = a0_AhHmc_hhHm_00(unitarity_s,3,2,2,1,6,9) 
scatter_matrix2(6,10) = a0_AhHmc_hhHm_00(unitarity_s,3,2,2,2,6,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(6,11) = a0_AhHmc_hhHm_00(unitarity_s,3,2,3,1,6,11) 
scatter_matrix2(6,12) = a0_AhHmc_hhHm_00(unitarity_s,3,2,3,2,6,12) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,1) = a0_hhHmc_AhHm_00(unitarity_s,1,1,1,1,7,1) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,2) = a0_hhHmc_AhHm_00(unitarity_s,1,1,1,2,7,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,3) = a0_hhHmc_AhHm_00(unitarity_s,1,1,2,1,7,3) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,4) = a0_hhHmc_AhHm_00(unitarity_s,1,1,2,2,7,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,5) = a0_hhHmc_AhHm_00(unitarity_s,1,1,3,1,7,5) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,6) = a0_hhHmc_AhHm_00(unitarity_s,1,1,3,2,7,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,7) = a0_hhHmc_hhHm_00(unitarity_s,1,1,1,1,7,7) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,8) = a0_hhHmc_hhHm_00(unitarity_s,1,1,1,2,7,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,9) = a0_hhHmc_hhHm_00(unitarity_s,1,1,2,1,7,9) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,10) = a0_hhHmc_hhHm_00(unitarity_s,1,1,2,2,7,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,11) = a0_hhHmc_hhHm_00(unitarity_s,1,1,3,1,7,11) 
If (IncludeGoldstoneExternal) scatter_matrix2(7,12) = a0_hhHmc_hhHm_00(unitarity_s,1,1,3,2,7,12) 
If (IncludeGoldstoneExternal) scatter_matrix2(8,1) = a0_hhHmc_AhHm_00(unitarity_s,1,2,1,1,8,1) 
scatter_matrix2(8,2) = a0_hhHmc_AhHm_00(unitarity_s,1,2,1,2,8,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(8,3) = a0_hhHmc_AhHm_00(unitarity_s,1,2,2,1,8,3) 
scatter_matrix2(8,4) = a0_hhHmc_AhHm_00(unitarity_s,1,2,2,2,8,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(8,5) = a0_hhHmc_AhHm_00(unitarity_s,1,2,3,1,8,5) 
scatter_matrix2(8,6) = a0_hhHmc_AhHm_00(unitarity_s,1,2,3,2,8,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(8,7) = a0_hhHmc_hhHm_00(unitarity_s,1,2,1,1,8,7) 
scatter_matrix2(8,8) = a0_hhHmc_hhHm_00(unitarity_s,1,2,1,2,8,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(8,9) = a0_hhHmc_hhHm_00(unitarity_s,1,2,2,1,8,9) 
scatter_matrix2(8,10) = a0_hhHmc_hhHm_00(unitarity_s,1,2,2,2,8,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(8,11) = a0_hhHmc_hhHm_00(unitarity_s,1,2,3,1,8,11) 
scatter_matrix2(8,12) = a0_hhHmc_hhHm_00(unitarity_s,1,2,3,2,8,12) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,1) = a0_hhHmc_AhHm_00(unitarity_s,2,1,1,1,9,1) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,2) = a0_hhHmc_AhHm_00(unitarity_s,2,1,1,2,9,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,3) = a0_hhHmc_AhHm_00(unitarity_s,2,1,2,1,9,3) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,4) = a0_hhHmc_AhHm_00(unitarity_s,2,1,2,2,9,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,5) = a0_hhHmc_AhHm_00(unitarity_s,2,1,3,1,9,5) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,6) = a0_hhHmc_AhHm_00(unitarity_s,2,1,3,2,9,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,7) = a0_hhHmc_hhHm_00(unitarity_s,2,1,1,1,9,7) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,8) = a0_hhHmc_hhHm_00(unitarity_s,2,1,1,2,9,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,9) = a0_hhHmc_hhHm_00(unitarity_s,2,1,2,1,9,9) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,10) = a0_hhHmc_hhHm_00(unitarity_s,2,1,2,2,9,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,11) = a0_hhHmc_hhHm_00(unitarity_s,2,1,3,1,9,11) 
If (IncludeGoldstoneExternal) scatter_matrix2(9,12) = a0_hhHmc_hhHm_00(unitarity_s,2,1,3,2,9,12) 
If (IncludeGoldstoneExternal) scatter_matrix2(10,1) = a0_hhHmc_AhHm_00(unitarity_s,2,2,1,1,10,1) 
scatter_matrix2(10,2) = a0_hhHmc_AhHm_00(unitarity_s,2,2,1,2,10,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(10,3) = a0_hhHmc_AhHm_00(unitarity_s,2,2,2,1,10,3) 
scatter_matrix2(10,4) = a0_hhHmc_AhHm_00(unitarity_s,2,2,2,2,10,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(10,5) = a0_hhHmc_AhHm_00(unitarity_s,2,2,3,1,10,5) 
scatter_matrix2(10,6) = a0_hhHmc_AhHm_00(unitarity_s,2,2,3,2,10,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(10,7) = a0_hhHmc_hhHm_00(unitarity_s,2,2,1,1,10,7) 
scatter_matrix2(10,8) = a0_hhHmc_hhHm_00(unitarity_s,2,2,1,2,10,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(10,9) = a0_hhHmc_hhHm_00(unitarity_s,2,2,2,1,10,9) 
scatter_matrix2(10,10) = a0_hhHmc_hhHm_00(unitarity_s,2,2,2,2,10,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(10,11) = a0_hhHmc_hhHm_00(unitarity_s,2,2,3,1,10,11) 
scatter_matrix2(10,12) = a0_hhHmc_hhHm_00(unitarity_s,2,2,3,2,10,12) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,1) = a0_hhHmc_AhHm_00(unitarity_s,3,1,1,1,11,1) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,2) = a0_hhHmc_AhHm_00(unitarity_s,3,1,1,2,11,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,3) = a0_hhHmc_AhHm_00(unitarity_s,3,1,2,1,11,3) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,4) = a0_hhHmc_AhHm_00(unitarity_s,3,1,2,2,11,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,5) = a0_hhHmc_AhHm_00(unitarity_s,3,1,3,1,11,5) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,6) = a0_hhHmc_AhHm_00(unitarity_s,3,1,3,2,11,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,7) = a0_hhHmc_hhHm_00(unitarity_s,3,1,1,1,11,7) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,8) = a0_hhHmc_hhHm_00(unitarity_s,3,1,1,2,11,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,9) = a0_hhHmc_hhHm_00(unitarity_s,3,1,2,1,11,9) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,10) = a0_hhHmc_hhHm_00(unitarity_s,3,1,2,2,11,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,11) = a0_hhHmc_hhHm_00(unitarity_s,3,1,3,1,11,11) 
If (IncludeGoldstoneExternal) scatter_matrix2(11,12) = a0_hhHmc_hhHm_00(unitarity_s,3,1,3,2,11,12) 
If (IncludeGoldstoneExternal) scatter_matrix2(12,1) = a0_hhHmc_AhHm_00(unitarity_s,3,2,1,1,12,1) 
scatter_matrix2(12,2) = a0_hhHmc_AhHm_00(unitarity_s,3,2,1,2,12,2) 
If (IncludeGoldstoneExternal) scatter_matrix2(12,3) = a0_hhHmc_AhHm_00(unitarity_s,3,2,2,1,12,3) 
scatter_matrix2(12,4) = a0_hhHmc_AhHm_00(unitarity_s,3,2,2,2,12,4) 
If (IncludeGoldstoneExternal) scatter_matrix2(12,5) = a0_hhHmc_AhHm_00(unitarity_s,3,2,3,1,12,5) 
scatter_matrix2(12,6) = a0_hhHmc_AhHm_00(unitarity_s,3,2,3,2,12,6) 
If (IncludeGoldstoneExternal) scatter_matrix2(12,7) = a0_hhHmc_hhHm_00(unitarity_s,3,2,1,1,12,7) 
scatter_matrix2(12,8) = a0_hhHmc_hhHm_00(unitarity_s,3,2,1,2,12,8) 
If (IncludeGoldstoneExternal) scatter_matrix2(12,9) = a0_hhHmc_hhHm_00(unitarity_s,3,2,2,1,12,9) 
scatter_matrix2(12,10) = a0_hhHmc_hhHm_00(unitarity_s,3,2,2,2,12,10) 
If (IncludeGoldstoneExternal) scatter_matrix2(12,11) = a0_hhHmc_hhHm_00(unitarity_s,3,2,3,1,12,11) 
scatter_matrix2(12,12) = a0_hhHmc_hhHm_00(unitarity_s,3,2,3,2,12,12) 


Select CASE (TUcutLevel)  
CASE (2) 
  scatter_matrix2B = scatter_matrix2
Do i=1,12 
  If (RemoveTUpoles(i).eq.1) Then
   scatter_matrix2(i,:) = 0._dp 
   scatter_matrix2(:,i) = 0._dp 
    If (AddR) scatter_matrix2(i,i) = -1111._dp ! to get a fixed order of the eigenvalues 
   scatter_matrix2B(:,i) = 0._dp 
  Else 
   scatter_matrix2B(i,:) = 0._dp 
  End If 
End Do 
CASE (1) 
If ((Abs(max_element_removed)/MaxVal(Abs(scatter_matrix2))).gt.cut_elements) Then 
 ! Write(*,*)  (Abs(max_element_removed)/MaxVal(Abs(scatter_matrix2)))  
End if 
End Select 
If (.not.pole_present) Then 
Call EigenSystem(scatter_matrix2,eigenvalues_matrix2,rot_matrix2,ierr,test)
 If ((TUcutLevel.eq.2).and.(AddR)) Then ! Calculate 'R' 
  scatter_matrix2B = MatMul(scatter_matrix2B,Conjg(Transpose(rot_matrix2))) 
   Do i=1,12 
    If (eigenvalues_matrix2 (i).lt.-1000._dp) Then
     eigenvalues_matrix2(i) = 0._dp 
    Else 
     eigenvalues_matrix2(i) = sqrt(eigenvalues_matrix2(i)**2+  sum(scatter_matrix2B(i,:)**2) )
    End If
   End Do 
 End If 
scattering_eigenvalue_trilinears=MaxVal(Abs(eigenvalues_matrix2)) 
Else 
  scattering_eigenvalue_trilinears = 0._dp 
End if 
If (scattering_eigenvalue_trilinears.gt.max_scattering_eigenvalue_trilinears) Then 
   max_scattering_eigenvalue_trilinears=scattering_eigenvalue_trilinears 
   unitarity_s_best=sqrt(unitarity_s)
End if 
 
!! 3. sub-matrix  
Pole_Present = .false. 
max_element_removed = 0._dp 
RemoveTUpoles = 0 
scatter_matrix3=0._dp 
If (IncludeGoldstoneExternal) scatter_matrix3(1,1) = a0_HmcHmc_HmHm_00(unitarity_s,1,1,1,1,1,1) 
If (IncludeGoldstoneExternal) scatter_matrix3(1,2) = a0_HmcHmc_HmHm_00(unitarity_s,1,1,1,2,1,2) 
If (IncludeGoldstoneExternal) scatter_matrix3(1,3) = a0_HmcHmc_HmHm_00(unitarity_s,1,1,2,2,1,3) 
If (IncludeGoldstoneExternal) scatter_matrix3(2,1) = a0_HmcHmc_HmHm_00(unitarity_s,1,2,1,1,2,1) 
If (IncludeGoldstoneExternal) scatter_matrix3(2,2) = a0_HmcHmc_HmHm_00(unitarity_s,1,2,1,2,2,2) 
If (IncludeGoldstoneExternal) scatter_matrix3(2,3) = a0_HmcHmc_HmHm_00(unitarity_s,1,2,2,2,2,3) 
If (IncludeGoldstoneExternal) scatter_matrix3(3,1) = a0_HmcHmc_HmHm_00(unitarity_s,2,2,1,1,3,1) 
If (IncludeGoldstoneExternal) scatter_matrix3(3,2) = a0_HmcHmc_HmHm_00(unitarity_s,2,2,1,2,3,2) 
scatter_matrix3(3,3) = a0_HmcHmc_HmHm_00(unitarity_s,2,2,2,2,3,3) 


Select CASE (TUcutLevel)  
CASE (2) 
  scatter_matrix3B = scatter_matrix3
Do i=1,3 
  If (RemoveTUpoles(i).eq.1) Then
   scatter_matrix3(i,:) = 0._dp 
   scatter_matrix3(:,i) = 0._dp 
    If (AddR) scatter_matrix3(i,i) = -1111._dp ! to get a fixed order of the eigenvalues 
   scatter_matrix3B(:,i) = 0._dp 
  Else 
   scatter_matrix3B(i,:) = 0._dp 
  End If 
End Do 
CASE (1) 
If ((Abs(max_element_removed)/MaxVal(Abs(scatter_matrix3))).gt.cut_elements) Then 
 ! Write(*,*)  (Abs(max_element_removed)/MaxVal(Abs(scatter_matrix3)))  
End if 
End Select 
If (.not.pole_present) Then 
Call EigenSystem(scatter_matrix3,eigenvalues_matrix3,rot_matrix3,ierr,test)
 If ((TUcutLevel.eq.2).and.(AddR)) Then ! Calculate 'R' 
  scatter_matrix3B = MatMul(scatter_matrix3B,Conjg(Transpose(rot_matrix3))) 
   Do i=1,3 
    If (eigenvalues_matrix3 (i).lt.-1000._dp) Then
     eigenvalues_matrix3(i) = 0._dp 
    Else 
     eigenvalues_matrix3(i) = sqrt(eigenvalues_matrix3(i)**2+  sum(scatter_matrix3B(i,:)**2) )
    End If
   End Do 
 End If 
scattering_eigenvalue_trilinears=MaxVal(Abs(eigenvalues_matrix3)) 
Else 
  scattering_eigenvalue_trilinears = 0._dp 
End if 
If (scattering_eigenvalue_trilinears.gt.max_scattering_eigenvalue_trilinears) Then 
   max_scattering_eigenvalue_trilinears=scattering_eigenvalue_trilinears 
   unitarity_s_best=sqrt(unitarity_s)
End if 
 
End do 

If (max_scattering_eigenvalue_trilinears.gt.0.5_dp) TreeUnitarityTrilinear=0._dp 
 
! Write(*,*) (max_scattering_eigenvalue_trilinears) 
 


 Contains 

Complex(dp) Function a0_AhAh_AhAh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = MAh(i2)
m3 = MAh(i3)
m4 = MAh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhAhAhAh(i1,i2,i3,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i2,iprop)
unicpl2(1)=cplAhAhAh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i2,iprop)
unicpl2(1)=cplAhAhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i3,iprop)
unicpl2(1)=cplAhAhAh(i2,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i3,iprop)
unicpl2(1)=cplAhAhhh(i2,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i4,iprop)
unicpl2(1)=cplAhAhAh(i2,i3,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i4,iprop)
unicpl2(1)=cplAhAhhh(i2,i3,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp=amp/sqrt(2._dp) 
If (i3.eq.i4) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp_poles=amp_poles/sqrt(2._dp) 
If (i3.eq.i4) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_AhAh_AhAh_00

Complex(dp) Function a0_AhAh_Ahhh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = MAh(i2)
m3 = MAh(i3)
m4 = Mhh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhAhAhhh(i1,i2,i3,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i2,iprop)
unicpl2(1)=cplAhAhhh(i3,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i2,iprop)
unicpl2(1)=cplAhhhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i3,iprop)
unicpl2(1)=cplAhAhhh(i2,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i3,iprop)
unicpl2(1)=cplAhhhhh(i2,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i4)
unicpl2(1)=cplAhAhAh(i2,i3,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i4,iprop)
unicpl2(1)=cplAhAhhh(i2,i3,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_AhAh_Ahhh_00

Complex(dp) Function a0_AhAh_hhhh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = MAh(i2)
m3 = Mhh(i3)
m4 = Mhh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhAhhhhh(i1,i2,i3,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i2,iprop)
unicpl2(1)=cplAhhhhh(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i2,iprop)
unicpl2(1)=cplhhhhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i3)
unicpl2(1)=cplAhAhhh(i2,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i3,iprop)
unicpl2(1)=cplAhhhhh(i2,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i4)
unicpl2(1)=cplAhAhhh(i2,iprop,i3)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i4,iprop)
unicpl2(1)=cplAhhhhh(i2,i3,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp=amp/sqrt(2._dp) 
If (i3.eq.i4) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp_poles=amp_poles/sqrt(2._dp) 
If (i3.eq.i4) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_AhAh_hhhh_00

Complex(dp) Function a0_AhAh_HmHmc_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = MAh(i2)
m3 = MHm(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhAhHmcHm(i1,i2,i3,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i2,iprop)
unicpl2(1)=cplAhHmcHm(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i2,iprop)
unicpl2(1)=cplhhHmcHm(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,i3,iprop)
unicpl2(1)=cplAhHmcHm(i2,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,iprop,i4)
unicpl2(1)=cplAhHmcHm(i2,i3,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_AhAh_HmHmc_00

Complex(dp) Function a0_Ahhh_AhAh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = Mhh(i2)
m3 = MAh(i3)
m4 = MAh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhAhAhhh(i1,i3,i4,i2)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i2)
unicpl2(1)=cplAhAhAh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i2,iprop)
unicpl2(1)=cplAhAhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i3,iprop)
unicpl2(1)=cplAhAhhh(i4,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i3,iprop)
unicpl2(1)=cplAhhhhh(i4,i2,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i4,iprop)
unicpl2(1)=cplAhAhhh(i3,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i4,iprop)
unicpl2(1)=cplAhhhhh(i3,i2,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i3.eq.i4) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i3.eq.i4) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_Ahhh_AhAh_00

Complex(dp) Function a0_Ahhh_Ahhh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = Mhh(i2)
m3 = MAh(i3)
m4 = Mhh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhAhhhhh(i1,i3,i2,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i2)
unicpl2(1)=cplAhAhhh(i3,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i2,iprop)
unicpl2(1)=cplAhhhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i3,iprop)
unicpl2(1)=cplAhhhhh(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i3,iprop)
unicpl2(1)=cplhhhhhh(i2,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i4)
unicpl2(1)=cplAhAhhh(i3,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i4,iprop)
unicpl2(1)=cplAhhhhh(i3,i2,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_Ahhh_Ahhh_00

Complex(dp) Function a0_Ahhh_hhhh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = Mhh(i2)
m3 = Mhh(i3)
m4 = Mhh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhhhhhhh(i1,i2,i3,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i2)
unicpl2(1)=cplAhhhhh(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i2,iprop)
unicpl2(1)=cplhhhhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i3)
unicpl2(1)=cplAhhhhh(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i3,iprop)
unicpl2(1)=cplhhhhhh(i2,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i4)
unicpl2(1)=cplAhhhhh(iprop,i2,i3)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i4,iprop)
unicpl2(1)=cplhhhhhh(i2,i3,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i3.eq.i4) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i3.eq.i4) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_Ahhh_hhhh_00

Complex(dp) Function a0_Ahhh_HmHmc_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = Mhh(i2)
m3 = MHm(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhhhHmcHm(i1,i2,i3,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i2)
unicpl2(1)=cplAhHmcHm(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i2,iprop)
unicpl2(1)=cplhhHmcHm(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,i3,iprop)
unicpl2(1)=cplhhHmcHm(i2,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,iprop,i4)
unicpl2(1)=cplhhHmcHm(i2,i3,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_Ahhh_HmHmc_00

Complex(dp) Function a0_AhHm_AhHmc_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = MHm(i2)
m3 = MAh(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhAhHmcHm(i1,i3,i2,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,i2,iprop)
unicpl2(1)=cplAhHmcHm(i3,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MHm(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i3,iprop)
unicpl2(1)=cplAhHmcHm(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i3,iprop)
unicpl2(1)=cplhhHmcHm(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,iprop,i4)
unicpl2(1)=cplAhHmcHm(i3,i2,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_AhHm_AhHmc_00

Complex(dp) Function a0_AhHm_hhHmc_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = MHm(i2)
m3 = Mhh(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhhhHmcHm(i1,i3,i2,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,i2,iprop)
unicpl2(1)=cplhhHmcHm(i3,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MHm(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i3)
unicpl2(1)=cplAhHmcHm(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i3,iprop)
unicpl2(1)=cplhhHmcHm(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,iprop,i4)
unicpl2(1)=cplhhHmcHm(i3,i2,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_AhHm_hhHmc_00

Complex(dp) Function a0_AhHmc_AhHm_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = MHm(i2)
m3 = MAh(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhAhHmcHm(i1,i3,i4,i2)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,iprop,i2)
unicpl2(1)=cplAhHmcHm(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MHm(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhAh(i1,i3,iprop)
unicpl2(1)=cplAhHmcHm(iprop,i4,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,i3,iprop)
unicpl2(1)=cplhhHmcHm(iprop,i4,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,i4,iprop)
unicpl2(1)=cplAhHmcHm(i3,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_AhHmc_AhHm_00

Complex(dp) Function a0_AhHmc_hhHm_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MAh(i1)
m2 = MHm(i2)
m3 = Mhh(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhhhHmcHm(i1,i3,i4,i2)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,iprop,i2)
unicpl2(1)=cplhhHmcHm(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MHm(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i1,iprop,i3)
unicpl2(1)=cplAhHmcHm(iprop,i4,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i1,i3,iprop)
unicpl2(1)=cplhhHmcHm(iprop,i4,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i1,i4,iprop)
unicpl2(1)=cplhhHmcHm(i3,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_AhHmc_hhHm_00

Complex(dp) Function a0_hhhh_AhAh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = Mhh(i1)
m2 = Mhh(i2)
m3 = MAh(i3)
m4 = MAh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhAhhhhh(i3,i4,i1,i2)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(iprop,i1,i2)
unicpl2(1)=cplAhAhAh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhhhhh(i1,i2,iprop)
unicpl2(1)=cplAhAhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i3,iprop,i1)
unicpl2(1)=cplAhAhhh(i4,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i3,i1,iprop)
unicpl2(1)=cplAhhhhh(i4,i2,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i4,iprop,i1)
unicpl2(1)=cplAhAhhh(i3,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i4,i1,iprop)
unicpl2(1)=cplAhhhhh(i3,i2,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp=amp/sqrt(2._dp) 
If (i3.eq.i4) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp_poles=amp_poles/sqrt(2._dp) 
If (i3.eq.i4) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_hhhh_AhAh_00

Complex(dp) Function a0_hhhh_Ahhh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = Mhh(i1)
m2 = Mhh(i2)
m3 = MAh(i3)
m4 = Mhh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhhhhhhh(i3,i1,i2,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(iprop,i1,i2)
unicpl2(1)=cplAhAhhh(i3,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhhhhh(i1,i2,iprop)
unicpl2(1)=cplAhhhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i3,iprop,i1)
unicpl2(1)=cplAhhhhh(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i3,i1,iprop)
unicpl2(1)=cplhhhhhh(i2,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(iprop,i1,i4)
unicpl2(1)=cplAhAhhh(i3,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhhhhh(i1,i4,iprop)
unicpl2(1)=cplAhhhhh(i3,i2,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_hhhh_Ahhh_00

Complex(dp) Function a0_hhhh_hhhh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = Mhh(i1)
m2 = Mhh(i2)
m3 = Mhh(i3)
m4 = Mhh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplhhhhhhhh(i1,i2,i3,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(iprop,i1,i2)
unicpl2(1)=cplAhhhhh(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhhhhh(i1,i2,iprop)
unicpl2(1)=cplhhhhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(iprop,i1,i3)
unicpl2(1)=cplAhhhhh(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhhhhh(i1,i3,iprop)
unicpl2(1)=cplhhhhhh(i2,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(iprop,i1,i4)
unicpl2(1)=cplAhhhhh(iprop,i2,i3)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhhhhh(i1,i4,iprop)
unicpl2(1)=cplhhhhhh(i2,i3,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp=amp/sqrt(2._dp) 
If (i3.eq.i4) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp_poles=amp_poles/sqrt(2._dp) 
If (i3.eq.i4) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_hhhh_hhhh_00

Complex(dp) Function a0_hhhh_HmHmc_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = Mhh(i1)
m2 = Mhh(i2)
m3 = MHm(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplhhhhHmcHm(i1,i2,i3,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(iprop,i1,i2)
unicpl2(1)=cplAhHmcHm(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhhhhh(i1,i2,iprop)
unicpl2(1)=cplhhHmcHm(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i1,i3,iprop)
unicpl2(1)=cplhhHmcHm(i2,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i1,iprop,i4)
unicpl2(1)=cplhhHmcHm(i2,i3,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_hhhh_HmHmc_00

Complex(dp) Function a0_hhHm_AhHmc_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = Mhh(i1)
m2 = MHm(i2)
m3 = MAh(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhhhHmcHm(i3,i1,i2,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i1,i2,iprop)
unicpl2(1)=cplAhHmcHm(i3,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MHm(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i3,iprop,i1)
unicpl2(1)=cplAhHmcHm(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i3,i1,iprop)
unicpl2(1)=cplhhHmcHm(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i1,iprop,i4)
unicpl2(1)=cplAhHmcHm(i3,i2,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_hhHm_AhHmc_00

Complex(dp) Function a0_hhHm_hhHmc_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = Mhh(i1)
m2 = MHm(i2)
m3 = Mhh(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplhhhhHmcHm(i1,i3,i2,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i1,i2,iprop)
unicpl2(1)=cplhhHmcHm(i3,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MHm(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(iprop,i1,i3)
unicpl2(1)=cplAhHmcHm(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhhhhh(i1,i3,iprop)
unicpl2(1)=cplhhHmcHm(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i1,iprop,i4)
unicpl2(1)=cplhhHmcHm(i3,i2,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_hhHm_hhHmc_00

Complex(dp) Function a0_hhHmc_AhHm_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = Mhh(i1)
m2 = MHm(i2)
m3 = MAh(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhhhHmcHm(i3,i1,i4,i2)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i1,iprop,i2)
unicpl2(1)=cplAhHmcHm(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MHm(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhAhhh(i3,iprop,i1)
unicpl2(1)=cplAhHmcHm(iprop,i4,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(i3,i1,iprop)
unicpl2(1)=cplhhHmcHm(iprop,i4,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i1,i4,iprop)
unicpl2(1)=cplAhHmcHm(i3,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_hhHmc_AhHm_00

Complex(dp) Function a0_hhHmc_hhHm_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = Mhh(i1)
m2 = MHm(i2)
m3 = Mhh(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplhhhhHmcHm(i1,i3,i4,i2)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i1,iprop,i2)
unicpl2(1)=cplhhHmcHm(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MHm(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhhhhh(iprop,i1,i3)
unicpl2(1)=cplAhHmcHm(iprop,i4,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhhhhh(i1,i3,iprop)
unicpl2(1)=cplhhHmcHm(iprop,i4,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i1,i4,iprop)
unicpl2(1)=cplhhHmcHm(i3,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_hhHmc_hhHm_00

Complex(dp) Function a0_HmHm_HmcHmc_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MHm(i1)
m2 = MHm(i2)
m3 = MHm(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplHmHmcHmcHm(i1,i2,i3,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhHmcHm(iprop,i1,i3)
unicpl2(1)=cplAhHmcHm(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhHmcHm(iprop,i1,i3)
unicpl2(1)=cplhhHmcHm(iprop,i2,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhHmcHm(iprop,i1,i4)
unicpl2(1)=cplAhHmcHm(iprop,i2,i3)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhHmcHm(iprop,i1,i4)
unicpl2(1)=cplhhHmcHm(iprop,i2,i3)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp=amp/sqrt(2._dp) 
If (i3.eq.i4) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp_poles=amp_poles/sqrt(2._dp) 
If (i3.eq.i4) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_HmHm_HmcHmc_00

Complex(dp) Function a0_HmHmc_AhAh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MHm(i1)
m2 = MHm(i2)
m3 = MAh(i3)
m4 = MAh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhAhHmcHm(i3,i4,i1,i2)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhHmcHm(iprop,i1,i2)
unicpl2(1)=cplAhAhAh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhHmcHm(iprop,i1,i2)
unicpl2(1)=cplAhAhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i3,i1,iprop)
unicpl2(1)=cplAhHmcHm(i4,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i4,i1,iprop)
unicpl2(1)=cplAhHmcHm(i3,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i3.eq.i4) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i3.eq.i4) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_HmHmc_AhAh_00

Complex(dp) Function a0_HmHmc_Ahhh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MHm(i1)
m2 = MHm(i2)
m3 = MAh(i3)
m4 = Mhh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplAhhhHmcHm(i3,i4,i1,i2)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhHmcHm(iprop,i1,i2)
unicpl2(1)=cplAhAhhh(i3,iprop,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhHmcHm(iprop,i1,i2)
unicpl2(1)=cplAhhhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplAhHmcHm(i3,i1,iprop)
unicpl2(1)=cplhhHmcHm(i4,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i4,i1,iprop)
unicpl2(1)=cplAhHmcHm(i3,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_HmHmc_Ahhh_00

Complex(dp) Function a0_HmHmc_hhhh_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MHm(i1)
m2 = MHm(i2)
m3 = Mhh(i3)
m4 = Mhh(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplhhhhHmcHm(i3,i4,i1,i2)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhHmcHm(iprop,i1,i2)
unicpl2(1)=cplAhhhhh(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhHmcHm(iprop,i1,i2)
unicpl2(1)=cplhhhhhh(i3,i4,iprop)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i3,i1,iprop)
unicpl2(1)=cplhhHmcHm(i4,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,2
unicpl1(1)=cplhhHmcHm(i4,i1,iprop)
unicpl2(1)=cplhhHmcHm(i3,iprop,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MHm(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MHm(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MHm(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i3.eq.i4) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i3.eq.i4) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_HmHmc_hhhh_00

Complex(dp) Function a0_HmHmc_HmHmc_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MHm(i1)
m2 = MHm(i2)
m3 = MHm(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplHmHmcHmcHm(i1,i3,i2,i4)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhHmcHm(iprop,i1,i2)
unicpl2(1)=cplAhHmcHm(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/MAh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhHmcHm(iprop,i1,i2)
unicpl2(1)=cplhhHmcHm(iprop,i3,i4)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Schannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If (Abs(1-s/Mhh(iProp)**2).lt.CutSpole) Then 
 Pole_Present = .true. 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! T-Channel 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhHmcHm(iprop,i1,i4)
unicpl2(1)=cplAhHmcHm(iprop,i3,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhHmcHm(iprop,i1,i4)
unicpl2(1)=cplhhHmcHm(iprop,i3,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_HmHmc_HmHmc_00

Complex(dp) Function a0_HmcHmc_HmHm_00(s,i1,i2,i3,i4,ind1,ind2)  Result(amp)
Implicit None 
Integer, Intent(in) :: i1,i2,i3,i4,ind1,ind2 
Real(dp), Intent(in) :: s 
Integer :: iprop, istart,c1,c2,c2end 
Logical :: pole_s_channel=.False. 
Real(dp) :: m1,m2,m3,m4 
Complex(dp) :: tempamp2(1,1) 
Complex(dp) :: amp_poles 
Complex(dp) :: unicpl1(8),unicpl2(8) 
amp = 0._dp 
amp_poles = 0._dp 
m1 = MHm(i1)
m2 = MHm(i2)
m3 = MHm(i3)
m4 = MHm(i4)
If ((s.gt.1.01_dp*(m3+m4)**2).and.(s.gt.1.01_dp*(m1+m2)**2)) Then 


! Quartic 
unicpl1(1)=cplHmHmcHmcHm(i3,i4,i1,i2)
amp = amp +(-2._dp)*(unicpl1(1))


! S-Channel 


! T-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhHmcHm(iprop,i3,i1)
unicpl2(1)=cplAhHmcHm(iprop,i4,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhHmcHm(iprop,i3,i1)
unicpl2(1)=cplhhHmcHm(iprop,i4,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Tchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckTpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "T",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 


! U-Channel 
istart=1
If (.not.IncludeGoldstoneContributions) istart=2
Do iprop=istart,3
unicpl1(1)=cplAhHmcHm(iprop,i4,i1)
unicpl2(1)=cplAhHmcHm(iprop,i3,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,MAh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,MAh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,MAh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
istart=1
Do iprop=istart,3
unicpl1(1)=cplhhHmcHm(iprop,i4,i1)
unicpl2(1)=cplhhHmcHm(iprop,i3,i2)
tempamp2(1,1)= unicpl1(1)*unicpl2(1)*Uchannel(m1,m2,m3,m4,Mhh(iProp),s,(1._dp,0._dp),(1._dp,0._dp)) 
If  (((s.lt.(CheckUpole(m1**2,m2**2,m3**2,m4**2,Mhh(iProp)**2)))).and.(maxval(Abs(tempamp2)).gt.1.0E-10_dp)) Then 
! Write(*,*) "U",m1,m2,m3,m4,Mhh(iProp)  
Select Case (TUcutLevel) 
 Case (3) 
   Pole_Present = .True. 
 Case (2) 
  RemoveTUpoles(ind1) = 1 
  RemoveTUpoles(ind2) = 1 
 Case (1) 
  amp_poles  = 0._dp
 Case (0) 
  amp = amp + tempamp2(1,1)
End Select 
Else 
  amp = amp + tempamp2(1,1)
End if 
End Do 
End if 
amp = 0.5_dp*oo16pi*amp*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp=amp/sqrt(2._dp) 
If (i3.eq.i4) amp=amp/sqrt(2._dp) 
If (TUcutLevel.eq.1) Then 
 amp_poles = 0.5_dp*oo16pi*amp_poles*sqrt(sqrt(Kaehler(s,m1**2,m2**2)*Kaehler(s,m3**2,m4**2)))/s 
If (i1.eq.i2) amp_poles=amp_poles/sqrt(2._dp) 
If (i3.eq.i4) amp_poles=amp_poles/sqrt(2._dp) 
  If ((Abs(amp_poles)/Abs(amp)).gt.cut_amplitudes) Then 
   ! Write(*,*) "TU ratio", (Abs(amp_poles)/Abs(amp))  
   If (Abs(amp).gt.max_element_removed) max_element_removed = Abs(amp) 
   amp = 0._dp 
  End if 
End if 
End Function a0_HmcHmc_HmHm_00

End Subroutine ScatteringEigenvaluesWithTrilinears

Complex(dp) Function Kaehler(a,b,c) 
Implicit None 
Real(dp),Intent(in)::a,b,c
Kaehler = a**2-2._dp*a*(b+c)+(b-c)**2 
End Function Kaehler 
  
Real(dp) Function CheckTpole(m1,m2,m3,m4,mP) 
Implicit None 
Real(dp),Intent(in)::m1,m2,m3,m4,mP
Complex(dp):: res
res = (m2*m3 - m3*m4 + m2*mP + m3*mP + m4*mP - mP**2 + m1*(-m2 + m4 + mP) + Sqrt(m1**2 + (m3 - mP)**2 &
  &  - 2*m1*(m3 + mP))*Sqrt(m2**2 + (m4 - mP)**2 - 2*m2*(m4 + mP)))/(2.*mP)
If (res.ne.res) res=0._dp 
If (Aimag(res).gt.1._dp) Then 
 CheckTpole = 0._dp 
Else 
 CheckTpole = Real(1.05_dp*res,dp) 
End If 
End Function CheckTpole 
 
Real(dp) Function CheckUpole(m1,m2,m3,m4,mP) 
Implicit None 
Real(dp),Intent(in)::m1,m2,m3,m4,mP
Complex(dp) :: res
res =(m2*m4-m3*m4+m2*mP+m3*mP+m4*mP-mP**2+m1*(-m2+m3+mP)+Sqrt(m2**2+(m3-mP)**2 &
  &-2*m2*(m3+mP))*Sqrt(m1**2+(m4-mP)**2-2*m1*(m4+mP)))/(2.*mP)
If (res.ne.res) res=0._dp 
If (Aimag(res).gt.1._dp) Then 
 CheckUpole = 0._dp 
Else 
 CheckUpole = Real(1.05_dp*res,dp) 
End If 
End Function CheckUpole 
 
Complex(dp) Function Schannel(m1,m2,m3,m4,mP,s,c1,c2) 
Implicit None 
Real(dp),Intent(in)::m1,m2,m3,m4,mP,s
Complex(dp),Intent(in)::c1,c2
Schannel = 2._dp/(-mP**2+s) 
Schannel = c1*c2*Schannel 
End Function Schannel 
 
Complex(dp) Function Uchannel(m1r,m2r,m3r,m4r,mPr,s,c1,c2) 
Implicit None 
Real(dp),Intent(in)::m1r,m2r,m3r,m4r,mPr,s
Complex(dp),Intent(in)::c1,c2
Complex(dp)::m1,m2,m3,m4,mP 
m1=Cmplx(m1r,0._dp)
m2=Cmplx(m2r,0._dp)
m3=Cmplx(m3r,0._dp)
m4=Cmplx(m4r,0._dp)
mP=Cmplx(mPr,0._dp)
Uchannel = (2*s*Log(-(((m1 - m2)*(m1 + m2)*(m3 - m4)*(m3 + m4) + (m1**2 + m2**2 + m3**2 + m4**2 - 2*mP**2)*s & 
  & - s**2 + Sqrt((m1**4 + (m2**2 - s)**2 - 2*m1**2*(m2**2 + s))*(m3**4 + (m4**2 - s)**2 - 2*m3**2*(m4**2 + s))))/& 
  &((-m1 + m2)*(m1 + m2)*(m3 - m4)*(m3 + m4) - (m1**2 + m2**2 + m3**2 + m4**2 - 2*mP**2)*s + s**2 + & 
  & Sqrt((m1**4 + (m2**2 - s)**2 - 2*m1**2*(m2**2 + s))*(m3**4 + (m4**2 - s)**2 - 2*m3**2*(m4**2 + s)))))))/& 
 &Sqrt((m1**4 + (m2**2 - s)**2 - 2*m1**2*(m2**2 + s))*(m3**4 + (m4**2 - s)**2 - 2*m3**2*(m4**2 + s))) 
Uchannel = c1*c2*Uchannel 
End Function Uchannel 
  
Complex(dp) Function Tchannel(m1r,m2r,m3r,m4r,mPr,s,c1,c2) 
Implicit None 
Real(dp),Intent(in)::m1r,m2r,m3r,m4r,mPr,s
Complex(dp),Intent(in)::c1,c2
Complex(dp)::m1,m2,m3,m4,mP 
m1=Cmplx(m1r,0._dp)
m2=Cmplx(m2r,0._dp)
m3=Cmplx(m3r,0._dp)
m4=Cmplx(m4r,0._dp)
mP=Cmplx(mPr,0._dp)
Tchannel =(2*s*Log(((m1-m2)*(m1+m2)*(m3-m4)*(m3+m4)-(m1**2+m2**2+m3**2+m4**2-2*mP**2)*s+s**2& 
  & -Sqrt((m1**4+(m2**2-s)**2-2*m1**2*(m2**2+s))*(m3**4+(m4**2-s)**2-2*m3**2*(m4**2+s))))/& 
  & ((m1-m2)*(m1+m2)*(m3-m4)*(m3+m4)-(m1**2+m2**2+m3**2+m4**2-2*mP**2)*s+s**2+Sqrt((m1**4+(m2**2-s)**2& 
  & -2*m1**2*(m2**2+s))*(m3**4+(m4**2-s)**2-2*m3**2*(m4**2+s))))))/& 
  & Sqrt((m1**4+(m2**2-s)**2-2*m1**2*(m2**2+s))*(m3**4+(m4**2-s)**2-2*m3**2*(m4**2+s))) 
Tchannel = c1*c2*Tchannel 
End Function Tchannel 
  
End Module Unitarity_BGLNCS 
