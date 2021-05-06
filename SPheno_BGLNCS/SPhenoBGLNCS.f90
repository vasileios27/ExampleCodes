! -----------------------------------------------------------------------------
! This file was automatically created by SARAH version 4.14.3
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223
! (c) Florian Staub, 2013
! ------------------------------------------------------------------------------
! File created at 11:18 on 6.5.2021
! ----------------------------------------------------------------------


Program SPhenoBGLNCS

Use Control
Use InputOutput_BGLNCS
Use LoopFunctions
Use Settings
Use LowEnergy_BGLNCS
Use Mathematics
Use Model_Data_BGLNCS
Use Tadpoles_BGLNCS
 !Use StandardModel
Use Unitarity_BGLNCS
 Use Boundaries_BGLNCS
 Use HiggsCS_BGLNCS
Use TreeLevelMasses_BGLNCS
Use LoopMasses_BGLNCS

Use BranchingRatios_BGLNCS

Implicit None

Real(dp) :: epsI=0.00001_dp, deltaM = 0.000001_dp
Real(dp) :: mGut = -1._dp, ratioWoM = 0._dp
Integer :: kont, n_tot

real(dp) :: MFv_save(6)

Integer,Parameter :: p_max=100
Real(dp) :: Ecms(p_max),Pm(p_max),Pp(p_max), dt, tz, Qin, gSM(11)
Real(dp) :: vev, sinw2
Complex(dp) :: YdSM(3,3), YuSM(3,3), YeSM(3,3)
Real(dp) :: vSM, g1SM, g2SM, g3SM
Integer :: i1
Logical :: ISR(p_max)=.False.
Logical :: CalcTBD
Real(dp) :: Tpar,Spar,Upar,ae,amu,atau,EDMe,EDMmu,EDMtau,dRho,BrBsGamma,ratioBsGamma,             &
& BrDmunu,ratioDmunu,BrDsmunu,ratioDsmunu,BrDstaunu,ratioDstaunu,BrBmunu,ratioBmunu,     &
& BrBtaunu,ratioBtaunu,BrKmunu,ratioKmunu,RK,RKSM,muEgamma,tauEgamma,tauMuGamma,         &
& CRmuEAl,CRmuETi,CRmuESr,CRmuESb,CRmuEAu,CRmuEPb,BRmuTo3e,BRtauTo3e,BRtauTo3mu,         &
& BRtauToemumu,BRtauTomuee,BRtauToemumu2,BRtauTomuee2,BrZtoMuE,BrZtoTauE,BrZtoTauMu,     &
& BrhtoMuE,BrhtoTauE,BrhtoTauMu,DeltaMBs,ratioDeltaMBs,DeltaMBq,ratioDeltaMBq,           &
& BrTautoEPi,BrTautoEEta,BrTautoEEtap,BrTautoMuPi,BrTautoMuEta,BrTautoMuEtap,            &
& BrB0dEE,ratioB0dEE,BrB0sEE,ratioB0sEE,BrB0dMuMu,ratioB0dMuMu,BrB0sMuMu,ratioB0sMuMu,   &
& BrB0dTauTau,ratioB0dTauTau,BrB0sTauTau,ratioB0sTauTau,BrBtoSEE,ratioBtoSEE,            &
& BrBtoSMuMu,ratioBtoSMuMu,BrBtoKee,ratioBtoKee,BrBtoKmumu,ratioBtoKmumu,BrBtoSnunu,     &
& ratioBtoSnunu,BrBtoDnunu,ratioBtoDnunu,BrKptoPipnunu,ratioKptoPipnunu,BrKltoPinunu,    &
& ratioKltoPinunu,BrK0eMu,ratioK0eMu,DelMK,ratioDelMK,epsK,ratioepsK

Tpar = 0._dp
Spar = 0._dp
Upar = 0._dp
ae = 0._dp
amu = 0._dp
atau = 0._dp
EDMe = 0._dp
EDMmu = 0._dp
EDMtau = 0._dp
dRho = 0._dp
BrBsGamma = 0._dp
ratioBsGamma = 0._dp
BrDmunu = 0._dp
ratioDmunu = 0._dp
BrDsmunu = 0._dp
ratioDsmunu = 0._dp
BrDstaunu = 0._dp
ratioDstaunu = 0._dp
BrBmunu = 0._dp
ratioBmunu = 0._dp
BrBtaunu = 0._dp
ratioBtaunu = 0._dp
BrKmunu = 0._dp
ratioKmunu = 0._dp
RK = 0._dp
RKSM = 0._dp
muEgamma = 0._dp
tauEgamma = 0._dp
tauMuGamma = 0._dp
CRmuEAl = 0._dp
CRmuETi = 0._dp
CRmuESr = 0._dp
CRmuESb = 0._dp
CRmuEAu = 0._dp
CRmuEPb = 0._dp
BRmuTo3e = 0._dp
BRtauTo3e = 0._dp
BRtauTo3mu = 0._dp
BRtauToemumu = 0._dp
BRtauTomuee = 0._dp
BRtauToemumu2 = 0._dp
BRtauTomuee2 = 0._dp
BrZtoMuE = 0._dp
BrZtoTauE = 0._dp
BrZtoTauMu = 0._dp
BrhtoMuE = 0._dp
BrhtoTauE = 0._dp
BrhtoTauMu = 0._dp
DeltaMBs = 0._dp
ratioDeltaMBs = 0._dp
DeltaMBq = 0._dp
ratioDeltaMBq = 0._dp
BrTautoEPi = 0._dp
BrTautoEEta = 0._dp
BrTautoEEtap = 0._dp
BrTautoMuPi = 0._dp
BrTautoMuEta = 0._dp
BrTautoMuEtap = 0._dp
BrB0dEE = 0._dp
ratioB0dEE = 0._dp
BrB0sEE = 0._dp
ratioB0sEE = 0._dp
BrB0dMuMu = 0._dp
ratioB0dMuMu = 0._dp
BrB0sMuMu = 0._dp
ratioB0sMuMu = 0._dp
BrB0dTauTau = 0._dp
ratioB0dTauTau = 0._dp
BrB0sTauTau = 0._dp
ratioB0sTauTau = 0._dp
BrBtoSEE = 0._dp
ratioBtoSEE = 0._dp
BrBtoSMuMu = 0._dp
ratioBtoSMuMu = 0._dp
BrBtoKee = 0._dp
ratioBtoKee = 0._dp
BrBtoKmumu = 0._dp
ratioBtoKmumu = 0._dp
BrBtoSnunu = 0._dp
ratioBtoSnunu = 0._dp
BrBtoDnunu = 0._dp
ratioBtoDnunu = 0._dp
BrKptoPipnunu = 0._dp
ratioKptoPipnunu = 0._dp
BrKltoPinunu = 0._dp
ratioKltoPinunu = 0._dp
BrK0eMu = 0._dp
ratioK0eMu = 0._dp
DelMK = 0._dp
ratioDelMK = 0._dp
epsK = 0._dp
ratioepsK = 0._dp
Call get_command_argument(1,inputFileName)
If (len_trim(inputFileName)==0) Then
  inputFileName="LesHouches.in.BGLNCS"
Else
  inputFileName=trim(inputFileName)
End if
Call get_command_argument(2,outputFileName)
If (len_trim(outputFileName)==0) Then
  outputFileName="SPheno.spc.BGLNCS"
Else
  outputFileName=trim(outputFileName)
End if
g1SM = 0._dp
g2SM = 0._dp
g3SM = 0._dp
vSM = 0._dp
YdSM = 0._dp
YeSM = 0._dp
YuSM = 0._dp
Call Set_All_Parameters_0()

Qin = SetRenormalizationScale(1.6E2_dp**2)
kont = 0
delta_Mass = 0.0001_dp
CalcTBD = .false.
Call ReadingData(kont)

 HighScaleModel = "LOW"
If ((MatchingOrder.lt.-1).or.(MatchingOrder.gt.2)) Then
  If (HighScaleModel.Eq."LOW") Then
    If (.not.CalculateOneLoopMasses) Then
       MatchingOrder = -1
    Else
       MatchingOrder =  2
    End if
   Else
       MatchingOrder =  2
   End If
End If
Select Case(MatchingOrder)
 Case(0)
   OneLoopMatching = .false.
   TwoLoopMatching = .false.
   GuessTwoLoopMatchingBSM = .false.
 Case(1)
   OneLoopMatching = .true.
   TwoLoopMatching = .false.
   GuessTwoLoopMatchingBSM = .false.
 Case(2)
   OneLoopMatching = .true.
   TwoLoopMatching = .true.
   GuessTwoLoopMatchingBSM = .true.
End Select
If (MatchingOrder.eq.-1) Then
 ! Setting values
 v1 = v1IN
 v2 = v2IN
 v3 = v3IN
 g1 = g1IN
 g2 = g2IN
 g3 = g3IN
 Lam1 = Lam1IN
 Lam3 = Lam3IN
 Lam4 = Lam4IN
 Lam2 = Lam2IN
 Lam1Dash = Lam1DashIN
 Lam2Dash = Lam2DashIN
 Lam3Dash = Lam3DashIN
 Aa3 = Aa3IN
 Aa4 = Aa4IN
 Y1d11 = Y1d11IN
 Y1d12 = Y1d12IN
 Y1d13 = Y1d13IN
 Y1d21 = Y1d21IN
 Y1d22 = Y1d22IN
 Y1d23 = Y1d23IN
 Y2d31 = Y2d31IN
 Y2d32 = Y2d32IN
 Y2d33 = Y2d33IN
 Y1u11 = Y1u11IN
 Y1u12 = Y1u12IN
 Y1u21 = Y1u21IN
 Y1u22 = Y1u22IN
 Y2u33 = Y2u33IN
 Y1e11 = Y1e11IN
 Y1e22 = Y1e22IN
 Y1e21 = Y1e21IN
 Y2e33 = Y2e33IN
 Y1n11 = Y1n11IN
 Y1n22 = Y1n22IN
 Y1n12 = Y1n12IN
 Y2n33 = Y2n33IN
 C23 = C23IN
 C32 = C32IN
 BB11 = BB11IN
 Aa1 = Aa1IN
 Aa2 = Aa2IN
 Mu1 = Mu1IN
 Mu2 = Mu2IN
 MuDash = MuDashIN
 Mub = MubIN
 Mu3 = Mu3IN
 Lam1 = Lambda1Input
Lam2 = Lambda2Input
Lam3 = Lambda3Input
Lam4 = Lambda4Input
Lam1Dash = Lambda1DashInput
Lam2Dash = Lambda2DashInput
Lam3Dash = Lambda3DashInput
Mu3 = Mu3Input
Mub = MubInput
Aa1 = Aa1Input
Aa2 = Aa2Input
Aa3 = Aa3Input
Aa4 = Aa4Input


 ! Setting VEVs used for low energy constraints
 v1MZ = v1
 v2MZ = v2
 v3MZ = v3
    sinW2=1._dp-mW2/mZ2
   vSM=1/Sqrt((G_F*Sqrt(2._dp)))
   g1SM=sqrt(4*Pi*Alpha_MZ/(1-sinW2))
   g2SM=sqrt(4*Pi*Alpha_MZ/Sinw2 )
   g3SM=sqrt(AlphaS_MZ*4*Pi)
   Do i1=1,3
      YuSM(i1,i1)=sqrt(2._dp)*mf_u(i1)/vSM
      YeSM(i1,i1)=sqrt(2._dp)*mf_l(i1)/vSM
      YdSM(i1,i1)=sqrt(2._dp)*mf_d(i1)/vSM
    End Do
    If (GenerationMixing) YuSM = Matmul(Transpose(CKM),YuSM)


! Transpose Yukawas to fit SPheno conventions
YuSM= Transpose(YuSM)
YdSM= Transpose(YdSM)
YeSM= Transpose(YeSM)

 ! Setting Boundary conditions
 Call SetMatchingConditions(g1SM,g2SM,g3SM,YuSM,YdSM,YeSM,vSM,v1,v2,v3,g1,             &
& g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,              &
& Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,               &
& Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,              &
& Mu2,MuDash,Mub,Mu3,.False.)

Lam1 = Lambda1Input
Lam2 = Lambda2Input
Lam3 = Lambda3Input
Lam4 = Lambda4Input
Lam1Dash = Lambda1DashInput
Lam2Dash = Lambda2DashInput
Lam3Dash = Lambda3DashInput
Mu3 = Mu3Input
Mub = MubInput
Aa1 = Aa1Input
Aa2 = Aa2Input
Aa3 = Aa3Input
Aa4 = Aa4Input
Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            &
& Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,          &
& Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,               &
& C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,(/ ZeroC, ZeroC, ZeroC /))

Call OneLoopMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,             &
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,              &
& ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,               &
& Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,           &
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              &
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,kont)


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

Else
   If (GetMassUncertainty) Then
   ! Uncertainty from Y_top
 If ((CalculateOneLoopMasses).and.(CalculateTwoLoopHiggsMasses)) Then
OneLoopMatching = .true.
TwoLoopMatching = .false.
GuessTwoLoopMatchingBSM = .True.
Elseif ((CalculateOneLoopMasses).and.(.not.CalculateTwoLoopHiggsMasses)) Then
OneLoopMatching = .true.
TwoLoopMatching = .false.
GuessTwoLoopMatchingBSM = .false.
Else
OneLoopMatching = .true.
TwoLoopMatching = .false.
GuessTwoLoopMatchingBSM = .false.
End if
Call CalculateSpectrum(n_run,delta_mass,WriteOut,kont,MAh,MAh2,MFd,MFd2,              &
& MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,               &
& ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,              &
& Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,           &
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,               &
& Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,mGUT)

n_tot =1
mass_uncertainty_Yt(n_tot:n_tot+2) = Mhh! difference will be taken later
n_tot = n_tot + 3
mass_uncertainty_Yt(n_tot:n_tot+2) = MAh! difference will be taken later
n_tot = n_tot + 3
mass_uncertainty_Yt(n_tot:n_tot+1) = MHm! difference will be taken later
If ((CalculateOneLoopMasses).and.(CalculateTwoLoopHiggsMasses)) Then
OneLoopMatching = .true.
TwoLoopMatching = .true.
GuessTwoLoopMatchingBSM = .false.
Elseif ((CalculateOneLoopMasses).and.(.not.CalculateTwoLoopHiggsMasses)) Then
OneLoopMatching = .false.
TwoLoopMatching = .false.
GuessTwoLoopMatchingBSM = .false.
Else
OneLoopMatching = .false.
TwoLoopMatching = .false.
GuessTwoLoopMatchingBSM = .false.
End if
  End if
 Call CalculateSpectrum(n_run,delta_mass,WriteOut,kont,MAh,MAh2,MFd,MFd2,              &
& MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,               &
& ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,              &
& Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,           &
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,               &
& Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,mGUT)

  If (GetMassUncertainty) Then
 Call GetScaleUncertainty(delta_mass,WriteOut,kont,MAh,MAh2,MFd,MFd2,MFe,              &
& MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,               &
& ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,             &
& Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          &
& Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,               &
& Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,mass_uncertainty_Q)

  End if
 End If
 ! Save correct Higgs masses for calculation of L -> 3 L'
MhhL = Mhh
Mhh2L = MhhL**2
MAhL = MAh
MAh2L = MAhL**2

v = Sqrt(v1**2 + v2**2)
TW = ACos(Abs(ZZ(1,1)))
If ((L_BR).And.(kont.Eq.0)) Then
 Call CalculateBR(CalcTBD,ratioWoM,epsI,deltaM,kont,MAh,MAh2,MFd,MFd2,MFe,             &
& MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,               &
& ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,             &
& Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,          &
& Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,               &
& Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,gPFu,gTFu,               &
& BRFu,gPFe,gTFe,BRFe,gPFd,gTFd,BRFd,gPFv,gTFv,BRFv,gPhh,gThh,BRhh,gPAh,gTAh,            &
& BRAh,gPHm,gTHm,BRHm,gPVZ,gTVZ,BRVZ,gPVWm,gTVWm,BRVWm)

Call HiggsCrossSections(Mhh,ratioGG,ratioPP,rHB_S_VWm,rHB_S_VZ,rHB_S_S_Fu(:,3)        &
& ,CS_Higgs_LHC,kont)

Call HiggsCrossSections(MAh,ratioPGG,ratioPPP,0._dp*rHB_S_VWm,0._dp*rHB_S_VZ,         &
& rHB_P_S_Fu(:,3),CS_PHiggs_LHC,kont)

End If

 If (CalculateLowEnergy) then

MFv_save = MFv

Call CalculateLowEnergyConstraints(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,             &
& Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,             &
& Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,               &
& Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,Tpar,Spar,            &
& Upar,ae,amu,atau,EDMe,EDMmu,EDMtau,dRho,BrBsGamma,ratioBsGamma,BrDmunu,ratioDmunu,     &
& BrDsmunu,ratioDsmunu,BrDstaunu,ratioDstaunu,BrBmunu,ratioBmunu,BrBtaunu,               &
& ratioBtaunu,BrKmunu,ratioKmunu,RK,RKSM,muEgamma,tauEgamma,tauMuGamma,CRmuEAl,          &
& CRmuETi,CRmuESr,CRmuESb,CRmuEAu,CRmuEPb,BRmuTo3e,BRtauTo3e,BRtauTo3mu,BRtauToemumu,    &
& BRtauTomuee,BRtauToemumu2,BRtauTomuee2,BrZtoMuE,BrZtoTauE,BrZtoTauMu,BrhtoMuE,         &
& BrhtoTauE,BrhtoTauMu,DeltaMBs,ratioDeltaMBs,DeltaMBq,ratioDeltaMBq,BrTautoEPi,         &
& BrTautoEEta,BrTautoEEtap,BrTautoMuPi,BrTautoMuEta,BrTautoMuEtap,BrB0dEE,               &
& ratioB0dEE,BrB0sEE,ratioB0sEE,BrB0dMuMu,ratioB0dMuMu,BrB0sMuMu,ratioB0sMuMu,           &
& BrB0dTauTau,ratioB0dTauTau,BrB0sTauTau,ratioB0sTauTau,BrBtoSEE,ratioBtoSEE,            &
& BrBtoSMuMu,ratioBtoSMuMu,BrBtoKee,ratioBtoKee,BrBtoKmumu,ratioBtoKmumu,BrBtoSnunu,     &
& ratioBtoSnunu,BrBtoDnunu,ratioBtoDnunu,BrKptoPipnunu,ratioKptoPipnunu,BrKltoPinunu,    &
& ratioKltoPinunu,BrK0eMu,ratioK0eMu,DelMK,ratioDelMK,epsK,ratioepsK)

MVZ = mz
MVZ2 = mz2
MVWm = mW
MVWm2 = mW2
If (WriteParametersAtQ) Then
Call TreeMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,MHm,            &
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,ZZ,               &
& v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              &
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               &
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              &
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,GenerationMixing,kont)

End If

MFv = nuMasses

MFv = MFv_save
MFv2 = MFV**2

End if

If ((FoundIterativeSolution).or.(WriteOutputForNonConvergence)) Then
If (OutputForMO) Then
Call RunningFermionMasses(MFe,MFe2,MFd,MFd2,MFu,MFu2,v1,v2,v3,g1,g2,g3,               &
& Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,              &
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               &
& Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,         &
& Mub,Mu3,kont)

End if
If (TreeLevelUnitarityLimits) Then
Write(*,*) "Calculating unitarity constraints "
Call ScatteringEigenvalues(v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,            &
& Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,             &
& Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,               &
& Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,deltaM,kont)

End if
If (TrilinearUnitarity) Then
If (.not.TreeLevelUnitarityLimits) Write(*,*) "Calculating unitarity constraints "
Call ScatteringEigenvaluesWithTrilinears(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,              &
& MFu2,MFv,MFv2,Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,              &
& ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,              &
& Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,             &
& Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,               &
& Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,deltaM,kont)

End if
Write(*,*) "Writing output files"
Call LesHouches_Out(67,11,kont,MGUT,Tpar,Spar,Upar,ae,amu,atau,EDMe,EDMmu,            &
& EDMtau,dRho,BrBsGamma,ratioBsGamma,BrDmunu,ratioDmunu,BrDsmunu,ratioDsmunu,            &
& BrDstaunu,ratioDstaunu,BrBmunu,ratioBmunu,BrBtaunu,ratioBtaunu,BrKmunu,ratioKmunu,     &
& RK,RKSM,muEgamma,tauEgamma,tauMuGamma,CRmuEAl,CRmuETi,CRmuESr,CRmuESb,CRmuEAu,         &
& CRmuEPb,BRmuTo3e,BRtauTo3e,BRtauTo3mu,BRtauToemumu,BRtauTomuee,BRtauToemumu2,          &
& BRtauTomuee2,BrZtoMuE,BrZtoTauE,BrZtoTauMu,BrhtoMuE,BrhtoTauE,BrhtoTauMu,              &
& DeltaMBs,ratioDeltaMBs,DeltaMBq,ratioDeltaMBq,BrTautoEPi,BrTautoEEta,BrTautoEEtap,     &
& BrTautoMuPi,BrTautoMuEta,BrTautoMuEtap,BrB0dEE,ratioB0dEE,BrB0sEE,ratioB0sEE,          &
& BrB0dMuMu,ratioB0dMuMu,BrB0sMuMu,ratioB0sMuMu,BrB0dTauTau,ratioB0dTauTau,              &
& BrB0sTauTau,ratioB0sTauTau,BrBtoSEE,ratioBtoSEE,BrBtoSMuMu,ratioBtoSMuMu,              &
& BrBtoKee,ratioBtoKee,BrBtoKmumu,ratioBtoKmumu,BrBtoSnunu,ratioBtoSnunu,BrBtoDnunu,     &
& ratioBtoDnunu,BrKptoPipnunu,ratioKptoPipnunu,BrKltoPinunu,ratioKltoPinunu,             &
& BrK0eMu,ratioK0eMu,DelMK,ratioDelMK,epsK,ratioepsK,GenerationMixing)

End if
Write(*,*) "Finished!"
Contains

Subroutine CalculateSpectrum(n_run,delta,WriteOut,kont,MAh,MAh2,MFd,MFd2,             &
& MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,               &
& ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,              &
& Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,           &
& Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,               &
& Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,mGUT)

Implicit None
Integer, Intent(in) :: n_run
Integer, Intent(inout) :: kont
Logical, Intent(in) :: WriteOut
Real(dp), Intent(in) :: delta
Real(dp), Intent(inout) :: mGUT
Real(dp),Intent(inout) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp),Intent(inout) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             &
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               &
& Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mub,Mu3

Real(dp),Intent(inout) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(6),MFv2(6),           &
& Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp),Intent(inout) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),Vv(6,6),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp),Intent(inout) :: v1,v2,v3

kont = 0
Call FirstGuess(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,MHm,            &
& MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,ZZ,               &
& v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              &
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               &
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              &
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,kont)

!If (kont.ne.0) Call TerminateProgram

If (SPA_Convention) Call SetRGEScale(1.e3_dp**2)

If (.Not.UseFixedScale) Then
 Call SetRGEScale(160._dp**2)
End If
Call Match_and_Run(delta,MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,            &
& Mhh2,MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,               &
& ZP,ZW,ZZ,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,              &
& Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,               &
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              &
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,mGut,kont,WriteOut,n_run)

If (kont.ne.0) Then
 Write(*,*) "Error appeared in calculation of masses "

 Call TerminateProgram
End If

End Subroutine CalculateSpectrum



Subroutine ReadingData(kont)
Implicit None
Integer,Intent(out)::kont
Logical::file_exists
kont=-123456
Inquire(file=inputFileName,exist=file_exists)
If (file_exists) Then
kont=1
Call LesHouches_Input(kont,Ecms,Pm,Pp,ISR,F_GMSB)
LesHouches_Format= .True.
Else
Write(*,*)&
& "File ",inputFileName," does not exist"
Call TerminateProgram
End If
End Subroutine ReadingData


Subroutine GetScaleUncertainty(delta,WriteOut,kont,MAhinput,MAh2input,MFdinput,       &
& MFd2input,MFeinput,MFe2input,MFuinput,MFu2input,MFvinput,MFv2input,Mhhinput,           &
& Mhh2input,MHminput,MHm2input,MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,          &
& ZDRinput,ZERinput,ZURinput,vinput,ZDLinput,ZELinput,ZULinput,Vvinput,ZAinput,          &
& ZHinput,ZPinput,ZWinput,ZZinput,v1input,v2input,v3input,g1input,g2input,               &
& g3input,Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,           &
& Lam3Dashinput,Aa3input,Aa4input,Y1d11input,Y1d12input,Y1d13input,Y1d21input,           &
& Y1d22input,Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,Y1u12input,          &
& Y1u21input,Y1u22input,Y2u33input,Y1e11input,Y1e22input,Y1e21input,Y2e33input,          &
& Y1n11input,Y1n22input,Y1n12input,Y2n33input,C23input,C32input,BB11input,               &
& Aa1input,Aa2input,Mu1input,Mu2input,MuDashinput,Mubinput,Mu3input,mass_Qerror)

Implicit None
Integer, Intent(inout) :: kont
Logical, Intent(in) :: WriteOut
Real(dp), Intent(in) :: delta
Real(dp) :: mass_in(23), mass_new(23)
Real(dp), Intent(out) :: mass_Qerror(23)
Real(dp) :: gD(85), Q, Qsave, Qstep, Qt, g_SM(62), mh_SM
Integer :: i1, i2, iupdown, ntot
Real(dp),Intent(in) :: g1input,g2input,g3input,Mu1input,Mu2input,MuDashinput

Complex(dp),Intent(in) :: Lam1input,Lam3input,Lam4input,Lam2input,Lam1Dashinput,Lam2Dashinput,Lam3Dashinput,    &
& Aa3input,Aa4input,Y1d11input,Y1d12input,Y1d13input,Y1d21input,Y1d22input,              &
& Y1d23input,Y2d31input,Y2d32input,Y2d33input,Y1u11input,Y1u12input,Y1u21input,          &
& Y1u22input,Y2u33input,Y1e11input,Y1e22input,Y1e21input,Y2e33input,Y1n11input,          &
& Y1n22input,Y1n12input,Y2n33input,C23input,C32input,BB11input,Aa1input,Aa2input,        &
& Mubinput,Mu3input

Real(dp),Intent(in) :: MAhinput(3),MAh2input(3),MFdinput(3),MFd2input(3),MFeinput(3),MFe2input(3),           &
& MFuinput(3),MFu2input(3),MFvinput(6),MFv2input(6),Mhhinput(3),Mhh2input(3),            &
& MHminput(2),MHm2input(2),MVWminput,MVWm2input,MVZinput,MVZ2input,TWinput,              &
& vinput,ZAinput(3,3),ZHinput(3,3)

Complex(dp),Intent(in) :: ZDRinput(3,3),ZERinput(3,3),ZURinput(3,3),ZDLinput(3,3),ZELinput(3,3),ZULinput(3,3),  &
& Vvinput(6,6),ZPinput(2,2),ZWinput(2,2),ZZinput(2,2)

Real(dp),Intent(in) :: v1input,v2input,v3input

Real(dp) :: g1,g2,g3,Mu1,Mu2,MuDash

Complex(dp) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             &
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               &
& Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mub,Mu3

Real(dp) :: MAh(3),MAh2(3),MFd(3),MFd2(3),MFe(3),MFe2(3),MFu(3),MFu2(3),MFv(6),MFv2(6),           &
& Mhh(3),Mhh2(3),MHm(2),MHm2(2),MVWm,MVWm2,MVZ,MVZ2,TW,v,ZA(3,3),ZH(3,3)

Complex(dp) :: ZDR(3,3),ZER(3,3),ZUR(3,3),ZDL(3,3),ZEL(3,3),ZUL(3,3),Vv(6,6),ZP(2,2),ZW(2,2),ZZ(2,2)

Real(dp) :: v1,v2,v3

kont = 0
Write(*,*) "Check scale uncertainty"
n_tot =1
mass_in(n_tot:n_tot+2) = Mhhinput
n_tot = n_tot + 3
mass_in(n_tot:n_tot+2) = MAhinput
n_tot = n_tot + 3
mass_in(n_tot:n_tot+1) = MHminput
mass_Qerror = 0._dp
Qsave=sqrt(getRenormalizationScale())
Do iupdown=1,2
If (iupdown.eq.1) Then
  Qstep=Qsave/7._dp
Else
  Qstep=-0.5_dp*Qsave/7._dp
End if
Do i1=1,7
Q=Qsave+i1*Qstep
Qt = SetRenormalizationScale(Q**2)
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
Y1e22 = Y1e22input
Y1e21 = Y1e21input
Y2e33 = Y2e33input
Y1n11 = Y1n11input
Y1n22 = Y1n22input
Y1n12 = Y1n12input
Y2n33 = Y2n33input
C23 = C23input
C32 = C32input
BB11 = BB11input
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


 ! --- GUT normalize gauge couplings ---
g1 = Sqrt(5._dp/3._dp)*g1
! -----------------------

Call ParametersToG85(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,         &
& Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,             &
& Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,             &
& BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,gD)

If (iupdown.eq.1) Then
 tz=Log(Q/Qsave)
 dt=-tz/50._dp
 Call odeint(gD,85,0._dp,tz,0.1_dp*delta,dt,0._dp,rge85,kont)
Else
 tz=-Log(Q/Qsave)
 dt=tz/50._dp
 Call odeint(gD,85,tz,0._dp,0.1_dp*delta,dt,0._dp,rge85,kont)
End if
Call GToParameters85(gD,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,               &
& Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,          &
& Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,               &
& C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3)



 ! --- Remove GUT-normalization of gauge couplings ---
g1 = Sqrt(3._dp/5._dp)*g1
! -----------------------

Call SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,            &
& Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,          &
& Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,               &
& C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,(/ ZeroC, ZeroC, ZeroC /))

Call OneLoopMasses(MAh,MAh2,MFd,MFd2,MFe,MFe2,MFu,MFu2,MFv,MFv2,Mhh,Mhh2,             &
& MHm,MHm2,MVWm,MVWm2,MVZ,MVZ2,TW,ZDR,ZER,ZUR,v,ZDL,ZEL,ZUL,Vv,ZA,ZH,ZP,ZW,              &
& ZZ,v1,v2,v3,g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,               &
& Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,           &
& Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,              &
& Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,kont)

If (((Calculate_mh_within_SM).and.(Mhh(2).gt.300._dp)).OR.(Force_mh_within_SM))Then
g_SM=g_SM_save
tz=0.5_dp*Log(mZ2/Q**2)
dt=tz/100._dp
g_SM(1)=Sqrt(5._dp/3._dp)*g_SM(1)
Call odeint(g_SM,62,tz,0._dp,delta,dt,0._dp,rge62_SM,kont)
g_SM(1)=Sqrt(3._dp/5._dp)*g_SM(1)
Call Get_mh_pole_SM(g_SM,Q**2,delta,Mhh2(1),mh_SM)
Mhh2(1) = mh_SM**2
Mhh(1) = mh_SM
End if
n_tot =1
mass_new(n_tot:n_tot+2) = Mhh
n_tot = n_tot + 3
mass_new(n_tot:n_tot+2) = MAh
n_tot = n_tot + 3
mass_new(n_tot:n_tot+1) = MHm
  Do i2=1,23
    If (Abs(mass_new(i2)-mass_in(i2)).gt.mass_Qerror(i2)) mass_Qerror(i2) = Abs(mass_new(i2)-mass_in(i2))
  End Do
End Do
End Do
  Do i2=1,23
    mass_uncertainty_Yt(i2) = Abs(mass_uncertainty_Yt(i2)-mass_in(i2))
  End Do
If (kont.ne.0) Then
 Write(*,*) "Error appeared in check of scale uncertainty "

 Call TerminateProgram
End If

Qt = SetRenormalizationScale(Qsave**2)
End Subroutine GetScaleUncertainty



End Program SPhenoBGLNCS
