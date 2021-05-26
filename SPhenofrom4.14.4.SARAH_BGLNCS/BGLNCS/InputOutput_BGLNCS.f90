! ------------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.4 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223,
!           1405.1434, 1411.0675, 1503.03098, 1703.09237, 1706.05372, 1805.07306  
! (c) Florian Staub, Mark Goodsell and Werner Porod 2020  
! ------------------------------------------------------------------------------  
! File created at 6:48 on 26.5.2021   
! ----------------------------------------------------------------------  
 
 
Module InputOutput_BGLNCS 
 
Use Control 
Use Settings 
!Use Experiment 
Use Model_Data_BGLNCS 
Use LoopFunctions 
Use AddLoopFunctions 
Use StandardModel 
Use LoopCouplings_BGLNCS 
 
Logical,Save::LesHouches_Format
Character(len=8),Save,Private::versionSARAH="4.14.4"
Integer,Private::i_cpv=0
Integer,Save,Private::in_kont(2)
Logical,Save::Add_Rparity= .False. 
Logical,Save::Write_HiggsBounds= .False. 
Logical,Save::Write_HiggsBounds5= .False. 
Character(len=40),Private::sp_info

Logical,Private::l_RP_Pythia= .False. 
Logical,Save,Private::Use_Flavour_States= .False. 
Real(dp),Save,Private::BrMin=1.e-4_dp 
Real(dp),Save,Private::SigMin=1.e-4_dp 
Character(len=120)::inputFileName,outputFileName 
Contains 
 
Subroutine LesHouches_Input(kont, Ecms, Pm, Pp, l_ISR, Fgmsb) 
 
Implicit None 
Integer, Intent(out) :: kont
Real(dp), Intent(out) :: Fgmsb, Ecms(:), Pm(:), Pp(:)
Logical, Intent(out) :: l_ISR(:)
Character(len=80) :: read_line
Integer :: i_mod=-1, i_sm=-1, i_par=-1, set_mod_par(25)=-1 &
& , i1, p_max, p_act, i_sp, i_model=-1, i_particles=-1
Real(dp) :: wert, Abs_Mu2, cosb2, cos2b, sinb2, RG0(3,3) &
 & , mat_D(3,3), R2(2,2), s12,s13,s23,c12,c13,c23
Logical :: check, calc_ferm, check_alpha(2)
Complex(dp) :: lam_vS
Logical, Save :: l_open = .True. 
 
Iname = Iname + 1
NameOfUnit(Iname) = "LesHouches_Input" 

check_alpha = .False. ! used to check consistency of alpha(mZ) calculation
in_kont = 0

Call InitializeStandardModel 
Call InitializeLoopFunctions 
 
i_mod = -1
i_sm = -1
i_par = -1
set_mod_par = -1 

ErrorLevel = -1
GenerationMixing=.False.
If (l_open) Then
   Open(ErrCan,file="Messages.out",status="unknown")
   Open(11,file="SPheno.out",status="unknown")
   l_open = .False.
End If 

Call Set_All_Parameters_0()

lam_vs = 0._dp
sp_info = " "
HighScaleModel="SARAH_Generated_Model" 
TwoLoopRGE = .True.
Fgmsb = 1.e12_dp
m32 = 1.e20_dp 
 
kont = 0
Open(99,file=inputFileName,status="old",err=200)
 
Do 
  Read(99,"(a80)",End=200,err=200) read_line 
  If (read_line(1:1).Eq."#") Cycle 
  If (read_line.Eq." ") Cycle 
  Call PutUpperCase(read_line) 
  If (read_line(1:5).Eq."BLOCK") Then  
    If (read_line(7:12).Eq."MODSEL") Then  
      kont = 0  
     Call Read_MODSEL(99,i_particles,i_model,i_cpv,kont)  
 CKMcomplex = CKM 
 If (i_cpv.Eq.0) Then 
 s12=lam_wolf 
 s23=s12**2*A_wolf 
 s13=s23*lam_wolf*Sqrt(eta_wolf**2+rho_wolf**2) 
 c12=Sqrt(1._dp-s12*s12) 
 c23=Sqrt(1._dp-s23*s23) 
 c13=Sqrt(1._dp-s13*s13) 
 CKM(1,1)=c12*c13 
 CKM(1,2)=s12*c13 
 CKM(1,3)=s13 
 CKM(2,1)=-s12*c23-c12*s23*s13 
 CKM(2,2)=c12*c23-s12*s23*s13 
 CKM(2,3)=s23*c13 
 CKM(3,1)=s12*s23-c12*c23*s13 
 CKM(3,2)=-c12*s23-s12*c23*s13 
 CKM(3,3)=c23*c13 
 End If 
    Else If (read_line(7:14).Eq."SMINPUTS") Then  
     Call Read_SMinput(99)  
    Else If (read_line(7:12).Eq."VCKMIN") Then  
     Call Read_CKM(99,i_cpv)  
    Else If (read_line(7:12).Eq."FCONST") Then  
     Call Read_FCONST(99)  
    Else If (read_line(7:11).Eq."FMASS") Then  
     Call Read_FMASS(99)  
    Else If (read_line(7:11).Eq."FLIFE") Then  
     Call Read_FLIFE(99)  
    Else If (read_line(7:17).Eq."SPHENOINPUT") Then  
     Call Read_SPhenoInput(99)  
    Else If (read_line(7:18).Eq."DECAYOPTIONS") Then  
     Call Read_DecayOptions(99)  
    Else If (read_line(7:12).Eq."MINPAR") Then  
     Call Read_MINPAR(99,0,i_model,set_mod_par,kont)  
    Else If (read_line(7:14).Eq."IMMINPAR") Then  
       If (i_cpv.Lt.2) Then 
       Call Warn_CPV(i_cpv,"IMMINPAR") 
       End If 
    Call Read_MINPAR(99,1,i_model,set_mod_par,kont)  
    Else If (read_line(7:12).Eq."EXTPAR") Then  
     Call Read_EXTPAR(99,0,i_model,set_mod_par,kont)  
    Else If (read_line(7:14).Eq."IMEXTPAR") Then  
       If (i_cpv.Lt.2) Then 
       Call Warn_CPV(i_cpv,"IMEXTPAR") 
       End If 
    Call Read_EXTPAR(99,1,i_model,set_mod_par,kont)  
   Else If (read_line(7:13).Eq."GAUGEIN") Then 
    Call Read_GAUGEIN(99,0,i_model,set_mod_par,kont) 
 
   Else If (read_line(7:15).Eq."IMGAUGEIN") Then 
    Call Read_GAUGEIN(99,1,i_model,set_mod_par,kont) 
 
   Else If (read_line(7:12).Eq."HMIXIN") Then 
    Call Read_HMIXIN(99,0,i_model,set_mod_par,kont) 
 
   Else If (read_line(7:14).Eq."IMHMIXIN") Then 
    Call Read_HMIXIN(99,1,i_model,set_mod_par,kont) 
 
   Else If (read_line(7:12).Eq."VEVSIN") Then 
    Call Read_VEVSIN(99,0,i_model,set_mod_par,kont) 
 
   Else If (read_line(7:14).Eq."IMVEVSIN") Then 
    Call Read_VEVSIN(99,1,i_model,set_mod_par,kont) 
 
   Else If (read_line(7:13).Eq."GAUGEIN") Then 
    Call Read_GAUGEIN(99,0,i_model,set_mod_par,kont) 
 
   Else If (read_line(7:15).Eq."IMGAUGEIN") Then 
    Call Read_GAUGEIN(99,1,i_model,set_mod_par,kont) 
 
   Else If (read_line(7:12).Eq."HMIXIN") Then 
    Call Read_HMIXIN(99,0,i_model,set_mod_par,kont) 
 
   Else If (read_line(7:14).Eq."IMHMIXIN") Then 
    Call Read_HMIXIN(99,1,i_model,set_mod_par,kont) 
 
   Else If (read_line(7:12).Eq."VEVSIN") Then 
    Call Read_VEVSIN(99,0,i_model,set_mod_par,kont) 
 
   Else If (read_line(7:14).Eq."IMVEVSIN") Then 
    Call Read_VEVSIN(99,1,i_model,set_mod_par,kont) 
 
End if 
End If 
End Do 
200 Close(99) 
gmZ = gamZ * mZ
gmZ2 = gmZ**2
mW2 = mZ2 * (0.5_dp + Sqrt(0.25_dp-Alpha_Mz*pi / (sqrt2*G_F*mZ2))) / 0.987_dp
mW = Sqrt(mW2) 
mW_SM = MW 
gamW = 2.06_dp 
gamW2 = gamW**2
gmW = gamW * mW
gmW2 = gmW**2
Alpha_mZ = Alpha_MSbar(mZ, mW)
If (calc_ferm) Call CalculateRunningMasses(mf_l,mf_d,mf_u&
&,Q_light_quarks,alpha_mZ,alphas_mZ,mZ&
&,mf_l_mZ,mf_d_mZ,mf_u_mZ,kont)


Iname=Iname-1
Contains
Subroutine Read_MINPAR(io,i_c,i_model,set_mod_par,kont) 
Implicit None 
Integer,Intent(in)::io,i_c,i_model 
Integer,Intent(inout)::kont,set_mod_par(:) 
Integer::i_par 
Real(dp)::wert 
Character(len=80)::read_line 
Do 
Read(io,*,End=200) read_line 
If (read_line(1:1).Eq."#") Cycle! this loop 
Backspace(io)! resetting to the beginning of the line 
If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b")) Exit! this loop 
Read(io,*) i_par,wert!,read_line 
If (i_par.Eq.1) Then 
If (i_c.Eq.0) Lambda1Input= Cmplx(wert,Aimag(Lambda1Input),dp) 
If (i_c.Eq.1) Lambda1Input= Cmplx(Real(Lambda1Input,dp),wert,dp) 
Else If (i_par.Eq.2) Then 
If (i_c.Eq.0) Lambda2Input= Cmplx(wert,Aimag(Lambda2Input),dp) 
If (i_c.Eq.1) Lambda2Input= Cmplx(Real(Lambda2Input,dp),wert,dp) 
Else If (i_par.Eq.3) Then 
If (i_c.Eq.0) Lambda3Input= Cmplx(wert,Aimag(Lambda3Input),dp) 
If (i_c.Eq.1) Lambda3Input= Cmplx(Real(Lambda3Input,dp),wert,dp) 
Else If (i_par.Eq.4) Then 
If (i_c.Eq.0) Lambda4Input= Cmplx(wert,Aimag(Lambda4Input),dp) 
If (i_c.Eq.1) Lambda4Input= Cmplx(Real(Lambda4Input,dp),wert,dp) 
Else If (i_par.Eq.5) Then 
If (i_c.Eq.0) Lambda1DashInput= Cmplx(wert,Aimag(Lambda1DashInput),dp) 
If (i_c.Eq.1) Lambda1DashInput= Cmplx(Real(Lambda1DashInput,dp),wert,dp) 
Else If (i_par.Eq.6) Then 
If (i_c.Eq.0) Lambda2DashInput= Cmplx(wert,Aimag(Lambda2DashInput),dp) 
If (i_c.Eq.1) Lambda2DashInput= Cmplx(Real(Lambda2DashInput,dp),wert,dp) 
Else If (i_par.Eq.7) Then 
If (i_c.Eq.0) Lambda3DashInput= Cmplx(wert,Aimag(Lambda3DashInput),dp) 
If (i_c.Eq.1) Lambda3DashInput= Cmplx(Real(Lambda3DashInput,dp),wert,dp) 
Else If (i_par.Eq.8) Then 
If (i_c.Eq.0) Mu3Input= Cmplx(wert,Aimag(Mu3Input),dp) 
If (i_c.Eq.1) Mu3Input= Cmplx(Real(Mu3Input,dp),wert,dp) 
Else If (i_par.Eq.9) Then 
If (i_c.Eq.0) MubInput= Cmplx(wert,Aimag(MubInput),dp) 
If (i_c.Eq.1) MubInput= Cmplx(Real(MubInput,dp),wert,dp) 
Else If (i_par.Eq.10) Then 
If (i_c.Eq.0) Aa1Input= Cmplx(wert,Aimag(Aa1Input),dp) 
If (i_c.Eq.1) Aa1Input= Cmplx(Real(Aa1Input,dp),wert,dp) 
Else If (i_par.Eq.11) Then 
If (i_c.Eq.0) Aa2Input= Cmplx(wert,Aimag(Aa2Input),dp) 
If (i_c.Eq.1) Aa2Input= Cmplx(Real(Aa2Input,dp),wert,dp) 
Else If (i_par.Eq.12) Then 
If (i_c.Eq.0) Aa3Input= Cmplx(wert,Aimag(Aa3Input),dp) 
If (i_c.Eq.1) Aa3Input= Cmplx(Real(Aa3Input,dp),wert,dp) 
Else If (i_par.Eq.13) Then 
If (i_c.Eq.0) Aa4Input= Cmplx(wert,Aimag(Aa4Input),dp) 
If (i_c.Eq.1) Aa4Input= Cmplx(Real(Aa4Input,dp),wert,dp) 
Else If (i_par.Eq.14) Then 
v3input= wert 
Else If (i_par.Eq.15) Then 
If (i_c.Eq.0) Y1d11input= Cmplx(wert,Aimag(Y1d11input),dp) 
If (i_c.Eq.1) Y1d11input= Cmplx(Real(Y1d11input,dp),wert,dp) 
Else If (i_par.Eq.16) Then 
If (i_c.Eq.0) Y1d12input= Cmplx(wert,Aimag(Y1d12input),dp) 
If (i_c.Eq.1) Y1d12input= Cmplx(Real(Y1d12input,dp),wert,dp) 
Else If (i_par.Eq.17) Then 
If (i_c.Eq.0) Y1d13input= Cmplx(wert,Aimag(Y1d13input),dp) 
If (i_c.Eq.1) Y1d13input= Cmplx(Real(Y1d13input,dp),wert,dp) 
Else If (i_par.Eq.18) Then 
If (i_c.Eq.0) Y1d21input= Cmplx(wert,Aimag(Y1d21input),dp) 
If (i_c.Eq.1) Y1d21input= Cmplx(Real(Y1d21input,dp),wert,dp) 
Else If (i_par.Eq.19) Then 
If (i_c.Eq.0) Y1d22input= Cmplx(wert,Aimag(Y1d22input),dp) 
If (i_c.Eq.1) Y1d22input= Cmplx(Real(Y1d22input,dp),wert,dp) 
Else If (i_par.Eq.20) Then 
If (i_c.Eq.0) Y1d23input= Cmplx(wert,Aimag(Y1d23input),dp) 
If (i_c.Eq.1) Y1d23input= Cmplx(Real(Y1d23input,dp),wert,dp) 
Else If (i_par.Eq.21) Then 
If (i_c.Eq.0) Y2d31input= Cmplx(wert,Aimag(Y2d31input),dp) 
If (i_c.Eq.1) Y2d31input= Cmplx(Real(Y2d31input,dp),wert,dp) 
Else If (i_par.Eq.22) Then 
If (i_c.Eq.0) Y2d32input= Cmplx(wert,Aimag(Y2d32input),dp) 
If (i_c.Eq.1) Y2d32input= Cmplx(Real(Y2d32input,dp),wert,dp) 
Else If (i_par.Eq.23) Then 
If (i_c.Eq.0) Y2d33input= Cmplx(wert,Aimag(Y2d33input),dp) 
If (i_c.Eq.1) Y2d33input= Cmplx(Real(Y2d33input,dp),wert,dp) 
Else If (i_par.Eq.24) Then 
If (i_c.Eq.0) Y1u11input= Cmplx(wert,Aimag(Y1u11input),dp) 
If (i_c.Eq.1) Y1u11input= Cmplx(Real(Y1u11input,dp),wert,dp) 
Else If (i_par.Eq.25) Then 
If (i_c.Eq.0) Y1u12input= Cmplx(wert,Aimag(Y1u12input),dp) 
If (i_c.Eq.1) Y1u12input= Cmplx(Real(Y1u12input,dp),wert,dp) 
Else If (i_par.Eq.26) Then 
If (i_c.Eq.0) Y1u21input= Cmplx(wert,Aimag(Y1u21input),dp) 
If (i_c.Eq.1) Y1u21input= Cmplx(Real(Y1u21input,dp),wert,dp) 
Else If (i_par.Eq.27) Then 
If (i_c.Eq.0) Y1u22input= Cmplx(wert,Aimag(Y1u22input),dp) 
If (i_c.Eq.1) Y1u22input= Cmplx(Real(Y1u22input,dp),wert,dp) 
Else If (i_par.Eq.28) Then 
If (i_c.Eq.0) Y2u33input= Cmplx(wert,Aimag(Y2u33input),dp) 
If (i_c.Eq.1) Y2u33input= Cmplx(Real(Y2u33input,dp),wert,dp) 
Else If (i_par.Eq.29) Then 
If (i_c.Eq.0) Y1e11input= Cmplx(wert,Aimag(Y1e11input),dp) 
If (i_c.Eq.1) Y1e11input= Cmplx(Real(Y1e11input,dp),wert,dp) 
Else If (i_par.Eq.30) Then 
If (i_c.Eq.0) Y1e12input= Cmplx(wert,Aimag(Y1e12input),dp) 
If (i_c.Eq.1) Y1e12input= Cmplx(Real(Y1e12input,dp),wert,dp) 
Else If (i_par.Eq.31) Then 
If (i_c.Eq.0) Y1e21input= Cmplx(wert,Aimag(Y1e21input),dp) 
If (i_c.Eq.1) Y1e21input= Cmplx(Real(Y1e21input,dp),wert,dp) 
Else If (i_par.Eq.32) Then 
If (i_c.Eq.0) Y1e22input= Cmplx(wert,Aimag(Y1e22input),dp) 
If (i_c.Eq.1) Y1e22input= Cmplx(Real(Y1e22input,dp),wert,dp) 
Else If (i_par.Eq.33) Then 
If (i_c.Eq.0) Y2e33input= Cmplx(wert,Aimag(Y2e33input),dp) 
If (i_c.Eq.1) Y2e33input= Cmplx(Real(Y2e33input,dp),wert,dp) 
Else If (i_par.Eq.34) Then 
If (i_c.Eq.0) Y1n11input= Cmplx(wert,Aimag(Y1n11input),dp) 
If (i_c.Eq.1) Y1n11input= Cmplx(Real(Y1n11input,dp),wert,dp) 
Else If (i_par.Eq.35) Then 
If (i_c.Eq.0) Y1n12input= Cmplx(wert,Aimag(Y1n12input),dp) 
If (i_c.Eq.1) Y1n12input= Cmplx(Real(Y1n12input,dp),wert,dp) 
Else If (i_par.Eq.36) Then 
If (i_c.Eq.0) Y1n21input= Cmplx(wert,Aimag(Y1n21input),dp) 
If (i_c.Eq.1) Y1n21input= Cmplx(Real(Y1n21input,dp),wert,dp) 
Else If (i_par.Eq.37) Then 
If (i_c.Eq.0) Y1n22input= Cmplx(wert,Aimag(Y1n22input),dp) 
If (i_c.Eq.1) Y1n22input= Cmplx(Real(Y1n22input,dp),wert,dp) 
Else If (i_par.Eq.38) Then 
If (i_c.Eq.0) Y2n33input= Cmplx(wert,Aimag(Y2n33input),dp) 
If (i_c.Eq.1) Y2n33input= Cmplx(Real(Y2n33input,dp),wert,dp) 
Else If (i_par.Eq.39) Then 
If (i_c.Eq.0) BB11input= Cmplx(wert,Aimag(BB11input),dp) 
If (i_c.Eq.1) BB11input= Cmplx(Real(BB11input,dp),wert,dp) 
Else If (i_par.Eq.40) Then 
If (i_c.Eq.0) BB12input= Cmplx(wert,Aimag(BB12input),dp) 
If (i_c.Eq.1) BB12input= Cmplx(Real(BB12input,dp),wert,dp) 
Else If (i_par.Eq.41) Then 
If (i_c.Eq.0) BB21input= Cmplx(wert,Aimag(BB21input),dp) 
If (i_c.Eq.1) BB21input= Cmplx(Real(BB21input,dp),wert,dp) 
Else If (i_par.Eq.42) Then 
If (i_c.Eq.0) BB22input= Cmplx(wert,Aimag(BB22input),dp) 
If (i_c.Eq.1) BB22input= Cmplx(Real(BB22input,dp),wert,dp) 
Else If (i_par.Eq.43) Then 
If (i_c.Eq.0) C13input= Cmplx(wert,Aimag(C13input),dp) 
If (i_c.Eq.1) C13input= Cmplx(Real(C13input,dp),wert,dp) 
Else If (i_par.Eq.44) Then 
If (i_c.Eq.0) C23input= Cmplx(wert,Aimag(C23input),dp) 
If (i_c.Eq.1) C23input= Cmplx(Real(C23input,dp),wert,dp) 
Else If (i_par.Eq.45) Then 
If (i_c.Eq.0) C31input= Cmplx(wert,Aimag(C31input),dp) 
If (i_c.Eq.1) C31input= Cmplx(Real(C31input,dp),wert,dp) 
Else If (i_par.Eq.46) Then 
If (i_c.Eq.0) C32input= Cmplx(wert,Aimag(C32input),dp) 
If (i_c.Eq.1) C32input= Cmplx(Real(C32input,dp),wert,dp) 
Else If (i_par.Eq.47) Then 
v1input= wert 
Else If (i_par.Eq.48) Then 
v2input= wert 
Else
Write(ErrCan,*) "Error in routine "//NameOfUnit(Iname)
If (i_c.Eq.0) Write(ErrCan,*) "Unknown entry for Block MINPAR ",i_par
If (i_c.Eq.1) Write(ErrCan,*) "Unknown entry for Block IMMINPAR ",i_par
If (i_c.Eq.0) Write(*,*) "Unknown entry for Block MINPAR ",i_par
If (i_c.Eq.1) Write(*,*) "Unknown entry for Block IMMINPAR ",i_par
Call AddError(304)
If (ErrorLevel.Eq.2) Call TerminateProgram
End If
End Do! i_par
200 Return
End Subroutine Read_MINPAR 
 
 
Subroutine Read_EXTPAR(io,i_c,i_model,set_mod_par,kont) 
Implicit None 
Integer,Intent(in)::io,i_c,i_model 
Integer,Intent(inout)::kont,set_mod_par(:) 
Integer::i_par 
Real(dp)::wert 
Character(len=80)::read_line 
Do 
Read(io,*,End=200) read_line 
If (read_line(1:1).Eq."#") Cycle! this loop 
Backspace(io)! resetting to the beginning of the line 
If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b")) Exit! this loop 
Read(io,*) i_par,wert!,read_line 
End Do! i_par
200 Return
End Subroutine Read_EXTPAR 
 
 
 Subroutine Read_MODSEL(io, i_particles, i_model, i_cpv, kont)
  Implicit None
   Integer, Intent(in) :: io
   Integer, Intent(out) :: i_particles, i_model, i_cpv
   Integer, Intent(inout) :: kont
    Real(dp) :: r_mod

   Integer :: i_mod, i_test, i_rp
   Character(len=80) :: read_line

   i_cpv = 0
   i_rp = 0

    Do 
     Read(io,*) read_line
     If (read_line(1:1).Eq."#") Cycle ! this loop
     Backspace(io) ! resetting to the beginning of the line
     If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b") ) Exit ! this loop

     Read(io,*) i_test,r_mod ! ,read_line
     If (i_test.Ne.12) Then
      Backspace(io)
      Read(io,*) i_test,i_mod ! ,read_line
     End If

!      Read(io,*) i_test,i_mod,read_line
     If (i_test.Eq.1) Then
      i_particles = i_test
      i_model = i_mod
      If ((i_mod.Lt.0).Or.(i_mod.Gt.99)) Then
       Write(ErrCan,*) "Error in routine "//NameOfUnit(Iname)
       Write(ErrCan,*) "MSSM, Unknown entry for Block MODSEL ",i_mod
       kont = -302
       Call AddError(-kont)
       Return
      Else If (i_mod.Eq.1) Then
       HighScaleModel = "GUT"
      Else If (i_mod.Eq.0) Then
       HighScaleModel = "LOW"
      End If

     Else If (i_test.Eq.2) Then
      BoundaryCondition = i_mod
     Else If (i_test.Eq.4) Then
      If (i_mod.Eq.1) Then
       i_rp = 1

      Else If (i_mod.Ne.0) Then
       Write(ErrCan,*) "Error in routine "//NameOfUnit(Iname)
       Write(ErrCan,*) "Unknown entry for Block MODSEL ",i_test,i_mod
       kont = -302
       Call AddError(-kont)
       Return
      End If

     Else If (i_test.Eq.5) Then
      i_cpv = i_mod
      If ((i_mod.Lt.0).Or.(i_mod.Gt.2)) Then
       Write(ErrCan,*) "Error in routine "//NameOfUnit(Iname)
       Write(ErrCan,*) "Unknown entry for Block MODSEL ",i_test,i_mod
       kont = -302
       Call AddError(-kont)
       Return
      End If

     Else If (i_test.Eq.6) Then
      If (i_mod.Eq.0) Then
       GenerationMixing = .False.
      Else If ((i_mod.Ge.1).And.(i_mod.Le.3)) Then
       GenerationMixing = .True.
      Else
       Write(ErrCan,*) "Error in routine "//NameOfUnit(Iname)
       Write(ErrCan,*) "GenerationMixing, Unknown entry for Block MODSEL ",i_mod
       kont = -302
       Call AddError(-kont)
       Return
      End If

    Else If (i_test.Eq.12) Then
      Call SetRGEScale(r_mod**2)  ! set Q_EWSB

     End If
    End Do ! i_mod

  End Subroutine Read_MODSEL

  Subroutine Read_SMinput(io)
  Implicit None
   Integer, Intent(in) :: io
   
   Integer :: i_sm
   Real(dp) :: wert
   Character(len=80) :: read_line

    Do 
     Read(io,*) read_line
     If (read_line(1:1).Eq."#") Cycle ! this loop
     Backspace(io) ! resetting to the beginning of the line
     If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b") ) Exit ! this loop

     Read(io,*) i_sm,wert,read_line

     Select Case(i_sm)
     Case(1)
      check_alpha(1) = .True.
      MZ_input = .True.
      Alpha_MZ_MS = 1._dp / wert

     Case(2) ! G_F
      G_F = wert

     Case(3) ! alpha_s(m_Z)
      alphaS_mZ = wert

     Case(4) ! m_Z
      mZ = wert
      mZ2 = mZ**2
      calc_ferm = .True.

     Case(5) ! m_b(m_b)^MSbar
      mf_d(3) = wert
      mf_d2(3) = mf_d(3)**2
      calc_ferm = .True.

     Case(6) ! m_t^pole
      mf_u(3) = wert
      mf_u2(3) = mf_u(3)**2

     Case(7) ! m_tau^pole
      mf_l(3) = wert
      mf_l2(3) = mf_l(3)**2
      calc_ferm = .True.

     Case(8) ! m_nu_3, input is in GeV
      Mf_nu(3) = wert

     Case(11) ! electron mass
      mf_l(1) = wert
      mf_l2(1) = wert**2
      calc_ferm = .True.

     Case(12) ! m_nu_1, input is in GeV
      Mf_nu(1) = wert 

     Case(13) ! muon mass
      mf_l(2) = wert
      mf_l2(2) = wert**2
      calc_ferm = .True.

     Case(14) ! m_nu_2, input is in eV, transform to GeV
      Mf_nu(2) = wert 

     Case(21) ! d-quark mass at 2 GeV
      mf_d(1) = wert
      mf_d2(1) = wert**2
      calc_ferm = .True.

     Case(22) ! u-quark mass at 2 GeV
      mf_u(1) = wert
      mf_u2(1) = wert**2
      calc_ferm = .True.

     Case(23) ! s-quark mass at 2 GeV
      mf_d(2) = wert
      mf_d2(2) = wert**2
      calc_ferm = .True.

     Case(24) ! c-quark mass at Q=m_c
      mf_u(2) = wert
      mf_u2(2) = wert**2
      calc_ferm = .True.

     Case Default
      If (output_screen) &
           & Write(*,*) "Ignoring unknown entry for Block SMINPUTS ",i_sm 
      Write(ErrCan,*) "Ignoring unknown entry for Block SMINPUTS ",i_sm 
     End Select

    End Do ! i_sm

  End Subroutine Read_SMinput

 Subroutine Read_CKM(io, i_cpv)
  Implicit None
   Integer, Intent(in) :: io, i_cpv

   Real(dp) :: s12, s13, s23, c12, c13, c23, phase
    
    Do 
     Read(io,*,End=200) read_line
!     Write(*,*) read_line
     Backspace(io) ! resetting to the beginning of the line
     If ((read_line(1:1).Eq."#").Or.(read_line(1:1).Eq."B")  &
                                .Or.(read_line(1:1).Eq."b") ) Exit ! this loop
     Read(io,*) i1, wert, read_line
     Select Case(i1)     
     Case(1)
      lam_wolf = wert
     Case(2)
      A_wolf = wert
     Case(3)
      rho_wolf = wert
     Case(4)
      eta_wolf = wert
     Case default
     End Select

    End Do

 200   s12 = lam_wolf
    s23 = s12**2 * A_wolf
    s13 = s23 * lam_wolf * Sqrt(eta_wolf**2+rho_wolf**2)
    If (i_cpv.Eq.0) Then
     Write(ErrCan,*) "Warning: CP violation is switched of, ignoring CKM phase."
     phase = 0._dp
    Else
     phase = Atan(eta_wolf/rho_wolf)
    End If


    c12 = Sqrt(1._dp-s12*s12)
    c23 = Sqrt(1._dp-s23*s23)
    c13 = Sqrt(1._dp-s13*s13)

    CKM(1,1) = c12 * c13
    CKM(1,2) = s12 * c13
    CKM(2,3) = s23 * c13
    CKM(3,3) = c23 * c13
    If (phase.Ne.0._dp) Then
     CKM(1,3) = s13 * Exp( (0._dp,-1._dp) * phase )
     CKM(2,1) = -s12*c23 -c12*s23*s13 * Exp( (0._dp,1._dp) * phase )
     CKM(2,2) = c12*c23 -s12*s23*s13 * Exp( (0._dp,1._dp) * phase )
     CKM(3,1) = s12*s23 -c12*c23*s13 * Exp( (0._dp,1._dp) * phase )
     CKM(3,2) = -c12*s23 - s12*c23*s13 * Exp( (0._dp,1._dp) * phase )
    Else
     CKM(1,3) = s13
     CKM(2,1) = -s12*c23 -c12*s23*s13
     CKM(2,2) = c12*c23 -s12*s23*s13
     CKM(3,1) = s12*s23 -c12*c23*s13
     CKM(3,2) = -c12*s23 - s12*c23*s13
    End If

  End Subroutine Read_CKM

 Subroutine Read_SPINFO(io, kont)
  Implicit None
   Integer, Intent(in) :: io
   Integer, Intent(inout) :: kont

    Do 
     Read(io,*,End=200) read_line
!     Write(*,*) read_line

     If (read_line(1:1).Eq."#") Cycle ! this loop
     Backspace(io) ! resetting to the beginning of the line
     If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b") ) Exit ! this loop

     Read(io,*) i_sp, read_line

     If (i_sp.Eq.1) Then
      sp_info = Trim(read_line)//" "//Trim(sp_info)
     Else If (i_sp.Eq.2) Then
      sp_info = Trim(sp_info)//" "//Trim(read_line)
     Else If (i_sp.Eq.4) Then ! there is some inconsistency, exit
      kont = -306
      Call AddError(306)
      Iname = Iname - 1
      Return
     Else
      Write(ErrCan,*) "Unknown entry in BLOCK SPINFO, is ignored:"
      Write(ErrCan,*) i_sp, read_line
     End If
    End Do

   200 Return

  End Subroutine Read_SPINFO

  Subroutine Read_SPhenoInput(io)
  Implicit None
   Integer, Intent(in) :: io

   Integer :: i_par
   Real(dp) :: wert
   Character(len=80) :: read_line

    ! This initialization is necessary for the arrar of production infos
    p_max = Size(Ecms)
    p_act = 0
    Ecms = 0._dp
    Pm = 0._dp
    Pp = 0._dp
    l_ISR = .False.
    Do 
     Read(io,*,End=200,err=200) read_line
!     Write(*,*) trim(read_line)
     If (read_line(1:1).Eq."#") Cycle ! this loop
     Backspace(io) ! resetting to the beginning of the line
     If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b") ) Exit ! this loop

     Read(io,*,End=200) i_par,wert,read_line
!     write(*,*) i_par,wert,trim(read_line)
     Select Case(i_par)
     Case(1)
      ErrorLevel = Int(wert)

     Case(2)
      If (Int(wert).Ne.0) Then
       SPA_convention = .True.
       Call SetRGEScale(1.e3_dp**2)
      End If

     Case(3)
      If (Int(wert).Ne.0) Then 
       External_Spectrum = .True.
       External_Higgs = .True.
      End If

     Case(4)
      If (Int(wert).Ne.0) Use_Flavour_States = .True.

     Case(5)
      If (Int(wert).Ne.0) FermionMassResummation = .False.
      
     Case(6)
       RXiNew = wert      

     Case(7)
       If (wert.eq.1) then
         CalculateTwoLoopHiggsMasses=.False.
!          TwoLoopMatching = .false.
!          OneLoopMatching = .false.         
       Else
         CalculateTwoLoopHiggsMasses=.True.
       End if

     Case(8)
        SELECT CASE (int(WERT))
        CASE ( 1 )
           PurelyNumericalEffPot = .true.
           CalculateMSSM2Loop = .false.
           TwoLoopMethod=1
        CASE ( 2 )
           PurelyNumericalEffPot = .false.
           CalculateMSSM2Loop = .false.
           TwoLoopMethod=2
        CASE ( 3 )
           CalculateMSSM2Loop = .false.
           TwoLoopMethod=3
        CASE ( 8 )
           CalculateMSSM2Loop = .True.
           TwoLoopMethod=8
        CASE ( 9 )
           CalculateMSSM2Loop = .True.
           TwoLoopMethod=9
        CASE DEFAULT 
           Write(*,*) "Unknown option for two-loop mass calculation"
           CalculateTwoLoopHiggsMasses=.False.
        END SELECT


 
     Case(9)
       If (wert.Ne.0) Then
        GaugelessLimit=.true.
       Else
        GaugelessLimit=.false.
       End If

     Case(400)
       hstep_pn = wert
     Case(401)
       hstep_sa = wert
       
     Case(410)
       TwoLoopRegulatorMass = wert       

     Case(10)
       If (wert.Ne.1) Then
        TwoLoopSafeMode=.false.
       Else
        TwoLoopSafeMode=.true.
       End If

     Case(11)  ! whether to calculate  branching ratios or not
      If (Int(wert).Eq.1) L_BR = .True.
      If (Int(wert).Eq.0) L_BR = .False.

     Case(12) ! minimal value such that a branching ratio is written out
      Call SetWriteMinBR(wert)

     Case(13) ! minimal value such that a branching ratio is written out
      If (wert.Eq.0) Then
           Enable3BDecaysF = .False.
           Enable3BDecaysS = .False.        
      Elseif (wert.Eq.1) Then
           Enable3BDecaysF = .True.
           Enable3BDecaysS = .False.        
      Elseif (wert.Eq.2) Then
           Enable3BDecaysS = .True.
           Enable3BDecaysF = .False.        
      Elseif (wert.Eq.3) Then
           Enable3BDecaysF = .True.
           Enable3BDecaysS = .True.        
      Else 
          Write(*,*) "Unknown option for flag 13 (three-body decays): ",wert
      End if


     Case(14) ! run SUSY couplings to scale of decaying particle
      If (wert.Eq.0) RunningCouplingsDecays = .False.    

     Case(15) ! run SUSY couplings to scale of decaying particle
      MinWidth = wert    
      
     Case(16) ! run SUSY couplings to scale of decaying particle
       If (wert.Ne.0) Then
        OneLoopDecays=.true.
       Else
        OneLoopDecays=.false.
       End If
!      Case(21)  ! whether to calculate cross sections or not
!       If (Int(wert).Eq.1) L_CS = .True.
!       If (Int(wert).Eq.0) L_CS = .False.
! 
!      Case(22) ! cms energy
!       p_act = p_act + 1
!       ! this test is necessary to avoid a memory violation
!       If (p_act.Le.p_max) Then
!        Ecms(p_act) = wert
!       Else
!        If (output_screen) &
!            & Write(*,*) "The number of required points for the calculation"// &
!            &  " of cross sections exceeds",p_max
!        If (output_screen) &
!            & Write(*,*) "Ignoring this information"
!        If (output_screen) &
!      &  Write(*,*) "Please enlarge the corresponding arrays in the main program."
!        Write(ErrCan,*) "The number of required points for the calculation"// &
!                &   " of cross sections exceeds",p_max
!        Write(ErrCan,*) "Ignoring this information"
!        Write(ErrCan,*) &
!          &"Please enlarge the corresponding arrays in the main program."
!       End If

!      Case (23) ! polarisation of incoming e- beam
!       If (Abs(wert).Gt.1._dp) Then
!        If (output_screen) Write(*,*) &
!            & "e- beam polarisation has to between -1 and 1 and not",wert
!        If (output_screen) &
!            & Write(*,*) "using now unpolarised e- beam"
!        Write(ErrCan,*) &
!           & "e- beam polarisation has to between -1 and 1 and not",wert
!        Write(ErrCan,*) "using now unpolarised e- beam"
!        If (p_act.Le.p_max) Pm(p_act) = 0
!       Else
!        If (p_act.Le.p_max) Pm(p_act) = wert
!       End If
! 
!      Case (24) ! polarisation of incoming e+ beam
!       If (Abs(wert).Gt.1._dp) Then
!        If (output_screen) Write(*,*) &
!            & "e+ beam polarisation has to between -1 and 1 and not",wert
!        If (output_screen) &
!            & Write(*,*) "using now unpolarised e+ beam"
!        Write(ErrCan,*) &
!           & "e+ beam polarisation has to between -1 and 1 and not",wert
!        Write(ErrCan,*) "using now unpolarised e+ beam"
!        If (p_act.Le.p_max) Pp(p_act) = 0
!       Else
!        If (p_act.Le.p_max) Pp(p_act) = wert
!       End If

!      Case(25)
!       If ((wert.Eq.1._dp).And.(p_act.Le.p_max)) L_ISR(p_act) = .True.
! 
!      Case(26) ! minimal value such that a cross section is written out
!       Call SetWriteMinSig(wert)

     Case(19) ! maximal number of iterations
      MatchingOrder = Int(wert)

     Case(20) 
      If (wert.eq.1._dp) GetMassUncertainty=.True.

     Case(31) ! setting a fixed GUT scale if wert > 0
      If (wert.Gt.0._dp) Call SetGUTScale(wert)

     Case(32) ! requires strict unification
      If (Int(wert).Ne.0) check = SetStrictUnification(.True.)

     Case(33) ! setting a fixed renormalization scale if wert > 0
      If (wert.Gt.0._dp) Call SetRGEScale(wert**2)

     Case(34) ! precision of mass calculation
      delta_mass = wert

     Case(35) ! maximal number of iterations
      n_run = Int(wert)

     Case(36) ! minimal number of iterations
      MinimalNumberIterations = Int(wert)

!      Case(36) ! write out debug information
!       If (wert.Eq.0) Then
!        WriteOut = .False.
!       Else
!        WriteOut = .True.
!       End If

     Case(37) ! if =1 -> CKM thourgh V_u, if =2 CKM through V_d 
      If ((wert.Eq.1._dp).Or.(wert.Eq.2._dp)) i1 =  SetYukawaScheme(Int(wert))

     Case(38) ! set looplevel of RGEs
      If (wert.Ne.2._dp) Then
       TwoLoopRGE=.False.
      Else
       TwoLoopRGE=.True.
      End If

     Case(39) ! write out debug information
      If (wert.Eq.0) Then
       WriteSLHA1 = .False.
      Else
       WriteSLHA1 = .True.
      End If


     Case(40) ! alpha(0)
      check_alpha(2) = .True.
      Alpha = 1._dp / wert

     Case(41) ! Z-boson width
      gamZ = wert

     Case(42) ! W-boson width
      gamW = wert

     Case(50) ! W-boson width
      If (wert.Ne.1._dp) Then
       RotateNegativeFermionMasses=.False.
      Else
       RotateNegativeFermionMasses=.True.
      End If

     Case(51)
      If (wert.Ne.1._dp) Then
       SwitchToSCKM=.False.
      Else
       SwitchToSCKM=.True.
      End If

     Case(52)
      If (wert.Ne.1._dp) Then
       IgnoreNegativeMasses=.False.
      Else
       IgnoreNegativeMasses=.True.
      End If

     Case(53)
      If (wert.Ne.1._dp) Then
       IgnoreNegativeMassesMZ=.False.
      Else
       IgnoreNegativeMassesMZ=.True.
      End If

     Case(54)
      If (wert.Ne.1._dp) Then
       WriteOutputForNonConvergence=.False.
      Else
       WriteOutputForNonConvergence=.True.
      End If

     Case(55)
      If (wert.Ne.0._dp) Then
       CalculateOneLoopMasses=.True.
      Else
       CalculateOneLoopMasses=.False.
!        TwoLoopMatching = .false.
!        OneLoopMatching = .false.
      End If

!      Case(56)
!       If (wert.Ne.0._dp) Then
!        CalculateTwoLoopHiggsMasses=.True.
!       Else
!        CalculateTwoLoopHiggsMasses=.False.
!       End If

     Case(57)
      If (wert.Ne.0._dp) Then
       CalculateLowEnergy=.True.
      Else
       CalculateLowEnergy=.False.
      End If

     Case(58)
      If (wert.Ne.0._dp) Then
        IncludeDeltaVB=.True.
        If (wert.Ne.2._dp) Then
         IncludeBSMdeltaVB=.True.
        Else
         IncludeBSMdeltaVB=.False.
        End If
      Else
       IncludeDeltaVB=.False.
      End If

     Case(60)
      If (wert.Ne.0._dp) Then
       KineticMixing=.True.
      Else
       KineticMixing=.False.
      End If

!      Case(61)
!       If (wert.Ne.0._dp) Then
!        SMrunningLowScaleInput=.True.
!       Else
!        SMrunningLowScaleInput=.False.
!       End If

     Case(62)
      If (wert.Ne.0._dp) Then
       RunningSUSYparametersLowEnergy=.True.
      Else
       RunningSUSYparametersLowEnergy=.False.
      End If

     Case(63)
      If (wert.Ne.0._dp) Then
       RunningSMparametersLowEnergy=.True.
      Else
       RunningSMparametersLowEnergy=.False.
      End If

     Case(64)
      If (wert.Ne.0._dp) Then
       WriteParametersAtQ=.True.
      Else
       WriteParametersAtQ=.False.
      End If
      
     Case(66)
      If (wert.Ne.1._dp) Then
       DecoupleAtRenScale=.False.
      Else
       DecoupleAtRenScale=.True.
      End If      
      
     Case(67)
      If (wert.Eq.0._dp) Then
       Calculate_mh_within_SM=.False.
      Else
       Calculate_mh_within_SM=.True.
        If (wert.Eq.2._dp) Then 
           Force_mh_within_SM = .true. 
        End if
      End If   
      
     Case(68)
      If (wert.Ne.1._dp) Then
       MatchZWpoleMasses=.False.
      Else
       MatchZWpoleMasses=.True.
      End If      

!      Case(70)
!       If (wert.Ne.0._dp) Then
!        SUSYrunningFromMZ=.True.
!       Else
!        SUSYrunningFromMZ=.False.
!       End If

     Case(65)
      If (wert.gt.0) SolutionTadpoleNr = wert 


     Case(75) ! Writes the parameter file for WHIZARD
      If (wert.Eq.1) Write_WHIZARD = .True.     

     Case(76) ! Writes input files for HiggsBounfs
      If (wert.Eq.1) Write_HiggsBounds = .True.
      If (wert.Eq.2) Write_HiggsBounds5 = .True.  
      
     Case(77) ! Use conventions for MO
      If (wert.Eq.1) Then 
        OutputForMO = .True.  
        RotateNegativeFermionMasses = .false.
      End if
      
     Case(78) ! Use conventions for MG
      If (wert.Eq.1) Then 
        OutputForMG = .True.  
      End if  
      
     Case(79) ! Writes Wilson coefficients in WCXF format
      If (wert.Eq.1) Write_WCXF = .True.         


     Case(80) ! exit for sure with non-zero value if a problem occurs
      If (wert.Eq.1) Non_Zero_Exit = .True.      

     Case(86) ! width to be counted as inivisble in HiggsBounds output
      WidthToBeInvisible = wert   

     Case(88) ! maximal mass allowed in loops
      MaxMassLoop = wert**2
   
     Case(89) ! maximal mass counted as numerical zero
      MaxMassNumericalZero = wert

     Case(95) ! Force mass matrices at 1-loop to be real
      If (wert.Eq.1) ForceRealMatrices  = .True.


!      Case(90) ! add R-parity at low energies
!       If (wert.Eq.1) Add_Rparity = .True.      
! 
!      Case(91) ! fit RP parameters such, that neutrino data are o.k.
!       If (wert.Eq.1) l_fit_RP_parameters = .True.      
! 
!      Case(92) ! for Pythia input
!       If (wert.Eq.1) l_RP_Pythia = .True.      
! 
!      Case(97) ! for Pythia input
!       If (wert.Eq.1) PrintPartialContributions = .True. 

      case(150) ! use 1l2lshifts
       if (wert.ne.1._dp) then
         include1l2lshift=.false.
       else
         include1l2lshift=.true.
       end if    

      case(151)
        if (wert.ne.1._dp) Then
         NewGBC=.false.
        else
         NewGBC=.true.
       end if
       
      case(201)
        if (wert.ne.1._dp) Then
         One_Loop_Matching =.false.
        else
         One_Loop_Matching =.true.
       end if       
       
      case(202)
        if (wert.ne.1._dp) Then
         Offdiaognal_WaveFunction_Matching =.false.
        else
         Offdiaognal_WaveFunction_Matching  =.true.
       end if        
       
     Case(440)
      If (wert.Ne.1._dp) Then
       TreeLevelUnitarityLimits=.False.
      Else
       TreeLevelUnitarityLimits=.True.
      End If    
      
     Case(441)
      If (wert.Ne.1._dp) Then
       TrilinearUnitarity=.False.
      Else
       TrilinearUnitarity=.True.
      End If   
      
     Case(442)
       unitarity_s_min = wert       

     Case(443)
       unitarity_s_max = wert       
       
     Case(444)
       unitarity_steps = wert           
       
     Case(445)
      If (wert.Ne.1._dp) Then
       RunRGEs_unitarity=.False.
      Else
       RunRGEs_unitarity=.True.
      End If 
       
     Case(446)
       TUcutLevel = wert          
       
     Case(447)
       CutSpole = wert              
       
       
     Case(510)
      If (wert.Ne.1._dp) Then
       WriteTreeLevelTadpoleSolutions=.False.
      Else
       WriteTreeLevelTadpoleSolutions=.True.
      End If

     Case(515)
      If (wert.Ne.0._dp) Then
       WriteGUTvalues=.True.
      Else
       WriteGUTvalues=.False.
      End If

     Case(520)
      If (wert.Ne.1._dp) Then
       WriteEffHiggsCouplingRatios=.False.
      Else
        WriteEffHiggsCouplingRatios=.True.
      End If
      If (OutputForMG) WriteEffHiggsCouplingRatios=.false.

     Case(521)
      If (wert.Ne.1._dp) Then
       HigherOrderDiboson=.False.
      Else
       HigherOrderDiboson=.True.
      End If    
      
     Case(522)
      If (wert.Ne.1._dp) Then
       PoleMassesInLoops=.False.
      Else
       PoleMassesInLoops=.True.
      End If      
      

     Case(525)
      If (wert.Ne.1._dp) Then
       WriteHiggsDiphotonLoopContributions=.False.
      Else
       WriteHiggsDiphotonLoopContributions=.True.
      End If

     Case(530)
      If (wert.Ne.1._dp) Then
       WriteTreeLevelTadpoleParameters=.False.
      Else
       WriteTreeLevelTadpoleParameters=.True.
      End If

     Case(550)
      If (wert.Ne.1._dp) Then
       CalcFT=.False.
      Else
       CalcFT=.True.
      End If

     Case(551)
      If (wert.Ne.1._dp) Then
       OneLoopFT=.False.
      Else
       OneLoopFT=.True.
      End If
! 
!      Case(600)  
!       Mass_Regulator_PhotonGluon = wert
!       
!      Case(610)  
!       If (wert.Ne.1._dp) Then
!        SquareFullAmplitudeDecays=.False.
!       Else
!        SquareFullAmplitudeDecays=.True.
!       End If      

     Case(990)
      If (wert.Ne.1._dp) Then
       MakeQTEST=.False.
      Else
       MakeQTEST=.True.
      End If
      

     Case(999)
      If (wert.Ne.1._dp) Then
       PrintDebugInformation=.False.
      Else
       PrintDebugInformation=.True.
      End If
 

    Case Default
      If (output_screen) Write(*,*) &
           & "Problem while reading SPhenoInput, ignoring unknown entry" &
           & ,i_par,wert
      Write(Errcan,*) &
          & "Problem while reading  SPhenoInput, ignoring unknown entry" &
               & ,i_par,wert
     End Select ! i_par

    End Do  ! i_par 

   200 Return

  End Subroutine Read_SPhenoInput
Subroutine Read_DecayOptions(io) 
Implicit None 
Integer,Intent(in) :: io 
Integer :: i_par, divset 
Real(dp) :: wert, divvalue 
Character(len=80) :: read_line 
Do 
Read(io,*,End=200,err=200) read_line 
If (read_line(1:1).Eq."#") Cycle! this loop 
Backspace(io)! resetting to the beginning of the line 
If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b")) Exit! this loop 
Read(io,*,End=200) i_par,wert ,read_line 
Select Case(i_par) 
Case(1) 
  If (wert.ne.1) Calc3BodyDecay_Fv= .False. 
Case(2) 
  If (wert.ne.1) Calc3BodyDecay_Fu= .False. 
Case(3) 
  If (wert.ne.1) Calc3BodyDecay_Fe= .False. 
Case(4) 
  If (wert.ne.1) Calc3BodyDecay_Fd= .False. 
Case Default 
 If (output_screen) Write(*,*)&
   & "Problem while reading DecayOptions, ignoring unknown entry"&
   &,i_par,wert
 Write(Errcan,*)&
   & "Problem while reading  DecayOptions, ignoring unknown entry"&
   &,i_par,wert 
End Select 
End Do! i_par 

200 Return 
End Subroutine Read_DecayOptions 


End Subroutine LesHouches_Input 
 
 
 
Subroutine LesHouches_Out(io_L,io,kont,M_GUT,Tpar,Spar,Upar,ae,amu,atau,              & 
& EDMe,EDMmu,EDMtau,dRho,BrBsGamma,ratioBsGamma,BrDmunu,ratioDmunu,BrDsmunu,             & 
& ratioDsmunu,BrDstaunu,ratioDstaunu,BrBmunu,ratioBmunu,BrBtaunu,ratioBtaunu,            & 
& BrKmunu,ratioKmunu,RK,RKSM,muEgamma,tauEgamma,tauMuGamma,CRmuEAl,CRmuETi,              & 
& CRmuESr,CRmuESb,CRmuEAu,CRmuEPb,BRmuTo3e,BRtauTo3e,BRtauTo3mu,BRtauToemumu,            & 
& BRtauTomuee,BRtauToemumu2,BRtauTomuee2,BrZtoMuE,BrZtoTauE,BrZtoTauMu,BrhtoMuE,         & 
& BrhtoTauE,BrhtoTauMu,DeltaMBs,ratioDeltaMBs,DeltaMBq,ratioDeltaMBq,BrTautoEPi,         & 
& BrTautoEEta,BrTautoEEtap,BrTautoMuPi,BrTautoMuEta,BrTautoMuEtap,BrB0dEE,               & 
& ratioB0dEE,BrB0sEE,ratioB0sEE,BrB0dMuMu,ratioB0dMuMu,BrB0sMuMu,ratioB0sMuMu,           & 
& BrB0dTauTau,ratioB0dTauTau,BrB0sTauTau,ratioB0sTauTau,BrBtoSEE,ratioBtoSEE,            & 
& BrBtoSMuMu,ratioBtoSMuMu,BrBtoKee,ratioBtoKee,BrBtoKmumu,ratioBtoKmumu,BrBtoSnunu,     & 
& ratioBtoSnunu,BrBtoDnunu,ratioBtoDnunu,BrKptoPipnunu,ratioKptoPipnunu,BrKltoPinunu,    & 
& ratioKltoPinunu,BrK0eMu,ratioK0eMu,DelMK,ratioDelMK,epsK,ratioepsK,GenerationMixing,f_name)

Implicit None 
Integer, Intent(in) :: io_L, io, kont
Real(dp),Intent(in) :: Tpar,Spar,Upar,ae,amu,atau,EDMe,EDMmu,EDMtau,dRho,BrBsGamma,ratioBsGamma,             & 
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

Real(dp), Intent(in) :: M_GUT
Character(len=8)::Datum 
Character(len=10)::Zeit 
Logical,Intent(in)::GenerationMixing 
Logical,Save::l_open= .True. 
Integer,Parameter::n_max=500 
Integer :: i1, i2 
Logical :: WriteNextBlock 
Character(len=30),Dimension(n_max)::Fnames,Lnames 
Character(len=*),Intent(in),Optional::f_name
Real(dp) :: Q, MassLSP(2), facPP, facGG, facPZ 
Integer :: CurrentPDG2(2), CurrentPDG3(3), PDGlsp(2) 
Integer::ierr,i_errors(1100),gt1,gt2,gt3,icount
Complex(dp) :: PDGhh(3),PDGAh(3),PDGHm(2),PDGVP,PDGVZ,PDGVG,PDGVWm,PDGgP,PDGgWm,PDGgWmC,             & 
& PDGgZ,PDGgG,PDGFd(3),PDGFu(3),PDGFe(3),PDGFv(6)

Character(len=30),Dimension(3):: NameParticlehh
Character(len=30),Dimension(3):: NameParticleAh
Character(len=30),Dimension(2):: NameParticleHm
Character(len=30) :: NameParticleVP
Character(len=30) :: NameParticleVZ
Character(len=30) :: NameParticleVG
Character(len=30) :: NameParticleVWm
Character(len=30) :: NameParticlegP
Character(len=30) :: NameParticlegWm
Character(len=30) :: NameParticlegWmC
Character(len=30) :: NameParticlegZ
Character(len=30) :: NameParticlegG
Character(len=30),Dimension(3):: NameParticleFd
Character(len=30),Dimension(3):: NameParticleFu
Character(len=30),Dimension(3):: NameParticleFe
Character(len=30),Dimension(6):: NameParticleFv
Complex(dp) :: Zbottom(2,2), Ztop(2,2), Ztau(2,2) 

 
 
 ! ----------- Set names and PDGs -------- 
 
PDGhh(1)=25
NameParticlehh(1)="hh_1"
PDGhh(2)=26
NameParticlehh(2)="hh_2"
PDGhh(3)=27
NameParticlehh(3)="hh_3"
PDGAh(1)=0
NameParticleAh(1)="Ah_1"
PDGAh(2)=35
NameParticleAh(2)="Ah_2"
PDGAh(3)=36
NameParticleAh(3)="Ah_3"
PDGHm(1)=0
NameParticleHm(1)="Hm_1"
PDGHm(2)=-37
NameParticleHm(2)="Hm_2"
PDGVP=22
NameParticleVP="VP"
PDGVZ=23
NameParticleVZ="VZ"
PDGVG=21
NameParticleVG="VG"
PDGVWm=-24
NameParticleVWm="VWm"
PDGgP=0
NameParticlegP="gP"
PDGgWm=0
NameParticlegWm="gWm"
PDGgWmC=0
NameParticlegWmC="gWmC"
PDGgZ=0
NameParticlegZ="gZ"
PDGgG=0
NameParticlegG="gG"
PDGFd(1)=1
NameParticleFd(1)="Fd_1"
PDGFd(2)=3
NameParticleFd(2)="Fd_2"
PDGFd(3)=5
NameParticleFd(3)="Fd_3"
PDGFu(1)=2
NameParticleFu(1)="Fu_1"
PDGFu(2)=4
NameParticleFu(2)="Fu_2"
PDGFu(3)=6
NameParticleFu(3)="Fu_3"
PDGFe(1)=11
NameParticleFe(1)="Fe_1"
PDGFe(2)=13
NameParticleFe(2)="Fe_2"
PDGFe(3)=15
NameParticleFe(3)="Fe_3"
PDGFv(1)=12
NameParticleFv(1)="Fv_1"
PDGFv(2)=14
NameParticleFv(2)="Fv_2"
PDGFv(3)=16
NameParticleFv(3)="Fv_3"
PDGFv(4)=8810012
NameParticleFv(4)="Fv_4"
PDGFv(5)=8810014
NameParticleFv(5)="Fv_5"
PDGFv(6)=8810016
NameParticleFv(6)="Fv_6"

 
 
 ! ----------- Use SLHA 1 conventions if demanded -------- 
 
If(WriteSLHA1) Write(*,*) "SLHA 1 output for given model not possible" 
Q=Sqrt(GetRenormalizationScale())
Call Date_and_time(datum,zeit)
If (l_open) Then
If (Present(f_name)) Then
Open(io_L,file=Trim(f_name),status="unknown")
Else
Open(io_L,file=outputFileName,status="unknown")
End If
l_open= .False.
End If
If (.Not.RotateNegativeFermionMasses) Then 
Do i1=1,6
  If (MaxVal(Abs(AImag(Vv(i1,:)))).gt.MaxVal(Abs(Real(Vv(i1,:),dp)))) Then 
MFv(i1) = - MFv(i1) 
Vv(i1,:) = -(0._dp,1._dp)*Vv(i1,:) 
  End If 
End Do 
End If 
 
Write(io_L,100) "# SUSY Les Houches Accord 2 - BGLNCS Spectrum + Decays + Flavour Observables"
Write(io_L,100) "# SPheno module generated by SARAH" 
Write(io_L,100) "# ----------------------------------------------------------------------" 
Write(io_L,100) "# SPheno "//version 
Write(io_L,100) "#   W. Porod, Comput. Phys. Commun. 153 (2003) 275-315, hep-ph/0301101"
Write(io_L,100) "#   W. Porod, F.Staub, Comput.Phys.Commun.183 (2012) 2458-2469, arXiv:1104.1573"
Write(io_L,100) "# SARAH: "//versionSARAH 
Write(io_L,100) "#   F. Staub; arXiv:0806.0538 (online manual)"
Write(io_L,100) "#   F. Staub; Comput. Phys. Commun. 181 (2010) 1077-1086; arXiv:0909.2863"
Write(io_L,100) "#   F. Staub; Comput. Phys. Commun. 182 (2011)  808-833; arXiv:1002.0840"
Write(io_L,100) "#   F. Staub; Comput. Phys. Commun. 184 (2013)  1792-1809; arXiv:1207.0906"
Write(io_L,100) "#   F. Staub; Comput. Phys. Commun. 185 (2014)  1773-1790; arXiv:1309.7223 "
Write(io_L,100) "# Including the calculation of flavor observables based on FlavorKit "
Write(io_L,100) "#   W. Porod, F. Staub, A. Vicente; Eur.Phys.J. C74 (2014) 8, 2992; arXiv:1405.1434 "
Write(io_L,100) "# Two-loop mass corrections to Higgs fields based on "
Write(io_L,100) "#   M. D. Goodsell, K. Nickel, F. Staub; Eur.Phys.J. C75 (2015) no.6, 290; arXiv:1411.0675 "
Write(io_L,100) "#   M. D. Goodsell, K. Nickel, F. Staub; Eur.Phys.J. C75 (2015) no.1, 32; arXiv:1503.03098"
Write(io_L,100) "#   M. D. Goodsell, F. Staub; Eur.Phys.J.C 77 (2017) 1, 46, arXiv:1604.05335"
Write(io_L,100) "#   J. Braathen, M. D. Goodsell, F. Staub; Eur.Phys.J.C 77 (2017) 11, 758; arXiv:1703.09237"
Write(io_L,100) "# Loop decays based on "
Write(io_L,100) "#   M. D. Goodsell, S. Liebler, F. Staub; Eur.Phys.J.C 77 (2017) 11, 757; arXiv:1706.05372"
Write(io_L,100) "# Unitarity routines based on "
Write(io_L,100) "#   M. D. Goodsell, F. Staub; Eur.Phys.J.C 78 (2018) 8, 649; arXiv:1805.07306"
Write(io_L,100) "#   M. D. Goodsell, R. Moutafis; arXiv:2012.09022"
Write(io_L,100) "#  "
Write(io_L,100) "# in case of problems send email to goodsell@lpthe.jussieu.fr and porod@physik.uni-wuerzburg.de, or consult the forum/wiki "
Write(io_L,100) "# ----------------------------------------------------------------------" 
Write(io_L,100) "# Created: "//Datum(7:8)//"."//Datum(5:6)//"."//Datum(1:4)&
&//",  "//Zeit(1:2)//":"//Zeit(3:4)
Write(io_L,100) "Block SPINFO         # Program information"
Write(io_L,100) "     1   SPhenoSARAH      # spectrum calculator"
Write(io_L,100) "     2   "//version//"    # version number of SPheno"
Write(io_L,100) "     9   "//versionSARAH//"    # version number of SARAH"
Write(io_L,100) "Block MODSEL  # Input parameters"
Write(io_L,110)  1, 1, " GUT scale input"
Write(io_L,110) 2, BoundaryCondition, " Boundary conditions "
If (i_cpv.Gt.0) Write(io_L,110) 5,i_cpv," switching on CP violation"
If (GenerationMixing) Write(io_L,110) &
&     6,1, " switching on flavour violation" 
Write(io_L,100) "Block MINPAR  # Input parameters"
Write(io_L,101) 1, Real(Lambda1Input,dp) ,"# Lambda1Input"
Write(io_L,101) 2, Real(Lambda2Input,dp) ,"# Lambda2Input"
Write(io_L,101) 3, Real(Lambda3Input,dp) ,"# Lambda3Input"
Write(io_L,101) 4, Real(Lambda4Input,dp) ,"# Lambda4Input"
Write(io_L,101) 5, Real(Lambda1DashInput,dp) ,"# Lambda1DashInput"
Write(io_L,101) 6, Real(Lambda2DashInput,dp) ,"# Lambda2DashInput"
Write(io_L,101) 7, Real(Lambda3DashInput,dp) ,"# Lambda3DashInput"
Write(io_L,101) 8, Real(Mu3Input,dp) ,"# Mu3Input"
Write(io_L,101) 9, Real(MubInput,dp) ,"# MubInput"
Write(io_L,101) 10, Real(Aa1Input,dp) ,"# Aa1Input"
Write(io_L,101) 11, Real(Aa2Input,dp) ,"# Aa2Input"
Write(io_L,101) 12, Real(Aa3Input,dp) ,"# Aa3Input"
Write(io_L,101) 13, Real(Aa4Input,dp) ,"# Aa4Input"
Write(io_L,101) 14, Real(v3input,dp) ,"# v3input"
Write(io_L,101) 15, Real(Y1d11input,dp) ,"# Y1d11input"
Write(io_L,101) 16, Real(Y1d12input,dp) ,"# Y1d12input"
Write(io_L,101) 17, Real(Y1d13input,dp) ,"# Y1d13input"
Write(io_L,101) 18, Real(Y1d21input,dp) ,"# Y1d21input"
Write(io_L,101) 19, Real(Y1d22input,dp) ,"# Y1d22input"
Write(io_L,101) 20, Real(Y1d23input,dp) ,"# Y1d23input"
Write(io_L,101) 21, Real(Y2d31input,dp) ,"# Y2d31input"
Write(io_L,101) 22, Real(Y2d32input,dp) ,"# Y2d32input"
Write(io_L,101) 23, Real(Y2d33input,dp) ,"# Y2d33input"
Write(io_L,101) 24, Real(Y1u11input,dp) ,"# Y1u11input"
Write(io_L,101) 25, Real(Y1u12input,dp) ,"# Y1u12input"
Write(io_L,101) 26, Real(Y1u21input,dp) ,"# Y1u21input"
Write(io_L,101) 27, Real(Y1u22input,dp) ,"# Y1u22input"
Write(io_L,101) 28, Real(Y2u33input,dp) ,"# Y2u33input"
Write(io_L,101) 29, Real(Y1e11input,dp) ,"# Y1e11input"
Write(io_L,101) 30, Real(Y1e12input,dp) ,"# Y1e12input"
Write(io_L,101) 31, Real(Y1e21input,dp) ,"# Y1e21input"
Write(io_L,101) 32, Real(Y1e22input,dp) ,"# Y1e22input"
Write(io_L,101) 33, Real(Y2e33input,dp) ,"# Y2e33input"
Write(io_L,101) 34, Real(Y1n11input,dp) ,"# Y1n11input"
Write(io_L,101) 35, Real(Y1n12input,dp) ,"# Y1n12input"
Write(io_L,101) 36, Real(Y1n21input,dp) ,"# Y1n21input"
Write(io_L,101) 37, Real(Y1n22input,dp) ,"# Y1n22input"
Write(io_L,101) 38, Real(Y2n33input,dp) ,"# Y2n33input"
Write(io_L,101) 39, Real(BB11input,dp) ,"# BB11input"
Write(io_L,101) 40, Real(BB12input,dp) ,"# BB12input"
Write(io_L,101) 41, Real(BB21input,dp) ,"# BB21input"
Write(io_L,101) 42, Real(BB22input,dp) ,"# BB22input"
Write(io_L,101) 43, Real(C13input,dp) ,"# C13input"
Write(io_L,101) 44, Real(C23input,dp) ,"# C23input"
Write(io_L,101) 45, Real(C31input,dp) ,"# C31input"
Write(io_L,101) 46, Real(C32input,dp) ,"# C32input"
Write(io_L,101) 47, Real(v1input,dp) ,"# v1input"
Write(io_L,101) 48, Real(v2input,dp) ,"# v2input"
WriteNextBlock = .False. 
If (Abs(Aimag(Lambda1Input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Lambda2Input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Lambda3Input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Lambda4Input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Lambda1DashInput)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Lambda2DashInput)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Lambda3DashInput)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Mu3Input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(MubInput)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Aa1Input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Aa2Input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Aa3Input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Aa4Input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1d11input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1d12input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1d13input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1d21input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1d22input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1d23input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y2d31input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y2d32input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y2d33input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1u11input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1u12input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1u21input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1u22input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y2u33input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1e11input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1e12input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1e21input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1e22input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y2e33input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1n11input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1n12input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1n21input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y1n22input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(Y2n33input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(BB11input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(BB12input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(BB21input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(BB22input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(C13input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(C23input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(C31input)).gt.0._dp) WriteNextBlock = .True. 
If (Abs(Aimag(C32input)).gt.0._dp) WriteNextBlock = .True. 
If(WriteNextBlock) Then 
Write(io_L,100) "Block IMMINPAR  # Input parameters"
If (Abs(Aimag(Lambda1Input)).gt.0._dp) Then 
Write(io_L,101) 1, Aimag(Lambda1Input) ,"# Lambda1Input"
End if 
If (Abs(Aimag(Lambda2Input)).gt.0._dp) Then 
Write(io_L,101) 2, Aimag(Lambda2Input) ,"# Lambda2Input"
End if 
If (Abs(Aimag(Lambda3Input)).gt.0._dp) Then 
Write(io_L,101) 3, Aimag(Lambda3Input) ,"# Lambda3Input"
End if 
If (Abs(Aimag(Lambda4Input)).gt.0._dp) Then 
Write(io_L,101) 4, Aimag(Lambda4Input) ,"# Lambda4Input"
End if 
If (Abs(Aimag(Lambda1DashInput)).gt.0._dp) Then 
Write(io_L,101) 5, Aimag(Lambda1DashInput) ,"# Lambda1DashInput"
End if 
If (Abs(Aimag(Lambda2DashInput)).gt.0._dp) Then 
Write(io_L,101) 6, Aimag(Lambda2DashInput) ,"# Lambda2DashInput"
End if 
If (Abs(Aimag(Lambda3DashInput)).gt.0._dp) Then 
Write(io_L,101) 7, Aimag(Lambda3DashInput) ,"# Lambda3DashInput"
End if 
If (Abs(Aimag(Mu3Input)).gt.0._dp) Then 
Write(io_L,101) 8, Aimag(Mu3Input) ,"# Mu3Input"
End if 
If (Abs(Aimag(MubInput)).gt.0._dp) Then 
Write(io_L,101) 9, Aimag(MubInput) ,"# MubInput"
End if 
If (Abs(Aimag(Aa1Input)).gt.0._dp) Then 
Write(io_L,101) 10, Aimag(Aa1Input) ,"# Aa1Input"
End if 
If (Abs(Aimag(Aa2Input)).gt.0._dp) Then 
Write(io_L,101) 11, Aimag(Aa2Input) ,"# Aa2Input"
End if 
If (Abs(Aimag(Aa3Input)).gt.0._dp) Then 
Write(io_L,101) 12, Aimag(Aa3Input) ,"# Aa3Input"
End if 
If (Abs(Aimag(Aa4Input)).gt.0._dp) Then 
Write(io_L,101) 13, Aimag(Aa4Input) ,"# Aa4Input"
End if 
If (Abs(Aimag(Y1d11input)).gt.0._dp) Then 
Write(io_L,101) 15, Aimag(Y1d11input) ,"# Y1d11input"
End if 
If (Abs(Aimag(Y1d12input)).gt.0._dp) Then 
Write(io_L,101) 16, Aimag(Y1d12input) ,"# Y1d12input"
End if 
If (Abs(Aimag(Y1d13input)).gt.0._dp) Then 
Write(io_L,101) 17, Aimag(Y1d13input) ,"# Y1d13input"
End if 
If (Abs(Aimag(Y1d21input)).gt.0._dp) Then 
Write(io_L,101) 18, Aimag(Y1d21input) ,"# Y1d21input"
End if 
If (Abs(Aimag(Y1d22input)).gt.0._dp) Then 
Write(io_L,101) 19, Aimag(Y1d22input) ,"# Y1d22input"
End if 
If (Abs(Aimag(Y1d23input)).gt.0._dp) Then 
Write(io_L,101) 20, Aimag(Y1d23input) ,"# Y1d23input"
End if 
If (Abs(Aimag(Y2d31input)).gt.0._dp) Then 
Write(io_L,101) 21, Aimag(Y2d31input) ,"# Y2d31input"
End if 
If (Abs(Aimag(Y2d32input)).gt.0._dp) Then 
Write(io_L,101) 22, Aimag(Y2d32input) ,"# Y2d32input"
End if 
If (Abs(Aimag(Y2d33input)).gt.0._dp) Then 
Write(io_L,101) 23, Aimag(Y2d33input) ,"# Y2d33input"
End if 
If (Abs(Aimag(Y1u11input)).gt.0._dp) Then 
Write(io_L,101) 24, Aimag(Y1u11input) ,"# Y1u11input"
End if 
If (Abs(Aimag(Y1u12input)).gt.0._dp) Then 
Write(io_L,101) 25, Aimag(Y1u12input) ,"# Y1u12input"
End if 
If (Abs(Aimag(Y1u21input)).gt.0._dp) Then 
Write(io_L,101) 26, Aimag(Y1u21input) ,"# Y1u21input"
End if 
If (Abs(Aimag(Y1u22input)).gt.0._dp) Then 
Write(io_L,101) 27, Aimag(Y1u22input) ,"# Y1u22input"
End if 
If (Abs(Aimag(Y2u33input)).gt.0._dp) Then 
Write(io_L,101) 28, Aimag(Y2u33input) ,"# Y2u33input"
End if 
If (Abs(Aimag(Y1e11input)).gt.0._dp) Then 
Write(io_L,101) 29, Aimag(Y1e11input) ,"# Y1e11input"
End if 
If (Abs(Aimag(Y1e12input)).gt.0._dp) Then 
Write(io_L,101) 30, Aimag(Y1e12input) ,"# Y1e12input"
End if 
If (Abs(Aimag(Y1e21input)).gt.0._dp) Then 
Write(io_L,101) 31, Aimag(Y1e21input) ,"# Y1e21input"
End if 
If (Abs(Aimag(Y1e22input)).gt.0._dp) Then 
Write(io_L,101) 32, Aimag(Y1e22input) ,"# Y1e22input"
End if 
If (Abs(Aimag(Y2e33input)).gt.0._dp) Then 
Write(io_L,101) 33, Aimag(Y2e33input) ,"# Y2e33input"
End if 
If (Abs(Aimag(Y1n11input)).gt.0._dp) Then 
Write(io_L,101) 34, Aimag(Y1n11input) ,"# Y1n11input"
End if 
If (Abs(Aimag(Y1n12input)).gt.0._dp) Then 
Write(io_L,101) 35, Aimag(Y1n12input) ,"# Y1n12input"
End if 
If (Abs(Aimag(Y1n21input)).gt.0._dp) Then 
Write(io_L,101) 36, Aimag(Y1n21input) ,"# Y1n21input"
End if 
If (Abs(Aimag(Y1n22input)).gt.0._dp) Then 
Write(io_L,101) 37, Aimag(Y1n22input) ,"# Y1n22input"
End if 
If (Abs(Aimag(Y2n33input)).gt.0._dp) Then 
Write(io_L,101) 38, Aimag(Y2n33input) ,"# Y2n33input"
End if 
If (Abs(Aimag(BB11input)).gt.0._dp) Then 
Write(io_L,101) 39, Aimag(BB11input) ,"# BB11input"
End if 
If (Abs(Aimag(BB12input)).gt.0._dp) Then 
Write(io_L,101) 40, Aimag(BB12input) ,"# BB12input"
End if 
If (Abs(Aimag(BB21input)).gt.0._dp) Then 
Write(io_L,101) 41, Aimag(BB21input) ,"# BB21input"
End if 
If (Abs(Aimag(BB22input)).gt.0._dp) Then 
Write(io_L,101) 42, Aimag(BB22input) ,"# BB22input"
End if 
If (Abs(Aimag(C13input)).gt.0._dp) Then 
Write(io_L,101) 43, Aimag(C13input) ,"# C13input"
End if 
If (Abs(Aimag(C23input)).gt.0._dp) Then 
Write(io_L,101) 44, Aimag(C23input) ,"# C23input"
End if 
If (Abs(Aimag(C31input)).gt.0._dp) Then 
Write(io_L,101) 45, Aimag(C31input) ,"# C31input"
End if 
If (Abs(Aimag(C32input)).gt.0._dp) Then 
Write(io_L,101) 46, Aimag(C32input) ,"# C32input"
End if 
End if 
Write(io_L,106) "Block gaugeGUT Q=",m_GUT,"# (GUT scale)" 
Write(io_L,104) 1,g1GUT, "# g1(Q)" 
Write(io_L,104) 2,g2GUT, "# g2(Q)" 
Write(io_L,104) 3,g3GUT, "# g3(Q)" 
Write(io_L,100) "Block SMINPUTS  # SM parameters"
Write(io_L,102) 1,1._dp/alpha_MZ,"# alpha_em^-1(MZ)^MSbar"
Write(io_L,102) 2,G_F,"# G_mu [GeV^-2]"
Write(io_L,102) 3,alphaS_MZ,"# alpha_s(MZ)^MSbar"
Write(io_L,102) 4,mZ,"# m_Z(pole)"
Write(io_L,102) 5,mf_d(3),"# m_b(m_b), MSbar"
Write(io_L,102) 6,mf_u(3),"# m_t(pole)"
Write(io_L,102) 7,mf_l(3),"# m_tau(pole)"
 
WriteNextBlock = .false. 
If (OutputForMG) WriteNextBlock = .True. 
Write(io_L,106) "Block GAUGE Q=",Q,"# (Renormalization Scale)" 
Write(io_L,104) 1,Real(g1,dp), "# g1" 
Write(io_L,104) 2,Real(g2,dp), "# g2" 
Write(io_L,104) 3,Real(g3,dp), "# g3" 
WriteNextBlock = .false. 
If (OutputForMG) WriteNextBlock = .True. 
Write(io_L,106) "Block HMIX Q=",Q,"# (Renormalization Scale)" 
Write(io_L,104) 31,Real(Lam1,dp), "# Lam1" 
If (Abs(Aimag(Lam1)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 33,Real(Lam3,dp), "# Lam3" 
If (Abs(Aimag(Lam3)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 34,Real(Lam4,dp), "# Lam4" 
If (Abs(Aimag(Lam4)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 32,Real(Lam2,dp), "# Lam2" 
If (Abs(Aimag(Lam2)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 35,Real(Lam1Dash,dp), "# Lam1Dash" 
If (Abs(Aimag(Lam1Dash)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 36,Real(Lam2Dash,dp), "# Lam2Dash" 
If (Abs(Aimag(Lam2Dash)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 37,Real(Lam3Dash,dp), "# Lam3Dash" 
If (Abs(Aimag(Lam3Dash)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 42,Real(Aa3,dp), "# Aa3" 
If (Abs(Aimag(Aa3)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 43,Real(Aa4,dp), "# Aa4" 
If (Abs(Aimag(Aa4)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 62,Real(Y1d11,dp), "# Y1d11" 
If (Abs(Aimag(Y1d11)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 63,Real(Y1d12,dp), "# Y1d12" 
If (Abs(Aimag(Y1d12)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 64,Real(Y1d13,dp), "# Y1d13" 
If (Abs(Aimag(Y1d13)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 65,Real(Y1d21,dp), "# Y1d21" 
If (Abs(Aimag(Y1d21)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 66,Real(Y1d22,dp), "# Y1d22" 
If (Abs(Aimag(Y1d22)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 67,Real(Y1d23,dp), "# Y1d23" 
If (Abs(Aimag(Y1d23)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 77,Real(Y2d31,dp), "# Y2d31" 
If (Abs(Aimag(Y2d31)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 78,Real(Y2d32,dp), "# Y2d32" 
If (Abs(Aimag(Y2d32)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 79,Real(Y2d33,dp), "# Y2d33" 
If (Abs(Aimag(Y2d33)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 44,Real(Y1u11,dp), "# Y1u11" 
If (Abs(Aimag(Y1u11)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 45,Real(Y1u12,dp), "# Y1u12" 
If (Abs(Aimag(Y1u12)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 47,Real(Y1u21,dp), "# Y1u21" 
If (Abs(Aimag(Y1u21)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 48,Real(Y1u22,dp), "# Y1u22" 
If (Abs(Aimag(Y1u22)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 61,Real(Y2u33,dp), "# Y2u33" 
If (Abs(Aimag(Y2u33)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 80,Real(Y1e11,dp), "# Y1e11" 
If (Abs(Aimag(Y1e11)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 81,Real(Y1e12,dp), "# Y1e12" 
If (Abs(Aimag(Y1e12)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 83,Real(Y1e21,dp), "# Y1e21" 
If (Abs(Aimag(Y1e21)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 84,Real(Y1e22,dp), "# Y1e22" 
If (Abs(Aimag(Y1e22)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 97,Real(Y2e33,dp), "# Y2e33" 
If (Abs(Aimag(Y2e33)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 98,Real(Y1n11,dp), "# Y1n11" 
If (Abs(Aimag(Y1n11)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 99,Real(Y1n12,dp), "# Y1n12" 
If (Abs(Aimag(Y1n12)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 101,Real(Y1n21,dp), "# Y1n21" 
If (Abs(Aimag(Y1n21)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 102,Real(Y1n22,dp), "# Y1n22" 
If (Abs(Aimag(Y1n22)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 115,Real(Y2n33,dp), "# Y2n33" 
If (Abs(Aimag(Y2n33)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 136,Real(C13,dp), "# C13" 
If (Abs(Aimag(C13)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 139,Real(C23,dp), "# C23" 
If (Abs(Aimag(C23)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 140,Real(C31,dp), "# C31" 
If (Abs(Aimag(C31)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 141,Real(C32,dp), "# C32" 
If (Abs(Aimag(C32)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 125,Real(BB11,dp), "# BB11" 
If (Abs(Aimag(BB11)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 126,Real(BB12,dp), "# BB12" 
If (Abs(Aimag(BB12)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 128,Real(BB21,dp), "# BB21" 
If (Abs(Aimag(BB21)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 129,Real(BB22,dp), "# BB22" 
If (Abs(Aimag(BB22)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 40,Real(Aa1,dp), "# Aa1" 
If (Abs(Aimag(Aa1)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 41,Real(Aa2,dp), "# Aa2" 
If (Abs(Aimag(Aa2)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 20,Real(Mu1,dp), "# Mu1" 
Write(io_L,104) 21,Real(Mu2,dp), "# Mu2" 
Write(io_L,104) 22,Real(MuDash,dp), "# MuDash" 
Write(io_L,104) 39,Real(Mub,dp), "# Mub" 
If (Abs(Aimag(Mub)).gt.0._dp) WriteNextBlock = .True. 
Write(io_L,104) 38,Real(Mu3,dp), "# Mu3" 
If (Abs(Aimag(Mu3)).gt.0._dp) WriteNextBlock = .True. 
If(WriteNextBlock) Then 
Write(io_L,106) "Block IMHMIX Q=",Q,"# (Renormalization Scale)" 
Write(io_L,104) 31,Aimag(Lam1), "# Lam1" 
Write(io_L,104) 33,Aimag(Lam3), "# Lam3" 
Write(io_L,104) 34,Aimag(Lam4), "# Lam4" 
Write(io_L,104) 32,Aimag(Lam2), "# Lam2" 
Write(io_L,104) 35,Aimag(Lam1Dash), "# Lam1Dash" 
Write(io_L,104) 36,Aimag(Lam2Dash), "# Lam2Dash" 
Write(io_L,104) 37,Aimag(Lam3Dash), "# Lam3Dash" 
Write(io_L,104) 42,Aimag(Aa3), "# Aa3" 
Write(io_L,104) 43,Aimag(Aa4), "# Aa4" 
Write(io_L,104) 62,Aimag(Y1d11), "# Y1d11" 
Write(io_L,104) 63,Aimag(Y1d12), "# Y1d12" 
Write(io_L,104) 64,Aimag(Y1d13), "# Y1d13" 
Write(io_L,104) 65,Aimag(Y1d21), "# Y1d21" 
Write(io_L,104) 66,Aimag(Y1d22), "# Y1d22" 
Write(io_L,104) 67,Aimag(Y1d23), "# Y1d23" 
Write(io_L,104) 77,Aimag(Y2d31), "# Y2d31" 
Write(io_L,104) 78,Aimag(Y2d32), "# Y2d32" 
Write(io_L,104) 79,Aimag(Y2d33), "# Y2d33" 
Write(io_L,104) 44,Aimag(Y1u11), "# Y1u11" 
Write(io_L,104) 45,Aimag(Y1u12), "# Y1u12" 
Write(io_L,104) 47,Aimag(Y1u21), "# Y1u21" 
Write(io_L,104) 48,Aimag(Y1u22), "# Y1u22" 
Write(io_L,104) 61,Aimag(Y2u33), "# Y2u33" 
Write(io_L,104) 80,Aimag(Y1e11), "# Y1e11" 
Write(io_L,104) 81,Aimag(Y1e12), "# Y1e12" 
Write(io_L,104) 83,Aimag(Y1e21), "# Y1e21" 
Write(io_L,104) 84,Aimag(Y1e22), "# Y1e22" 
Write(io_L,104) 97,Aimag(Y2e33), "# Y2e33" 
Write(io_L,104) 98,Aimag(Y1n11), "# Y1n11" 
Write(io_L,104) 99,Aimag(Y1n12), "# Y1n12" 
Write(io_L,104) 101,Aimag(Y1n21), "# Y1n21" 
Write(io_L,104) 102,Aimag(Y1n22), "# Y1n22" 
Write(io_L,104) 115,Aimag(Y2n33), "# Y2n33" 
Write(io_L,104) 136,Aimag(C13), "# C13" 
Write(io_L,104) 139,Aimag(C23), "# C23" 
Write(io_L,104) 140,Aimag(C31), "# C31" 
Write(io_L,104) 141,Aimag(C32), "# C32" 
Write(io_L,104) 125,Aimag(BB11), "# BB11" 
Write(io_L,104) 126,Aimag(BB12), "# BB12" 
Write(io_L,104) 128,Aimag(BB21), "# BB21" 
Write(io_L,104) 129,Aimag(BB22), "# BB22" 
Write(io_L,104) 40,Aimag(Aa1), "# Aa1" 
Write(io_L,104) 41,Aimag(Aa2), "# Aa2" 
Write(io_L,104) 39,Aimag(Mub), "# Mub" 
Write(io_L,104) 38,Aimag(Mu3), "# Mu3" 
End if 
WriteNextBlock = .false. 
If (OutputForMG) WriteNextBlock = .True. 
Write(io_L,106) "Block VEVS Q=",Q,"# (Renormalization Scale)" 
Write(io_L,104) 2,Real(v1,dp), "# v1" 
Write(io_L,104) 3,Real(v2,dp), "# v2" 
Write(io_L,104) 4,Real(v3,dp), "# v3" 
Write(io_L,104) 2,Real(v1,dp), "# v1" 
Write(io_L,104) 3,Real(v2,dp), "# v2" 
Write(io_L,104) 4,Real(v3,dp), "# v3" 
Write(io_L,104) 1,Real(Sqrt(v1**2 + v2**2),dp), "# v" 
If (WriteTreeLevelTadpoleParameters) Then 
If (HighScaleModel.Eq."LOW") Then 
WriteNextBlock = .false. 
If (OutputForMG) WriteNextBlock = .True. 
Write(io_L,106) "Block TREEHMIX Q=",Q,"# (Renormalization Scale)" 
Write(io_L,104) 20,Real(Mu1Tree,dp), "# Mu1" 
Write(io_L,104) 21,Real(Mu2Tree,dp), "# Mu2" 
Write(io_L,104) 22,Real(MuDashTree,dp), "# MuDash" 
WriteNextBlock = .false. 
If (OutputForMG) WriteNextBlock = .True. 
Write(io_L,106) "Block LOOPHMIX Q=",Q,"# (Renormalization Scale)" 
Write(io_L,104) 20,Real(Mu11L,dp), "# Mu1" 
Write(io_L,104) 21,Real(Mu21L,dp), "# Mu2" 
Write(io_L,104) 22,Real(MuDash1L,dp), "# MuDash" 
Else 
WriteNextBlock = .false. 
If (OutputForMG) WriteNextBlock = .True. 
Write(io_L,106) "Block TREEHMIX Q=",Q,"# (Renormalization Scale)" 
Write(io_L,104) 20,Real(Mu1Tree,dp), "# Mu1" 
Write(io_L,104) 21,Real(Mu2Tree,dp), "# Mu2" 
Write(io_L,104) 22,Real(MuDashTree,dp), "# MuDash" 
WriteNextBlock = .false. 
If (OutputForMG) WriteNextBlock = .True. 
Write(io_L,106) "Block LOOPHMIX Q=",Q,"# (Renormalization Scale)" 
Write(io_L,104) 20,Real(Mu11L,dp), "# Mu1" 
Write(io_L,104) 21,Real(Mu21L,dp), "# Mu2" 
Write(io_L,104) 22,Real(MuDash1L,dp), "# MuDash" 
End if 
End if 
If (WriteGUTvalues) Then 
Write(io_L,106) "Block GAUGEGUT Q=",M_GUT,"# (GUT scale)" 
Write(io_L,104) 1,Real(g1GUT,dp), "# g1" 
Write(io_L,104) 2,Real(g2GUT,dp), "# g2" 
Write(io_L,104) 3,Real(g3GUT,dp), "# g3" 
Write(io_L,106) "Block HMIXGUT Q=",M_GUT,"# (GUT scale)" 
Write(io_L,104) 31,Real(Lam1GUT,dp), "# Lam1" 
Write(io_L,104) 33,Real(Lam3GUT,dp), "# Lam3" 
Write(io_L,104) 34,Real(Lam4GUT,dp), "# Lam4" 
Write(io_L,104) 32,Real(Lam2GUT,dp), "# Lam2" 
Write(io_L,104) 35,Real(Lam1DashGUT,dp), "# Lam1Dash" 
Write(io_L,104) 36,Real(Lam2DashGUT,dp), "# Lam2Dash" 
Write(io_L,104) 37,Real(Lam3DashGUT,dp), "# Lam3Dash" 
Write(io_L,104) 42,Real(Aa3GUT,dp), "# Aa3" 
Write(io_L,104) 43,Real(Aa4GUT,dp), "# Aa4" 
Write(io_L,104) 62,Real(Y1d11GUT,dp), "# Y1d11" 
Write(io_L,104) 63,Real(Y1d12GUT,dp), "# Y1d12" 
Write(io_L,104) 64,Real(Y1d13GUT,dp), "# Y1d13" 
Write(io_L,104) 65,Real(Y1d21GUT,dp), "# Y1d21" 
Write(io_L,104) 66,Real(Y1d22GUT,dp), "# Y1d22" 
Write(io_L,104) 67,Real(Y1d23GUT,dp), "# Y1d23" 
Write(io_L,104) 77,Real(Y2d31GUT,dp), "# Y2d31" 
Write(io_L,104) 78,Real(Y2d32GUT,dp), "# Y2d32" 
Write(io_L,104) 79,Real(Y2d33GUT,dp), "# Y2d33" 
Write(io_L,104) 44,Real(Y1u11GUT,dp), "# Y1u11" 
Write(io_L,104) 45,Real(Y1u12GUT,dp), "# Y1u12" 
Write(io_L,104) 47,Real(Y1u21GUT,dp), "# Y1u21" 
Write(io_L,104) 48,Real(Y1u22GUT,dp), "# Y1u22" 
Write(io_L,104) 61,Real(Y2u33GUT,dp), "# Y2u33" 
Write(io_L,104) 80,Real(Y1e11GUT,dp), "# Y1e11" 
Write(io_L,104) 81,Real(Y1e12GUT,dp), "# Y1e12" 
Write(io_L,104) 83,Real(Y1e21GUT,dp), "# Y1e21" 
Write(io_L,104) 84,Real(Y1e22GUT,dp), "# Y1e22" 
Write(io_L,104) 97,Real(Y2e33GUT,dp), "# Y2e33" 
Write(io_L,104) 98,Real(Y1n11GUT,dp), "# Y1n11" 
Write(io_L,104) 99,Real(Y1n12GUT,dp), "# Y1n12" 
Write(io_L,104) 101,Real(Y1n21GUT,dp), "# Y1n21" 
Write(io_L,104) 102,Real(Y1n22GUT,dp), "# Y1n22" 
Write(io_L,104) 115,Real(Y2n33GUT,dp), "# Y2n33" 
Write(io_L,104) 136,Real(C13GUT,dp), "# C13" 
Write(io_L,104) 139,Real(C23GUT,dp), "# C23" 
Write(io_L,104) 140,Real(C31GUT,dp), "# C31" 
Write(io_L,104) 141,Real(C32GUT,dp), "# C32" 
Write(io_L,104) 125,Real(BB11GUT,dp), "# BB11" 
Write(io_L,104) 126,Real(BB12GUT,dp), "# BB12" 
Write(io_L,104) 128,Real(BB21GUT,dp), "# BB21" 
Write(io_L,104) 129,Real(BB22GUT,dp), "# BB22" 
Write(io_L,104) 40,Real(Aa1GUT,dp), "# Aa1" 
Write(io_L,104) 41,Real(Aa2GUT,dp), "# Aa2" 
Write(io_L,104) 20,Real(Mu1GUT,dp), "# Mu1" 
Write(io_L,104) 21,Real(Mu2GUT,dp), "# Mu2" 
Write(io_L,104) 22,Real(MuDashGUT,dp), "# MuDash" 
Write(io_L,104) 39,Real(MubGUT,dp), "# Mub" 
Write(io_L,104) 38,Real(Mu3GUT,dp), "# Mu3" 
End if 
 
MassLSP = 100000._dp 
Write(io_L,100) "Block MASS  # Mass spectrum"
Write(io_L,100) "#   PDG code      mass          particle" 
 Write(io_L,102) INT(Abs(PDGhh(1))),Mhh(1),"# "//Trim(NameParticlehh(1))// "" 
 Write(io_L,102) INT(Abs(PDGhh(2))),Mhh(2),"# "//Trim(NameParticlehh(2))// "" 
 Write(io_L,102) INT(Abs(PDGhh(3))),Mhh(3),"# "//Trim(NameParticlehh(3))// "" 
 Write(io_L,102) INT(Abs(PDGAh(2))),MAh(2),"# "//Trim(NameParticleAh(2))// "" 
 Write(io_L,102) INT(Abs(PDGAh(3))),MAh(3),"# "//Trim(NameParticleAh(3))// "" 
 Write(io_L,102) INT(Abs(PDGHm(2))),MHm(2),"# "//Trim(NameParticleHm(2))// "" 
If (OutputForMG)  Write(io_L,102) 22,0._dp,"# VP" 
 Write(io_L,102) 23,MVZ,"# VZ" 
If (OutputForMG)  Write(io_L,102) 21,0._dp,"# VG" 
 Write(io_L,102) 24,MVWm,"# VWm" 
 Write(io_L,102) INT(Abs(PDGFd(1))),MFd(1),"# "//Trim(NameParticleFd(1))// "" 
 Write(io_L,102) INT(Abs(PDGFd(2))),MFd(2),"# "//Trim(NameParticleFd(2))// "" 
 Write(io_L,102) INT(Abs(PDGFd(3))),MFd(3),"# "//Trim(NameParticleFd(3))// "" 
 Write(io_L,102) INT(Abs(PDGFu(1))),MFu(1),"# "//Trim(NameParticleFu(1))// "" 
 Write(io_L,102) INT(Abs(PDGFu(2))),MFu(2),"# "//Trim(NameParticleFu(2))// "" 
 Write(io_L,102) INT(Abs(PDGFu(3))),MFu(3),"# "//Trim(NameParticleFu(3))// "" 
 Write(io_L,102) INT(Abs(PDGFe(1))),MFe(1),"# "//Trim(NameParticleFe(1))// "" 
 Write(io_L,102) INT(Abs(PDGFe(2))),MFe(2),"# "//Trim(NameParticleFe(2))// "" 
 Write(io_L,102) INT(Abs(PDGFe(3))),MFe(3),"# "//Trim(NameParticleFe(3))// "" 
 Write(io_L,102) INT(Abs(PDGFv(1))),MFv(1),"# "//Trim(NameParticleFv(1))// "" 
 Write(io_L,102) INT(Abs(PDGFv(2))),MFv(2),"# "//Trim(NameParticleFv(2))// "" 
 Write(io_L,102) INT(Abs(PDGFv(3))),MFv(3),"# "//Trim(NameParticleFv(3))// "" 
 Write(io_L,102) INT(Abs(PDGFv(4))),MFv(4),"# "//Trim(NameParticleFv(4))// "" 
 Write(io_L,102) INT(Abs(PDGFv(5))),MFv(5),"# "//Trim(NameParticleFv(5))// "" 
 Write(io_L,102) INT(Abs(PDGFv(6))),MFv(6),"# "//Trim(NameParticleFv(6))// "" 

 
If (GetMassUncertainty) Then
Write(io_L,100) "Block DMASS  # Overall uncertainty"
 Write(io_L,102) INT(Abs(25)), Sqrt(mass_uncertainty_Q(1)**2+mass_uncertainty_Yt(1)**2),"# Mhh(1) " 
 Write(io_L,102) INT(Abs(26)), Sqrt(mass_uncertainty_Q(2)**2+mass_uncertainty_Yt(2)**2),"# Mhh(2) " 
 Write(io_L,102) INT(Abs(27)), Sqrt(mass_uncertainty_Q(3)**2+mass_uncertainty_Yt(3)**2),"# Mhh(3) " 
 Write(io_L,102) INT(Abs(35)), Sqrt(mass_uncertainty_Q(5)**2+mass_uncertainty_Yt(5)**2),"# MAh(2) " 
 Write(io_L,102) INT(Abs(36)), Sqrt(mass_uncertainty_Q(6)**2+mass_uncertainty_Yt(6)**2),"# MAh(3) " 
 Write(io_L,102) INT(Abs(-37)), Sqrt(mass_uncertainty_Q(8)**2+mass_uncertainty_Yt(8)**2),"# MHm(2) " 
Write(io_L,100) "Block DMASSQ  # Scale uncertainty"
 Write(io_L,102) INT(Abs(25)), mass_uncertainty_Q(1),"# Mhh(1) " 
 Write(io_L,102) INT(Abs(26)), mass_uncertainty_Q(2),"# Mhh(2) " 
 Write(io_L,102) INT(Abs(27)), mass_uncertainty_Q(3),"# Mhh(3) " 
 Write(io_L,102) INT(Abs(35)), mass_uncertainty_Q(5),"# MAh(2) " 
 Write(io_L,102) INT(Abs(36)), mass_uncertainty_Q(6),"# MAh(3) " 
 Write(io_L,102) INT(Abs(-37)), mass_uncertainty_Q(8),"# MHm(2) " 
Write(io_L,100) "Block DMASST  # Top Matching uncertainty"
 Write(io_L,102) INT(Abs(25)), mass_uncertainty_Yt(1),"# Mhh(1) " 
 Write(io_L,102) INT(Abs(26)), mass_uncertainty_Yt(2),"# Mhh(2) " 
 Write(io_L,102) INT(Abs(27)), mass_uncertainty_Yt(3),"# Mhh(3) " 
 Write(io_L,102) INT(Abs(35)), mass_uncertainty_Yt(5),"# MAh(2) " 
 Write(io_L,102) INT(Abs(36)), mass_uncertainty_Yt(6),"# MAh(3) " 
 Write(io_L,102) INT(Abs(-37)), mass_uncertainty_Yt(8),"# MHm(2) " 
End if
Write(io_L,106) "Block ZH_SCALARMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,ZH(1,1), "# ZH(1,1)" 
Write(io_L,107)1,2,ZH(1,2), "# ZH(1,2)" 
Write(io_L,107)1,3,ZH(1,3), "# ZH(1,3)" 
Write(io_L,107)2,1,ZH(2,1), "# ZH(2,1)" 
Write(io_L,107)2,2,ZH(2,2), "# ZH(2,2)" 
Write(io_L,107)2,3,ZH(2,3), "# ZH(2,3)" 
Write(io_L,107)3,1,ZH(3,1), "# ZH(3,1)" 
Write(io_L,107)3,2,ZH(3,2), "# ZH(3,2)" 
Write(io_L,107)3,3,ZH(3,3), "# ZH(3,3)" 
Write(io_L,106) "Block ZA Q=",Q,"# ()" 
Write(io_L,107)1,1,ZA(1,1), "# ZA(1,1)" 
Write(io_L,107)1,2,ZA(1,2), "# ZA(1,2)" 
Write(io_L,107)1,3,ZA(1,3), "# ZA(1,3)" 
Write(io_L,107)2,1,ZA(2,1), "# ZA(2,1)" 
Write(io_L,107)2,2,ZA(2,2), "# ZA(2,2)" 
Write(io_L,107)2,3,ZA(2,3), "# ZA(2,3)" 
Write(io_L,107)3,1,ZA(3,1), "# ZA(3,1)" 
Write(io_L,107)3,2,ZA(3,2), "# ZA(3,2)" 
Write(io_L,107)3,3,ZA(3,3), "# ZA(3,3)" 
Write(io_L,106) "Block ZP Q=",Q,"# ()" 
Write(io_L,107)1,1,Real(ZP(1,1),dp), "# Real(ZP(1,1),dp)" 
Write(io_L,107)1,2,Real(ZP(1,2),dp), "# Real(ZP(1,2),dp)" 
Write(io_L,107)2,1,Real(ZP(2,1),dp), "# Real(ZP(2,1),dp)" 
Write(io_L,107)2,2,Real(ZP(2,2),dp), "# Real(ZP(2,2),dp)" 
If ((MaxVal(Abs(AImag(ZP))).gt.0._dp).OR.(OutputForMG)) Then 
Write(io_L,106) "Block IMZP Q=",Q,"# ()" 
Write(io_L,107)1,1,Aimag(ZP(1,1)), "# Aimag(ZP(1,1))" 
Write(io_L,107)1,2,Aimag(ZP(1,2)), "# Aimag(ZP(1,2))" 
Write(io_L,107)2,1,Aimag(ZP(2,1)), "# Aimag(ZP(2,1))" 
Write(io_L,107)2,2,Aimag(ZP(2,2)), "# Aimag(ZP(2,2))" 
End If 

Write(io_L,106) "Block UDLMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Real(ZDL(1,1),dp), "# Real(ZDL(1,1),dp)" 
Write(io_L,107)1,2,Real(ZDL(1,2),dp), "# Real(ZDL(1,2),dp)" 
Write(io_L,107)1,3,Real(ZDL(1,3),dp), "# Real(ZDL(1,3),dp)" 
Write(io_L,107)2,1,Real(ZDL(2,1),dp), "# Real(ZDL(2,1),dp)" 
Write(io_L,107)2,2,Real(ZDL(2,2),dp), "# Real(ZDL(2,2),dp)" 
Write(io_L,107)2,3,Real(ZDL(2,3),dp), "# Real(ZDL(2,3),dp)" 
Write(io_L,107)3,1,Real(ZDL(3,1),dp), "# Real(ZDL(3,1),dp)" 
Write(io_L,107)3,2,Real(ZDL(3,2),dp), "# Real(ZDL(3,2),dp)" 
Write(io_L,107)3,3,Real(ZDL(3,3),dp), "# Real(ZDL(3,3),dp)" 
If ((MaxVal(Abs(AImag(ZDL))).gt.0._dp).OR.(OutputForMG)) Then 
Write(io_L,106) "Block IMUDLMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Aimag(ZDL(1,1)), "# Aimag(ZDL(1,1))" 
Write(io_L,107)1,2,Aimag(ZDL(1,2)), "# Aimag(ZDL(1,2))" 
Write(io_L,107)1,3,Aimag(ZDL(1,3)), "# Aimag(ZDL(1,3))" 
Write(io_L,107)2,1,Aimag(ZDL(2,1)), "# Aimag(ZDL(2,1))" 
Write(io_L,107)2,2,Aimag(ZDL(2,2)), "# Aimag(ZDL(2,2))" 
Write(io_L,107)2,3,Aimag(ZDL(2,3)), "# Aimag(ZDL(2,3))" 
Write(io_L,107)3,1,Aimag(ZDL(3,1)), "# Aimag(ZDL(3,1))" 
Write(io_L,107)3,2,Aimag(ZDL(3,2)), "# Aimag(ZDL(3,2))" 
Write(io_L,107)3,3,Aimag(ZDL(3,3)), "# Aimag(ZDL(3,3))" 
End If 

Write(io_L,106) "Block UDRMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Real(ZDR(1,1),dp), "# Real(ZDR(1,1),dp)" 
Write(io_L,107)1,2,Real(ZDR(1,2),dp), "# Real(ZDR(1,2),dp)" 
Write(io_L,107)1,3,Real(ZDR(1,3),dp), "# Real(ZDR(1,3),dp)" 
Write(io_L,107)2,1,Real(ZDR(2,1),dp), "# Real(ZDR(2,1),dp)" 
Write(io_L,107)2,2,Real(ZDR(2,2),dp), "# Real(ZDR(2,2),dp)" 
Write(io_L,107)2,3,Real(ZDR(2,3),dp), "# Real(ZDR(2,3),dp)" 
Write(io_L,107)3,1,Real(ZDR(3,1),dp), "# Real(ZDR(3,1),dp)" 
Write(io_L,107)3,2,Real(ZDR(3,2),dp), "# Real(ZDR(3,2),dp)" 
Write(io_L,107)3,3,Real(ZDR(3,3),dp), "# Real(ZDR(3,3),dp)" 
If ((MaxVal(Abs(AImag(ZDR))).gt.0._dp).OR.(OutputForMG)) Then 
Write(io_L,106) "Block IMUDRMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Aimag(ZDR(1,1)), "# Aimag(ZDR(1,1))" 
Write(io_L,107)1,2,Aimag(ZDR(1,2)), "# Aimag(ZDR(1,2))" 
Write(io_L,107)1,3,Aimag(ZDR(1,3)), "# Aimag(ZDR(1,3))" 
Write(io_L,107)2,1,Aimag(ZDR(2,1)), "# Aimag(ZDR(2,1))" 
Write(io_L,107)2,2,Aimag(ZDR(2,2)), "# Aimag(ZDR(2,2))" 
Write(io_L,107)2,3,Aimag(ZDR(2,3)), "# Aimag(ZDR(2,3))" 
Write(io_L,107)3,1,Aimag(ZDR(3,1)), "# Aimag(ZDR(3,1))" 
Write(io_L,107)3,2,Aimag(ZDR(3,2)), "# Aimag(ZDR(3,2))" 
Write(io_L,107)3,3,Aimag(ZDR(3,3)), "# Aimag(ZDR(3,3))" 
End If 

Write(io_L,106) "Block UULMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Real(ZUL(1,1),dp), "# Real(ZUL(1,1),dp)" 
Write(io_L,107)1,2,Real(ZUL(1,2),dp), "# Real(ZUL(1,2),dp)" 
Write(io_L,107)1,3,Real(ZUL(1,3),dp), "# Real(ZUL(1,3),dp)" 
Write(io_L,107)2,1,Real(ZUL(2,1),dp), "# Real(ZUL(2,1),dp)" 
Write(io_L,107)2,2,Real(ZUL(2,2),dp), "# Real(ZUL(2,2),dp)" 
Write(io_L,107)2,3,Real(ZUL(2,3),dp), "# Real(ZUL(2,3),dp)" 
Write(io_L,107)3,1,Real(ZUL(3,1),dp), "# Real(ZUL(3,1),dp)" 
Write(io_L,107)3,2,Real(ZUL(3,2),dp), "# Real(ZUL(3,2),dp)" 
Write(io_L,107)3,3,Real(ZUL(3,3),dp), "# Real(ZUL(3,3),dp)" 
If ((MaxVal(Abs(AImag(ZUL))).gt.0._dp).OR.(OutputForMG)) Then 
Write(io_L,106) "Block IMUULMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Aimag(ZUL(1,1)), "# Aimag(ZUL(1,1))" 
Write(io_L,107)1,2,Aimag(ZUL(1,2)), "# Aimag(ZUL(1,2))" 
Write(io_L,107)1,3,Aimag(ZUL(1,3)), "# Aimag(ZUL(1,3))" 
Write(io_L,107)2,1,Aimag(ZUL(2,1)), "# Aimag(ZUL(2,1))" 
Write(io_L,107)2,2,Aimag(ZUL(2,2)), "# Aimag(ZUL(2,2))" 
Write(io_L,107)2,3,Aimag(ZUL(2,3)), "# Aimag(ZUL(2,3))" 
Write(io_L,107)3,1,Aimag(ZUL(3,1)), "# Aimag(ZUL(3,1))" 
Write(io_L,107)3,2,Aimag(ZUL(3,2)), "# Aimag(ZUL(3,2))" 
Write(io_L,107)3,3,Aimag(ZUL(3,3)), "# Aimag(ZUL(3,3))" 
End If 

Write(io_L,106) "Block UURMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Real(ZUR(1,1),dp), "# Real(ZUR(1,1),dp)" 
Write(io_L,107)1,2,Real(ZUR(1,2),dp), "# Real(ZUR(1,2),dp)" 
Write(io_L,107)1,3,Real(ZUR(1,3),dp), "# Real(ZUR(1,3),dp)" 
Write(io_L,107)2,1,Real(ZUR(2,1),dp), "# Real(ZUR(2,1),dp)" 
Write(io_L,107)2,2,Real(ZUR(2,2),dp), "# Real(ZUR(2,2),dp)" 
Write(io_L,107)2,3,Real(ZUR(2,3),dp), "# Real(ZUR(2,3),dp)" 
Write(io_L,107)3,1,Real(ZUR(3,1),dp), "# Real(ZUR(3,1),dp)" 
Write(io_L,107)3,2,Real(ZUR(3,2),dp), "# Real(ZUR(3,2),dp)" 
Write(io_L,107)3,3,Real(ZUR(3,3),dp), "# Real(ZUR(3,3),dp)" 
If ((MaxVal(Abs(AImag(ZUR))).gt.0._dp).OR.(OutputForMG)) Then 
Write(io_L,106) "Block IMUURMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Aimag(ZUR(1,1)), "# Aimag(ZUR(1,1))" 
Write(io_L,107)1,2,Aimag(ZUR(1,2)), "# Aimag(ZUR(1,2))" 
Write(io_L,107)1,3,Aimag(ZUR(1,3)), "# Aimag(ZUR(1,3))" 
Write(io_L,107)2,1,Aimag(ZUR(2,1)), "# Aimag(ZUR(2,1))" 
Write(io_L,107)2,2,Aimag(ZUR(2,2)), "# Aimag(ZUR(2,2))" 
Write(io_L,107)2,3,Aimag(ZUR(2,3)), "# Aimag(ZUR(2,3))" 
Write(io_L,107)3,1,Aimag(ZUR(3,1)), "# Aimag(ZUR(3,1))" 
Write(io_L,107)3,2,Aimag(ZUR(3,2)), "# Aimag(ZUR(3,2))" 
Write(io_L,107)3,3,Aimag(ZUR(3,3)), "# Aimag(ZUR(3,3))" 
End If 

Write(io_L,106) "Block UELMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Real(ZEL(1,1),dp), "# Real(ZEL(1,1),dp)" 
Write(io_L,107)1,2,Real(ZEL(1,2),dp), "# Real(ZEL(1,2),dp)" 
Write(io_L,107)1,3,Real(ZEL(1,3),dp), "# Real(ZEL(1,3),dp)" 
Write(io_L,107)2,1,Real(ZEL(2,1),dp), "# Real(ZEL(2,1),dp)" 
Write(io_L,107)2,2,Real(ZEL(2,2),dp), "# Real(ZEL(2,2),dp)" 
Write(io_L,107)2,3,Real(ZEL(2,3),dp), "# Real(ZEL(2,3),dp)" 
Write(io_L,107)3,1,Real(ZEL(3,1),dp), "# Real(ZEL(3,1),dp)" 
Write(io_L,107)3,2,Real(ZEL(3,2),dp), "# Real(ZEL(3,2),dp)" 
Write(io_L,107)3,3,Real(ZEL(3,3),dp), "# Real(ZEL(3,3),dp)" 
If ((MaxVal(Abs(AImag(ZEL))).gt.0._dp).OR.(OutputForMG)) Then 
Write(io_L,106) "Block IMUELMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Aimag(ZEL(1,1)), "# Aimag(ZEL(1,1))" 
Write(io_L,107)1,2,Aimag(ZEL(1,2)), "# Aimag(ZEL(1,2))" 
Write(io_L,107)1,3,Aimag(ZEL(1,3)), "# Aimag(ZEL(1,3))" 
Write(io_L,107)2,1,Aimag(ZEL(2,1)), "# Aimag(ZEL(2,1))" 
Write(io_L,107)2,2,Aimag(ZEL(2,2)), "# Aimag(ZEL(2,2))" 
Write(io_L,107)2,3,Aimag(ZEL(2,3)), "# Aimag(ZEL(2,3))" 
Write(io_L,107)3,1,Aimag(ZEL(3,1)), "# Aimag(ZEL(3,1))" 
Write(io_L,107)3,2,Aimag(ZEL(3,2)), "# Aimag(ZEL(3,2))" 
Write(io_L,107)3,3,Aimag(ZEL(3,3)), "# Aimag(ZEL(3,3))" 
End If 

Write(io_L,106) "Block UERMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Real(ZER(1,1),dp), "# Real(ZER(1,1),dp)" 
Write(io_L,107)1,2,Real(ZER(1,2),dp), "# Real(ZER(1,2),dp)" 
Write(io_L,107)1,3,Real(ZER(1,3),dp), "# Real(ZER(1,3),dp)" 
Write(io_L,107)2,1,Real(ZER(2,1),dp), "# Real(ZER(2,1),dp)" 
Write(io_L,107)2,2,Real(ZER(2,2),dp), "# Real(ZER(2,2),dp)" 
Write(io_L,107)2,3,Real(ZER(2,3),dp), "# Real(ZER(2,3),dp)" 
Write(io_L,107)3,1,Real(ZER(3,1),dp), "# Real(ZER(3,1),dp)" 
Write(io_L,107)3,2,Real(ZER(3,2),dp), "# Real(ZER(3,2),dp)" 
Write(io_L,107)3,3,Real(ZER(3,3),dp), "# Real(ZER(3,3),dp)" 
If ((MaxVal(Abs(AImag(ZER))).gt.0._dp).OR.(OutputForMG)) Then 
Write(io_L,106) "Block IMUERMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Aimag(ZER(1,1)), "# Aimag(ZER(1,1))" 
Write(io_L,107)1,2,Aimag(ZER(1,2)), "# Aimag(ZER(1,2))" 
Write(io_L,107)1,3,Aimag(ZER(1,3)), "# Aimag(ZER(1,3))" 
Write(io_L,107)2,1,Aimag(ZER(2,1)), "# Aimag(ZER(2,1))" 
Write(io_L,107)2,2,Aimag(ZER(2,2)), "# Aimag(ZER(2,2))" 
Write(io_L,107)2,3,Aimag(ZER(2,3)), "# Aimag(ZER(2,3))" 
Write(io_L,107)3,1,Aimag(ZER(3,1)), "# Aimag(ZER(3,1))" 
Write(io_L,107)3,2,Aimag(ZER(3,2)), "# Aimag(ZER(3,2))" 
Write(io_L,107)3,3,Aimag(ZER(3,3)), "# Aimag(ZER(3,3))" 
End If 

Write(io_L,106) "Block ZM_SCALARMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Real(Vv(1,1),dp), "# Real(Vv(1,1),dp)" 
Write(io_L,107)1,2,Real(Vv(1,2),dp), "# Real(Vv(1,2),dp)" 
Write(io_L,107)1,3,Real(Vv(1,3),dp), "# Real(Vv(1,3),dp)" 
Write(io_L,107)1,4,Real(Vv(1,4),dp), "# Real(Vv(1,4),dp)" 
Write(io_L,107)1,5,Real(Vv(1,5),dp), "# Real(Vv(1,5),dp)" 
Write(io_L,107)1,6,Real(Vv(1,6),dp), "# Real(Vv(1,6),dp)" 
Write(io_L,107)2,1,Real(Vv(2,1),dp), "# Real(Vv(2,1),dp)" 
Write(io_L,107)2,2,Real(Vv(2,2),dp), "# Real(Vv(2,2),dp)" 
Write(io_L,107)2,3,Real(Vv(2,3),dp), "# Real(Vv(2,3),dp)" 
Write(io_L,107)2,4,Real(Vv(2,4),dp), "# Real(Vv(2,4),dp)" 
Write(io_L,107)2,5,Real(Vv(2,5),dp), "# Real(Vv(2,5),dp)" 
Write(io_L,107)2,6,Real(Vv(2,6),dp), "# Real(Vv(2,6),dp)" 
Write(io_L,107)3,1,Real(Vv(3,1),dp), "# Real(Vv(3,1),dp)" 
Write(io_L,107)3,2,Real(Vv(3,2),dp), "# Real(Vv(3,2),dp)" 
Write(io_L,107)3,3,Real(Vv(3,3),dp), "# Real(Vv(3,3),dp)" 
Write(io_L,107)3,4,Real(Vv(3,4),dp), "# Real(Vv(3,4),dp)" 
Write(io_L,107)3,5,Real(Vv(3,5),dp), "# Real(Vv(3,5),dp)" 
Write(io_L,107)3,6,Real(Vv(3,6),dp), "# Real(Vv(3,6),dp)" 
Write(io_L,107)4,1,Real(Vv(4,1),dp), "# Real(Vv(4,1),dp)" 
Write(io_L,107)4,2,Real(Vv(4,2),dp), "# Real(Vv(4,2),dp)" 
Write(io_L,107)4,3,Real(Vv(4,3),dp), "# Real(Vv(4,3),dp)" 
Write(io_L,107)4,4,Real(Vv(4,4),dp), "# Real(Vv(4,4),dp)" 
Write(io_L,107)4,5,Real(Vv(4,5),dp), "# Real(Vv(4,5),dp)" 
Write(io_L,107)4,6,Real(Vv(4,6),dp), "# Real(Vv(4,6),dp)" 
Write(io_L,107)5,1,Real(Vv(5,1),dp), "# Real(Vv(5,1),dp)" 
Write(io_L,107)5,2,Real(Vv(5,2),dp), "# Real(Vv(5,2),dp)" 
Write(io_L,107)5,3,Real(Vv(5,3),dp), "# Real(Vv(5,3),dp)" 
Write(io_L,107)5,4,Real(Vv(5,4),dp), "# Real(Vv(5,4),dp)" 
Write(io_L,107)5,5,Real(Vv(5,5),dp), "# Real(Vv(5,5),dp)" 
Write(io_L,107)5,6,Real(Vv(5,6),dp), "# Real(Vv(5,6),dp)" 
Write(io_L,107)6,1,Real(Vv(6,1),dp), "# Real(Vv(6,1),dp)" 
Write(io_L,107)6,2,Real(Vv(6,2),dp), "# Real(Vv(6,2),dp)" 
Write(io_L,107)6,3,Real(Vv(6,3),dp), "# Real(Vv(6,3),dp)" 
Write(io_L,107)6,4,Real(Vv(6,4),dp), "# Real(Vv(6,4),dp)" 
Write(io_L,107)6,5,Real(Vv(6,5),dp), "# Real(Vv(6,5),dp)" 
Write(io_L,107)6,6,Real(Vv(6,6),dp), "# Real(Vv(6,6),dp)" 
If ((MaxVal(Abs(AImag(Vv))).gt.0._dp).OR.(OutputForMG)) Then 
Write(io_L,106) "Block IMZM_SCALARMIX Q=",Q,"# ()" 
Write(io_L,107)1,1,Aimag(Vv(1,1)), "# Aimag(Vv(1,1))" 
Write(io_L,107)1,2,Aimag(Vv(1,2)), "# Aimag(Vv(1,2))" 
Write(io_L,107)1,3,Aimag(Vv(1,3)), "# Aimag(Vv(1,3))" 
Write(io_L,107)1,4,Aimag(Vv(1,4)), "# Aimag(Vv(1,4))" 
Write(io_L,107)1,5,Aimag(Vv(1,5)), "# Aimag(Vv(1,5))" 
Write(io_L,107)1,6,Aimag(Vv(1,6)), "# Aimag(Vv(1,6))" 
Write(io_L,107)2,1,Aimag(Vv(2,1)), "# Aimag(Vv(2,1))" 
Write(io_L,107)2,2,Aimag(Vv(2,2)), "# Aimag(Vv(2,2))" 
Write(io_L,107)2,3,Aimag(Vv(2,3)), "# Aimag(Vv(2,3))" 
Write(io_L,107)2,4,Aimag(Vv(2,4)), "# Aimag(Vv(2,4))" 
Write(io_L,107)2,5,Aimag(Vv(2,5)), "# Aimag(Vv(2,5))" 
Write(io_L,107)2,6,Aimag(Vv(2,6)), "# Aimag(Vv(2,6))" 
Write(io_L,107)3,1,Aimag(Vv(3,1)), "# Aimag(Vv(3,1))" 
Write(io_L,107)3,2,Aimag(Vv(3,2)), "# Aimag(Vv(3,2))" 
Write(io_L,107)3,3,Aimag(Vv(3,3)), "# Aimag(Vv(3,3))" 
Write(io_L,107)3,4,Aimag(Vv(3,4)), "# Aimag(Vv(3,4))" 
Write(io_L,107)3,5,Aimag(Vv(3,5)), "# Aimag(Vv(3,5))" 
Write(io_L,107)3,6,Aimag(Vv(3,6)), "# Aimag(Vv(3,6))" 
Write(io_L,107)4,1,Aimag(Vv(4,1)), "# Aimag(Vv(4,1))" 
Write(io_L,107)4,2,Aimag(Vv(4,2)), "# Aimag(Vv(4,2))" 
Write(io_L,107)4,3,Aimag(Vv(4,3)), "# Aimag(Vv(4,3))" 
Write(io_L,107)4,4,Aimag(Vv(4,4)), "# Aimag(Vv(4,4))" 
Write(io_L,107)4,5,Aimag(Vv(4,5)), "# Aimag(Vv(4,5))" 
Write(io_L,107)4,6,Aimag(Vv(4,6)), "# Aimag(Vv(4,6))" 
Write(io_L,107)5,1,Aimag(Vv(5,1)), "# Aimag(Vv(5,1))" 
Write(io_L,107)5,2,Aimag(Vv(5,2)), "# Aimag(Vv(5,2))" 
Write(io_L,107)5,3,Aimag(Vv(5,3)), "# Aimag(Vv(5,3))" 
Write(io_L,107)5,4,Aimag(Vv(5,4)), "# Aimag(Vv(5,4))" 
Write(io_L,107)5,5,Aimag(Vv(5,5)), "# Aimag(Vv(5,5))" 
Write(io_L,107)5,6,Aimag(Vv(5,6)), "# Aimag(Vv(5,6))" 
Write(io_L,107)6,1,Aimag(Vv(6,1)), "# Aimag(Vv(6,1))" 
Write(io_L,107)6,2,Aimag(Vv(6,2)), "# Aimag(Vv(6,2))" 
Write(io_L,107)6,3,Aimag(Vv(6,3)), "# Aimag(Vv(6,3))" 
Write(io_L,107)6,4,Aimag(Vv(6,4)), "# Aimag(Vv(6,4))" 
Write(io_L,107)6,5,Aimag(Vv(6,5)), "# Aimag(Vv(6,5))" 
Write(io_L,107)6,6,Aimag(Vv(6,6)), "# Aimag(Vv(6,6))" 
End If 

Write(io_L,106) "Block ZZ Q=",Q,"# ()" 
Write(io_L,107)1,1,Real(ZZ(1,1),dp), "# Real(ZZ(1,1),dp)" 
Write(io_L,107)1,2,Real(ZZ(1,2),dp), "# Real(ZZ(1,2),dp)" 
Write(io_L,107)2,1,Real(ZZ(2,1),dp), "# Real(ZZ(2,1),dp)" 
Write(io_L,107)2,2,Real(ZZ(2,2),dp), "# Real(ZZ(2,2),dp)" 
If ((MaxVal(Abs(AImag(ZZ))).gt.0._dp).OR.(OutputForMG)) Then 
Write(io_L,106) "Block IMZZ Q=",Q,"# ()" 
Write(io_L,107)1,1,Aimag(ZZ(1,1)), "# Aimag(ZZ(1,1))" 
Write(io_L,107)1,2,Aimag(ZZ(1,2)), "# Aimag(ZZ(1,2))" 
Write(io_L,107)2,1,Aimag(ZZ(2,1)), "# Aimag(ZZ(2,1))" 
Write(io_L,107)2,2,Aimag(ZZ(2,2)), "# Aimag(ZZ(2,2))" 
End If 

Write(io_L,100) "Block SPheno # SPheno internal parameters " 
Write(io_L,102) 1,1.*ErrorLevel,"# ErrorLevel"

If (SPA_convention) Then
Write(io_L,102) 2,1.,"# SPA_conventions"
Else
Write(io_L,102) 2,0.,"# SPA_conventions"
End if

If (L_BR) Then
Write(io_L,102) 11,1.,"# Branching ratios"
Else
Write(io_L,102) 11,0.,"# Branching ratios"
End if


If (Enable3BDecays) Then
Write(io_L,102) 13,1.,"# 3 Body decays"
Else
Write(io_L,102) 13,0.,"# 3 Body decays"
End if


Write(io_L,102) 31,m_GUT,"# GUT scale"

Write(io_L,102) 33,Q,"# Renormalization scale"

Write(io_L,102) 34,delta_mass,"# Precision"

Write(io_L,102) 35,1.*n_run,"# Iterations"


If (TwoLoopRGE) Then
Write(io_L,102) 38,2.,"# RGE level"
Else
Write(io_L,102) 38,1.,"# RGE level"
End if

Write(io_L,102) 40,Alpha,"# Alpha"

Write(io_L,102) 41,gamZ,"# Gamma_Z"

Write(io_L,102) 42,gamW,"# Gamma_W"

If (RotateNegativeFermionMasses) Then
Write(io_L,102) 50,1.,"# Rotate negative fermion masses"
Else
Write(io_L,102) 50,0.,"# Rotate negative fermion masses"
End if


If (SwitchToSCKM) Then
Write(io_L,102) 51,1.,"# Switch to SCKM matrix"
Else
Write(io_L,102) 51,0.,"# Switch to SCKM matrix"
End if


If (IgnoreNegativeMasses) Then
Write(io_L,102) 52,1.,"# Ignore negative masses"
Else
Write(io_L,102) 52,0.,"# Ignore negative masses"
End if


If (IgnoreNegativeMassesMZ) Then
Write(io_L,102) 53,1.,"# Ignore negative masses at MZ"
Else
Write(io_L,102) 53,0.,"# Ignore negative masses at MZ"
End if


If (CalculateOneLoopMasses) Then
Write(io_L,102) 55,1.,"# Calculate one loop masses"
Else
Write(io_L,102) 55,0.,"# Calculate one loop masses"
End if


If (CalculateTwoLoopHiggsMasses) Then
Write(io_L,102) 56,1.,"# Calculate two-loop Higgs masses"
Else
Write(io_L,102) 56,0.,"# Calculate two-loop Higgs masses"
End if

If (CalculateLowEnergy) Then
Write(io_L,102) 57,1.,"# Calculate low energy"
Else
Write(io_L,102) 57,0.,"# Calculate low energy"
End if

If (KineticMixing) Then
Write(io_L,102) 60,1.,"# Include kinetic mixing"
Else
Write(io_L,102) 60,0.,"# Include kinetic mixing"
End if

Write(io_L,102) 65,1.*SolutionTadpoleNr,"# Solution of tadpole equation"
 


 
If(Write_WHIZARD) Call WriteWHIZARD 
 
If(Write_HiggsBounds) Call WriteHiggsBounds 
 
If(Write_HiggsBounds5) Call WriteHiggsBounds5 
 
If(Write_WCXF) Call WriteWCXF
 

 
If (L_BR) Then 
Write(io_L,100) "Block HiggsLHC7 # Higgs production cross section at LHC7 [pb] (rescaled SM values from HXSWG) " 
Do i1=1,3
CurrentPDG2(1) = Abs(PDGhh(i1)) 
If (CS_Higgs_LHC(1,i1,1).gt.0._dp) Write(io_L,119) 1, CurrentPDG2(1), CS_Higgs_LHC(1,i1,1), " # Gluon fusion " 
If (CS_Higgs_LHC(1,i1,2).gt.0._dp) Write(io_L,119) 2, CurrentPDG2(1), CS_Higgs_LHC(1,i1,2), " # Vector boson fusion " 
If (CS_Higgs_LHC(1,i1,3).gt.0._dp) Write(io_L,119) 3, CurrentPDG2(1), CS_Higgs_LHC(1,i1,3), " # W-H production " 
If (CS_Higgs_LHC(1,i1,4).gt.0._dp) Write(io_L,119) 4, CurrentPDG2(1), CS_Higgs_LHC(1,i1,4), " # Z-H production " 
If (CS_Higgs_LHC(1,i1,5).gt.0._dp) Write(io_L,119) 5, CurrentPDG2(1), CS_Higgs_LHC(1,i1,5), " # t-t-H production " 
End Do 
Do i1=2,3
CurrentPDG2(1) = Abs(PDGAh(i1)) 
If (CS_PHiggs_LHC(1,i1,1).gt.0._dp) Write(io_L,119) 1, CurrentPDG2(1), CS_PHiggs_LHC(1,i1,1), " # Gluon fusion " 
If (CS_PHiggs_LHC(1,i1,2).gt.0._dp) Write(io_L,119) 2, CurrentPDG2(1), CS_PHiggs_LHC(1,i1,2), " # Vector boson fusion " 
If (CS_PHiggs_LHC(1,i1,3).gt.0._dp) Write(io_L,119) 3, CurrentPDG2(1), CS_PHiggs_LHC(1,i1,3), " # W-H production " 
If (CS_PHiggs_LHC(1,i1,4).gt.0._dp) Write(io_L,119) 4, CurrentPDG2(1), CS_PHiggs_LHC(1,i1,4), " # Z-H production " 
If (CS_PHiggs_LHC(1,i1,5).gt.0._dp) Write(io_L,119) 5, CurrentPDG2(1), CS_PHiggs_LHC(1,i1,5), " # t-t-H production " 
End Do 
Write(io_L,100) "Block HiggsLHC8 # Higgs production cross section at LHC8 [pb] (rescaled SM values from HXSWG) " 
Do i1=1,3
CurrentPDG2(1) = Abs(PDGhh(i1)) 
If (CS_Higgs_LHC(2,i1,1).gt.0._dp) Write(io_L,119) 1, CurrentPDG2(1), CS_Higgs_LHC(2,i1,1), " # Gluon fusion " 
If (CS_Higgs_LHC(2,i1,2).gt.0._dp) Write(io_L,119) 2, CurrentPDG2(1), CS_Higgs_LHC(2,i1,2), " # Vector boson fusion " 
If (CS_Higgs_LHC(2,i1,3).gt.0._dp) Write(io_L,119) 3, CurrentPDG2(1), CS_Higgs_LHC(2,i1,3), " # W-H production " 
If (CS_Higgs_LHC(2,i1,4).gt.0._dp) Write(io_L,119) 4, CurrentPDG2(1), CS_Higgs_LHC(2,i1,4), " # Z-H production " 
If (CS_Higgs_LHC(2,i1,5).gt.0._dp) Write(io_L,119) 5, CurrentPDG2(1), CS_Higgs_LHC(2,i1,5), " # t-t-H production " 
End Do 
Do i1=2,3
CurrentPDG2(1) = Abs(PDGAh(i1)) 
If (CS_PHiggs_LHC(2,i1,1).gt.0._dp) Write(io_L,119) 1, CurrentPDG2(1), CS_PHiggs_LHC(2,i1,1), " # Gluon fusion " 
If (CS_PHiggs_LHC(2,i1,2).gt.0._dp) Write(io_L,119) 2, CurrentPDG2(1), CS_PHiggs_LHC(2,i1,2), " # Vector boson fusion " 
If (CS_PHiggs_LHC(2,i1,3).gt.0._dp) Write(io_L,119) 3, CurrentPDG2(1), CS_PHiggs_LHC(2,i1,3), " # W-H production " 
If (CS_PHiggs_LHC(2,i1,4).gt.0._dp) Write(io_L,119) 4, CurrentPDG2(1), CS_PHiggs_LHC(2,i1,4), " # Z-H production " 
If (CS_PHiggs_LHC(2,i1,5).gt.0._dp) Write(io_L,119) 5, CurrentPDG2(1), CS_PHiggs_LHC(2,i1,5), " # t-t-H production " 
End Do 
Write(io_L,100) "Block HiggsLHC13 # Higgs production cross section at LHC13 [pb] (rescaled SM values from SusHI 1.5.0) " 
Do i1=1,3
CurrentPDG2(1) = Abs(PDGhh(i1)) 
If (CS_Higgs_LHC(3,i1,1).gt.0._dp) Write(io_L,119) 1, CurrentPDG2(1), CS_Higgs_LHC(3,i1,1), " # Gluon fusion " 
If (CS_Higgs_LHC(3,i1,2).gt.0._dp) Write(io_L,119) 2, CurrentPDG2(1), CS_Higgs_LHC(3,i1,2), " # Vector boson fusion " 
If (CS_Higgs_LHC(3,i1,3).gt.0._dp) Write(io_L,119) 3, CurrentPDG2(1), CS_Higgs_LHC(3,i1,3), " # W-H production " 
If (CS_Higgs_LHC(3,i1,4).gt.0._dp) Write(io_L,119) 4, CurrentPDG2(1), CS_Higgs_LHC(3,i1,4), " # Z-H production " 
If (CS_Higgs_LHC(3,i1,5).gt.0._dp) Write(io_L,119) 5, CurrentPDG2(1), CS_Higgs_LHC(3,i1,5), " # t-t-H production " 
End Do 
Do i1=2,3
CurrentPDG2(1) = Abs(PDGAh(i1)) 
If (CS_PHiggs_LHC(3,i1,1).gt.0._dp) Write(io_L,119) 1, CurrentPDG2(1), CS_PHiggs_LHC(3,i1,1), " # Gluon fusion " 
If (CS_PHiggs_LHC(3,i1,2).gt.0._dp) Write(io_L,119) 2, CurrentPDG2(1), CS_PHiggs_LHC(3,i1,2), " # Vector boson fusion " 
If (CS_PHiggs_LHC(3,i1,3).gt.0._dp) Write(io_L,119) 3, CurrentPDG2(1), CS_PHiggs_LHC(3,i1,3), " # W-H production " 
If (CS_PHiggs_LHC(3,i1,4).gt.0._dp) Write(io_L,119) 4, CurrentPDG2(1), CS_PHiggs_LHC(3,i1,4), " # Z-H production " 
If (CS_PHiggs_LHC(3,i1,5).gt.0._dp) Write(io_L,119) 5, CurrentPDG2(1), CS_PHiggs_LHC(3,i1,5), " # t-t-H production " 
End Do 
Write(io_L,100) "Block HiggsLHC14 # Higgs production cross section at LHC14 [pb] (rescaled SM values from SusHI 1.5.0)" 
Do i1=1,3
CurrentPDG2(1) = Abs(PDGhh(i1)) 
If (CS_Higgs_LHC(4,i1,1).gt.0._dp) Write(io_L,119) 1, CurrentPDG2(1), CS_Higgs_LHC(4,i1,1), " # Gluon fusion " 
If (CS_Higgs_LHC(4,i1,2).gt.0._dp) Write(io_L,119) 2, CurrentPDG2(1), CS_Higgs_LHC(4,i1,2), " # Vector boson fusion " 
If (CS_Higgs_LHC(4,i1,3).gt.0._dp) Write(io_L,119) 3, CurrentPDG2(1), CS_Higgs_LHC(4,i1,3), " # W-H production " 
If (CS_Higgs_LHC(4,i1,4).gt.0._dp) Write(io_L,119) 4, CurrentPDG2(1), CS_Higgs_LHC(4,i1,4), " # Z-H production " 
If (CS_Higgs_LHC(4,i1,5).gt.0._dp) Write(io_L,119) 5, CurrentPDG2(1), CS_Higgs_LHC(4,i1,5), " # t-t-H production " 
End Do 
Do i1=2,3
CurrentPDG2(1) = Abs(PDGAh(i1)) 
If (CS_PHiggs_LHC(4,i1,1).gt.0._dp) Write(io_L,119) 1, CurrentPDG2(1), CS_PHiggs_LHC(4,i1,1), " # Gluon fusion " 
If (CS_PHiggs_LHC(4,i1,2).gt.0._dp) Write(io_L,119) 2, CurrentPDG2(1), CS_PHiggs_LHC(4,i1,2), " # Vector boson fusion " 
If (CS_PHiggs_LHC(4,i1,3).gt.0._dp) Write(io_L,119) 3, CurrentPDG2(1), CS_PHiggs_LHC(4,i1,3), " # W-H production " 
If (CS_PHiggs_LHC(4,i1,4).gt.0._dp) Write(io_L,119) 4, CurrentPDG2(1), CS_PHiggs_LHC(4,i1,4), " # Z-H production " 
If (CS_PHiggs_LHC(4,i1,5).gt.0._dp) Write(io_L,119) 5, CurrentPDG2(1), CS_PHiggs_LHC(4,i1,5), " # t-t-H production " 
End Do 
Write(io_L,100) "Block HiggsFCC100 # Higgs production cross section at FCC-pp [pb] (rescaled SM values from SusHI 1.5.0)" 
Do i1=1,3
CurrentPDG2(1) = Abs(PDGhh(i1)) 
If (CS_Higgs_LHC(5,i1,1).gt.0._dp) Write(io_L,119) 1, CurrentPDG2(1), CS_Higgs_LHC(5,i1,1), " # Gluon fusion " 
If (CS_Higgs_LHC(5,i1,2).gt.0._dp) Write(io_L,119) 2, CurrentPDG2(1), CS_Higgs_LHC(5,i1,2), " # Vector boson fusion " 
If (CS_Higgs_LHC(5,i1,3).gt.0._dp) Write(io_L,119) 3, CurrentPDG2(1), CS_Higgs_LHC(5,i1,3), " # W-H production " 
If (CS_Higgs_LHC(5,i1,4).gt.0._dp) Write(io_L,119) 4, CurrentPDG2(1), CS_Higgs_LHC(5,i1,4), " # Z-H production " 
If (CS_Higgs_LHC(5,i1,5).gt.0._dp) Write(io_L,119) 5, CurrentPDG2(1), CS_Higgs_LHC(5,i1,5), " # t-t-H production " 
End Do 
Do i1=2,3
CurrentPDG2(1) = Abs(PDGAh(i1)) 
If (CS_PHiggs_LHC(5,i1,1).gt.0._dp) Write(io_L,119) 1, CurrentPDG2(1), CS_PHiggs_LHC(5,i1,1), " # Gluon fusion " 
If (CS_PHiggs_LHC(5,i1,2).gt.0._dp) Write(io_L,119) 2, CurrentPDG2(1), CS_PHiggs_LHC(5,i1,2), " # Vector boson fusion " 
If (CS_PHiggs_LHC(5,i1,3).gt.0._dp) Write(io_L,119) 3, CurrentPDG2(1), CS_PHiggs_LHC(5,i1,3), " # W-H production " 
If (CS_PHiggs_LHC(5,i1,4).gt.0._dp) Write(io_L,119) 4, CurrentPDG2(1), CS_PHiggs_LHC(5,i1,4), " # Z-H production " 
If (CS_PHiggs_LHC(5,i1,5).gt.0._dp) Write(io_L,119) 5, CurrentPDG2(1), CS_PHiggs_LHC(5,i1,5), " # t-t-H production " 
End Do 
If (WriteEffHiggsCouplingRatios) Then 
Write(io_L,100) "Block HiggsCouplingsFermions # " 
Write(io_L,1101) rHB_S_S_Fd(1,3),rHB_S_P_Fd(1,3), 3 ,25,5,5, " # h_1 b b coupling " 
Write(io_L,1101) rHB_S_S_Fd(1,2),rHB_S_P_Fd(1,2), 3 ,25,3,3, " # h_1 s s coupling " 
Write(io_L,1101) rHB_S_S_Fu(1,3),rHB_S_P_Fu(1,3), 3 ,25,6,6, " # h_1 t t coupling  " 
Write(io_L,1101) rHB_S_S_Fu(1,2),rHB_S_P_Fu(1,2),3 ,25,4,4, " # h_1 c c coupling " 
Write(io_L,1101) rHB_S_S_Fe(1,3),rHB_S_P_Fe(1,3), 3 ,25,15,15, " # h_1 tau tau coupling " 
Write(io_L,1101) rHB_S_S_Fe(1,2),rHB_S_P_Fe(1,2), 3 ,25,13,13, " # h_1 mu mu coupling  " 
Write(io_L,1101) rHB_S_S_Fd(2,3),rHB_S_P_Fd(2,3), 3 ,26,5,5, " # h_2 b b coupling " 
Write(io_L,1101) rHB_S_S_Fd(2,2),rHB_S_P_Fd(2,2), 3 ,26,3,3, " # h_2 s s coupling " 
Write(io_L,1101) rHB_S_S_Fu(2,3),rHB_S_P_Fu(2,3), 3 ,26,6,6, " # h_2 t t coupling  " 
Write(io_L,1101) rHB_S_S_Fu(2,2),rHB_S_P_Fu(2,2),3 ,26,4,4, " # h_2 c c coupling " 
Write(io_L,1101) rHB_S_S_Fe(2,3),rHB_S_P_Fe(2,3), 3 ,26,15,15, " # h_2 tau tau coupling " 
Write(io_L,1101) rHB_S_S_Fe(2,2),rHB_S_P_Fe(2,2), 3 ,26,13,13, " # h_2 mu mu coupling  " 
Write(io_L,1101) rHB_S_S_Fd(3,3),rHB_S_P_Fd(3,3), 3 ,27,5,5, " # h_3 b b coupling " 
Write(io_L,1101) rHB_S_S_Fd(3,2),rHB_S_P_Fd(3,2), 3 ,27,3,3, " # h_3 s s coupling " 
Write(io_L,1101) rHB_S_S_Fu(3,3),rHB_S_P_Fu(3,3), 3 ,27,6,6, " # h_3 t t coupling  " 
Write(io_L,1101) rHB_S_S_Fu(3,2),rHB_S_P_Fu(3,2),3 ,27,4,4, " # h_3 c c coupling " 
Write(io_L,1101) rHB_S_S_Fe(3,3),rHB_S_P_Fe(3,3), 3 ,27,15,15, " # h_3 tau tau coupling " 
Write(io_L,1101) rHB_S_S_Fe(3,2),rHB_S_P_Fe(3,2), 3 ,27,13,13, " # h_3 mu mu coupling  " 
Write(io_L,1101) rHB_P_S_Fd(2,3),rHB_P_P_Fd(2,3), 3 ,35,5,5, " # A_2 b b coupling " 
Write(io_L,1101) rHB_P_S_Fd(2,2),rHB_P_P_Fd(2,2), 3 ,35,3,3, " # A_2 s s coupling " 
Write(io_L,1101) rHB_P_S_Fu(2,3),rHB_P_P_Fu(2,3), 3 ,35,6,6, " # A_2 t t coupling "  
Write(io_L,1101) rHB_P_S_Fu(2,2),rHB_P_P_Fu(2,2), 3 ,35,4,4, " # A_2 c c coupling " 
Write(io_L,1101) rHB_P_S_Fe(2,3),rHB_P_P_Fe(2,3), 3 ,35,15,15, " # A_2 tau tau coupling " 
Write(io_L,1101) rHB_P_S_Fe(2,2),rHB_P_P_Fe(2,2), 3 ,35,13,13, " # A_2 mu mu coupling " 
Write(io_L,1101) rHB_P_S_Fd(3,3),rHB_P_P_Fd(3,3), 3 ,36,5,5, " # A_3 b b coupling " 
Write(io_L,1101) rHB_P_S_Fd(3,2),rHB_P_P_Fd(3,2), 3 ,36,3,3, " # A_3 s s coupling " 
Write(io_L,1101) rHB_P_S_Fu(3,3),rHB_P_P_Fu(3,3), 3 ,36,6,6, " # A_3 t t coupling "  
Write(io_L,1101) rHB_P_S_Fu(3,2),rHB_P_P_Fu(3,2), 3 ,36,4,4, " # A_3 c c coupling " 
Write(io_L,1101) rHB_P_S_Fe(3,3),rHB_P_P_Fe(3,3), 3 ,36,15,15, " # A_3 tau tau coupling " 
Write(io_L,1101) rHB_P_S_Fe(3,2),rHB_P_P_Fe(3,2), 3 ,36,13,13, " # A_3 mu mu coupling " 
Write(io_L,100) "Block HiggsCouplingsBosons # " 
Write(io_L,1102) rHB_S_VWm(1),3 ,25,24,24, " # h_1 W W coupling " 
Write(io_L,1102) rHB_S_VZ(1),3 ,25,23,23, " # h_1 Z Z coupling  " 
Write(io_L,1102) 0._dp ,3 ,25,23,22, " # h_1 Z gamma coupling " 
Write(io_L,1102) Real(ratioPP(1),dp),3 ,25,22,22, " # h_1 gamma gamma coupling " 
Write(io_L,1102) Real(ratioGG(1),dp),3 ,25,21,21, " # h_1 g g coupling " 
Write(io_L,1103) 0._dp,4 ,25,21,21,23, " # h_1 g g Z coupling " 
Write(io_L,1102) rHB_S_VWm(2),3 ,26,24,24, " # h_2 W W coupling " 
Write(io_L,1102) rHB_S_VZ(2),3 ,26,23,23, " # h_2 Z Z coupling  " 
Write(io_L,1102) 0._dp ,3 ,26,23,22, " # h_2 Z gamma coupling " 
Write(io_L,1102) Real(ratioPP(2),dp),3 ,26,22,22, " # h_2 gamma gamma coupling " 
Write(io_L,1102) Real(ratioGG(2),dp),3 ,26,21,21, " # h_2 g g coupling " 
Write(io_L,1103) 0._dp,4 ,26,21,21,23, " # h_2 g g Z coupling " 
Write(io_L,1102) rHB_S_VWm(3),3 ,27,24,24, " # h_3 W W coupling " 
Write(io_L,1102) rHB_S_VZ(3),3 ,27,23,23, " # h_3 Z Z coupling  " 
Write(io_L,1102) 0._dp ,3 ,27,23,22, " # h_3 Z gamma coupling " 
Write(io_L,1102) Real(ratioPP(3),dp),3 ,27,22,22, " # h_3 gamma gamma coupling " 
Write(io_L,1102) Real(ratioGG(3),dp),3 ,27,21,21, " # h_3 g g coupling " 
Write(io_L,1103) 0._dp,4 ,27,21,21,23, " # h_3 g g Z coupling " 
Write(io_L,1102) rHB_P_VWm(2),3 ,35,24,24, " # A_2 W W coupling " 
Write(io_L,1102) rHB_P_VZ(2),3 ,35,23,23, " # A_2 Z Z coupling " 
Write(io_L,1102) 0._dp ,3 ,35,23,22, " # A_2 Z gamma coupling " 
Write(io_L,1102) Real(ratioPPP(2),dp),3 ,35,22,22, " # A_2 gamma gamma coupling " 
Write(io_L,1102) Real(ratioPGG(2),dp),3 ,35,21,21, " # A_2 g g coupling " 
Write(io_L,1103) 0._dp,4 ,35,21,21,23, " # A_2 g g Z coupling " 
Write(io_L,1102) rHB_P_VWm(3),3 ,36,24,24, " # A_3 W W coupling " 
Write(io_L,1102) rHB_P_VZ(3),3 ,36,23,23, " # A_3 Z Z coupling " 
Write(io_L,1102) 0._dp ,3 ,36,23,22, " # A_3 Z gamma coupling " 
Write(io_L,1102) Real(ratioPPP(3),dp),3 ,36,22,22, " # A_3 gamma gamma coupling " 
Write(io_L,1102) Real(ratioPGG(3),dp),3 ,36,21,21, " # A_3 g g coupling " 
Write(io_L,1103) 0._dp,4 ,36,21,21,23, " # A_3 g g Z coupling " 
Write(io_L,1102) Real(CPL_H_H_Z(1,1), dp),3 ,25,25,23, " # h_1 h_1 Z coupling  "
Write(io_L,1102) Real(CPL_H_H_Z(1,2), dp),3 ,25,26,23, " # h_1 h_2 Z coupling  "
Write(io_L,1102) Real(CPL_H_H_Z(1,3), dp),3 ,25,27,23, " # h_1 h_3 Z coupling  "
Write(io_L,1102) Real(CPL_A_H_Z(2,1), dp),3 ,25,35,23, " # h_1 A_2 Z coupling " 
Write(io_L,1102) Real(CPL_A_H_Z(3,1), dp),3 ,25,36,23, " # h_1 A_3 Z coupling " 
Write(io_L,1102) Real(CPL_H_H_Z(2,1), dp),3 ,26,25,23, " # h_2 h_1 Z coupling  "
Write(io_L,1102) Real(CPL_H_H_Z(2,2), dp),3 ,26,26,23, " # h_2 h_2 Z coupling  "
Write(io_L,1102) Real(CPL_H_H_Z(2,3), dp),3 ,26,27,23, " # h_2 h_3 Z coupling  "
Write(io_L,1102) Real(CPL_A_H_Z(2,2), dp),3 ,26,35,23, " # h_2 A_2 Z coupling " 
Write(io_L,1102) Real(CPL_A_H_Z(3,2), dp),3 ,26,36,23, " # h_2 A_3 Z coupling " 
Write(io_L,1102) Real(CPL_H_H_Z(3,1), dp),3 ,27,25,23, " # h_3 h_1 Z coupling  "
Write(io_L,1102) Real(CPL_H_H_Z(3,2), dp),3 ,27,26,23, " # h_3 h_2 Z coupling  "
Write(io_L,1102) Real(CPL_H_H_Z(3,3), dp),3 ,27,27,23, " # h_3 h_3 Z coupling  "
Write(io_L,1102) Real(CPL_A_H_Z(2,3), dp),3 ,27,35,23, " # h_3 A_2 Z coupling " 
Write(io_L,1102) Real(CPL_A_H_Z(3,3), dp),3 ,27,36,23, " # h_3 A_3 Z coupling " 
Write(io_L,1102) Real(CPL_A_A_Z(2,2), dp),3 ,35,35,23, " # A_2 A_2 Z coupling " 
Write(io_L,1102) Real(CPL_A_A_Z(3,2), dp),3 ,35,36,23, " # A_2 A_3 Z coupling " 
Write(io_L,1102) Real(CPL_A_A_Z(2,3), dp),3 ,36,35,23, " # A_3 A_2 Z coupling " 
Write(io_L,1102) Real(CPL_A_A_Z(3,3), dp),3 ,36,36,23, " # A_3 A_3 Z coupling " 
End If 

 
If (WriteHiggsDiphotonLoopContributions) Then 
Write(io_L,100) "Block HPPloops # Loop contributions to H-Photon-Photon coupling " 
Do i1=1,3
CurrentPDG2(1) = Abs(PDGhh(i1)) 
Do i2=1,1
CurrentPDG2(2) = Abs(PDGVWm) 
Write(io_L,122) CurrentPDG2(1), CurrentPDG2(2), HPPloopVWm(i1), " # h(",i1,")-VWm(",i2,")-loop " 
End do 
Do i2=2,2
CurrentPDG2(2) = Abs(PDGHm(i2)) 
Write(io_L,122) CurrentPDG2(1), CurrentPDG2(2), HPPloopHm(i2,i1), " # h(",i1,")-Hm(",i2,")-loop " 
End do 
Do i2=1,3
CurrentPDG2(2) = Abs(PDGFd(i2)) 
Write(io_L,122) CurrentPDG2(1), CurrentPDG2(2), HPPloopFd(i2,i1), " # h(",i1,")-Fd(",i2,")-loop " 
End do 
Do i2=1,3
CurrentPDG2(2) = Abs(PDGFu(i2)) 
Write(io_L,122) CurrentPDG2(1), CurrentPDG2(2), HPPloopFu(i2,i1), " # h(",i1,")-Fu(",i2,")-loop " 
End do 
Do i2=1,3
CurrentPDG2(2) = Abs(PDGFe(i2)) 
Write(io_L,122) CurrentPDG2(1), CurrentPDG2(2), HPPloopFe(i2,i1), " # h(",i1,")-Fe(",i2,")-loop " 
End do 
End Do 
End if 

 
Write(io_L,100) "Block EFFHIGGSCOUPLINGS # values of loop-induced couplings " 
facPP = Alpha*Sqrt(2._dp*G_F/sqrt(2._dp))/(2._dp*Pi) 
facGG = Sqrt(2._dp*G_F/sqrt(2._dp))/(Sqrt(2._dp)*2._dp*Pi)*sqrt(8._dp/9._dp)
facPZ = 0._dp 
Do i1=1,3
CurrentPDG3(1) = Abs(PDGhh(i1)) 
CurrentPDG3(2) = Abs(PDGVP) 
CurrentPDG3(3) = Abs(PDGVP) 
Write(io_L,121) CurrentPDG3(1), CurrentPDG3(2), CurrentPDG3(3), Abs(CoupHPP(i1))*facPP, " # H-Photon-Photon " 
CurrentPDG3(2) = Abs(PDGVG) 
CurrentPDG3(3) = Abs(PDGVG) 
Write(io_L,121) CurrentPDG3(1), CurrentPDG3(2), CurrentPDG3(3), Abs(CoupHGG(i1))*facGG, " # H-Gluon-Gluon " 
CurrentPDG3(2) = Abs(PDGVP) 
CurrentPDG3(3) = Abs(PDGVZ) 
Write(io_L,121) CurrentPDG3(1), CurrentPDG3(2), CurrentPDG3(3), 0._dp, " # H-Photon-Z (not yet calculated by SPheno) " 
End Do 
Do i1=2,3
CurrentPDG3(1) = Abs(PDGAh(i1)) 
CurrentPDG3(2) = Abs(PDGVP) 
CurrentPDG3(3) = Abs(PDGVP) 
Write(io_L,121) CurrentPDG3(1), CurrentPDG3(2), CurrentPDG3(3), Abs(CoupAPP(i1))*facPP, " # A-Photon-Photon " 
CurrentPDG3(2) = Abs(PDGVG) 
CurrentPDG3(3) = Abs(PDGVG) 
Write(io_L,121) CurrentPDG3(1), CurrentPDG3(2), CurrentPDG3(3), Abs(CoupAGG(i1))*facGG, " # A-Gluon-Gluon " 
CurrentPDG3(2) = Abs(PDGVP) 
CurrentPDG3(3) = Abs(PDGVZ) 
Write(io_L,121) CurrentPDG3(1), CurrentPDG3(2), CurrentPDG3(3), 0._dp, " # A-Photon-Z (not yet calculated by SPheno) " 
End Do 
End If 

 
Write(io_L,100) "Block SPhenoLowEnergy # low energy observables " 
Write(io_L,1010) 1,Tpar,  "# T-parameter (1-loop BSM)" 
Write(io_L,1010) 2,Spar,  "# S-parameter (1-loop BSM)" 
Write(io_L,1010) 3,Upar,  "# U-parameter (1-loop BSM)" 
Write(io_L,1010) 20,ae,  "# (g-2)_e" 
Write(io_L,1010) 21,amu,  "# (g-2)_mu" 
Write(io_L,1010) 22,atau,  "# (g-2)_tau" 
Write(io_L,1010) 23,EDMe,  "# EDM(e)" 
Write(io_L,1010) 24,EDMmu,  "# EDM(mu)" 
Write(io_L,1010) 25,EDMtau,  "# EDM(tau)" 
Write(io_L,1010) 39,dRho,  "# delta(rho)" 
Write(io_L,100) "Block FlavorKitQFV # quark flavor violating observables " 
Write(io_L,1010) 200,BrBsGamma,  "# BR(B->X_s gamma)" 
Write(io_L,1010) 201,ratioBsGamma,  "# BR(B->X_s gamma)/BR(B->X_s gamma)_SM" 
Write(io_L,1010) 300,BrDmunu,  "# BR(D->mu nu)" 
Write(io_L,1010) 301,ratioDmunu,  "# BR(D->mu nu)/BR(D->mu nu)_SM" 
Write(io_L,1010) 400,BrDsmunu,  "# BR(Ds->mu nu)" 
Write(io_L,1010) 401,ratioDsmunu,  "# BR(Ds->mu nu)/BR(Ds->mu nu)_SM" 
Write(io_L,1010) 402,BrDstaunu,  "# BR(Ds->tau nu)" 
Write(io_L,1010) 403,ratioDstaunu,  "# BR(Ds->tau nu)/BR(Ds->tau nu)_SM" 
Write(io_L,1010) 500,BrBmunu,  "# BR(B->mu nu)" 
Write(io_L,1010) 501,ratioBmunu,  "# BR(B->mu nu)/BR(B->mu nu)_SM" 
Write(io_L,1010) 502,BrBtaunu,  "# BR(B->tau nu)" 
Write(io_L,1010) 503,ratioBtaunu,  "# BR(B->tau nu)/BR(B->tau nu)_SM" 
Write(io_L,1010) 600,BrKmunu,  "# BR(K->mu nu)" 
Write(io_L,1010) 601,ratioKmunu,  "# BR(K->mu nu)/BR(K->mu nu)_SM" 
Write(io_L,1010) 602,RK,  "# R_K = BR(K->e nu)/(K->mu nu)" 
Write(io_L,1010) 603,RKSM,  "# R_K^SM = BR(K->e nu)_SM/(K->mu nu)_SM" 
Write(io_L,1010) 1900,DeltaMBs,  "# Delta(M_Bs)" 
Write(io_L,1010) 1901,ratioDeltaMBs,  "# Delta(M_Bs)/Delta(M_Bs)_SM" 
Write(io_L,1010) 1902,DeltaMBq,  "# Delta(M_Bd)" 
Write(io_L,1010) 1903,ratioDeltaMBq,  "# Delta(M_Bd)/Delta(M_Bd)_SM" 
Write(io_L,1010) 4000,BrB0dEE,  "# BR(B^0_d->e e)" 
Write(io_L,1010) 4001,ratioB0dEE,  "# BR(B^0_d->e e)/BR(B^0_d->e e)_SM" 
Write(io_L,1010) 4002,BrB0sEE,  "# BR(B^0_s->e e)" 
Write(io_L,1010) 4003,ratioB0sEE,  "# BR(B^0_s->e e)/BR(B^0_s->e e)_SM" 
Write(io_L,1010) 4004,BrB0dMuMu,  "# BR(B^0_d->mu mu)" 
Write(io_L,1010) 4005,ratioB0dMuMu,  "# BR(B^0_d->mu mu)/BR(B^0_d->mu mu)_SM" 
Write(io_L,1010) 4006,BrB0sMuMu,  "# BR(B^0_s->mu mu)" 
Write(io_L,1010) 4007,ratioB0sMuMu,  "# BR(B^0_s->mu mu)/BR(B^0_s->mu mu)_SM" 
Write(io_L,1010) 4008,BrB0dTauTau,  "# BR(B^0_d->tau tau)" 
Write(io_L,1010) 4009,ratioB0dTauTau,  "# BR(B^0_d->tau tau)/BR(B^0_d->tau tau)_SM" 
Write(io_L,1010) 4010,BrB0sTauTau,  "# BR(B^0_s->tau tau)" 
Write(io_L,1010) 4011,ratioB0sTauTau,  "# BR(B^0_s->tau tau)/BR(B^0_s->tau tau)_SM" 
Write(io_L,1010) 5000,BrBtoSEE,  "# BR(B-> s e e)" 
Write(io_L,1010) 5001,ratioBtoSEE,  "# BR(B-> s e e)/BR(B-> s e e)_SM" 
Write(io_L,1010) 5002,BrBtoSMuMu,  "# BR(B-> s mu mu)" 
Write(io_L,1010) 5003,ratioBtoSMuMu,  "# BR(B-> s mu mu)/BR(B-> s mu mu)_SM" 
Write(io_L,1010) 6000,BrBtoKee,  "# BR(B -> K mu mu)" 
Write(io_L,1010) 6001,ratioBtoKee,  "# BR(B -> K mu mu)/BR(B -> K mu mu)_SM" 
Write(io_L,1010) 6002,BrBtoKmumu,  "# BR(B -> K mu mu)" 
Write(io_L,1010) 6003,ratioBtoKmumu,  "# BR(B -> K mu mu)/BR(B -> K mu mu)_SM" 
Write(io_L,1010) 7000,BrBtoSnunu,  "# BR(B->s nu nu)" 
Write(io_L,1010) 7001,ratioBtoSnunu,  "# BR(B->s nu nu)/BR(B->s nu nu)_SM" 
Write(io_L,1010) 7002,BrBtoDnunu,  "# BR(B->D nu nu)" 
Write(io_L,1010) 7003,ratioBtoDnunu,  "# BR(B->D nu nu)/BR(B->D nu nu)_SM" 
Write(io_L,1010) 8000,BrKptoPipnunu,  "# BR(K^+ -> pi^+ nu nu)" 
Write(io_L,1010) 8001,ratioKptoPipnunu,  "# BR(K^+ -> pi^+ nu nu)/BR(K^+ -> pi^+ nu nu)_SM" 
Write(io_L,1010) 8002,BrKltoPinunu,  "# BR(K_L -> pi^0 nu nu)" 
Write(io_L,1010) 8003,ratioKltoPinunu,  "# BR(K_L -> pi^0 nu nu)/BR(K_L -> pi^0 nu nu)_SM" 
Write(io_L,1010) 8004,BrK0eMu,  "# BR(K^0_L -> e mu)" 
Write(io_L,1010) 8005,ratioK0eMu,  "# BR(K^0_L -> e mu)/BR(K^0_L -> e mu)_SM" 
Write(io_L,1010) 9100,DelMK,  "# Delta(M_K)" 
Write(io_L,1010) 9102,ratioDelMK,  "# Delta(M_K)/Delta(M_K)_SM" 
Write(io_L,1010) 9103,epsK,  "# epsilon_K" 
Write(io_L,1010) 9104,ratioepsK,  "# epsilon_K/epsilon_K^SM" 
Write(io_L,100) "Block FlavorKitLFV # lepton flavor violating observables " 
Write(io_L,1010) 701,muEgamma,  "# BR(mu->e gamma)" 
Write(io_L,1010) 702,tauEgamma,  "# BR(tau->e gamma)" 
Write(io_L,1010) 703,tauMuGamma,  "# BR(tau->mu gamma)" 
Write(io_L,1010) 800,CRmuEAl,  "# CR(mu-e, Al)" 
Write(io_L,1010) 801,CRmuETi,  "# CR(mu-e, Ti)" 
Write(io_L,1010) 802,CRmuESr,  "# CR(mu-e, Sr)" 
Write(io_L,1010) 803,CRmuESb,  "# CR(mu-e, Sb)" 
Write(io_L,1010) 804,CRmuEAu,  "# CR(mu-e, Au)" 
Write(io_L,1010) 805,CRmuEPb,  "# CR(mu-e, Pb)" 
Write(io_L,1010) 901,BRmuTo3e,  "# BR(mu->3e)" 
Write(io_L,1010) 902,BRtauTo3e,  "# BR(tau->3e)" 
Write(io_L,1010) 903,BRtauTo3mu,  "# BR(tau->3mu)" 
Write(io_L,1010) 904,BRtauToemumu,  "# BR(tau- -> e- mu+ mu-)" 
Write(io_L,1010) 905,BRtauTomuee,  "# BR(tau- -> mu- e+ e-)" 
Write(io_L,1010) 906,BRtauToemumu2,  "# BR(tau- -> e+ mu- mu-)" 
Write(io_L,1010) 907,BRtauTomuee2,  "# BR(tau- -> mu+ e- e-)" 
Write(io_L,1010) 1001,BrZtoMuE,  "# BR(Z->e mu)" 
Write(io_L,1010) 1002,BrZtoTauE,  "# BR(Z->e tau)" 
Write(io_L,1010) 1003,BrZtoTauMu,  "# BR(Z->mu tau)" 
Write(io_L,1010) 1101,BrhtoMuE,  "# BR(h->e mu)" 
Write(io_L,1010) 1102,BrhtoTauE,  "# BR(h->e tau)" 
Write(io_L,1010) 1103,BrhtoTauMu,  "# BR(h->mu tau)" 
Write(io_L,1010) 2001,BrTautoEPi,  "# BR(tau->e pi)" 
Write(io_L,1010) 2002,BrTautoEEta,  "# BR(tau->e eta)" 
Write(io_L,1010) 2003,BrTautoEEtap,  "# BR(tau->e eta')" 
Write(io_L,1010) 2004,BrTautoMuPi,  "# BR(tau->mu pi)" 
Write(io_L,1010) 2005,BrTautoMuEta,  "# BR(tau->mu eta)" 
Write(io_L,1010) 2006,BrTautoMuEtap,  "# BR(tau->mu eta')" 

 
Write(io_L,100) "Block FWCOEF Q=  1.60000000E+02  # Wilson coefficients at scale Q " 
Write(io_L,222) "    0305" , "4422" , "00", "0", Real(coeffC7sm,dp),  " # coeffC7sm"  
Write(io_L,222) "    0305" , "4422" , "00", "2", Real(coeffC7,dp),  " # coeffC7"  
Write(io_L,222) "    0305" , "4322" , "00", "2", Real(coeffC7p,dp),  " # coeffC7p"  
Write(io_L,222) "    0305" , "4422" , "00", "1", Real(coeffC7NP,dp),  " # coeffC7NP"  
Write(io_L,222) "    0305" , "4322" , "00", "1", Real(coeffC7pNP,dp),  " # coeffC7pNP"  
Write(io_L,222) "    0305" , "6421" , "00", "0", Real(coeffC8sm,dp),  " # coeffC8sm"  
Write(io_L,222) "    0305" , "6421" , "00", "2", Real(coeffC8,dp),  " # coeffC8"  
Write(io_L,222) "    0305" , "6321" , "00", "2", Real(coeffC8p,dp),  " # coeffC8p"  
Write(io_L,222) "    0305" , "6421" , "00", "1", Real(coeffC8NP,dp),  " # coeffC8NP"  
Write(io_L,222) "    0305" , "6321" , "00", "1", Real(coeffC8pNP,dp),  " # coeffC8pNP"  
Write(io_L,222) "03051111" , "4133" , "00", "0", Real(coeffC9eeSM,dp),  " # coeffC9eeSM"  
Write(io_L,222) "03051111" , "4133" , "00", "2", Real(coeffC9ee,dp),  " # coeffC9ee"  
Write(io_L,222) "03051111" , "4233" , "00", "2", Real(coeffC9Pee,dp),  " # coeffC9Pee"  
Write(io_L,222) "03051111" , "4133" , "00", "1", Real(coeffC9eeNP,dp),  " # coeffC9eeNP"  
Write(io_L,222) "03051111" , "4233" , "00", "1", Real(coeffC9PeeNP,dp),  " # coeffC9PeeNP"  
Write(io_L,222) "03051111" , "4137" , "00", "0", Real(coeffC10eeSM,dp),  " # coeffC10eeSM"  
Write(io_L,222) "03051111" , "4137" , "00", "2", Real(coeffC10ee,dp),  " # coeffC10ee"  
Write(io_L,222) "03051111" , "4237" , "00", "2", Real(coeffC10Pee,dp),  " # coeffC10Pee"  
Write(io_L,222) "03051111" , "4137" , "00", "1", Real(coeffC10eeNP,dp),  " # coeffC10eeNP"  
Write(io_L,222) "03051111" , "4237" , "00", "1", Real(coeffC10PeeNP,dp),  " # coeffC10PeeNP"  
Write(io_L,222) "03051313" , "4133" , "00", "0", Real(coeffC9mumuSM,dp),  " # coeffC9mumuSM"  
Write(io_L,222) "03051313" , "4133" , "00", "2", Real(coeffC9mumu,dp),  " # coeffC9mumu"  
Write(io_L,222) "03051313" , "4233" , "00", "2", Real(coeffC9Pmumu,dp),  " # coeffC9Pmumu"  
Write(io_L,222) "03051313" , "4133" , "00", "1", Real(coeffC9mumuNP,dp),  " # coeffC9mumuNP"  
Write(io_L,222) "03051313" , "4233" , "00", "1", Real(coeffC9PmumuNP,dp),  " # coeffC9PmumuNP"  
Write(io_L,222) "03051313" , "4137" , "00", "0", Real(coeffC10mumuSM,dp),  " # coeffC10mumuSM"  
Write(io_L,222) "03051313" , "4137" , "00", "2", Real(coeffC10mumu,dp),  " # coeffC10mumu"  
Write(io_L,222) "03051313" , "4237" , "00", "2", Real(coeffC10Pmumu,dp),  " # coeffC10Pmumu"  
Write(io_L,222) "03051313" , "4137" , "00", "1", Real(coeffC10mumuNP,dp),  " # coeffC10mumuNP"  
Write(io_L,222) "03051313" , "4237" , "00", "1", Real(coeffC10PmumuNP,dp),  " # coeffC10PmumuNP"  
Write(io_L,222) "03051212" , "4141" , "00", "0", Real(coeffCLnu1nu1SM,dp),  " # coeffCLnu1nu1SM"  
Write(io_L,222) "03051212" , "4141" , "00", "2", Real(coeffCLnu1nu1,dp),  " # coeffCLnu1nu1"  
Write(io_L,222) "03051212" , "4241" , "00", "2", Real(coeffCLPnu1nu1,dp),  " # coeffCLPnu1nu1"  
Write(io_L,222) "03051212" , "4141" , "00", "1", Real(coeffCLnu1nu1NP,dp),  " # coeffCLnu1nu1NP"  
Write(io_L,222) "03051212" , "4241" , "00", "1", Real(coeffCLPnu1nu1NP,dp),  " # coeffCLPnu1nu1NP"  
Write(io_L,222) "03051414" , "4141" , "00", "0", Real(coeffCLnu2nu2SM,dp),  " # coeffCLnu2nu2SM"  
Write(io_L,222) "03051414" , "4141" , "00", "2", Real(coeffCLnu2nu2,dp),  " # coeffCLnu2nu2"  
Write(io_L,222) "03051414" , "4241" , "00", "2", Real(coeffCLPnu2nu2,dp),  " # coeffCLPnu2nu2"  
Write(io_L,222) "03051414" , "4141" , "00", "1", Real(coeffCLnu2nu2NP,dp),  " # coeffCLnu2nu2NP"  
Write(io_L,222) "03051414" , "4241" , "00", "1", Real(coeffCLPnu2nu2NP,dp),  " # coeffCLPnu2nu2NP"  
Write(io_L,222) "03051616" , "4141" , "00", "0", Real(coeffCLnu3nu3SM,dp),  " # coeffCLnu3nu3SM"  
Write(io_L,222) "03051616" , "4141" , "00", "2", Real(coeffCLnu3nu3,dp),  " # coeffCLnu3nu3"  
Write(io_L,222) "03051616" , "4241" , "00", "2", Real(coeffCLPnu3nu3,dp),  " # coeffCLPnu3nu3"  
Write(io_L,222) "03051616" , "4141" , "00", "1", Real(coeffCLnu3nu3NP,dp),  " # coeffCLnu3nu3NP"  
Write(io_L,222) "03051616" , "4241" , "00", "1", Real(coeffCLPnu3nu3NP,dp),  " # coeffCLPnu3nu3NP"  
Write(io_L,222) "03051212" , "4142" , "00", "0", Real(coeffCRnu1nu1SM,dp),  " # coeffCRnu1nu1SM"  
Write(io_L,222) "03051212" , "4142" , "00", "2", Real(coeffCRnu1nu1,dp),  " # coeffCRnu1nu1"  
Write(io_L,222) "03051212" , "4242" , "00", "2", Real(coeffCRPnu1nu1,dp),  " # coeffCRPnu1nu1"  
Write(io_L,222) "03051212" , "4142" , "00", "1", Real(coeffCRnu1nu1NP,dp),  " # coeffCRnu1nu1NP"  
Write(io_L,222) "03051212" , "4242" , "00", "1", Real(coeffCRPnu1nu1NP,dp),  " # coeffCRPnu1nu1NP"  
Write(io_L,222) "03051414" , "4142" , "00", "0", Real(coeffCRnu2nu2SM,dp),  " # coeffCRnu2nu2SM"  
Write(io_L,222) "03051414" , "4142" , "00", "2", Real(coeffCRnu2nu2,dp),  " # coeffCRnu2nu2"  
Write(io_L,222) "03051414" , "4242" , "00", "2", Real(coeffCRPnu2nu2,dp),  " # coeffCRPnu2nu2"  
Write(io_L,222) "03051414" , "4142" , "00", "1", Real(coeffCRnu2nu2NP,dp),  " # coeffCRnu2nu2NP"  
Write(io_L,222) "03051414" , "4242" , "00", "1", Real(coeffCRPnu2nu2NP,dp),  " # coeffCRPnu2nu2NP"  
Write(io_L,222) "03051616" , "4142" , "00", "0", Real(coeffCRnu3nu3SM,dp),  " # coeffCRnu3nu3SM"  
Write(io_L,222) "03051616" , "4142" , "00", "2", Real(coeffCRnu3nu3,dp),  " # coeffCRnu3nu3"  
Write(io_L,222) "03051616" , "4242" , "00", "2", Real(coeffCRPnu3nu3,dp),  " # coeffCRPnu3nu3"  
Write(io_L,222) "03051616" , "4142" , "00", "1", Real(coeffCRnu3nu3NP,dp),  " # coeffCRnu3nu3NP"  
Write(io_L,222) "03051616" , "4242" , "00", "1", Real(coeffCRPnu3nu3NP,dp),  " # coeffCRPnu3nu3NP"  
Write(io_L,222) "01030103" , "3131" , "00", "2", Real(coeffKK_SLL,dp),  " # coeffKK_SLL"  
Write(io_L,222) "01030103" , "3232" , "00", "2", Real(coeffKK_SRR,dp),  " # coeffKK_SRR"  
Write(io_L,222) "01030103" , "3132" , "00", "2", Real(coeffKK_SLR,dp),  " # coeffKK_SLR"  
Write(io_L,222) "01030103" , "4141" , "00", "2", Real(coeffKK_VLL,dp),  " # coeffKK_VLL"  
Write(io_L,222) "01030103" , "4242" , "00", "2", Real(coeffKK_VRR,dp),  " # coeffKK_VRR"  
Write(io_L,222) "01030103" , "4142" , "00", "2", Real(coeffKK_VLR,dp),  " # coeffKK_VLR"  
Write(io_L,222) "01030103" , "4343" , "00", "2", Real(coeffKK_TLL,dp),  " # coeffKK_TLL"  
Write(io_L,222) "01030103" , "4444" , "00", "2", Real(coeffKK_TRR,dp),  " # coeffKK_TRR"  
Write(io_L,222) "01050105" , "3131" , "00", "2", Real(coeffBB_SLL,dp),  " # coeffBB_SLL"  
Write(io_L,222) "01050105" , "3232" , "00", "2", Real(coeffBB_SRR,dp),  " # coeffBB_SRR"  
Write(io_L,222) "01050105" , "3132" , "00", "2", Real(coeffBB_SLR,dp),  " # coeffBB_SLR"  
Write(io_L,222) "01050105" , "4141" , "00", "2", Real(coeffBB_VLL,dp),  " # coeffBB_VLL"  
Write(io_L,222) "01050105" , "4242" , "00", "2", Real(coeffBB_VRR,dp),  " # coeffBB_VRR"  
Write(io_L,222) "01050105" , "4142" , "00", "2", Real(coeffBB_VLR,dp),  " # coeffBB_VLR"  
Write(io_L,222) "01050105" , "4343" , "00", "2", Real(coeffBB_TLL,dp),  " # coeffBB_TLL"  
Write(io_L,222) "01050105" , "4444" , "00", "2", Real(coeffBB_TRR,dp),  " # coeffBB_TRR"  
Write(io_L,222) "03050305" , "3131" , "00", "2", Real(coeffBsBs_SLL,dp),  " # coeffBsBs_SLL"  
Write(io_L,222) "03050305" , "3232" , "00", "2", Real(coeffBsBs_SRR,dp),  " # coeffBsBs_SRR"  
Write(io_L,222) "03050305" , "3132" , "00", "2", Real(coeffBsBs_SLR,dp),  " # coeffBsBs_SLR"  
Write(io_L,222) "03050305" , "4141" , "00", "2", Real(coeffBsBs_VLL,dp),  " # coeffBsBs_VLL"  
Write(io_L,222) "03050305" , "4242" , "00", "2", Real(coeffBsBs_VRR,dp),  " # coeffBsBs_VRR"  
Write(io_L,222) "03050305" , "4142" , "00", "2", Real(coeffBsBs_VLR,dp),  " # coeffBsBs_VLR"  
Write(io_L,222) "03050305" , "4343" , "00", "2", Real(coeffBsBs_TLL,dp),  " # coeffBsBs_TLL"  
Write(io_L,222) "03050305" , "4444" , "00", "2", Real(coeffBsBs_TRR,dp),  " # coeffBsBs_TRR"  
Write(io_L,222) "01030103" , "3131" , "00", "1", Real(coeffKK_SLLNP,dp),  " # coeffKK_SLLNP"  
Write(io_L,222) "01030103" , "3232" , "00", "1", Real(coeffKK_SRRNP,dp),  " # coeffKK_SRRNP"  
Write(io_L,222) "01030103" , "3132" , "00", "1", Real(coeffKK_SLRNP,dp),  " # coeffKK_SLRNP"  
Write(io_L,222) "01030103" , "4141" , "00", "1", Real(coeffKK_VLLNP,dp),  " # coeffKK_VLLNP"  
Write(io_L,222) "01030103" , "4242" , "00", "1", Real(coeffKK_VRRNP,dp),  " # coeffKK_VRRNP"  
Write(io_L,222) "01030103" , "4142" , "00", "1", Real(coeffKK_VLRNP,dp),  " # coeffKK_VLRNP"  
Write(io_L,222) "01030103" , "4343" , "00", "1", Real(coeffKK_TLLNP,dp),  " # coeffKK_TLLNP"  
Write(io_L,222) "01030103" , "4444" , "00", "1", Real(coeffKK_TRRNP,dp),  " # coeffKK_TRRNP"  
Write(io_L,222) "01050105" , "3131" , "00", "1", Real(coeffBB_SLLNP,dp),  " # coeffBB_SLLNP"  
Write(io_L,222) "01050105" , "3232" , "00", "1", Real(coeffBB_SRRNP,dp),  " # coeffBB_SRRNP"  
Write(io_L,222) "01050105" , "3132" , "00", "1", Real(coeffBB_SLRNP,dp),  " # coeffBB_SLRNP"  
Write(io_L,222) "01050105" , "4141" , "00", "1", Real(coeffBB_VLLNP,dp),  " # coeffBB_VLLNP"  
Write(io_L,222) "01050105" , "4242" , "00", "1", Real(coeffBB_VRRNP,dp),  " # coeffBB_VRRNP"  
Write(io_L,222) "01050105" , "4142" , "00", "1", Real(coeffBB_VLRNP,dp),  " # coeffBB_VLRNP"  
Write(io_L,222) "01050105" , "4343" , "00", "1", Real(coeffBB_TLLNP,dp),  " # coeffBB_TLLNP"  
Write(io_L,222) "01050105" , "4444" , "00", "1", Real(coeffBB_TRRNP,dp),  " # coeffBB_TRRNP"  
Write(io_L,222) "03050305" , "3131" , "00", "1", Real(coeffBsBs_SLLNP,dp),  " # coeffBsBs_SLLNP"  
Write(io_L,222) "03050305" , "3232" , "00", "1", Real(coeffBsBs_SRRNP,dp),  " # coeffBsBs_SRRNP"  
Write(io_L,222) "03050305" , "3132" , "00", "1", Real(coeffBsBs_SLRNP,dp),  " # coeffBsBs_SLRNP"  
Write(io_L,222) "03050305" , "4141" , "00", "1", Real(coeffBsBs_VLLNP,dp),  " # coeffBsBs_VLLNP"  
Write(io_L,222) "03050305" , "4242" , "00", "1", Real(coeffBsBs_VRRNP,dp),  " # coeffBsBs_VRRNP"  
Write(io_L,222) "03050305" , "4142" , "00", "1", Real(coeffBsBs_VLRNP,dp),  " # coeffBsBs_VLRNP"  
Write(io_L,222) "03050305" , "4343" , "00", "1", Real(coeffBsBs_TLLNP,dp),  " # coeffBsBs_TLLNP"  
Write(io_L,222) "03050305" , "4444" , "00", "1", Real(coeffBsBs_TRRNP,dp),  " # coeffBsBs_TRRNP"  
Write(io_L,222) "01030103" , "3131" , "00", "0", Real(coeffKK_SLLSM,dp),  " # coeffKK_SLLSM"  
Write(io_L,222) "01030103" , "3232" , "00", "0", Real(coeffKK_SRRSM,dp),  " # coeffKK_SRRSM"  
Write(io_L,222) "01030103" , "3132" , "00", "0", Real(coeffKK_SLRSM,dp),  " # coeffKK_SLRSM"  
Write(io_L,222) "01030103" , "4141" , "00", "0", Real(coeffKK_VLLSM,dp),  " # coeffKK_VLLSM"  
Write(io_L,222) "01030103" , "4242" , "00", "0", Real(coeffKK_VRRSM,dp),  " # coeffKK_VRRSM"  
Write(io_L,222) "01030103" , "4142" , "00", "0", Real(coeffKK_VLRSM,dp),  " # coeffKK_VLRSM"  
Write(io_L,222) "01030103" , "4343" , "00", "0", Real(coeffKK_TLLSM,dp),  " # coeffKK_TLLSM"  
Write(io_L,222) "01030103" , "4444" , "00", "0", Real(coeffKK_TRRSM,dp),  " # coeffKK_TRRSM"  
Write(io_L,222) "01050105" , "3131" , "00", "0", Real(coeffBB_SLLSM,dp),  " # coeffBB_SLLSM"  
Write(io_L,222) "01050105" , "3232" , "00", "0", Real(coeffBB_SRRSM,dp),  " # coeffBB_SRRSM"  
Write(io_L,222) "01050105" , "3132" , "00", "0", Real(coeffBB_SLRSM,dp),  " # coeffBB_SLRSM"  
Write(io_L,222) "01050105" , "4141" , "00", "0", Real(coeffBB_VLLSM,dp),  " # coeffBB_VLLSM"  
Write(io_L,222) "01050105" , "4242" , "00", "0", Real(coeffBB_VRRSM,dp),  " # coeffBB_VRRSM"  
Write(io_L,222) "01050105" , "4142" , "00", "0", Real(coeffBB_VLRSM,dp),  " # coeffBB_VLRSM"  
Write(io_L,222) "01050105" , "4343" , "00", "0", Real(coeffBB_TLLSM,dp),  " # coeffBB_TLLSM"  
Write(io_L,222) "01050105" , "4444" , "00", "0", Real(coeffBB_TRRSM,dp),  " # coeffBB_TRRSM"  
Write(io_L,222) "03050305" , "3131" , "00", "0", Real(coeffBsBs_SLLSM,dp),  " # coeffBsBs_SLLSM"  
Write(io_L,222) "03050305" , "3232" , "00", "0", Real(coeffBsBs_SRRSM,dp),  " # coeffBsBs_SRRSM"  
Write(io_L,222) "03050305" , "3132" , "00", "0", Real(coeffBsBs_SLRSM,dp),  " # coeffBsBs_SLRSM"  
Write(io_L,222) "03050305" , "4141" , "00", "0", Real(coeffBsBs_VLLSM,dp),  " # coeffBsBs_VLLSM"  
Write(io_L,222) "03050305" , "4242" , "00", "0", Real(coeffBsBs_VRRSM,dp),  " # coeffBsBs_VRRSM"  
Write(io_L,222) "03050305" , "4142" , "00", "0", Real(coeffBsBs_VLRSM,dp),  " # coeffBsBs_VLRSM"  
Write(io_L,222) "03050305" , "4343" , "00", "0", Real(coeffBsBs_TLLSM,dp),  " # coeffBsBs_TLLSM"  
Write(io_L,222) "03050305" , "4444" , "00", "0", Real(coeffBsBs_TRRSM,dp),  " # coeffBsBs_TRRSM"  
Write(io_L,100) "Block IMFWCOEF Q=  1.60000000E+02  # Im(Wilson coefficients) at scale Q " 
Write(io_L,222) "    0305" , "4422" , "00", "0", Aimag(coeffC7sm),  " # coeffC7sm"  
Write(io_L,222) "    0305" , "4422" , "00", "2", Aimag(coeffC7),  " # coeffC7"  
Write(io_L,222) "    0305" , "4322" , "00", "2", Aimag(coeffC7p),  " # coeffC7p"  
Write(io_L,222) "    0305" , "4422" , "00", "1", Aimag(coeffC7NP),  " # coeffC7NP"  
Write(io_L,222) "    0305" , "4322" , "00", "1", Aimag(coeffC7pNP),  " # coeffC7pNP"  
Write(io_L,222) "    0305" , "6421" , "00", "0", Aimag(coeffC8sm),  " # coeffC8sm"  
Write(io_L,222) "    0305" , "6421" , "00", "2", Aimag(coeffC8),  " # coeffC8"  
Write(io_L,222) "    0305" , "6321" , "00", "2", Aimag(coeffC8p),  " # coeffC8p"  
Write(io_L,222) "    0305" , "6421" , "00", "1", Aimag(coeffC8NP),  " # coeffC8NP"  
Write(io_L,222) "    0305" , "6321" , "00", "1", Aimag(coeffC8pNP),  " # coeffC8pNP"  
Write(io_L,222) "03051111" , "4133" , "00", "0", Aimag(coeffC9eeSM),  " # coeffC9eeSM"  
Write(io_L,222) "03051111" , "4133" , "00", "2", Aimag(coeffC9ee),  " # coeffC9ee"  
Write(io_L,222) "03051111" , "4233" , "00", "2", Aimag(coeffC9Pee),  " # coeffC9Pee"  
Write(io_L,222) "03051111" , "4133" , "00", "1", Aimag(coeffC9eeNP),  " # coeffC9eeNP"  
Write(io_L,222) "03051111" , "4233" , "00", "1", Aimag(coeffC9PeeNP),  " # coeffC9PeeNP"  
Write(io_L,222) "03051111" , "4137" , "00", "0", Aimag(coeffC10eeSM),  " # coeffC10eeSM"  
Write(io_L,222) "03051111" , "4137" , "00", "2", Aimag(coeffC10ee),  " # coeffC10ee"  
Write(io_L,222) "03051111" , "4237" , "00", "2", Aimag(coeffC10Pee),  " # coeffC10Pee"  
Write(io_L,222) "03051111" , "4137" , "00", "1", Aimag(coeffC10eeNP),  " # coeffC10eeNP"  
Write(io_L,222) "03051111" , "4237" , "00", "1", Aimag(coeffC10PeeNP),  " # coeffC10PeeNP"  
Write(io_L,222) "03051313" , "4133" , "00", "0", Aimag(coeffC9mumuSM),  " # coeffC9mumuSM"  
Write(io_L,222) "03051313" , "4133" , "00", "2", Aimag(coeffC9mumu),  " # coeffC9mumu"  
Write(io_L,222) "03051313" , "4233" , "00", "2", Aimag(coeffC9Pmumu),  " # coeffC9Pmumu"  
Write(io_L,222) "03051313" , "4133" , "00", "1", Aimag(coeffC9mumuNP),  " # coeffC9mumuNP"  
Write(io_L,222) "03051313" , "4233" , "00", "1", Aimag(coeffC9PmumuNP),  " # coeffC9PmumuNP"  
Write(io_L,222) "03051313" , "4137" , "00", "0", Aimag(coeffC10mumuSM),  " # coeffC10mumuSM"  
Write(io_L,222) "03051313" , "4137" , "00", "2", Aimag(coeffC10mumu),  " # coeffC10mumu"  
Write(io_L,222) "03051313" , "4237" , "00", "2", Aimag(coeffC10Pmumu),  " # coeffC10Pmumu"  
Write(io_L,222) "03051313" , "4137" , "00", "1", Aimag(coeffC10mumuNP),  " # coeffC10mumuNP"  
Write(io_L,222) "03051313" , "4237" , "00", "1", Aimag(coeffC10PmumuNP),  " # coeffC10PmumuNP"  
Write(io_L,222) "03051212" , "4141" , "00", "0", Aimag(coeffCLnu1nu1SM),  " # coeffCLnu1nu1SM"  
Write(io_L,222) "03051212" , "4141" , "00", "2", Aimag(coeffCLnu1nu1),  " # coeffCLnu1nu1"  
Write(io_L,222) "03051212" , "4241" , "00", "2", Aimag(coeffCLPnu1nu1),  " # coeffCLPnu1nu1"  
Write(io_L,222) "03051212" , "4141" , "00", "1", Aimag(coeffCLnu1nu1NP),  " # coeffCLnu1nu1NP"  
Write(io_L,222) "03051212" , "4241" , "00", "1", Aimag(coeffCLPnu1nu1NP),  " # coeffCLPnu1nu1NP"  
Write(io_L,222) "03051414" , "4141" , "00", "0", Aimag(coeffCLnu2nu2SM),  " # coeffCLnu2nu2SM"  
Write(io_L,222) "03051414" , "4141" , "00", "2", Aimag(coeffCLnu2nu2),  " # coeffCLnu2nu2"  
Write(io_L,222) "03051414" , "4241" , "00", "2", Aimag(coeffCLPnu2nu2),  " # coeffCLPnu2nu2"  
Write(io_L,222) "03051414" , "4141" , "00", "1", Aimag(coeffCLnu2nu2NP),  " # coeffCLnu2nu2NP"  
Write(io_L,222) "03051414" , "4241" , "00", "1", Aimag(coeffCLPnu2nu2NP),  " # coeffCLPnu2nu2NP"  
Write(io_L,222) "03051616" , "4141" , "00", "0", Aimag(coeffCLnu3nu3SM),  " # coeffCLnu3nu3SM"  
Write(io_L,222) "03051616" , "4141" , "00", "2", Aimag(coeffCLnu3nu3),  " # coeffCLnu3nu3"  
Write(io_L,222) "03051616" , "4241" , "00", "2", Aimag(coeffCLPnu3nu3),  " # coeffCLPnu3nu3"  
Write(io_L,222) "03051616" , "4141" , "00", "1", Aimag(coeffCLnu3nu3NP),  " # coeffCLnu3nu3NP"  
Write(io_L,222) "03051616" , "4241" , "00", "1", Aimag(coeffCLPnu3nu3NP),  " # coeffCLPnu3nu3NP"  
Write(io_L,222) "03051212" , "4142" , "00", "0", Aimag(coeffCRnu1nu1SM),  " # coeffCRnu1nu1SM"  
Write(io_L,222) "03051212" , "4142" , "00", "2", Aimag(coeffCRnu1nu1),  " # coeffCRnu1nu1"  
Write(io_L,222) "03051212" , "4242" , "00", "2", Aimag(coeffCRPnu1nu1),  " # coeffCRPnu1nu1"  
Write(io_L,222) "03051212" , "4142" , "00", "1", Aimag(coeffCRnu1nu1NP),  " # coeffCRnu1nu1NP"  
Write(io_L,222) "03051212" , "4242" , "00", "1", Aimag(coeffCRPnu1nu1NP),  " # coeffCRPnu1nu1NP"  
Write(io_L,222) "03051414" , "4142" , "00", "0", Aimag(coeffCRnu2nu2SM),  " # coeffCRnu2nu2SM"  
Write(io_L,222) "03051414" , "4142" , "00", "2", Aimag(coeffCRnu2nu2),  " # coeffCRnu2nu2"  
Write(io_L,222) "03051414" , "4242" , "00", "2", Aimag(coeffCRPnu2nu2),  " # coeffCRPnu2nu2"  
Write(io_L,222) "03051414" , "4142" , "00", "1", Aimag(coeffCRnu2nu2NP),  " # coeffCRnu2nu2NP"  
Write(io_L,222) "03051414" , "4242" , "00", "1", Aimag(coeffCRPnu2nu2NP),  " # coeffCRPnu2nu2NP"  
Write(io_L,222) "03051616" , "4142" , "00", "0", Aimag(coeffCRnu3nu3SM),  " # coeffCRnu3nu3SM"  
Write(io_L,222) "03051616" , "4142" , "00", "2", Aimag(coeffCRnu3nu3),  " # coeffCRnu3nu3"  
Write(io_L,222) "03051616" , "4242" , "00", "2", Aimag(coeffCRPnu3nu3),  " # coeffCRPnu3nu3"  
Write(io_L,222) "03051616" , "4142" , "00", "1", Aimag(coeffCRnu3nu3NP),  " # coeffCRnu3nu3NP"  
Write(io_L,222) "03051616" , "4242" , "00", "1", Aimag(coeffCRPnu3nu3NP),  " # coeffCRPnu3nu3NP"  
Write(io_L,222) "01030103" , "3131" , "00", "2", Aimag(coeffKK_SLL),  " # coeffKK_SLL"  
Write(io_L,222) "01030103" , "3232" , "00", "2", Aimag(coeffKK_SRR),  " # coeffKK_SRR"  
Write(io_L,222) "01030103" , "3132" , "00", "2", Aimag(coeffKK_SLR),  " # coeffKK_SLR"  
Write(io_L,222) "01030103" , "4141" , "00", "2", Aimag(coeffKK_VLL),  " # coeffKK_VLL"  
Write(io_L,222) "01030103" , "4242" , "00", "2", Aimag(coeffKK_VRR),  " # coeffKK_VRR"  
Write(io_L,222) "01030103" , "4142" , "00", "2", Aimag(coeffKK_VLR),  " # coeffKK_VLR"  
Write(io_L,222) "01030103" , "4343" , "00", "2", Aimag(coeffKK_TLL),  " # coeffKK_TLL"  
Write(io_L,222) "01030103" , "4444" , "00", "2", Aimag(coeffKK_TRR),  " # coeffKK_TRR"  
Write(io_L,222) "01050105" , "3131" , "00", "2", Aimag(coeffBB_SLL),  " # coeffBB_SLL"  
Write(io_L,222) "01050105" , "3232" , "00", "2", Aimag(coeffBB_SRR),  " # coeffBB_SRR"  
Write(io_L,222) "01050105" , "3132" , "00", "2", Aimag(coeffBB_SLR),  " # coeffBB_SLR"  
Write(io_L,222) "01050105" , "4141" , "00", "2", Aimag(coeffBB_VLL),  " # coeffBB_VLL"  
Write(io_L,222) "01050105" , "4242" , "00", "2", Aimag(coeffBB_VRR),  " # coeffBB_VRR"  
Write(io_L,222) "01050105" , "4142" , "00", "2", Aimag(coeffBB_VLR),  " # coeffBB_VLR"  
Write(io_L,222) "01050105" , "4343" , "00", "2", Aimag(coeffBB_TLL),  " # coeffBB_TLL"  
Write(io_L,222) "01050105" , "4444" , "00", "2", Aimag(coeffBB_TRR),  " # coeffBB_TRR"  
Write(io_L,222) "03050305" , "3131" , "00", "2", Aimag(coeffBsBs_SLL),  " # coeffBsBs_SLL"  
Write(io_L,222) "03050305" , "3232" , "00", "2", Aimag(coeffBsBs_SRR),  " # coeffBsBs_SRR"  
Write(io_L,222) "03050305" , "3132" , "00", "2", Aimag(coeffBsBs_SLR),  " # coeffBsBs_SLR"  
Write(io_L,222) "03050305" , "4141" , "00", "2", Aimag(coeffBsBs_VLL),  " # coeffBsBs_VLL"  
Write(io_L,222) "03050305" , "4242" , "00", "2", Aimag(coeffBsBs_VRR),  " # coeffBsBs_VRR"  
Write(io_L,222) "03050305" , "4142" , "00", "2", Aimag(coeffBsBs_VLR),  " # coeffBsBs_VLR"  
Write(io_L,222) "03050305" , "4343" , "00", "2", Aimag(coeffBsBs_TLL),  " # coeffBsBs_TLL"  
Write(io_L,222) "03050305" , "4444" , "00", "2", Aimag(coeffBsBs_TRR),  " # coeffBsBs_TRR"  
Write(io_L,222) "01030103" , "3131" , "00", "1", Aimag(coeffKK_SLLNP),  " # coeffKK_SLLNP"  
Write(io_L,222) "01030103" , "3232" , "00", "1", Aimag(coeffKK_SRRNP),  " # coeffKK_SRRNP"  
Write(io_L,222) "01030103" , "3132" , "00", "1", Aimag(coeffKK_SLRNP),  " # coeffKK_SLRNP"  
Write(io_L,222) "01030103" , "4141" , "00", "1", Aimag(coeffKK_VLLNP),  " # coeffKK_VLLNP"  
Write(io_L,222) "01030103" , "4242" , "00", "1", Aimag(coeffKK_VRRNP),  " # coeffKK_VRRNP"  
Write(io_L,222) "01030103" , "4142" , "00", "1", Aimag(coeffKK_VLRNP),  " # coeffKK_VLRNP"  
Write(io_L,222) "01030103" , "4343" , "00", "1", Aimag(coeffKK_TLLNP),  " # coeffKK_TLLNP"  
Write(io_L,222) "01030103" , "4444" , "00", "1", Aimag(coeffKK_TRRNP),  " # coeffKK_TRRNP"  
Write(io_L,222) "01050105" , "3131" , "00", "1", Aimag(coeffBB_SLLNP),  " # coeffBB_SLLNP"  
Write(io_L,222) "01050105" , "3232" , "00", "1", Aimag(coeffBB_SRRNP),  " # coeffBB_SRRNP"  
Write(io_L,222) "01050105" , "3132" , "00", "1", Aimag(coeffBB_SLRNP),  " # coeffBB_SLRNP"  
Write(io_L,222) "01050105" , "4141" , "00", "1", Aimag(coeffBB_VLLNP),  " # coeffBB_VLLNP"  
Write(io_L,222) "01050105" , "4242" , "00", "1", Aimag(coeffBB_VRRNP),  " # coeffBB_VRRNP"  
Write(io_L,222) "01050105" , "4142" , "00", "1", Aimag(coeffBB_VLRNP),  " # coeffBB_VLRNP"  
Write(io_L,222) "01050105" , "4343" , "00", "1", Aimag(coeffBB_TLLNP),  " # coeffBB_TLLNP"  
Write(io_L,222) "01050105" , "4444" , "00", "1", Aimag(coeffBB_TRRNP),  " # coeffBB_TRRNP"  
Write(io_L,222) "03050305" , "3131" , "00", "1", Aimag(coeffBsBs_SLLNP),  " # coeffBsBs_SLLNP"  
Write(io_L,222) "03050305" , "3232" , "00", "1", Aimag(coeffBsBs_SRRNP),  " # coeffBsBs_SRRNP"  
Write(io_L,222) "03050305" , "3132" , "00", "1", Aimag(coeffBsBs_SLRNP),  " # coeffBsBs_SLRNP"  
Write(io_L,222) "03050305" , "4141" , "00", "1", Aimag(coeffBsBs_VLLNP),  " # coeffBsBs_VLLNP"  
Write(io_L,222) "03050305" , "4242" , "00", "1", Aimag(coeffBsBs_VRRNP),  " # coeffBsBs_VRRNP"  
Write(io_L,222) "03050305" , "4142" , "00", "1", Aimag(coeffBsBs_VLRNP),  " # coeffBsBs_VLRNP"  
Write(io_L,222) "03050305" , "4343" , "00", "1", Aimag(coeffBsBs_TLLNP),  " # coeffBsBs_TLLNP"  
Write(io_L,222) "03050305" , "4444" , "00", "1", Aimag(coeffBsBs_TRRNP),  " # coeffBsBs_TRRNP"  
Write(io_L,222) "01030103" , "3131" , "00", "0", Aimag(coeffKK_SLLSM),  " # coeffKK_SLLSM"  
Write(io_L,222) "01030103" , "3232" , "00", "0", Aimag(coeffKK_SRRSM),  " # coeffKK_SRRSM"  
Write(io_L,222) "01030103" , "3132" , "00", "0", Aimag(coeffKK_SLRSM),  " # coeffKK_SLRSM"  
Write(io_L,222) "01030103" , "4141" , "00", "0", Aimag(coeffKK_VLLSM),  " # coeffKK_VLLSM"  
Write(io_L,222) "01030103" , "4242" , "00", "0", Aimag(coeffKK_VRRSM),  " # coeffKK_VRRSM"  
Write(io_L,222) "01030103" , "4142" , "00", "0", Aimag(coeffKK_VLRSM),  " # coeffKK_VLRSM"  
Write(io_L,222) "01030103" , "4343" , "00", "0", Aimag(coeffKK_TLLSM),  " # coeffKK_TLLSM"  
Write(io_L,222) "01030103" , "4444" , "00", "0", Aimag(coeffKK_TRRSM),  " # coeffKK_TRRSM"  
Write(io_L,222) "01050105" , "3131" , "00", "0", Aimag(coeffBB_SLLSM),  " # coeffBB_SLLSM"  
Write(io_L,222) "01050105" , "3232" , "00", "0", Aimag(coeffBB_SRRSM),  " # coeffBB_SRRSM"  
Write(io_L,222) "01050105" , "3132" , "00", "0", Aimag(coeffBB_SLRSM),  " # coeffBB_SLRSM"  
Write(io_L,222) "01050105" , "4141" , "00", "0", Aimag(coeffBB_VLLSM),  " # coeffBB_VLLSM"  
Write(io_L,222) "01050105" , "4242" , "00", "0", Aimag(coeffBB_VRRSM),  " # coeffBB_VRRSM"  
Write(io_L,222) "01050105" , "4142" , "00", "0", Aimag(coeffBB_VLRSM),  " # coeffBB_VLRSM"  
Write(io_L,222) "01050105" , "4343" , "00", "0", Aimag(coeffBB_TLLSM),  " # coeffBB_TLLSM"  
Write(io_L,222) "01050105" , "4444" , "00", "0", Aimag(coeffBB_TRRSM),  " # coeffBB_TRRSM"  
Write(io_L,222) "03050305" , "3131" , "00", "0", Aimag(coeffBsBs_SLLSM),  " # coeffBsBs_SLLSM"  
Write(io_L,222) "03050305" , "3232" , "00", "0", Aimag(coeffBsBs_SRRSM),  " # coeffBsBs_SRRSM"  
Write(io_L,222) "03050305" , "3132" , "00", "0", Aimag(coeffBsBs_SLRSM),  " # coeffBsBs_SLRSM"  
Write(io_L,222) "03050305" , "4141" , "00", "0", Aimag(coeffBsBs_VLLSM),  " # coeffBsBs_VLLSM"  
Write(io_L,222) "03050305" , "4242" , "00", "0", Aimag(coeffBsBs_VRRSM),  " # coeffBsBs_VRRSM"  
Write(io_L,222) "03050305" , "4142" , "00", "0", Aimag(coeffBsBs_VLRSM),  " # coeffBsBs_VLRSM"  
Write(io_L,222) "03050305" , "4343" , "00", "0", Aimag(coeffBsBs_TLLSM),  " # coeffBsBs_TLLSM"  
Write(io_L,222) "03050305" , "4444" , "00", "0", Aimag(coeffBsBs_TRRSM),  " # coeffBsBs_TRRSM"  
Write(io_L,100) "Block TREELEVELUNITARITY #  " 
Write(io_L,1010) 0, TreeUnitarity,  "# Tree-level unitarity limits fulfilled or not "  
Write(io_L,1010) 1, max_scattering_eigenvalue,  "# Maximal scattering eigenvalue "  
If (TrilinearUnitarity) Then  
Write(io_L,100) "Block TREELEVELUNITARITYwTRILINEARS #  " 
Write(io_L,1010) 0, TreeUnitarityTrilinear,  "# Tree-level unitarity limits fulfilled or not "  
Write(io_L,1010) 1, max_scattering_eigenvalue_trilinears,  "# Maximal scattering eigenvalue "  
Write(io_L,1010) 2, unitarity_s_best,  "# best scattering energy "  
Write(io_L,1010) 11, unitarity_s_min,  "# min scattering energy "  
Write(io_L,1010) 12, unitarity_s_max,  "# max scattering energy "  
Write(io_L,1010) 13, 1._dp*unitarity_steps,  "# steps "  
End If 

 

 
 !-------------------------------
!Fu
!-------------------------------
 
If ((gTFu(1).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFu(1)),gTFu(1),Trim(NameParticleFu(1)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRFu(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFu(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(1))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFu(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = -PDGHm(gt2) 
Write(io_L,201) BRFu(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(1))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleHm(gt2))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFu(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRFu(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(1))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRFu(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFu(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(1))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFu(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFu(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(1))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFu(1,25:267)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFu(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFu(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(1))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFu(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFu(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(1))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=gt1,3
If (BRFu(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFu(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(1))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=gt2,6
If (BRFu(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFu(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(1))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,6
If (BRFu(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFu(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(1))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 
If ((gTFu(2).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFu(2)),gTFu(2),Trim(NameParticleFu(2)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRFu(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFu(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(2))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFu(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = -PDGHm(gt2) 
Write(io_L,201) BRFu(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(2))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleHm(gt2))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFu(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRFu(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(2))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRFu(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFu(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(2))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFu(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFu(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(2))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFu(2,25:267)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFu(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFu(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(2))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFu(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFu(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(2))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=gt1,3
If (BRFu(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFu(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(2))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=gt2,6
If (BRFu(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFu(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(2))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,6
If (BRFu(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFu(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(2))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 
If ((gTFu(3).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFu(3)),gTFu(3),Trim(NameParticleFu(3)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRFu(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFu(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(3))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFu(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = -PDGHm(gt2) 
Write(io_L,201) BRFu(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(3))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleHm(gt2))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFu(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRFu(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(3))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRFu(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFu(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(3))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFu(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFu(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFu(3))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFu(3,25:267)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFu(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFu(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(3))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFu(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFu(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(3))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=gt1,3
If (BRFu(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFu(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(3))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=gt2,6
If (BRFu(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFu(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(3))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,6
If (BRFu(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFu(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFu(3))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 

 
 !-------------------------------
!Fe
!-------------------------------
 
If ((gTFe(1).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFe(1)),gTFe(1),Trim(NameParticleFe(1)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRFe(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFe(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(1))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRFe(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFe(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(1))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFe(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFe(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(1))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 6
  Do gt2=2, 2
If (BRFe(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRFe(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(1))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
If (BRFe(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGVWm 
Write(io_L,201) BRFe(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(1))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFe(1,31:273)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFe(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFe(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(1))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=gt1,3
If (BRFe(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFe(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(1))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFe(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFe(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(1))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=gt2,6
If (BRFe(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFe(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(1))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFe(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFe(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(1))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 
If ((gTFe(2).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFe(2)),gTFe(2),Trim(NameParticleFe(2)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRFe(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFe(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(2))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRFe(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFe(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(2))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFe(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFe(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(2))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 6
  Do gt2=2, 2
If (BRFe(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRFe(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
If (BRFe(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGVWm 
Write(io_L,201) BRFe(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFe(2,31:273)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFe(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFe(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(2))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=gt1,3
If (BRFe(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFe(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(2))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFe(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFe(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(2))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=gt2,6
If (BRFe(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFe(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(2))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFe(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFe(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 
If ((gTFe(3).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFe(3)),gTFe(3),Trim(NameParticleFe(3)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRFe(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFe(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(3))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRFe(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFe(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(3))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFe(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFe(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(3))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 6
  Do gt2=2, 2
If (BRFe(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRFe(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
If (BRFe(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGVWm 
Write(io_L,201) BRFe(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFe(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFe(3,31:273)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFe(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFe(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(3))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=gt1,3
If (BRFe(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFe(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(3))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFe(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFe(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(3))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=gt2,6
If (BRFe(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFe(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(3))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFe(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFe(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFe(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 

 
 !-------------------------------
!Fd
!-------------------------------
 
If ((gTFd(1).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFd(1)),gTFd(1),Trim(NameParticleFd(1)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRFd(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFd(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(1))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRFd(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFd(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(1))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFd(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFd(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(1))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFd(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRFd(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(1))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFd(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGVWm 
Write(io_L,201) BRFd(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(1))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFd(1,25:267)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=gt1,3
If (BRFd(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFd(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(1))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFd(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFd(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(1))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFd(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFd(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(1))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=gt2,6
If (BRFd(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFd(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(1))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=1,3
If (BRFd(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFd(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(1))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 
If ((gTFd(2).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFd(2)),gTFd(2),Trim(NameParticleFd(2)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRFd(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFd(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(2))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRFd(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFd(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(2))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFd(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFd(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(2))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFd(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRFd(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(2))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFd(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGVWm 
Write(io_L,201) BRFd(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(2))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFd(2,25:267)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=gt1,3
If (BRFd(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFd(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(2))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFd(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFd(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(2))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFd(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFd(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(2))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=gt2,6
If (BRFd(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFd(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(2))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=1,3
If (BRFd(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFd(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(2))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 
If ((gTFd(3).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFd(3)),gTFd(3),Trim(NameParticleFd(3)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRFd(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFd(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(3))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRFd(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFd(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(3))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFd(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFd(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFd(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(3))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFd(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRFd(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(3))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFd(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = PDGVWm 
Write(io_L,201) BRFd(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFd(3))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFd(3,25:267)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=gt1,3
If (BRFd(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFd(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(3))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFd(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFd(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(3))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFd(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFd(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(3))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=gt2,6
If (BRFd(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFd(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFd(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(3))//" -> "//Trim(NameParticleFd(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,6
    Do gt3=1,3
If (BRFd(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFu(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFd(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFd(3))//" -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 

 
 !-------------------------------
!Fv
!-------------------------------
 
If ((gTFv(1).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFv(1)),gTFv(1),Trim(NameParticleFv(1)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 6
  Do gt2=2, 3
If (BRFv(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFv(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFv(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGHm(gt2) 
Write(io_L,201) BRFv(1,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleHm(gt2))//"^* "//")"
Write(io_L,201) BRFv(1,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFv(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRFv(1,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRFv(1,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 6
  Do gt2=1, 3
If (BRFv(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFv(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
If (BRFv(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFv(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFv(1,43:447)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFv(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFv(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=gt1,6
    Do gt3=gt2,6
If (BRFv(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFv(1,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(1,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(1,icount)/2._dp,3,CurrentPDG3, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
Write(io_L,202) BRFv(1,icount)/2._dp,3,-CurrentPDG3, & 
 & Trim(NameParticleFv(1))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//Trim(NameParticleFu(gt3))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 
If ((gTFv(2).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFv(2)),gTFv(2),Trim(NameParticleFv(2)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 6
  Do gt2=2, 3
If (BRFv(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFv(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFv(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGHm(gt2) 
Write(io_L,201) BRFv(2,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleHm(gt2))//"^* "//")"
Write(io_L,201) BRFv(2,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFv(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRFv(2,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRFv(2,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 6
  Do gt2=1, 3
If (BRFv(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFv(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
If (BRFv(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFv(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFv(2,43:447)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFv(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFv(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=gt1,6
    Do gt3=gt2,6
If (BRFv(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFv(2,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(2,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(2,icount)/2._dp,3,CurrentPDG3, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
Write(io_L,202) BRFv(2,icount)/2._dp,3,-CurrentPDG3, & 
 & Trim(NameParticleFv(2))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//Trim(NameParticleFu(gt3))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 
If ((gTFv(3).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFv(3)),gTFv(3),Trim(NameParticleFv(3)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 6
  Do gt2=2, 3
If (BRFv(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFv(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFv(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGHm(gt2) 
Write(io_L,201) BRFv(3,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleHm(gt2))//"^* "//")"
Write(io_L,201) BRFv(3,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFv(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRFv(3,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRFv(3,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 6
  Do gt2=1, 3
If (BRFv(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFv(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
If (BRFv(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFv(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFv(3,43:447)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFv(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFv(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=gt1,6
    Do gt3=gt2,6
If (BRFv(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFv(3,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(3,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(3,icount)/2._dp,3,CurrentPDG3, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
Write(io_L,202) BRFv(3,icount)/2._dp,3,-CurrentPDG3, & 
 & Trim(NameParticleFv(3))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//Trim(NameParticleFu(gt3))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 
If ((gTFv(4).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFv(4)),gTFv(4),Trim(NameParticleFv(4)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 6
  Do gt2=2, 3
If (BRFv(4,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFv(4,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFv(4,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGHm(gt2) 
Write(io_L,201) BRFv(4,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleHm(gt2))//"^* "//")"
Write(io_L,201) BRFv(4,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFv(4,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRFv(4,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRFv(4,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 6
  Do gt2=1, 3
If (BRFv(4,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFv(4,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
If (BRFv(4,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFv(4,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFv(4,43:447)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(4,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFv(4,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(4,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFv(4,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(4,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(4,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=gt1,6
    Do gt3=gt2,6
If (BRFv(4,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFv(4,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(4,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(4,icount)/2._dp,3,CurrentPDG3, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
Write(io_L,202) BRFv(4,icount)/2._dp,3,-CurrentPDG3, & 
 & Trim(NameParticleFv(4))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//Trim(NameParticleFu(gt3))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 
If ((gTFv(5).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFv(5)),gTFv(5),Trim(NameParticleFv(5)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 6
  Do gt2=2, 3
If (BRFv(5,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFv(5,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFv(5,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGHm(gt2) 
Write(io_L,201) BRFv(5,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleHm(gt2))//"^* "//")"
Write(io_L,201) BRFv(5,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFv(5,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRFv(5,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRFv(5,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 6
  Do gt2=1, 3
If (BRFv(5,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFv(5,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
If (BRFv(5,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFv(5,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFv(5,43:447)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(5,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFv(5,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(5,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFv(5,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(5,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(5,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=gt1,6
    Do gt3=gt2,6
If (BRFv(5,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFv(5,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(5,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(5,icount)/2._dp,3,CurrentPDG3, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
Write(io_L,202) BRFv(5,icount)/2._dp,3,-CurrentPDG3, & 
 & Trim(NameParticleFv(5))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//Trim(NameParticleFu(gt3))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 
If ((gTFv(6).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGFv(6)),gTFv(6),Trim(NameParticleFv(6)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 6
  Do gt2=2, 3
If (BRFv(6,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRFv(6,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 2
If (BRFv(6,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGHm(gt2) 
Write(io_L,201) BRFv(6,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleHm(gt2))//"^* "//")"
Write(io_L,201) BRFv(6,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRFv(6,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFe(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRFv(6,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRFv(6,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 6
  Do gt2=1, 3
If (BRFv(6,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRFv(6,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
If (BRFv(6,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRFv(6,icount),2,CurrentPDG2, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
If (Maxval(BRFv(6,43:447)).Gt.BRmin) Then 
Write(io_L,100) "#    BR                NDA      ID1      ID2       ID3" 
End If 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(6,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFd(gt3) 
Write(io_L,202) BRFv(6,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFd(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(6,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFe(gt2) 
CurrentPDG3(3) = PDGFe(gt3) 
Write(io_L,202) BRFv(6,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//Trim(NameParticleFe(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(6,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = -PDGFu(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(6,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFu(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,6
  Do gt2=gt1,6
    Do gt3=gt2,6
If (BRFv(6,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFv(gt1) 
CurrentPDG3(2) = PDGFv(gt2) 
CurrentPDG3(3) = PDGFv(gt3) 
Write(io_L,202) BRFv(6,icount),3,CurrentPDG3, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//Trim(NameParticleFv(gt3))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
Do gt1=1,3
  Do gt2=1,3
    Do gt3=1,3
If (BRFv(6,icount).Gt.BrMin) Then 
CurrentPDG3(1) = PDGFe(gt1) 
CurrentPDG3(2) = -PDGFd(gt2) 
CurrentPDG3(3) = PDGFu(gt3) 
Write(io_L,202) BRFv(6,icount)/2._dp,3,CurrentPDG3, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFe(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//Trim(NameParticleFu(gt3))//" "//")"
Write(io_L,202) BRFv(6,icount)/2._dp,3,-CurrentPDG3, & 
 & Trim(NameParticleFv(6))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//Trim(NameParticleFu(gt3))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
End Do 
 
End if 

 
 !-------------------------------
!hh
!-------------------------------
 
If ((gThh(1).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGhh(1)),gThh(1),Trim(NameParticlehh(1)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVP 
CurrentPDG2(2) = PDGVP 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleVP)//" "//Trim(NameParticleVP)//" "//")"
End if 
icount = icount +1 
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVG 
CurrentPDG2(2) = PDGVG 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleVG)//" "//Trim(NameParticleVG)//" "//")"
End if 
icount = icount +1 
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVZ 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleVZ)//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGVWm 
CurrentPDG2(2) = PDGVWm 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleVWm)//"^* "//Trim(NameParticleVWm)//"_virt "//")"
End if 
icount = icount +1 
Do gt1= 2, 3
  Do gt2= gt1, 3
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGAh(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleAh(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 3
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGAh(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleAh(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFd(gt1) 
CurrentPDG2(2) = PDGFd(gt2) 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleFd(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFe(gt1) 
CurrentPDG2(2) = PDGFe(gt2) 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFe(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFu(gt1) 
CurrentPDG2(2) = PDGFu(gt2) 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleFu(gt1))//"^* "//Trim(NameParticleFu(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
  Do gt2= gt1, 6
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGFv(gt2) 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//")"
If (((gTFv(gt1).le.WidthToBeInvisible).and.(gTFv(gt2).le.WidthToBeInvisible)).OR. & 
 & ((WidthToBeInvisible.le.-1._dp).And.(CurrentPDG2(1).eq.PDGLSP(1)).And.(CurrentPDG2(2).eq.PDGLSP(1)))) Then 
  BRinvH(1) = BRinvH(1)+BRhh(1,icount) 
End if 
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2= gt1, 3
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 2
  Do gt2=2, 2
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 2
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGHm(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRhh(1,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleHm(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRhh(1,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVWm 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleVWm)//" "//Trim(NameParticleVWm)//"^* "//")"
End if 
icount = icount +1 
If (BRhh(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVZ 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRhh(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(1))//" -> "//Trim(NameParticleVZ)//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
End if 
If ((gThh(2).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGhh(2)),gThh(2),Trim(NameParticlehh(2)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVP 
CurrentPDG2(2) = PDGVP 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleVP)//" "//Trim(NameParticleVP)//" "//")"
End if 
icount = icount +1 
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVG 
CurrentPDG2(2) = PDGVG 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleVG)//" "//Trim(NameParticleVG)//" "//")"
End if 
icount = icount +1 
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVZ 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleVZ)//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGVWm 
CurrentPDG2(2) = PDGVWm 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleVWm)//"^* "//Trim(NameParticleVWm)//"_virt "//")"
End if 
icount = icount +1 
Do gt1= 2, 3
  Do gt2= gt1, 3
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGAh(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleAh(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 3
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGAh(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleAh(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFd(gt1) 
CurrentPDG2(2) = PDGFd(gt2) 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleFd(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFe(gt1) 
CurrentPDG2(2) = PDGFe(gt2) 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFe(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFu(gt1) 
CurrentPDG2(2) = PDGFu(gt2) 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleFu(gt1))//"^* "//Trim(NameParticleFu(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
  Do gt2= gt1, 6
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGFv(gt2) 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//")"
If (((gTFv(gt1).le.WidthToBeInvisible).and.(gTFv(gt2).le.WidthToBeInvisible)).OR. & 
 & ((WidthToBeInvisible.le.-1._dp).And.(CurrentPDG2(1).eq.PDGLSP(1)).And.(CurrentPDG2(2).eq.PDGLSP(1)))) Then 
  BRinvH(2) = BRinvH(2)+BRhh(2,icount) 
End if 
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2= gt1, 3
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 2
  Do gt2=2, 2
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 2
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGHm(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRhh(2,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleHm(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRhh(2,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVWm 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleVWm)//" "//Trim(NameParticleVWm)//"^* "//")"
End if 
icount = icount +1 
If (BRhh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVZ 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRhh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(2))//" -> "//Trim(NameParticleVZ)//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
End if 
If ((gThh(3).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGhh(3)),gThh(3),Trim(NameParticlehh(3)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVP 
CurrentPDG2(2) = PDGVP 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleVP)//" "//Trim(NameParticleVP)//" "//")"
End if 
icount = icount +1 
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVG 
CurrentPDG2(2) = PDGVG 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleVG)//" "//Trim(NameParticleVG)//" "//")"
End if 
icount = icount +1 
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVZ 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleVZ)//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGVWm 
CurrentPDG2(2) = PDGVWm 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleVWm)//"^* "//Trim(NameParticleVWm)//"_virt "//")"
End if 
icount = icount +1 
Do gt1= 2, 3
  Do gt2= gt1, 3
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGAh(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleAh(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 3
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGAh(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleAh(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFd(gt1) 
CurrentPDG2(2) = PDGFd(gt2) 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleFd(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFe(gt1) 
CurrentPDG2(2) = PDGFe(gt2) 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFe(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFu(gt1) 
CurrentPDG2(2) = PDGFu(gt2) 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleFu(gt1))//"^* "//Trim(NameParticleFu(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
  Do gt2= gt1, 6
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGFv(gt2) 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//")"
If (((gTFv(gt1).le.WidthToBeInvisible).and.(gTFv(gt2).le.WidthToBeInvisible)).OR. & 
 & ((WidthToBeInvisible.le.-1._dp).And.(CurrentPDG2(1).eq.PDGLSP(1)).And.(CurrentPDG2(2).eq.PDGLSP(1)))) Then 
  BRinvH(3) = BRinvH(3)+BRhh(3,icount) 
End if 
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2= gt1, 3
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 2
  Do gt2=2, 2
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 2
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGHm(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRhh(3,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleHm(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRhh(3,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVWm 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleVWm)//" "//Trim(NameParticleVWm)//"^* "//")"
End if 
icount = icount +1 
If (BRhh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVZ 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRhh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticlehh(3))//" -> "//Trim(NameParticleVZ)//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
End if 

 
 !-------------------------------
!Ah
!-------------------------------
 
If ((gTAh(2).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGAh(2)),gTAh(2),Trim(NameParticleAh(2)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVP 
CurrentPDG2(2) = PDGVP 
Write(io_L,201) BRAh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticleVP)//" "//Trim(NameParticleVP)//" "//")"
End if 
icount = icount +1 
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVG 
CurrentPDG2(2) = PDGVG 
Write(io_L,201) BRAh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticleVG)//" "//Trim(NameParticleVG)//" "//")"
End if 
icount = icount +1 
Do gt1= 2, 3
  Do gt2= gt1, 3
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGAh(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRAh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticleAh(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRAh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFd(gt1) 
CurrentPDG2(2) = PDGFd(gt2) 
Write(io_L,201) BRAh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticleFd(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFe(gt1) 
CurrentPDG2(2) = PDGFe(gt2) 
Write(io_L,201) BRAh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFe(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFu(gt1) 
CurrentPDG2(2) = PDGFu(gt2) 
Write(io_L,201) BRAh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticleFu(gt1))//"^* "//Trim(NameParticleFu(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
  Do gt2= gt1, 6
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGFv(gt2) 
Write(io_L,201) BRAh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//")"
If (((gTFv(gt1).le.WidthToBeInvisible).and.(gTFv(gt2).le.WidthToBeInvisible)).OR. & 
 & ((WidthToBeInvisible.le.-1._dp).And.(CurrentPDG2(1).eq.PDGLSP(1)).And.(CurrentPDG2(2).eq.PDGLSP(1)))) Then 
  BRinvA(2) = BRinvA(2)+BRAh(2,icount) 
End if 
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2= gt1, 3
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRAh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRAh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 2, 2
  Do gt2=2, 2
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRAh(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 2
If (BRAh(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGHm(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRAh(2,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticleHm(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRAh(2,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleAh(2))//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
End if 
If ((gTAh(3).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGAh(3)),gTAh(3),Trim(NameParticleAh(3)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVP 
CurrentPDG2(2) = PDGVP 
Write(io_L,201) BRAh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticleVP)//" "//Trim(NameParticleVP)//" "//")"
End if 
icount = icount +1 
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVG 
CurrentPDG2(2) = PDGVG 
Write(io_L,201) BRAh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticleVG)//" "//Trim(NameParticleVG)//" "//")"
End if 
icount = icount +1 
Do gt1= 2, 3
  Do gt2= gt1, 3
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGAh(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRAh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticleAh(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRAh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFd(gt1) 
CurrentPDG2(2) = PDGFd(gt2) 
Write(io_L,201) BRAh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticleFd(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFe(gt1) 
CurrentPDG2(2) = PDGFe(gt2) 
Write(io_L,201) BRAh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFe(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFu(gt1) 
CurrentPDG2(2) = PDGFu(gt2) 
Write(io_L,201) BRAh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticleFu(gt1))//"^* "//Trim(NameParticleFu(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
  Do gt2= gt1, 6
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGFv(gt2) 
Write(io_L,201) BRAh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//")"
If (((gTFv(gt1).le.WidthToBeInvisible).and.(gTFv(gt2).le.WidthToBeInvisible)).OR. & 
 & ((WidthToBeInvisible.le.-1._dp).And.(CurrentPDG2(1).eq.PDGLSP(1)).And.(CurrentPDG2(2).eq.PDGLSP(1)))) Then 
  BRinvA(3) = BRinvA(3)+BRAh(3,icount) 
End if 
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2= gt1, 3
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRAh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRAh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 2, 2
  Do gt2=2, 2
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRAh(3,icount),2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 2
If (BRAh(3,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGHm(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRAh(3,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticleHm(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRAh(3,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleAh(3))//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
End if 

 
 !-------------------------------
!Hm
!-------------------------------
 
If ((gTHm(2).gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) -INT(PDGHm(2)),gTHm(2),Trim(NameParticleHm(2)) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 2, 2
  Do gt2=2, 3
If (BRHm(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRHm(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleHm(2))//"^* -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 3
If (BRHm(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGAh(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRHm(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleHm(2))//"^* -> "//Trim(NameParticleAh(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRHm(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = -PDGFd(gt2) 
Write(io_L,201) BRHm(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleHm(2))//"^* -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
  Do gt2=1, 3
If (BRHm(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = -PDGFe(gt2) 
Write(io_L,201) BRHm(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleHm(2))//"^* -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 2
  Do gt2=1, 3
If (BRHm(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRHm(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleHm(2))//"^* -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRHm(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRHm(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleHm(2))//"^* -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 2, 2
If (BRHm(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRHm(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleHm(2))//"^* -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
If (BRHm(2,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGVWm 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRHm(2,icount),2,CurrentPDG2, & 
 & Trim(NameParticleHm(2))//"^* -> "//Trim(NameParticleVWm)//"^* "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
End if 

 
 !-------------------------------
!VZ
!-------------------------------
 
If ((gTVZ.gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) INT(PDGVZ),gTVZ,Trim(NameParticleVZ) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 1, 3
  Do gt2=2, 3
If (BRVZ(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRVZ(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVZ)//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRVZ(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFd(gt1) 
CurrentPDG2(2) = PDGFd(gt2) 
Write(io_L,201) BRVZ(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVZ)//" -> "//Trim(NameParticleFd(gt1))//"^* "//Trim(NameParticleFd(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRVZ(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFe(gt1) 
CurrentPDG2(2) = PDGFe(gt2) 
Write(io_L,201) BRVZ(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVZ)//" -> "//Trim(NameParticleFe(gt1))//"^* "//Trim(NameParticleFe(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRVZ(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGFu(gt1) 
CurrentPDG2(2) = PDGFu(gt2) 
Write(io_L,201) BRVZ(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVZ)//" -> "//Trim(NameParticleFu(gt1))//"^* "//Trim(NameParticleFu(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
  Do gt2= gt1, 6
If (BRVZ(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = PDGFv(gt2) 
Write(io_L,201) BRVZ(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVZ)//" -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFv(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRVZ(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRVZ(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVZ)//" -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 2, 2
  Do gt2=2, 2
If (BRVZ(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGHm(gt2) 
Write(io_L,201) BRVZ(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVZ)//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleHm(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 2
If (BRVZ(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGHm(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRVZ(1,icount)/2._dp,2,CurrentPDG2, & 
 & Trim(NameParticleVZ)//" -> "//Trim(NameParticleHm(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
Write(io_L,201) BRVZ(1,icount)/2._dp,2,-CurrentPDG2, & 
 & Trim(NameParticleVZ)//" -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleVWm)//" "//")"
End if 
icount = icount +1 
  End Do 
If (BRVZ(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGVWm 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRVZ(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVZ)//" -> "//Trim(NameParticleVWm)//" "//Trim(NameParticleVWm)//"^* "//")"
End if 
icount = icount +1 
End if 

 
 !-------------------------------
!VWm
!-------------------------------
 
If ((gTVWm.gt.MinWidth).or.(OutputForMG)) Then 
Write(io_L,200) -INT(PDGVWm),gTVWm,Trim(NameParticleVWm) 
Write(io_L,100) "#    BR                NDA      ID1      ID2" 
icount = 1 
Do gt1= 2, 2
  Do gt2=2, 3
If (BRVWm(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGAh(gt2) 
Write(io_L,201) BRVWm(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVWm)//"^* -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleAh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
  Do gt2=1, 3
If (BRVWm(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFu(gt1) 
CurrentPDG2(2) = -PDGFd(gt2) 
Write(io_L,201) BRVWm(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVWm)//"^* -> "//Trim(NameParticleFu(gt1))//" "//Trim(NameParticleFd(gt2))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 6
  Do gt2=1, 3
If (BRVWm(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGFv(gt1) 
CurrentPDG2(2) = -PDGFe(gt2) 
Write(io_L,201) BRVWm(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVWm)//"^* -> "//Trim(NameParticleFv(gt1))//" "//Trim(NameParticleFe(gt2))//"^* "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 2, 2
  Do gt2=1, 3
If (BRVWm(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGhh(gt2) 
Write(io_L,201) BRVWm(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVWm)//"^* -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticlehh(gt2))//" "//")"
End if 
icount = icount +1 
  End Do 
End Do 
 
Do gt1= 1, 3
If (BRVWm(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = PDGhh(gt1) 
CurrentPDG2(2) = -PDGVWm 
Write(io_L,201) BRVWm(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVWm)//"^* -> "//Trim(NameParticlehh(gt1))//" "//Trim(NameParticleVWm)//"^* "//")"
End if 
icount = icount +1 
  End Do 
Do gt1= 2, 2
If (BRVWm(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGHm(gt1) 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRVWm(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVWm)//"^* -> "//Trim(NameParticleHm(gt1))//"^* "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
  End Do 
If (BRVWm(1,icount).Gt.BrMin) Then 
CurrentPDG2(1) = -PDGVWm 
CurrentPDG2(2) = PDGVZ 
Write(io_L,201) BRVWm(1,icount),2,CurrentPDG2, & 
 & Trim(NameParticleVWm)//"^* -> "//Trim(NameParticleVWm)//"^* "//Trim(NameParticleVZ)//" "//")"
End if 
icount = icount +1 
End if 
! Information needed by MadGraph 
If (OutputForMG) Then 
Write(io_L,200) INT(Abs(PDGVG)),0._dp, "VG" 
Write(io_L,200) INT(Abs(PDGVP)),0._dp, "VP" 
End if 
99 Format(1x,i5,3x,a) 
100 Format(a) 
101 Format(2x,i3,2x,1P,e16.8,2x,a) 
1010 Format(2x,i6,2x,1P,e16.8,2x,a) 
102 Format(1x,i9,3x,1P,e16.8,2x,a) 
103 Format(a13,1P,e16.8,2x,a) 
104 Format(i4,2x,1P,e16.8,2x,a) 
105 Format(1x,2i3,3x,1P,e16.8,3x,a) 
106 Format(a,1P,e16.8,2x,a) 
107 Format(2i3,3x,1P,e16.8,3x,a) 
127 Format(3i3,3x,1P,e16.8,3x,a) 
117 Format(i3,i8,3x,1P,e16.8,3x,a) 
118 Format(i3,i10,3x,1P,a) 
119 Format(i3,i10,3x,1P,3x,e16.8,a) 
120 Format(i3,i10,3x,1P,3x,e16.8,a,i2,a,i2,a) 
121 Format(i10,3x,i10,3x,i10,3x,e16.8,a) 
122 Format(i10,i10,3x,1P,3x,e16.8,a,i2,a,i2,a) 
108 Format(9x,1P,E16.8,0P,3x,a) 
109 Format(1x,3i3,3x,1P,e16.8,3x,a) 
110 Format(3x,2i3,3x,"# ",a) 
200 Format("DECAY",1x,I9,3x,1P,E16.8,0P,3x,"# ",a) 
210 Format("DECAY1L",1x,I9,3x,1P,E16.8,0P,3x,"# ",a) 
201 Format(3x,1P,e16.8,0p,3x,I2,3x,2(i10,1x),2x,"# BR(",a) 
202 Format(3x,1P,e16.8,0p,3x,I2,3x,3(i10,1x),2x,"# BR(",a) 
222 Format(1x,a8,1x,a4,3x,a2,3x,a1,3x,E16.8,3x,a) 
4711 Format(3x,1P,e16.8,0p,3x,I2,3x,2(i10,1x),2x," # ",A)
4712 Format("XS 11 -11 ",F7.1," ",F5.2," ",F5.2," ",A)

5410 Format(a25,1p,e16.7) 
5411 Format(a25,1p,"(",e16.7,",",e16.7,")") 
1101 Format(1P,2x,e16.8,2x,e16.8,0P,5x,i4,5x,3i10,a) 
1102 Format(1P,2x,e16.8,0P,5x,i4,5x,3i10,a) 
1103 Format(1P,2x,e16.8,0P,5x,i4,5x,4i10,a) 
End Subroutine LesHouches_Out 
 
 
Subroutine WriteWHIZARD 
   Open(123,file="WHIZARD.par.BGLNCS",status="unknown")
Write(123,*) "# Couplings and VEVs" 
 
Write(123,*) "" 
Write(123,*) "Lam1_r = ",Real(Lam1,dp) 
Write(123,*) "Lam1_i = ",AImag(Lam1) 
Write(123,*) "Lam3_r = ",Real(Lam3,dp) 
Write(123,*) "Lam3_i = ",AImag(Lam3) 
Write(123,*) "Lam4_r = ",Real(Lam4,dp) 
Write(123,*) "Lam4_i = ",AImag(Lam4) 
Write(123,*) "Lam2_r = ",Real(Lam2,dp) 
Write(123,*) "Lam2_i = ",AImag(Lam2) 
Write(123,*) "Lam1Dash_r = ",Real(Lam1Dash,dp) 
Write(123,*) "Lam1Dash_i = ",AImag(Lam1Dash) 
Write(123,*) "Lam2Dash_r = ",Real(Lam2Dash,dp) 
Write(123,*) "Lam2Dash_i = ",AImag(Lam2Dash) 
Write(123,*) "Lam3Dash_r = ",Real(Lam3Dash,dp) 
Write(123,*) "Lam3Dash_i = ",AImag(Lam3Dash) 
Write(123,*) "Aa3_r = ",Real(Aa3,dp) 
Write(123,*) "Aa3_i = ",AImag(Aa3) 
Write(123,*) "Aa4_r = ",Real(Aa4,dp) 
Write(123,*) "Aa4_i = ",AImag(Aa4) 
Write(123,*) "Y1d11_r = ",Real(Y1d11,dp) 
Write(123,*) "Y1d11_i = ",AImag(Y1d11) 
Write(123,*) "Y1d12_r = ",Real(Y1d12,dp) 
Write(123,*) "Y1d12_i = ",AImag(Y1d12) 
Write(123,*) "Y1d13_r = ",Real(Y1d13,dp) 
Write(123,*) "Y1d13_i = ",AImag(Y1d13) 
Write(123,*) "Y1d21_r = ",Real(Y1d21,dp) 
Write(123,*) "Y1d21_i = ",AImag(Y1d21) 
Write(123,*) "Y1d22_r = ",Real(Y1d22,dp) 
Write(123,*) "Y1d22_i = ",AImag(Y1d22) 
Write(123,*) "Y1d23_r = ",Real(Y1d23,dp) 
Write(123,*) "Y1d23_i = ",AImag(Y1d23) 
Write(123,*) "Y2d31_r = ",Real(Y2d31,dp) 
Write(123,*) "Y2d31_i = ",AImag(Y2d31) 
Write(123,*) "Y2d32_r = ",Real(Y2d32,dp) 
Write(123,*) "Y2d32_i = ",AImag(Y2d32) 
Write(123,*) "Y2d33_r = ",Real(Y2d33,dp) 
Write(123,*) "Y2d33_i = ",AImag(Y2d33) 
Write(123,*) "Y1u11_r = ",Real(Y1u11,dp) 
Write(123,*) "Y1u11_i = ",AImag(Y1u11) 
Write(123,*) "Y1u12_r = ",Real(Y1u12,dp) 
Write(123,*) "Y1u12_i = ",AImag(Y1u12) 
Write(123,*) "Y1u21_r = ",Real(Y1u21,dp) 
Write(123,*) "Y1u21_i = ",AImag(Y1u21) 
Write(123,*) "Y1u22_r = ",Real(Y1u22,dp) 
Write(123,*) "Y1u22_i = ",AImag(Y1u22) 
Write(123,*) "Y2u33_r = ",Real(Y2u33,dp) 
Write(123,*) "Y2u33_i = ",AImag(Y2u33) 
Write(123,*) "Y1e11_r = ",Real(Y1e11,dp) 
Write(123,*) "Y1e11_i = ",AImag(Y1e11) 
Write(123,*) "Y1e12_r = ",Real(Y1e12,dp) 
Write(123,*) "Y1e12_i = ",AImag(Y1e12) 
Write(123,*) "Y1e21_r = ",Real(Y1e21,dp) 
Write(123,*) "Y1e21_i = ",AImag(Y1e21) 
Write(123,*) "Y1e22_r = ",Real(Y1e22,dp) 
Write(123,*) "Y1e22_i = ",AImag(Y1e22) 
Write(123,*) "Y2e33_r = ",Real(Y2e33,dp) 
Write(123,*) "Y2e33_i = ",AImag(Y2e33) 
Write(123,*) "Y1n11_r = ",Real(Y1n11,dp) 
Write(123,*) "Y1n11_i = ",AImag(Y1n11) 
Write(123,*) "Y1n12_r = ",Real(Y1n12,dp) 
Write(123,*) "Y1n12_i = ",AImag(Y1n12) 
Write(123,*) "Y1n21_r = ",Real(Y1n21,dp) 
Write(123,*) "Y1n21_i = ",AImag(Y1n21) 
Write(123,*) "Y1n22_r = ",Real(Y1n22,dp) 
Write(123,*) "Y1n22_i = ",AImag(Y1n22) 
Write(123,*) "Y2n33_r = ",Real(Y2n33,dp) 
Write(123,*) "Y2n33_i = ",AImag(Y2n33) 
Write(123,*) "C13_r = ",Real(C13,dp) 
Write(123,*) "C13_i = ",AImag(C13) 
Write(123,*) "C23_r = ",Real(C23,dp) 
Write(123,*) "C23_i = ",AImag(C23) 
Write(123,*) "C31_r = ",Real(C31,dp) 
Write(123,*) "C31_i = ",AImag(C31) 
Write(123,*) "C32_r = ",Real(C32,dp) 
Write(123,*) "C32_i = ",AImag(C32) 
Write(123,*) "BB11_r = ",Real(BB11,dp) 
Write(123,*) "BB11_i = ",AImag(BB11) 
Write(123,*) "BB12_r = ",Real(BB12,dp) 
Write(123,*) "BB12_i = ",AImag(BB12) 
Write(123,*) "BB21_r = ",Real(BB21,dp) 
Write(123,*) "BB21_i = ",AImag(BB21) 
Write(123,*) "BB22_r = ",Real(BB22,dp) 
Write(123,*) "BB22_i = ",AImag(BB22) 
Write(123,*) "Aa1_r = ",Real(Aa1,dp) 
Write(123,*) "Aa1_i = ",AImag(Aa1) 
Write(123,*) "Aa2_r = ",Real(Aa2,dp) 
Write(123,*) "Aa2_i = ",AImag(Aa2) 
Write(123,*) "v3= ",v3
Write(123,*) "v3= ",v3
Write(123,*) "" 
Write(123,*) "" 

 
 
 Write(123,*) "# Dependent parameters " 
 
Write(123,*) "" 
Write(123,*) "" 
Write(123,*) "" 

 
 
 Write(123,*) "# Necessary MINPAR parameters " 
 
Write(123,*) "" 
Write(123,*) "" 
Write(123,*) "" 

 
 
 Write(123,*) "# Masses of particles" 
 
Write(123,*) "" 
Write(123,*) "Mh1= ", Abs(Mhh(1)) 
Write(123,*) "Mh2= ", Abs(Mhh(2)) 
Write(123,*) "Mh3= ", Abs(Mhh(3)) 
Write(123,*) "MAh2= ", Abs(MAh(2)) 
Write(123,*) "MAh3= ", Abs(MAh(3)) 
Write(123,*) "MHm2= ", Abs(MHm(2)) 
Write(123,*) "MZ= ", Abs(MVZ) 
Write(123,*) "Md1= ", Abs(MFd(1)) 
Write(123,*) "Md2= ", Abs(MFd(2)) 
Write(123,*) "Md3= ", Abs(MFd(3)) 
Write(123,*) "Mu1= ", Abs(MFu(1)) 
Write(123,*) "Mu2= ", Abs(MFu(2)) 
Write(123,*) "Mu3= ", Abs(MFu(3)) 
Write(123,*) "Me1= ", Abs(MFe(1)) 
Write(123,*) "Me2= ", Abs(MFe(2)) 
Write(123,*) "Me3= ", Abs(MFe(3)) 
Write(123,*) "Mnu1= ", Abs(MFv(1)) 
Write(123,*) "Mnu2= ", Abs(MFv(2)) 
Write(123,*) "Mnu3= ", Abs(MFv(3)) 
Write(123,*) "Mnu4= ", Abs(MFv(4)) 
Write(123,*) "Mnu5= ", Abs(MFv(5)) 
Write(123,*) "Mnu6= ", Abs(MFv(6)) 
Write(123,*) "" 
Write(123,*) "" 

 
 
 Write(123,*) "# Widths of particles" 
 
Write(123,*) "" 
Write(123,*) "Wu3 = ",gTFu(3)
Write(123,*) "Wnu1 = ",gTFv(1)
Write(123,*) "Wnu2 = ",gTFv(2)
Write(123,*) "Wnu3 = ",gTFv(3)
Write(123,*) "Wnu4 = ",gTFv(4)
Write(123,*) "Wnu5 = ",gTFv(5)
Write(123,*) "Wnu6 = ",gTFv(6)
Write(123,*) "Wh1 = ",gThh(1)
Write(123,*) "Wh2 = ",gThh(2)
Write(123,*) "Wh3 = ",gThh(3)
Write(123,*) "WAh3 = ",gTAh(3)
Write(123,*) "WHm2 = ",gTHm(2)
Write(123,*) "WZ = ",gTVZ
Write(123,*) "WWm = ",gTVWm
Write(123,*) "" 
Write(123,*) "" 

 
 
 Write(123,*) "# Mixing matrices" 
 
Write(123,*) "" 
Write(123,*) "ZH11= ",ZH(1,1)
Write(123,*) "ZH12= ",ZH(1,2)
Write(123,*) "ZH13= ",ZH(1,3)
Write(123,*) "ZH21= ",ZH(2,1)
Write(123,*) "ZH22= ",ZH(2,2)
Write(123,*) "ZH23= ",ZH(2,3)
Write(123,*) "ZH31= ",ZH(3,1)
Write(123,*) "ZH32= ",ZH(3,2)
Write(123,*) "ZH33= ",ZH(3,3)
Write(123,*) "ZA11= ",ZA(1,1)
Write(123,*) "ZA12= ",ZA(1,2)
Write(123,*) "ZA13= ",ZA(1,3)
Write(123,*) "ZA21= ",ZA(2,1)
Write(123,*) "ZA22= ",ZA(2,2)
Write(123,*) "ZA23= ",ZA(2,3)
Write(123,*) "ZA31= ",ZA(3,1)
Write(123,*) "ZA32= ",ZA(3,2)
Write(123,*) "ZA33= ",ZA(3,3)
If (MHm(1).Gt.0._dp) Then 
Write(123,*) "ZP11_r = ", Real(ZP(1,1),dp)
Write(123,*) "ZP11_i = ", AImag(ZP(1,1))
Write(123,*) "ZP12_r = ", Real(ZP(1,2),dp)
Write(123,*) "ZP12_i = ", AImag(ZP(1,2))
Else 
Write(123,*) "ZP11_i = ", Real(ZP(1,1),dp)
Write(123,*) "ZP11_r = ", -AImag(ZP(1,1))
Write(123,*) "ZP12_i = ", Real(ZP(1,2),dp)
Write(123,*) "ZP12_r = ", -AImag(ZP(1,2))
End if 
If (MHm(2).Gt.0._dp) Then 
Write(123,*) "ZP21_r = ", Real(ZP(2,1),dp)
Write(123,*) "ZP21_i = ", AImag(ZP(2,1))
Write(123,*) "ZP22_r = ", Real(ZP(2,2),dp)
Write(123,*) "ZP22_i = ", AImag(ZP(2,2))
Else 
Write(123,*) "ZP21_i = ", Real(ZP(2,1),dp)
Write(123,*) "ZP21_r = ", -AImag(ZP(2,1))
Write(123,*) "ZP22_i = ", Real(ZP(2,2),dp)
Write(123,*) "ZP22_r = ", -AImag(ZP(2,2))
End if 
If (MFv(1).Gt.0._dp) Then 
Write(123,*) "Vv11_r = ", Real(Vv(1,1),dp)
Write(123,*) "Vv11_i = ", AImag(Vv(1,1))
Write(123,*) "Vv12_r = ", Real(Vv(1,2),dp)
Write(123,*) "Vv12_i = ", AImag(Vv(1,2))
Write(123,*) "Vv13_r = ", Real(Vv(1,3),dp)
Write(123,*) "Vv13_i = ", AImag(Vv(1,3))
Write(123,*) "Vv14_r = ", Real(Vv(1,4),dp)
Write(123,*) "Vv14_i = ", AImag(Vv(1,4))
Write(123,*) "Vv15_r = ", Real(Vv(1,5),dp)
Write(123,*) "Vv15_i = ", AImag(Vv(1,5))
Write(123,*) "Vv16_r = ", Real(Vv(1,6),dp)
Write(123,*) "Vv16_i = ", AImag(Vv(1,6))
Else 
Write(123,*) "Vv11_i = ", Real(Vv(1,1),dp)
Write(123,*) "Vv11_r = ", -AImag(Vv(1,1))
Write(123,*) "Vv12_i = ", Real(Vv(1,2),dp)
Write(123,*) "Vv12_r = ", -AImag(Vv(1,2))
Write(123,*) "Vv13_i = ", Real(Vv(1,3),dp)
Write(123,*) "Vv13_r = ", -AImag(Vv(1,3))
Write(123,*) "Vv14_i = ", Real(Vv(1,4),dp)
Write(123,*) "Vv14_r = ", -AImag(Vv(1,4))
Write(123,*) "Vv15_i = ", Real(Vv(1,5),dp)
Write(123,*) "Vv15_r = ", -AImag(Vv(1,5))
Write(123,*) "Vv16_i = ", Real(Vv(1,6),dp)
Write(123,*) "Vv16_r = ", -AImag(Vv(1,6))
End if 
If (MFv(2).Gt.0._dp) Then 
Write(123,*) "Vv21_r = ", Real(Vv(2,1),dp)
Write(123,*) "Vv21_i = ", AImag(Vv(2,1))
Write(123,*) "Vv22_r = ", Real(Vv(2,2),dp)
Write(123,*) "Vv22_i = ", AImag(Vv(2,2))
Write(123,*) "Vv23_r = ", Real(Vv(2,3),dp)
Write(123,*) "Vv23_i = ", AImag(Vv(2,3))
Write(123,*) "Vv24_r = ", Real(Vv(2,4),dp)
Write(123,*) "Vv24_i = ", AImag(Vv(2,4))
Write(123,*) "Vv25_r = ", Real(Vv(2,5),dp)
Write(123,*) "Vv25_i = ", AImag(Vv(2,5))
Write(123,*) "Vv26_r = ", Real(Vv(2,6),dp)
Write(123,*) "Vv26_i = ", AImag(Vv(2,6))
Else 
Write(123,*) "Vv21_i = ", Real(Vv(2,1),dp)
Write(123,*) "Vv21_r = ", -AImag(Vv(2,1))
Write(123,*) "Vv22_i = ", Real(Vv(2,2),dp)
Write(123,*) "Vv22_r = ", -AImag(Vv(2,2))
Write(123,*) "Vv23_i = ", Real(Vv(2,3),dp)
Write(123,*) "Vv23_r = ", -AImag(Vv(2,3))
Write(123,*) "Vv24_i = ", Real(Vv(2,4),dp)
Write(123,*) "Vv24_r = ", -AImag(Vv(2,4))
Write(123,*) "Vv25_i = ", Real(Vv(2,5),dp)
Write(123,*) "Vv25_r = ", -AImag(Vv(2,5))
Write(123,*) "Vv26_i = ", Real(Vv(2,6),dp)
Write(123,*) "Vv26_r = ", -AImag(Vv(2,6))
End if 
If (MFv(3).Gt.0._dp) Then 
Write(123,*) "Vv31_r = ", Real(Vv(3,1),dp)
Write(123,*) "Vv31_i = ", AImag(Vv(3,1))
Write(123,*) "Vv32_r = ", Real(Vv(3,2),dp)
Write(123,*) "Vv32_i = ", AImag(Vv(3,2))
Write(123,*) "Vv33_r = ", Real(Vv(3,3),dp)
Write(123,*) "Vv33_i = ", AImag(Vv(3,3))
Write(123,*) "Vv34_r = ", Real(Vv(3,4),dp)
Write(123,*) "Vv34_i = ", AImag(Vv(3,4))
Write(123,*) "Vv35_r = ", Real(Vv(3,5),dp)
Write(123,*) "Vv35_i = ", AImag(Vv(3,5))
Write(123,*) "Vv36_r = ", Real(Vv(3,6),dp)
Write(123,*) "Vv36_i = ", AImag(Vv(3,6))
Else 
Write(123,*) "Vv31_i = ", Real(Vv(3,1),dp)
Write(123,*) "Vv31_r = ", -AImag(Vv(3,1))
Write(123,*) "Vv32_i = ", Real(Vv(3,2),dp)
Write(123,*) "Vv32_r = ", -AImag(Vv(3,2))
Write(123,*) "Vv33_i = ", Real(Vv(3,3),dp)
Write(123,*) "Vv33_r = ", -AImag(Vv(3,3))
Write(123,*) "Vv34_i = ", Real(Vv(3,4),dp)
Write(123,*) "Vv34_r = ", -AImag(Vv(3,4))
Write(123,*) "Vv35_i = ", Real(Vv(3,5),dp)
Write(123,*) "Vv35_r = ", -AImag(Vv(3,5))
Write(123,*) "Vv36_i = ", Real(Vv(3,6),dp)
Write(123,*) "Vv36_r = ", -AImag(Vv(3,6))
End if 
If (MFv(4).Gt.0._dp) Then 
Write(123,*) "Vv41_r = ", Real(Vv(4,1),dp)
Write(123,*) "Vv41_i = ", AImag(Vv(4,1))
Write(123,*) "Vv42_r = ", Real(Vv(4,2),dp)
Write(123,*) "Vv42_i = ", AImag(Vv(4,2))
Write(123,*) "Vv43_r = ", Real(Vv(4,3),dp)
Write(123,*) "Vv43_i = ", AImag(Vv(4,3))
Write(123,*) "Vv44_r = ", Real(Vv(4,4),dp)
Write(123,*) "Vv44_i = ", AImag(Vv(4,4))
Write(123,*) "Vv45_r = ", Real(Vv(4,5),dp)
Write(123,*) "Vv45_i = ", AImag(Vv(4,5))
Write(123,*) "Vv46_r = ", Real(Vv(4,6),dp)
Write(123,*) "Vv46_i = ", AImag(Vv(4,6))
Else 
Write(123,*) "Vv41_i = ", Real(Vv(4,1),dp)
Write(123,*) "Vv41_r = ", -AImag(Vv(4,1))
Write(123,*) "Vv42_i = ", Real(Vv(4,2),dp)
Write(123,*) "Vv42_r = ", -AImag(Vv(4,2))
Write(123,*) "Vv43_i = ", Real(Vv(4,3),dp)
Write(123,*) "Vv43_r = ", -AImag(Vv(4,3))
Write(123,*) "Vv44_i = ", Real(Vv(4,4),dp)
Write(123,*) "Vv44_r = ", -AImag(Vv(4,4))
Write(123,*) "Vv45_i = ", Real(Vv(4,5),dp)
Write(123,*) "Vv45_r = ", -AImag(Vv(4,5))
Write(123,*) "Vv46_i = ", Real(Vv(4,6),dp)
Write(123,*) "Vv46_r = ", -AImag(Vv(4,6))
End if 
If (MFv(5).Gt.0._dp) Then 
Write(123,*) "Vv51_r = ", Real(Vv(5,1),dp)
Write(123,*) "Vv51_i = ", AImag(Vv(5,1))
Write(123,*) "Vv52_r = ", Real(Vv(5,2),dp)
Write(123,*) "Vv52_i = ", AImag(Vv(5,2))
Write(123,*) "Vv53_r = ", Real(Vv(5,3),dp)
Write(123,*) "Vv53_i = ", AImag(Vv(5,3))
Write(123,*) "Vv54_r = ", Real(Vv(5,4),dp)
Write(123,*) "Vv54_i = ", AImag(Vv(5,4))
Write(123,*) "Vv55_r = ", Real(Vv(5,5),dp)
Write(123,*) "Vv55_i = ", AImag(Vv(5,5))
Write(123,*) "Vv56_r = ", Real(Vv(5,6),dp)
Write(123,*) "Vv56_i = ", AImag(Vv(5,6))
Else 
Write(123,*) "Vv51_i = ", Real(Vv(5,1),dp)
Write(123,*) "Vv51_r = ", -AImag(Vv(5,1))
Write(123,*) "Vv52_i = ", Real(Vv(5,2),dp)
Write(123,*) "Vv52_r = ", -AImag(Vv(5,2))
Write(123,*) "Vv53_i = ", Real(Vv(5,3),dp)
Write(123,*) "Vv53_r = ", -AImag(Vv(5,3))
Write(123,*) "Vv54_i = ", Real(Vv(5,4),dp)
Write(123,*) "Vv54_r = ", -AImag(Vv(5,4))
Write(123,*) "Vv55_i = ", Real(Vv(5,5),dp)
Write(123,*) "Vv55_r = ", -AImag(Vv(5,5))
Write(123,*) "Vv56_i = ", Real(Vv(5,6),dp)
Write(123,*) "Vv56_r = ", -AImag(Vv(5,6))
End if 
If (MFv(6).Gt.0._dp) Then 
Write(123,*) "Vv61_r = ", Real(Vv(6,1),dp)
Write(123,*) "Vv61_i = ", AImag(Vv(6,1))
Write(123,*) "Vv62_r = ", Real(Vv(6,2),dp)
Write(123,*) "Vv62_i = ", AImag(Vv(6,2))
Write(123,*) "Vv63_r = ", Real(Vv(6,3),dp)
Write(123,*) "Vv63_i = ", AImag(Vv(6,3))
Write(123,*) "Vv64_r = ", Real(Vv(6,4),dp)
Write(123,*) "Vv64_i = ", AImag(Vv(6,4))
Write(123,*) "Vv65_r = ", Real(Vv(6,5),dp)
Write(123,*) "Vv65_i = ", AImag(Vv(6,5))
Write(123,*) "Vv66_r = ", Real(Vv(6,6),dp)
Write(123,*) "Vv66_i = ", AImag(Vv(6,6))
Else 
Write(123,*) "Vv61_i = ", Real(Vv(6,1),dp)
Write(123,*) "Vv61_r = ", -AImag(Vv(6,1))
Write(123,*) "Vv62_i = ", Real(Vv(6,2),dp)
Write(123,*) "Vv62_r = ", -AImag(Vv(6,2))
Write(123,*) "Vv63_i = ", Real(Vv(6,3),dp)
Write(123,*) "Vv63_r = ", -AImag(Vv(6,3))
Write(123,*) "Vv64_i = ", Real(Vv(6,4),dp)
Write(123,*) "Vv64_r = ", -AImag(Vv(6,4))
Write(123,*) "Vv65_i = ", Real(Vv(6,5),dp)
Write(123,*) "Vv65_r = ", -AImag(Vv(6,5))
Write(123,*) "Vv66_i = ", Real(Vv(6,6),dp)
Write(123,*) "Vv66_r = ", -AImag(Vv(6,6))
End if 
Write(123,*) "ZDL11_r = ", Real(ZDL(1,1),dp)
Write(123,*) "ZDL11_i = ", AImag(ZDL(1,1))
Write(123,*) "ZDL12_r = ", Real(ZDL(1,2),dp)
Write(123,*) "ZDL12_i = ", AImag(ZDL(1,2))
Write(123,*) "ZDL13_r = ", Real(ZDL(1,3),dp)
Write(123,*) "ZDL13_i = ", AImag(ZDL(1,3))
Write(123,*) "ZDL21_r = ", Real(ZDL(2,1),dp)
Write(123,*) "ZDL21_i = ", AImag(ZDL(2,1))
Write(123,*) "ZDL22_r = ", Real(ZDL(2,2),dp)
Write(123,*) "ZDL22_i = ", AImag(ZDL(2,2))
Write(123,*) "ZDL23_r = ", Real(ZDL(2,3),dp)
Write(123,*) "ZDL23_i = ", AImag(ZDL(2,3))
Write(123,*) "ZDL31_r = ", Real(ZDL(3,1),dp)
Write(123,*) "ZDL31_i = ", AImag(ZDL(3,1))
Write(123,*) "ZDL32_r = ", Real(ZDL(3,2),dp)
Write(123,*) "ZDL32_i = ", AImag(ZDL(3,2))
Write(123,*) "ZDL33_r = ", Real(ZDL(3,3),dp)
Write(123,*) "ZDL33_i = ", AImag(ZDL(3,3))
Write(123,*) "ZDR11_r = ", Real(ZDR(1,1),dp)
Write(123,*) "ZDR11_i = ", AImag(ZDR(1,1))
Write(123,*) "ZDR12_r = ", Real(ZDR(1,2),dp)
Write(123,*) "ZDR12_i = ", AImag(ZDR(1,2))
Write(123,*) "ZDR13_r = ", Real(ZDR(1,3),dp)
Write(123,*) "ZDR13_i = ", AImag(ZDR(1,3))
Write(123,*) "ZDR21_r = ", Real(ZDR(2,1),dp)
Write(123,*) "ZDR21_i = ", AImag(ZDR(2,1))
Write(123,*) "ZDR22_r = ", Real(ZDR(2,2),dp)
Write(123,*) "ZDR22_i = ", AImag(ZDR(2,2))
Write(123,*) "ZDR23_r = ", Real(ZDR(2,3),dp)
Write(123,*) "ZDR23_i = ", AImag(ZDR(2,3))
Write(123,*) "ZDR31_r = ", Real(ZDR(3,1),dp)
Write(123,*) "ZDR31_i = ", AImag(ZDR(3,1))
Write(123,*) "ZDR32_r = ", Real(ZDR(3,2),dp)
Write(123,*) "ZDR32_i = ", AImag(ZDR(3,2))
Write(123,*) "ZDR33_r = ", Real(ZDR(3,3),dp)
Write(123,*) "ZDR33_i = ", AImag(ZDR(3,3))
Write(123,*) "ZUL11_r = ", Real(ZUL(1,1),dp)
Write(123,*) "ZUL11_i = ", AImag(ZUL(1,1))
Write(123,*) "ZUL12_r = ", Real(ZUL(1,2),dp)
Write(123,*) "ZUL12_i = ", AImag(ZUL(1,2))
Write(123,*) "ZUL13_r = ", Real(ZUL(1,3),dp)
Write(123,*) "ZUL13_i = ", AImag(ZUL(1,3))
Write(123,*) "ZUL21_r = ", Real(ZUL(2,1),dp)
Write(123,*) "ZUL21_i = ", AImag(ZUL(2,1))
Write(123,*) "ZUL22_r = ", Real(ZUL(2,2),dp)
Write(123,*) "ZUL22_i = ", AImag(ZUL(2,2))
Write(123,*) "ZUL23_r = ", Real(ZUL(2,3),dp)
Write(123,*) "ZUL23_i = ", AImag(ZUL(2,3))
Write(123,*) "ZUL31_r = ", Real(ZUL(3,1),dp)
Write(123,*) "ZUL31_i = ", AImag(ZUL(3,1))
Write(123,*) "ZUL32_r = ", Real(ZUL(3,2),dp)
Write(123,*) "ZUL32_i = ", AImag(ZUL(3,2))
Write(123,*) "ZUL33_r = ", Real(ZUL(3,3),dp)
Write(123,*) "ZUL33_i = ", AImag(ZUL(3,3))
Write(123,*) "ZUR11_r = ", Real(ZUR(1,1),dp)
Write(123,*) "ZUR11_i = ", AImag(ZUR(1,1))
Write(123,*) "ZUR12_r = ", Real(ZUR(1,2),dp)
Write(123,*) "ZUR12_i = ", AImag(ZUR(1,2))
Write(123,*) "ZUR13_r = ", Real(ZUR(1,3),dp)
Write(123,*) "ZUR13_i = ", AImag(ZUR(1,3))
Write(123,*) "ZUR21_r = ", Real(ZUR(2,1),dp)
Write(123,*) "ZUR21_i = ", AImag(ZUR(2,1))
Write(123,*) "ZUR22_r = ", Real(ZUR(2,2),dp)
Write(123,*) "ZUR22_i = ", AImag(ZUR(2,2))
Write(123,*) "ZUR23_r = ", Real(ZUR(2,3),dp)
Write(123,*) "ZUR23_i = ", AImag(ZUR(2,3))
Write(123,*) "ZUR31_r = ", Real(ZUR(3,1),dp)
Write(123,*) "ZUR31_i = ", AImag(ZUR(3,1))
Write(123,*) "ZUR32_r = ", Real(ZUR(3,2),dp)
Write(123,*) "ZUR32_i = ", AImag(ZUR(3,2))
Write(123,*) "ZUR33_r = ", Real(ZUR(3,3),dp)
Write(123,*) "ZUR33_i = ", AImag(ZUR(3,3))
Write(123,*) "ZEL11_r = ", Real(ZEL(1,1),dp)
Write(123,*) "ZEL11_i = ", AImag(ZEL(1,1))
Write(123,*) "ZEL12_r = ", Real(ZEL(1,2),dp)
Write(123,*) "ZEL12_i = ", AImag(ZEL(1,2))
Write(123,*) "ZEL13_r = ", Real(ZEL(1,3),dp)
Write(123,*) "ZEL13_i = ", AImag(ZEL(1,3))
Write(123,*) "ZEL21_r = ", Real(ZEL(2,1),dp)
Write(123,*) "ZEL21_i = ", AImag(ZEL(2,1))
Write(123,*) "ZEL22_r = ", Real(ZEL(2,2),dp)
Write(123,*) "ZEL22_i = ", AImag(ZEL(2,2))
Write(123,*) "ZEL23_r = ", Real(ZEL(2,3),dp)
Write(123,*) "ZEL23_i = ", AImag(ZEL(2,3))
Write(123,*) "ZEL31_r = ", Real(ZEL(3,1),dp)
Write(123,*) "ZEL31_i = ", AImag(ZEL(3,1))
Write(123,*) "ZEL32_r = ", Real(ZEL(3,2),dp)
Write(123,*) "ZEL32_i = ", AImag(ZEL(3,2))
Write(123,*) "ZEL33_r = ", Real(ZEL(3,3),dp)
Write(123,*) "ZEL33_i = ", AImag(ZEL(3,3))
Write(123,*) "ZER11_r = ", Real(ZER(1,1),dp)
Write(123,*) "ZER11_i = ", AImag(ZER(1,1))
Write(123,*) "ZER12_r = ", Real(ZER(1,2),dp)
Write(123,*) "ZER12_i = ", AImag(ZER(1,2))
Write(123,*) "ZER13_r = ", Real(ZER(1,3),dp)
Write(123,*) "ZER13_i = ", AImag(ZER(1,3))
Write(123,*) "ZER21_r = ", Real(ZER(2,1),dp)
Write(123,*) "ZER21_i = ", AImag(ZER(2,1))
Write(123,*) "ZER22_r = ", Real(ZER(2,2),dp)
Write(123,*) "ZER22_i = ", AImag(ZER(2,2))
Write(123,*) "ZER23_r = ", Real(ZER(2,3),dp)
Write(123,*) "ZER23_i = ", AImag(ZER(2,3))
Write(123,*) "ZER31_r = ", Real(ZER(3,1),dp)
Write(123,*) "ZER31_i = ", AImag(ZER(3,1))
Write(123,*) "ZER32_r = ", Real(ZER(3,2),dp)
Write(123,*) "ZER32_i = ", AImag(ZER(3,2))
Write(123,*) "ZER33_r = ", Real(ZER(3,3),dp)
Write(123,*) "ZER33_i = ", AImag(ZER(3,3))
Write(123,*) "ZZ11_r = ", Real(ZZ(1,1),dp)
Write(123,*) "ZZ11_i = ", AImag(ZZ(1,1))
Write(123,*) "ZZ12_r = ", Real(ZZ(1,2),dp)
Write(123,*) "ZZ12_i = ", AImag(ZZ(1,2))
Write(123,*) "ZZ21_r = ", Real(ZZ(2,1),dp)
Write(123,*) "ZZ21_i = ", AImag(ZZ(2,1))
Write(123,*) "ZZ22_r = ", Real(ZZ(2,2),dp)
Write(123,*) "ZZ22_i = ", AImag(ZZ(2,2))
    Close(123) 
End Subroutine WriteWHIZARD 

 
 Subroutine WriteHiggsBounds 
Implicit None 
Open(87,file="MH_GammaTot.dat",status="unknown") 
Open(88,file="MHplus_GammaTot.dat",status="unknown") 
Open(89,file="effC.dat",status="unknown") 
Open(90,file="BR_H_NP.dat",status="unknown") 
Open(91,file="BR_Hplus.dat",status="unknown") 
Open(92,file="BR_t.dat",status="unknown") 
Open(93,file="LEP_HpHm_CS_ratios.dat",status="unknown") 
Write(87,"(I1)",advance="No") 1 
Write(88,"(I1)",advance="No") 1 
Write(87,"(2e16.8)",advance="No") Mhh(1)
Write(87,"(2e16.8)",advance="No") Mhh(2)
Write(87,"(2e16.8)",advance="No") Mhh(3)
Write(87,"(2e16.8)",advance="No") MAh(2)
Write(87,"(2e16.8)",advance="No") MAh(3)
Write(88,"(2e16.8)",advance="No") MHm(2)
Write(87,"(2e16.8)",advance="No") gThh(1)
Write(87,"(2e16.8)",advance="No") gThh(2)
Write(87,"(2e16.8)",advance="No") gThh(3)
Write(87,"(2e16.8)",advance="No") gTAh(2)
Write(87,"(2e16.8)",advance="No") gTAh(3)
Write(88,"(2e16.8)",advance="No") gTHm(2)
Write(90,"(I1)",advance="No") 1 
Write(90,"(e16.8)",advance="No") BRinvH(1)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRinvH(2)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRinvH(3)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRinvA(2)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRinvA(3)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRHHH(1,2)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHHH(1,3)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHAA(1,2)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHAA(1,3)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHHH(2,1)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHHH(2,3)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHAA(2,2)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHAA(2,3)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHHH(3,1)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHHH(3,2)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHAA(3,2)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHAA(3,3)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRAHH(2,1)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHH(2,2)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHH(2,3)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAAA(2,3)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHH(3,1)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHH(3,2)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHH(3,3)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAAA(3,2)/gTAh(3) 

 
 
Write(92,"(I1)",advance="No") 1
Write(92,"(e16.8)",advance="No") BR_tWb/gTFu(3) 
Write(92,"(e16.8)",advance="No") BR_tHb(2)/gTFu(3) 
Write(91,"(I1)",advance="No") 1 
Write(91,"(3e16.8)",advance="No") BR_Hcs(2)/gTHm(2) 
Write(91,"(3e16.8)",advance="No") BR_Hcb(2)/gTHm(2) 
Write(91,"(3e16.8)",advance="No") BR_Htaunu(2)/gTHm(2) 
Write(89,"(I1)",advance="No") 1 
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(1,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fd(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fd(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(1,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fd(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fd(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(1,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fu(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fu(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(1,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fu(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fu(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(1,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fd(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fd(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(1,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fd(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fd(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(1,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fu(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fu(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(1,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fu(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fu(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(1,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fe(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fe(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(1,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fe(2,2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fe(3,2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(1,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fe(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fe(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(1,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fe(2,3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fe(3,3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_VWm(1)**2
Write(89,"(3e16.8)",advance="No") rHB_S_VWm(2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_VWm(3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_VWm(2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_VWm(3)**2
Write(89,"(3e16.8)",advance="No") rHB_S_VZ(1)**2
Write(89,"(3e16.8)",advance="No") rHB_S_VZ(2)**2
Write(89,"(3e16.8)",advance="No") rHB_S_VZ(3)**2
Write(89,"(3e16.8)",advance="No") rHB_P_VZ(2)**2
Write(89,"(3e16.8)",advance="No") rHB_P_VZ(3)**2
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No")  Real(ratioPP(1),dp)**2 
Write(89,"(3e16.8)",advance="No")  Real(ratioPP(2),dp)**2 
Write(89,"(3e16.8)",advance="No")  Real(ratioPP(3),dp)**2 
Write(89,"(3e16.8)",advance="No") Real(ratioPPP(2),dp)**2 
Write(89,"(3e16.8)",advance="No") Real(ratioPPP(3),dp)**2 
Write(89,"(3e16.8)",advance="No")  Real(ratioGG(1),dp)**2 
Write(89,"(3e16.8)",advance="No")  Real(ratioGG(2),dp)**2 
Write(89,"(3e16.8)",advance="No")  Real(ratioGG(3),dp)**2 
Write(89,"(3e16.8)",advance="No") Real(ratioPGG(2),dp)**2 
Write(89,"(3e16.8)",advance="No") Real(ratioPGG(3),dp)**2 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(e16.8)",advance="No") Real(CPL_H_H_Z(1,1), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_H_H_Z(1,2), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_H_H_Z(2,2), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_H_H_Z(1,3), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_H_H_Z(2,3), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_H_H_Z(3,3), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_A_H_Z(2,1), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_A_H_Z(2,2), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_A_H_Z(2,3), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_A_A_Z(2,2), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_A_H_Z(3,1), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_A_H_Z(3,2), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_A_H_Z(3,3), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_A_A_Z(3,2), dp) 
 Write(89,"(e16.8)",advance="No") Real(CPL_A_A_Z(3,3), dp) 
 
 
 
Write(93,"(I1)",advance="No") 1 
Write(93,"(e16.8)",advance="No") 0.0000 
Close(87) 
Close(88) 
Close(90) 
Close(91) 
Close(92) 
Close(93) 
End Subroutine WriteHiggsBounds 
 
 

 
 Subroutine WriteHiggsBounds5 
Implicit None 
Open(87,file="MH_GammaTot.dat",status="unknown") 
Open(88,file="MHplus_GammaTot.dat",status="unknown") 
Open(89,file="effC.dat",status="unknown") 
Open(90,file="BR_H_NP.dat",status="unknown") 
Open(91,file="BR_Hplus.dat",status="unknown") 
Open(92,file="BR_t.dat",status="unknown") 
Write(87,"(I1)",advance="No") 1 
Write(88,"(I1)",advance="No") 1 
Write(87,"(2e16.8)",advance="No") Mhh(1)
Write(87,"(2e16.8)",advance="No") Mhh(2)
Write(87,"(2e16.8)",advance="No") Mhh(3)
Write(87,"(2e16.8)",advance="No") MAh(2)
Write(87,"(2e16.8)",advance="No") MAh(3)
Write(88,"(2e16.8)",advance="No") MHm(2)
Write(87,"(2e16.8)",advance="No") gThh(1)
Write(87,"(2e16.8)",advance="No") gThh(2)
Write(87,"(2e16.8)",advance="No") gThh(3)
Write(87,"(2e16.8)",advance="No") gTAh(2)
Write(87,"(2e16.8)",advance="No") gTAh(3)
Write(88,"(2e16.8)",advance="No") gTHm(2)
Write(90,"(I1)",advance="No") 1 
Write(90,"(e16.8)",advance="No") BRinvH(1)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRinvH(2)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRinvH(3)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRinvA(2)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRinvA(3)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRHHHijk(1,2,2)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHHHijk(1,3,2)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHHHijk(1,3,3)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHHAijk(1,2,2)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHHAijk(1,3,2)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHAAijk(1,2,2)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHHAijk(1,2,3)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHHAijk(1,3,3)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHAAijk(1,2,3)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHAAijk(1,3,3)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHHHijk(2,1,1)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHHHijk(2,3,1)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHHHijk(2,3,3)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHHAijk(2,1,2)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHHAijk(2,3,2)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHAAijk(2,2,2)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHHAijk(2,1,3)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHHAijk(2,3,3)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHAAijk(2,2,3)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHAAijk(2,3,3)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHHHijk(3,1,1)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHHHijk(3,2,1)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHHHijk(3,2,2)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHHAijk(3,1,2)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHHAijk(3,2,2)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHAAijk(3,2,2)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHHAijk(3,1,3)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHHAijk(3,2,3)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHAAijk(3,2,3)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHAAijk(3,3,3)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRAHHijk(2,1,1)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHHijk(2,2,1)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHHijk(2,2,2)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHHijk(2,3,1)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHHijk(2,3,2)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHHijk(2,3,3)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHAijk(2,1,3)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHAijk(2,2,3)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHAijk(2,3,3)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAAAijk(2,3,3)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHHijk(3,1,1)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHHijk(3,2,1)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHHijk(3,2,2)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHHijk(3,3,1)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHHijk(3,3,2)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHHijk(3,3,3)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHAijk(3,1,2)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHAijk(3,2,2)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHAijk(3,3,2)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAAAijk(3,2,2)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRHHZ(1,2)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHHZ(1,3)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHAZ(1,2)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHAZ(1,3)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHHZ(2,1)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHHZ(2,3)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHAZ(2,2)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHAZ(2,3)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHHZ(3,1)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHHZ(3,2)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHAZ(3,2)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRHAZ(3,3)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRAHZ(2,1)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHZ(2,2)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHZ(2,3)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAAZ(2,3)/gTAh(2) 
Write(90,"(e16.8)",advance="No") BRAHZ(3,1)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHZ(3,2)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAHZ(3,3)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRAAZ(3,2)/gTAh(3) 
Write(90,"(e16.8)",advance="No") BRHll(1,1,2)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHll(2,1,2)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHll(3,1,2)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRAll(2,1,2)/gTAh(2)  
Write(90,"(e16.8)",advance="No") BRAll(3,1,2)/gTAh(3)  
Write(90,"(e16.8)",advance="No") BRHll(1,1,3)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHll(2,1,3)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHll(3,1,3)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRAll(2,1,3)/gTAh(2)  
Write(90,"(e16.8)",advance="No") BRAll(3,1,3)/gTAh(3)  
Write(90,"(e16.8)",advance="No") BRHll(1,2,3)/gThh(1) 
Write(90,"(e16.8)",advance="No") BRHll(2,2,3)/gThh(2) 
Write(90,"(e16.8)",advance="No") BRHll(3,2,3)/gThh(3) 
Write(90,"(e16.8)",advance="No") BRAll(2,2,3)/gTAh(2)  
Write(90,"(e16.8)",advance="No") BRAll(3,2,3)/gTAh(3)  
Write(90,"(e16.8)",advance="No") BRHHpW(1,2)/gThh(1)  
Write(90,"(e16.8)",advance="No") BRHHpW(2,2)/gThh(2)  
Write(90,"(e16.8)",advance="No") BRHHpW(3,2)/gThh(3)  
Write(90,"(e16.8)",advance="No") BRAHpW(2,2)/gTAh(2)  
Write(90,"(e16.8)",advance="No") BRAHpW(3,2)/gTAh(3)  

 
 
Write(92,"(I1)",advance="No") 1
Write(92,"(e16.8)",advance="No") BR_tWb/gTFu(3) 
Write(92,"(e16.8)",advance="No") BR_tHb(2)/gTFu(3) 
Write(91,"(I1)",advance="No") 1 
Write(91,"(3e16.8)",advance="No") BR_Hcs(2)/gTHm(2) 
Write(91,"(3e16.8)",advance="No") BR_Hcb(2)/gTHm(2) 
Write(91,"(3e16.8)",advance="No") BR_Htaunu(2)/gTHm(2) 
Write(91,"(3e16.8)",advance="No") BR_HpTB(2)/gTHm(2) 
Write(91,"(3e16.8)",advance="No") BR_HpWZ(2)/gTHm(2) 
Write(91,"(3e16.8)",advance="No")BR_HpHW(2,1)/gTHm(2) 
Write(91,"(3e16.8)",advance="No")BR_HpHW(2,2)/gTHm(2) 
Write(91,"(3e16.8)",advance="No")BR_HpHW(2,3)/gTHm(2) 
Write(91,"(3e16.8)",advance="No")BR_HpAW(2,2)/gTHm(2) 
Write(91,"(3e16.8)",advance="No")BR_HpAW(2,3)/gTHm(2) 
Write(89,"(I1)",advance="No") 1 
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(1,2)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(2,2)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(3,2)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fd(2,2)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fd(3,2)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(1,2)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(2,2)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(3,2)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fd(2,2)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fd(3,2)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(1,2)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(2,2)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(3,2)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fu(2,2)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fu(3,2)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(1,2)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(2,2)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(3,2)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fu(2,2)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fu(3,2)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(1,3)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(2,3)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fd(3,3)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fd(2,3)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fd(3,3)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(1,3)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(2,3)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fd(3,3)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fd(2,3)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fd(3,3)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(1,3)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(2,3)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fu(3,3)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fu(2,3)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fu(3,3)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(1,3)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(2,3)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fu(3,3)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fu(2,3)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fu(3,3)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(1,2)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(2,2)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(3,2)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fe(2,2)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fe(3,2)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(1,2)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(2,2)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(3,2)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fe(2,2)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fe(3,2)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(1,3)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(2,3)
Write(89,"(3e16.8)",advance="No") rHB_S_S_Fe(3,3)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fe(2,3)
Write(89,"(3e16.8)",advance="No") rHB_P_S_Fe(3,3)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(1,3)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(2,3)
Write(89,"(3e16.8)",advance="No") rHB_S_P_Fe(3,3)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fe(2,3)
Write(89,"(3e16.8)",advance="No") rHB_P_P_Fe(3,3)
Write(89,"(3e16.8)",advance="No") rHB_S_VWm(1)
Write(89,"(3e16.8)",advance="No") rHB_S_VWm(2)
Write(89,"(3e16.8)",advance="No") rHB_S_VWm(3)
Write(89,"(3e16.8)",advance="No") rHB_P_VWm(2)
Write(89,"(3e16.8)",advance="No") rHB_P_VWm(3)
Write(89,"(3e16.8)",advance="No") rHB_S_VZ(1)
Write(89,"(3e16.8)",advance="No") rHB_S_VZ(2)
Write(89,"(3e16.8)",advance="No") rHB_S_VZ(3)
Write(89,"(3e16.8)",advance="No") rHB_P_VZ(2)
Write(89,"(3e16.8)",advance="No") rHB_P_VZ(3)
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No") 0._dp 
Write(89,"(3e16.8)",advance="No")  Real(ratioPP(1),dp) 
Write(89,"(3e16.8)",advance="No")  Real(ratioPP(2),dp) 
Write(89,"(3e16.8)",advance="No")  Real(ratioPP(3),dp) 
Write(89,"(3e16.8)",advance="No") Real(ratioPPP(2),dp) 
Write(89,"(3e16.8)",advance="No") Real(ratioPPP(3),dp) 
Write(89,"(3e16.8)",advance="No")  Real(ratioGG(1),dp) 
Write(89,"(3e16.8)",advance="No")  Real(ratioGG(2),dp) 
Write(89,"(3e16.8)",advance="No")  Real(ratioGG(3),dp) 
Write(89,"(3e16.8)",advance="No") Real(ratioPGG(2),dp) 
Write(89,"(3e16.8)",advance="No") Real(ratioPGG(3),dp) 
Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_H_H_Z(1,1), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_H_H_Z(1,2), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_H_H_Z(2,2), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_H_H_Z(1,3), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_H_H_Z(2,3), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_H_H_Z(3,3), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_A_H_Z(2,1), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_A_H_Z(2,2), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_A_H_Z(2,3), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_A_A_Z(2,2), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_A_H_Z(3,1), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_A_H_Z(3,2), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_A_H_Z(3,3), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_A_A_Z(3,2), dp)) 
 Write(89,"(e16.8)",advance="No") Sqrt(Real(CPL_A_A_Z(3,3), dp)) 
 
 
 
Close(87) 
Close(88) 
Close(90) 
Close(91) 
Close(92) 
End Subroutine WriteHiggsBounds5 
 
 
 Subroutine ReadMatrixC(io, nmax1, nmax2, mat, ic, mat_name, kont, fill)
  Implicit None
   Character(len=*) :: mat_name
   Integer, Intent(in) :: nmax1, nmax2, io, ic
   Integer, Intent(in), Optional :: fill
   Complex(dp), Intent(inout) :: mat(nmax1, nmax2)
   Integer, Intent(out) :: kont

   Character(len=80) :: read_line
   Integer :: i1, i2
   Real(dp) :: wert

   kont = 0

   Iname = Iname + 1
   NameOfUnit(Iname) = "ReadMatrixC"
   Do 
    Read(io,*,End=200) read_line
!     Write(*,*) read_line
    If (read_line(1:1).Eq."#") Cycle ! ignore comments
    Backspace(io)                    ! resetting to the beginning of the line
    If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b") ) Then
     Iname = Iname - 1
     Return ! new block
    End If

    Read(io,*) i1, i2, wert, read_line

    If ((i1.Lt.1).Or.(i1.Gt.nmax1)) Then
     Write(ErrCan,*) "Problem while reading "//mat_name//" in routine"// &
        & Trim(NameOfUnit(Iname))//", index i1=",i1
     Iname = Iname - 2
     kont = -305 
     Call TerminateProgram()
    End If
    If ((i2.Lt.1).Or.(i2.Gt.nmax2)) Then
     Write(ErrCan,*) "Problem while reading "//mat_name//" in routine"// &
        & Trim(NameOfUnit(Iname))//", index i2=",i2
     Iname = Iname - 2
     kont = -305 
     Call TerminateProgram()
    End If

    If (ic.Eq.0) Then
     mat(i1,i2) = Cmplx(0._dp,Aimag(mat(i1,i2)),dp) + wert
     If (Present(fill).And.(i1.Ne.i2)) &
       &  mat(i2,i1) = Cmplx(0._dp, Aimag(mat(i2,i1)), dp) + wert
    Else If (ic.Eq.1) Then
     mat(i1,i2) = Real(mat(i1,i2),dp) + Cmplx(0._dp, wert, dp)
     !-------------------------------------------------------------
     ! if fill==1 -> matrix is hermitian
     ! if fill==2 -> matrix is complex symmetric
     !-------------------------------------------------------------
     If (Present(fill).And.(i1.Ne.i2)) Then
      If (fill.Eq.1) mat(i2,i1) = Real(mat(i2,i1),dp) - Cmplx(0._dp, wert, dp)
      If (fill.Eq.2) mat(i2,i1) = Real(mat(i2,i1),dp) + Cmplx(0._dp, wert, dp)
     End If
    End If

   End Do

   200 Return

  End Subroutine ReadMatrixC

 
  Subroutine ReadMatrixR(io, nmax1, nmax2, mat, mat_name, kont)
  Implicit None
   Character(len=*) :: mat_name
   Integer, Intent(in) :: nmax1, nmax2, io
   Real(dp), Intent(inout) :: mat(nmax1, nmax2)
   Integer, Intent(out) :: kont

   Character(len=80) :: read_line
   Integer :: i1, i2
   Real(dp) :: wert

   kont = 0

   Iname = Iname + 1
   NameOfUnit(Iname) = "ReadMatrixR"
   Do 
    Read(io,*,End=200) read_line
!     Write(*,*) read_line
    If (read_line(1:1).Eq."#") Cycle ! ignore comments
    Backspace(io)                    ! resetting to the beginning of the line
    If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b") ) Then
     Iname = Iname - 1
     Return ! new block
    End If

    Read(io,*) i1, i2, wert, read_line

    If ((i1.Lt.1).Or.(i1.Gt.nmax1)) Then
     Write(ErrCan,*) "Problem while reading "//mat_name//" in routine"// &
        & Trim(NameOfUnit(Iname))//", index i1=",i1
     Iname = Iname - 2
     kont = -305 
     Call TerminateProgram()
    End If
    If ((i2.Lt.1).Or.(i2.Gt.nmax2)) Then
     Write(ErrCan,*) "Problem while reading "//mat_name//" in routine"// &
        & Trim(NameOfUnit(Iname))//", index i2=",i2
     Iname = Iname - 2
     kont = -305 
     Call TerminateProgram()
    End If

    mat(i1,i2) = wert

   End Do

   200 Return

  End Subroutine ReadMatrixR

  
  Subroutine ReadVectorC(io, nmax, vec, ic, vec_name, kont)
  Implicit None
   Character(len=*) :: vec_name
   Integer, Intent(in) :: nmax, io, ic
   Complex(dp), Intent(inout) :: vec(nmax)
   Integer, Intent(out) :: kont

   Character(len=80) :: read_line
   Integer :: i1
   Real(dp) :: wert

   kont = 0

   Iname = Iname + 1
   NameOfUnit(Iname) = "ReadVectorC"
   Do 
    Read(io,*,End=200) read_line
!     Write(*,*) read_line
    If (read_line(1:1).Eq."#") Cycle ! ignore comments
    Backspace(io)                    ! resetting to the beginning of the line
    If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b") ) Then
     Iname = Iname - 1
     Return ! new block
    End If

    Read(io,*) i1, wert, read_line

    If ((i1.Lt.1).Or.(i1.Gt.nmax)) Then
     Write(ErrCan,*) "Problem while reading "//vec_name//" in routine"// &
        & Trim(NameOfUnit(Iname))//", index i1=",i1
     Iname = Iname - 2
     kont = -305 
     Call TerminateProgram()
    End If

    If (ic.Eq.0) vec(i1) = Cmplx(0._dp, Aimag(vec(i1)), dp) + wert
    If (ic.Eq.1) vec(i1) = Real(vec(i1),dp) + Cmplx(0._dp, wert, dp)

   End Do

   200 Return

  End Subroutine ReadVectorC


Subroutine ReadScalarC(io, vec, ic, vec_name, kont)
  Implicit None
   Character(len=*) :: vec_name
   Integer, Intent(in) :: io, ic
   Complex(dp), Intent(inout) :: vec
   Integer, Intent(out) :: kont

   Character(len=80) :: read_line
   Integer :: i1
   Real(dp) :: wert

   kont = 0

   Iname = Iname + 1
   NameOfUnit(Iname) = "ReadVectorC"
   Do 
    Read(io,*,End=200) read_line
!     Write(*,*) read_line
    If (read_line(1:1).Eq."#") Cycle ! ignore comments
    Backspace(io)                    ! resetting to the beginning of the line
    If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b") ) Then
     Iname = Iname - 1
     Return ! new block
    End If

    Read(io,*) wert, read_line

!     If ((i1.Lt.1).Or.(i1.Gt.nmax)) Then
!      Write(ErrCan,*) "Problem while reading "//vec_name//" in routine"// &
!         & Trim(NameOfUnit(Iname))//", index i1=",i1
!      Iname = Iname - 2
!      kont = -305 
!      Call TerminateProgram()
!     End If

    If (ic.Eq.0) vec = Cmplx(0._dp, Aimag(vec), dp) + wert
    If (ic.Eq.1) vec = Real(vec,dp) + Cmplx(0._dp, wert, dp)

   End Do

   200 Return

  End Subroutine ReadScalarC

  
  Subroutine ReadVectorR(io, nmax, vec, vec_name, kont)
  Implicit None
   Character(len=*) :: vec_name
   Integer, Intent(in) :: nmax, io
   Real(dp), Intent(inout) :: vec(nmax)
   Integer, Intent(out) :: kont

   Character(len=80) :: read_line
   Integer :: i1
   Real(dp) :: wert

   kont = 0

   Iname = Iname + 1
   NameOfUnit(Iname) = "ReadVectorR"
   Do 
    Read(io,*,End=200) read_line
!     Write(*,*) read_line
    If (read_line(1:1).Eq."#") Cycle ! ignore comments
    Backspace(io)                    ! resetting to the beginning of the line
    If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b") ) Then
     Iname = Iname - 1
     Return ! new block
    End If

    Read(io,*) i1, wert, read_line

    If ((i1.Lt.1).Or.(i1.Gt.nmax)) Then
     Write(ErrCan,*) "Problem while reading "//vec_name//" in routine"// &
        & Trim(NameOfUnit(Iname))//", index i1=",i1
     Iname = Iname - 2
     kont = -305 
     Call TerminateProgram()
    End If

    vec(i1) = wert

   End Do

   200 Return

  End Subroutine ReadVectorR

Subroutine ReadScalarR(io, vec, vec_name, kont)
  Implicit None
   Character(len=*) :: vec_name
   Integer, Intent(in) :: io
   Real(dp), Intent(inout) :: vec
   Integer, Intent(out) :: kont

   Character(len=80) :: read_line
   Integer :: i1
   Real(dp) :: wert

   kont = 0

   Iname = Iname + 1
   NameOfUnit(Iname) = "ReadVectorR"
   Do 
    Read(io,*,End=200) read_line
!     Write(*,*) read_line
    If (read_line(1:1).Eq."#") Cycle ! ignore comments
    Backspace(io)                    ! resetting to the beginning of the line
    If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b") ) Then
     Iname = Iname - 1
     Return ! new block
    End If

    Read(io,*) wert, read_line

!     If ((i1.Lt.1).Or.(i1.Gt.nmax)) Then
!      Write(ErrCan,*) "Problem while reading "//vec_name//" in routine"// &
!         & Trim(NameOfUnit(Iname))//", index i1=",i1
!      Iname = Iname - 2
!      kont = -305 
!      Call TerminateProgram()
!     End If

    vec = wert

   End Do

   200 Return

  End Subroutine ReadScalarR

  
  Subroutine ReadTensorC(io, nmax1, nmax2, nmax3, mat, ic, mat_name, kont)
  Implicit None
   Character(len=*) :: mat_name
   Integer, Intent(in) :: nmax1, nmax2, nmax3, io, ic
   Complex(dp), Intent(inout) :: mat(nmax1, nmax2, nmax3)
   Integer, Intent(out) :: kont

   Character(len=80) :: read_line
   Integer :: i1, i2, i3
   Real(dp) :: wert

   kont = 0

   Iname = Iname + 1
   NameOfUnit(Iname) = "ReadTensorC"
   Do 
    Read(io,*,End=200) read_line
!     Write(*,*) read_line
    If (read_line(1:1).Eq."#") Cycle ! ignore comments
    Backspace(io)                    ! resetting to the beginning of the line
    If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b") ) Then
     Iname = Iname - 1
     Return ! new block
    End If

    Read(io,*) i1, i2, i3, wert, read_line

    If ((i1.Lt.1).Or.(i1.Gt.nmax1)) Then
     Write(ErrCan,*) "Problem while reading "//mat_name//" in routine"// &
        & Trim(NameOfUnit(Iname))//", index i1=",i1
     Iname = Iname - 2
     kont = -305 
     Call TerminateProgram()
    End If
    If ((i2.Lt.1).Or.(i2.Gt.nmax2)) Then
     Write(ErrCan,*) "Problem while reading "//mat_name//" in routine"// &
        & Trim(NameOfUnit(Iname))//", index i2=",i2
     Iname = Iname - 2
     kont = -305 
     Call TerminateProgram()
    End If
    If ((i3.Lt.1).Or.(i3.Gt.nmax3)) Then
     Write(ErrCan,*) "Problem while reading "//mat_name//" in routine"// &
        & Trim(NameOfUnit(Iname))//", index i3=",i3
     Iname = Iname - 2
     kont = -305 
     Call TerminateProgram()
    End If

    If (ic.Eq.0) mat(i1,i2,i3) = Cmplx(0._dp, Aimag(mat(i1,i2,i3)), dp) + wert
    If (ic.Eq.1) mat(i1,i2,i3) = mat(i1,i2,i3) + Cmplx(0._dp, wert, dp)

   End Do

   200 Return

  End Subroutine ReadTensorC

 Subroutine SetWriteMinBR(wert)
 !-------------------------------------------------------------------
 ! sets the minimal branching ratio (=wert) appearing in the output
 !-------------------------------------------------------------------
 Implicit None
  Real(dp), Intent(in) :: wert
  BrMin = wert
 End Subroutine SetWriteMinBR


 Subroutine SetWriteMinSig(wert)
 !-------------------------------------------------------------------
 ! sets the minimal cross section (=wert) appearing in the output
 !-------------------------------------------------------------------
 Implicit None
  Real(dp), Intent(in) :: wert
  SigMin = wert
 End Subroutine SetWriteMinSig

 Subroutine Warn_CPV(i_cpv, name)
  Implicit None 
   Integer, Intent(in) :: i_cpv
   Character(len=*), Intent(in) :: name
   If (i_cpv.Eq.0) Write(ErrCan,*) "CP violation is switched off"
   If (i_cpv.Eq.1) Write(ErrCan,*) "CP violation beyond CKM is switched off"
   Write(ErrCan,*) "Ignoring block "//Trim(name)
   If (ErrorLevel.Eq.2) Call TerminateProgram
  End Subroutine Warn_CPV

Subroutine PutUpperCase(name)
 Implicit None
  Character(len=80), Intent(inout) :: name
  Integer :: len=80, i1
  Do i1=1,len
   If (name(i1:i1).Eq."a") name(i1:i1) = "A"
   If (name(i1:i1).Eq."b") name(i1:i1) = "B"
   If (name(i1:i1).Eq."c") name(i1:i1) = "C"
   If (name(i1:i1).Eq."d") name(i1:i1) = "D"
   If (name(i1:i1).Eq."e") name(i1:i1) = "E"
   If (name(i1:i1).Eq."f") name(i1:i1) = "F"
   If (name(i1:i1).Eq."g") name(i1:i1) = "G"
   If (name(i1:i1).Eq."h") name(i1:i1) = "H"
   If (name(i1:i1).Eq."i") name(i1:i1) = "I"
   If (name(i1:i1).Eq."j") name(i1:i1) = "J"
   If (name(i1:i1).Eq."k") name(i1:i1) = "K"
   If (name(i1:i1).Eq."l") name(i1:i1) = "L"
   If (name(i1:i1).Eq."m") name(i1:i1) = "M"
   If (name(i1:i1).Eq."n") name(i1:i1) = "N"
   If (name(i1:i1).Eq."o") name(i1:i1) = "O"
   If (name(i1:i1).Eq."p") name(i1:i1) = "P"
   If (name(i1:i1).Eq."q") name(i1:i1) = "Q"
   If (name(i1:i1).Eq."r") name(i1:i1) = "R"
   If (name(i1:i1).Eq."s") name(i1:i1) = "S"
   If (name(i1:i1).Eq."t") name(i1:i1) = "T"
   If (name(i1:i1).Eq."u") name(i1:i1) = "U"
   If (name(i1:i1).Eq."v") name(i1:i1) = "V"
   If (name(i1:i1).Eq."w") name(i1:i1) = "W"
   If (name(i1:i1).Eq."x") name(i1:i1) = "X"
   If (name(i1:i1).Eq."y") name(i1:i1) = "Y"
   If (name(i1:i1).Eq."z") name(i1:i1) = "Z"
  End Do
 End Subroutine PutUpperCase

Subroutine Read_FLIFE(io) 
Implicit None 
Integer,Intent(in)::io 
Real(dp)::r_mod,wert 
Integer::i_mod,i_test,i_rp 
Character(len=80)::read_line 
Do 
Read(io,*) read_line 
If (read_line(1:1).Eq."#") Cycle! this loop 
Backspace(io)! resetting to the beginning of the line
If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b")) Exit! this loop 
Read(io,*) i_test,wert!,read_line 
   if (i_test.Eq.111) Then 
    tau_pi0 =wert 
  Else if (i_test.Eq.211) Then 
    tau_pip =wert 
  Else if (i_test.Eq.113) Then 
    tau_rho0 =wert 
  Else if (i_test.Eq.421) Then 
    tau_D0 =wert 
  Else if (i_test.Eq.411) Then 
    tau_Dp =wert 
  Else if (i_test.Eq.431) Then 
    tau_DSp =wert 
  Else if (i_test.Eq.433) Then 
    tau_DSsp =wert 
  Else if (i_test.Eq.221) Then 
    tau_eta =wert 
  Else if (i_test.Eq.331) Then 
    tau_etap =wert 
  Else if (i_test.Eq.223) Then 
    tau_omega =wert 
  Else if (i_test.Eq.333) Then 
    tau_phi =wert 
  Else if (i_test.Eq.130) Then 
    tau_KL0 =wert 
  Else if (i_test.Eq.310) Then 
    tau_KS0 =wert 
  Else if (i_test.Eq.311) Then 
    tau_K0 =wert 
  Else if (i_test.Eq.321) Then 
    tau_Kp =wert 
  Else if (i_test.Eq.511) Then 
    tau_B0d =wert 
  Else if (i_test.Eq.531) Then 
    tau_B0s =wert 
  Else if (i_test.Eq.521) Then 
    tau_Bp =wert 
  Else if (i_test.Eq.513) Then 
    tau_B0c =wert 
  Else if (i_test.Eq.523) Then 
    tau_Bpc =wert 
  Else if (i_test.Eq.541) Then 
    tau_Bcp =wert 
  Else if (i_test.Eq.313) Then 
    tau_K0c =wert 
  Else if (i_test.Eq.323) Then 
    tau_Kpc =wert 
  Else if (i_test.Eq.441) Then 
    tau_etac =wert 
  Else if (i_test.Eq.443) Then 
    tau_JPsi =wert 
  Else if (i_test.Eq.553) Then 
    tau_Ups =wert 
End If 
End Do! i_mod 
End Subroutine Read_FLIFE 


Subroutine Read_FMASS(io) 
Implicit None 
Integer,Intent(in)::io 
Real(dp)::r_mod,wert 
Integer::i_mod,i_test,i_rp 
Character(len=80)::read_line 
Do 
Read(io,*) read_line 
If (read_line(1:1).Eq."#") Cycle! this loop 
Backspace(io)! resetting to the beginning of the line
If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b")) Exit! this loop 
Read(io,*) i_test,wert!,read_line 
   if (i_test.Eq.111) Then 
    mass_pi0 =wert 
  Else if (i_test.Eq.211) Then 
    mass_pip =wert 
  Else if (i_test.Eq.113) Then 
    mass_rho0 =wert 
  Else if (i_test.Eq.421) Then 
    mass_D0 =wert 
  Else if (i_test.Eq.411) Then 
    mass_Dp =wert 
  Else if (i_test.Eq.431) Then 
    mass_DSp =wert 
  Else if (i_test.Eq.433) Then 
    mass_DSsp =wert 
  Else if (i_test.Eq.221) Then 
    mass_eta =wert 
  Else if (i_test.Eq.331) Then 
    mass_etap =wert 
  Else if (i_test.Eq.223) Then 
    mass_omega =wert 
  Else if (i_test.Eq.333) Then 
    mass_phi =wert 
  Else if (i_test.Eq.130) Then 
    mass_KL0 =wert 
  Else if (i_test.Eq.310) Then 
    mass_KS0 =wert 
  Else if (i_test.Eq.311) Then 
    mass_K0 =wert 
  Else if (i_test.Eq.321) Then 
    mass_Kp =wert 
  Else if (i_test.Eq.511) Then 
    mass_B0d =wert 
  Else if (i_test.Eq.531) Then 
    mass_B0s =wert 
  Else if (i_test.Eq.521) Then 
    mass_Bp =wert 
  Else if (i_test.Eq.513) Then 
    mass_B0c =wert 
  Else if (i_test.Eq.523) Then 
    mass_Bpc =wert 
  Else if (i_test.Eq.541) Then 
    mass_Bcp =wert 
  Else if (i_test.Eq.313) Then 
    mass_K0c =wert 
  Else if (i_test.Eq.323) Then 
    mass_Kpc =wert 
  Else if (i_test.Eq.441) Then 
    mass_etac =wert 
  Else if (i_test.Eq.443) Then 
    mass_JPSi =wert 
  Else if (i_test.Eq.553) Then 
    mass_Ups =wert 
End If 
End Do! i_mod 
End Subroutine Read_FMASS 


Subroutine Read_FCONST(io) 
Implicit None 
Integer,Intent(in)::io 
Real(dp)::r_mod,wert 
Integer::i_mod,i_test,i_rp 
Character(len=80)::read_line 
Do 
Read(io,*) read_line 
If (read_line(1:1).Eq."#") Cycle! this loop 
Backspace(io)! resetting to the beginning of the line
If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b")) Exit! this loop 
Read(io,*) i_test, i_mod, wert!,read_line 
    If (i_test.Eq.111) Then 
    If (i_mod.Eq.1) Then 
    f_pi_CONST =wert 
    End If 
   Else If (i_test.Eq.213) Then 
    If (i_mod.Eq.1) Then 
    f_rho_CONST =wert 
   Else If (i_mod.Eq.11) Then 
    h_rho_CONST =wert 
    End If 
   Else If (i_test.Eq.221) Then 
    If (i_mod.Eq.1) Then 
    f_eta_q_CONST =wert 
   Else If (i_mod.Eq.2) Then 
    f_eta_s_CONST =wert 
   Else If (i_mod.Eq.11) Then 
    h_eta_q_CONST =wert 
   Else If (i_mod.Eq.12) Then 
    h_eta_s_CONST =wert 
    End If 
   Else If (i_test.Eq.223) Then 
    If (i_mod.Eq.1) Then 
    f_omega_q_CONST =wert 
   Else If (i_mod.Eq.2) Then 
    f_omega_s_CONST =wert 
   Else If (i_mod.Eq.11) Then 
    h_omega_q_CONST =wert 
   Else If (i_mod.Eq.12) Then 
    h_omega_s_CONST =wert 
    End If 
   Else If (i_test.Eq.231) Then 
    If (i_mod.Eq.1) Then 
    f_etap_CONST =wert 
    End If 
   Else If (i_test.Eq.311) Then 
    If (i_mod.Eq.1) Then 
    f_k_CONST =wert 
    End If 
   Else If (i_test.Eq.321) Then 
    If (i_mod.Eq.1) Then 
    f_Kp_CONST =wert 
   Else If (i_mod.Eq.11) Then 
    h_k_CONST =wert 
    End If 
   Else If (i_test.Eq.411) Then 
    If (i_mod.Eq.1) Then 
    f_Dp_CONST =wert 
    End If 
   Else If (i_test.Eq.421) Then 
    If (i_mod.Eq.1) Then 
    f_D_CONST =wert 
    End If 
   Else If (i_test.Eq.431) Then 
    If (i_mod.Eq.1) Then 
    f_Dsp_CONST =wert 
    End If 
   Else If (i_test.Eq.511) Then 
    If (i_mod.Eq.1) Then 
    f_B0d_CONST =wert 
    End If 
   Else If (i_test.Eq.521) Then 
    If (i_mod.Eq.1) Then 
    f_Bp_CONST =wert 
    End If 
   Else If (i_test.Eq.531) Then 
    If (i_mod.Eq.1) Then 
    f_B0s_CONST =wert 
    End If 
End If 
End Do! i_mod 
End Subroutine Read_FCONST 


Subroutine WriteWCXF 
   Open(123,file="WC.BGLNCS_1.json",status="unknown")
Write(123,*) "{"
Write(123,*) '  "eft": "WET",'
Write(123,*) '  "basis": "FlavorKit",'
Write(123,*) '  "scale": "160.",'
Write(123,*) '  "values": {'
Write(123,*) '    "DVLL_2323": {' 
Write(123,*) '      "Re": ',Real(DVLL_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_2323) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_2323": {' 
Write(123,*) '      "Re": ',Real(DVRR_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_2323) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_2323": {' 
Write(123,*) '      "Re": ',Real(DVLR_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_2323) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2323": {' 
Write(123,*) '      "Re": ',Real(DSRR_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2323) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_3232": {' 
Write(123,*) '      "Re": ',Real(DSRR_3232,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_3232) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1313": {' 
Write(123,*) '      "Re": ',Real(DVLL_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1313) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1313": {' 
Write(123,*) '      "Re": ',Real(DVRR_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1313) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1313": {' 
Write(123,*) '      "Re": ',Real(DVLR_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1313) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1313": {' 
Write(123,*) '      "Re": ',Real(DSRR_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1313) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_3131": {' 
Write(123,*) '      "Re": ',Real(DSRR_3131,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_3131) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1212": {' 
Write(123,*) '      "Re": ',Real(DVLL_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1212) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1212": {' 
Write(123,*) '      "Re": ',Real(DVRR_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1212) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1212": {' 
Write(123,*) '      "Re": ',Real(DVLR_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1212) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1212": {' 
Write(123,*) '      "Re": ',Real(DSRR_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1212) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2121": {' 
Write(123,*) '      "Re": ',Real(DSRR_2121,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2121) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1323": {' 
Write(123,*) '      "Re": ',Real(DVLL_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1323) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1323": {' 
Write(123,*) '      "Re": ',Real(DVRR_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1323) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1323": {' 
Write(123,*) '      "Re": ',Real(DVLR_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1323) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_2313": {' 
Write(123,*) '      "Re": ',Real(DVLR_2313,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_2313) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1323": {' 
Write(123,*) '      "Re": ',Real(DSRR_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1323) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_3132": {' 
Write(123,*) '      "Re": ',Real(DSRR_3132,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_3132) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1232": {' 
Write(123,*) '      "Re": ',Real(DVLL_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1232) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1232": {' 
Write(123,*) '      "Re": ',Real(DVRR_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1232) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1232": {' 
Write(123,*) '      "Re": ',Real(DVLR_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1232) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_2321": {' 
Write(123,*) '      "Re": ',Real(DVLR_2321,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_2321) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1232": {' 
Write(123,*) '      "Re": ',Real(DSRR_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1232) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2123": {' 
Write(123,*) '      "Re": ',Real(DSRR_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2123) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1213": {' 
Write(123,*) '      "Re": ',Real(DVLL_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1213) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1213": {' 
Write(123,*) '      "Re": ',Real(DVRR_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1213) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1213": {' 
Write(123,*) '      "Re": ',Real(DVLR_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1213) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1312": {' 
Write(123,*) '      "Re": ',Real(DVLR_1312,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1312) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1213": {' 
Write(123,*) '      "Re": ',Real(DSRR_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1213) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2131": {' 
Write(123,*) '      "Re": ',Real(DSRR_2131,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2131) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3111": {' 
Write(123,*) '      "Re": ',Real(GVLL_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3111) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3121": {' 
Write(123,*) '      "Re": ',Real(GVLL_3121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3121) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3131": {' 
Write(123,*) '      "Re": ',Real(GVLL_3131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3131) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3111": {' 
Write(123,*) '      "Re": ',Real(GVRL_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3111) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3121": {' 
Write(123,*) '      "Re": ',Real(GVRL_3121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3121) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3131": {' 
Write(123,*) '      "Re": ',Real(GVRL_3131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3131) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3111": {' 
Write(123,*) '      "Re": ',Real(GSLL_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3111) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3121": {' 
Write(123,*) '      "Re": ',Real(GSLL_3121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3121) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3131": {' 
Write(123,*) '      "Re": ',Real(GSLL_3131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3131) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3111": {' 
Write(123,*) '      "Re": ',Real(GSRL_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3111) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3121": {' 
Write(123,*) '      "Re": ',Real(GSRL_3121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3121) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3131": {' 
Write(123,*) '      "Re": ',Real(GSRL_3131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3131) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3211": {' 
Write(123,*) '      "Re": ',Real(GVLL_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3211) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3221": {' 
Write(123,*) '      "Re": ',Real(GVLL_3221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3221) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3231": {' 
Write(123,*) '      "Re": ',Real(GVLL_3231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3231) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3211": {' 
Write(123,*) '      "Re": ',Real(GVRL_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3211) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3221": {' 
Write(123,*) '      "Re": ',Real(GVRL_3221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3221) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3231": {' 
Write(123,*) '      "Re": ',Real(GVRL_3231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3231) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3211": {' 
Write(123,*) '      "Re": ',Real(GSLL_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3211) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3221": {' 
Write(123,*) '      "Re": ',Real(GSLL_3221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3221) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3231": {' 
Write(123,*) '      "Re": ',Real(GSLL_3231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3231) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3211": {' 
Write(123,*) '      "Re": ',Real(GSRL_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3211) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3221": {' 
Write(123,*) '      "Re": ',Real(GSRL_3221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3221) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3231": {' 
Write(123,*) '      "Re": ',Real(GSRL_3231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3231) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2111": {' 
Write(123,*) '      "Re": ',Real(GVLL_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2111) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2121": {' 
Write(123,*) '      "Re": ',Real(GVLL_2121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2121) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2131": {' 
Write(123,*) '      "Re": ',Real(GVLL_2131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2131) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2111": {' 
Write(123,*) '      "Re": ',Real(GVRL_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2111) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2121": {' 
Write(123,*) '      "Re": ',Real(GVRL_2121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2121) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2131": {' 
Write(123,*) '      "Re": ',Real(GVRL_2131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2131) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2111": {' 
Write(123,*) '      "Re": ',Real(GSLL_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2111) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2121": {' 
Write(123,*) '      "Re": ',Real(GSLL_2121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2121) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2131": {' 
Write(123,*) '      "Re": ',Real(GSLL_2131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2131) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2111": {' 
Write(123,*) '      "Re": ',Real(GSRL_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2111) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2121": {' 
Write(123,*) '      "Re": ',Real(GSRL_2121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2121) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2131": {' 
Write(123,*) '      "Re": ',Real(GSRL_2131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2131) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2211": {' 
Write(123,*) '      "Re": ',Real(GVLL_2211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2211) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2221": {' 
Write(123,*) '      "Re": ',Real(GVLL_2221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2221) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2231": {' 
Write(123,*) '      "Re": ',Real(GVLL_2231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2231) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2211": {' 
Write(123,*) '      "Re": ',Real(GVRL_2211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2211) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2221": {' 
Write(123,*) '      "Re": ',Real(GVRL_2221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2221) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2231": {' 
Write(123,*) '      "Re": ',Real(GVRL_2231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2231) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2211": {' 
Write(123,*) '      "Re": ',Real(GSLL_2211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2211) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2221": {' 
Write(123,*) '      "Re": ',Real(GSLL_2221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2221) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2231": {' 
Write(123,*) '      "Re": ',Real(GSLL_2231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2231) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2211": {' 
Write(123,*) '      "Re": ',Real(GSRL_2211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2211) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2221": {' 
Write(123,*) '      "Re": ',Real(GSRL_2221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2221) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2231": {' 
Write(123,*) '      "Re": ',Real(GSRL_2231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2231) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1111": {' 
Write(123,*) '      "Re": ',Real(GVLL_1111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1111) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1121": {' 
Write(123,*) '      "Re": ',Real(GVLL_1121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1121) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1131": {' 
Write(123,*) '      "Re": ',Real(GVLL_1131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1131) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1111": {' 
Write(123,*) '      "Re": ',Real(GVRL_1111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1111) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1121": {' 
Write(123,*) '      "Re": ',Real(GVRL_1121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1121) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1131": {' 
Write(123,*) '      "Re": ',Real(GVRL_1131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1131) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1111": {' 
Write(123,*) '      "Re": ',Real(GSLL_1111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1111) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1121": {' 
Write(123,*) '      "Re": ',Real(GSLL_1121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1121) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1131": {' 
Write(123,*) '      "Re": ',Real(GSLL_1131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1131) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1111": {' 
Write(123,*) '      "Re": ',Real(GSRL_1111,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1111) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1121": {' 
Write(123,*) '      "Re": ',Real(GSRL_1121,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1121) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1131": {' 
Write(123,*) '      "Re": ',Real(GSRL_1131,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1131) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1211": {' 
Write(123,*) '      "Re": ',Real(GVLL_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1211) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1221": {' 
Write(123,*) '      "Re": ',Real(GVLL_1221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1221) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1231": {' 
Write(123,*) '      "Re": ',Real(GVLL_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1231) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1211": {' 
Write(123,*) '      "Re": ',Real(GVRL_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1211) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1221": {' 
Write(123,*) '      "Re": ',Real(GVRL_1221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1221) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1231": {' 
Write(123,*) '      "Re": ',Real(GVRL_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1231) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1211": {' 
Write(123,*) '      "Re": ',Real(GSLL_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1211) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1221": {' 
Write(123,*) '      "Re": ',Real(GSLL_1221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1221) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1231": {' 
Write(123,*) '      "Re": ',Real(GSLL_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1231) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1211": {' 
Write(123,*) '      "Re": ',Real(GSRL_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1211) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1221": {' 
Write(123,*) '      "Re": ',Real(GSRL_1221,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1221) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1231": {' 
Write(123,*) '      "Re": ',Real(GSRL_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1231) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3112": {' 
Write(123,*) '      "Re": ',Real(GVLL_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3112) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3122": {' 
Write(123,*) '      "Re": ',Real(GVLL_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3122) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3132": {' 
Write(123,*) '      "Re": ',Real(GVLL_3132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3132) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3112": {' 
Write(123,*) '      "Re": ',Real(GVRL_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3112) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3122": {' 
Write(123,*) '      "Re": ',Real(GVRL_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3122) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3132": {' 
Write(123,*) '      "Re": ',Real(GVRL_3132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3132) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3112": {' 
Write(123,*) '      "Re": ',Real(GSLL_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3112) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3122": {' 
Write(123,*) '      "Re": ',Real(GSLL_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3122) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3132": {' 
Write(123,*) '      "Re": ',Real(GSLL_3132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3132) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3112": {' 
Write(123,*) '      "Re": ',Real(GSRL_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3112) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3122": {' 
Write(123,*) '      "Re": ',Real(GSRL_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3122) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3132": {' 
Write(123,*) '      "Re": ',Real(GSRL_3132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3132) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3212": {' 
Write(123,*) '      "Re": ',Real(GVLL_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3212) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3222": {' 
Write(123,*) '      "Re": ',Real(GVLL_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3222) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3232": {' 
Write(123,*) '      "Re": ',Real(GVLL_3232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3232) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3212": {' 
Write(123,*) '      "Re": ',Real(GVRL_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3212) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3222": {' 
Write(123,*) '      "Re": ',Real(GVRL_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3222) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3232": {' 
Write(123,*) '      "Re": ',Real(GVRL_3232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3232) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3212": {' 
Write(123,*) '      "Re": ',Real(GSLL_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3212) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3222": {' 
Write(123,*) '      "Re": ',Real(GSLL_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3222) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3232": {' 
Write(123,*) '      "Re": ',Real(GSLL_3232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3232) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3212": {' 
Write(123,*) '      "Re": ',Real(GSRL_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3212) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3222": {' 
Write(123,*) '      "Re": ',Real(GSRL_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3222) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3232": {' 
Write(123,*) '      "Re": ',Real(GSRL_3232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3232) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2112": {' 
Write(123,*) '      "Re": ',Real(GVLL_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2112) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2122": {' 
Write(123,*) '      "Re": ',Real(GVLL_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2122) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2132": {' 
Write(123,*) '      "Re": ',Real(GVLL_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2132) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2112": {' 
Write(123,*) '      "Re": ',Real(GVRL_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2112) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2122": {' 
Write(123,*) '      "Re": ',Real(GVRL_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2122) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2132": {' 
Write(123,*) '      "Re": ',Real(GVRL_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2132) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2112": {' 
Write(123,*) '      "Re": ',Real(GSLL_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2112) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2122": {' 
Write(123,*) '      "Re": ',Real(GSLL_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2122) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2132": {' 
Write(123,*) '      "Re": ',Real(GSLL_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2132) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2112": {' 
Write(123,*) '      "Re": ',Real(GSRL_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2112) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2122": {' 
Write(123,*) '      "Re": ',Real(GSRL_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2122) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2132": {' 
Write(123,*) '      "Re": ',Real(GSRL_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2132) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2212": {' 
Write(123,*) '      "Re": ',Real(GVLL_2212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2212) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2222": {' 
Write(123,*) '      "Re": ',Real(GVLL_2222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2222) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2232": {' 
Write(123,*) '      "Re": ',Real(GVLL_2232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2232) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2212": {' 
Write(123,*) '      "Re": ',Real(GVRL_2212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2212) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2222": {' 
Write(123,*) '      "Re": ',Real(GVRL_2222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2222) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2232": {' 
Write(123,*) '      "Re": ',Real(GVRL_2232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2232) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2212": {' 
Write(123,*) '      "Re": ',Real(GSLL_2212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2212) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2222": {' 
Write(123,*) '      "Re": ',Real(GSLL_2222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2222) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2232": {' 
Write(123,*) '      "Re": ',Real(GSLL_2232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2232) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2212": {' 
Write(123,*) '      "Re": ',Real(GSRL_2212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2212) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2222": {' 
Write(123,*) '      "Re": ',Real(GSRL_2222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2222) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2232": {' 
Write(123,*) '      "Re": ',Real(GSRL_2232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2232) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1112": {' 
Write(123,*) '      "Re": ',Real(GVLL_1112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1112) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1122": {' 
Write(123,*) '      "Re": ',Real(GVLL_1122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1122) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1132": {' 
Write(123,*) '      "Re": ',Real(GVLL_1132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1132) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1112": {' 
Write(123,*) '      "Re": ',Real(GVRL_1112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1112) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1122": {' 
Write(123,*) '      "Re": ',Real(GVRL_1122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1122) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1132": {' 
Write(123,*) '      "Re": ',Real(GVRL_1132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1132) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1112": {' 
Write(123,*) '      "Re": ',Real(GSLL_1112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1112) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1122": {' 
Write(123,*) '      "Re": ',Real(GSLL_1122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1122) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1132": {' 
Write(123,*) '      "Re": ',Real(GSLL_1132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1132) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1112": {' 
Write(123,*) '      "Re": ',Real(GSRL_1112,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1112) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1122": {' 
Write(123,*) '      "Re": ',Real(GSRL_1122,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1122) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1132": {' 
Write(123,*) '      "Re": ',Real(GSRL_1132,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1132) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1212": {' 
Write(123,*) '      "Re": ',Real(GVLL_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1212) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1222": {' 
Write(123,*) '      "Re": ',Real(GVLL_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1222) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1232": {' 
Write(123,*) '      "Re": ',Real(GVLL_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1232) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1212": {' 
Write(123,*) '      "Re": ',Real(GVRL_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1212) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1222": {' 
Write(123,*) '      "Re": ',Real(GVRL_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1222) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1232": {' 
Write(123,*) '      "Re": ',Real(GVRL_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1232) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1212": {' 
Write(123,*) '      "Re": ',Real(GSLL_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1212) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1222": {' 
Write(123,*) '      "Re": ',Real(GSLL_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1222) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1232": {' 
Write(123,*) '      "Re": ',Real(GSLL_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1232) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1212": {' 
Write(123,*) '      "Re": ',Real(GSRL_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1212) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1222": {' 
Write(123,*) '      "Re": ',Real(GSRL_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1222) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1232": {' 
Write(123,*) '      "Re": ',Real(GSRL_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1232) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3113": {' 
Write(123,*) '      "Re": ',Real(GVLL_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3113) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3123": {' 
Write(123,*) '      "Re": ',Real(GVLL_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3123) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3133": {' 
Write(123,*) '      "Re": ',Real(GVLL_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3133) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3113": {' 
Write(123,*) '      "Re": ',Real(GVRL_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3113) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3123": {' 
Write(123,*) '      "Re": ',Real(GVRL_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3123) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3133": {' 
Write(123,*) '      "Re": ',Real(GVRL_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3133) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3113": {' 
Write(123,*) '      "Re": ',Real(GSLL_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3113) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3123": {' 
Write(123,*) '      "Re": ',Real(GSLL_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3123) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3133": {' 
Write(123,*) '      "Re": ',Real(GSLL_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3133) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3113": {' 
Write(123,*) '      "Re": ',Real(GSRL_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3113) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3123": {' 
Write(123,*) '      "Re": ',Real(GSRL_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3123) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3133": {' 
Write(123,*) '      "Re": ',Real(GSRL_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3133) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3213": {' 
Write(123,*) '      "Re": ',Real(GVLL_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3213) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3223": {' 
Write(123,*) '      "Re": ',Real(GVLL_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3223) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_3233": {' 
Write(123,*) '      "Re": ',Real(GVLL_3233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_3233) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3213": {' 
Write(123,*) '      "Re": ',Real(GVRL_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3213) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3223": {' 
Write(123,*) '      "Re": ',Real(GVRL_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3223) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_3233": {' 
Write(123,*) '      "Re": ',Real(GVRL_3233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_3233) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3213": {' 
Write(123,*) '      "Re": ',Real(GSLL_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3213) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3223": {' 
Write(123,*) '      "Re": ',Real(GSLL_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3223) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_3233": {' 
Write(123,*) '      "Re": ',Real(GSLL_3233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_3233) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3213": {' 
Write(123,*) '      "Re": ',Real(GSRL_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3213) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3223": {' 
Write(123,*) '      "Re": ',Real(GSRL_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3223) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_3233": {' 
Write(123,*) '      "Re": ',Real(GSRL_3233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_3233) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2113": {' 
Write(123,*) '      "Re": ',Real(GVLL_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2113) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2123": {' 
Write(123,*) '      "Re": ',Real(GVLL_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2123) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2133": {' 
Write(123,*) '      "Re": ',Real(GVLL_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2133) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2113": {' 
Write(123,*) '      "Re": ',Real(GVRL_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2113) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2123": {' 
Write(123,*) '      "Re": ',Real(GVRL_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2123) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2133": {' 
Write(123,*) '      "Re": ',Real(GVRL_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2133) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2113": {' 
Write(123,*) '      "Re": ',Real(GSLL_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2113) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2123": {' 
Write(123,*) '      "Re": ',Real(GSLL_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2123) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2133": {' 
Write(123,*) '      "Re": ',Real(GSLL_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2133) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2113": {' 
Write(123,*) '      "Re": ',Real(GSRL_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2113) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2123": {' 
Write(123,*) '      "Re": ',Real(GSRL_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2123) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2133": {' 
Write(123,*) '      "Re": ',Real(GSRL_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2133) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2213": {' 
Write(123,*) '      "Re": ',Real(GVLL_2213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2213) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2223": {' 
Write(123,*) '      "Re": ',Real(GVLL_2223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2223) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_2233": {' 
Write(123,*) '      "Re": ',Real(GVLL_2233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_2233) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2213": {' 
Write(123,*) '      "Re": ',Real(GVRL_2213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2213) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2223": {' 
Write(123,*) '      "Re": ',Real(GVRL_2223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2223) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_2233": {' 
Write(123,*) '      "Re": ',Real(GVRL_2233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_2233) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2213": {' 
Write(123,*) '      "Re": ',Real(GSLL_2213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2213) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2223": {' 
Write(123,*) '      "Re": ',Real(GSLL_2223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2223) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_2233": {' 
Write(123,*) '      "Re": ',Real(GSLL_2233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_2233) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2213": {' 
Write(123,*) '      "Re": ',Real(GSRL_2213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2213) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2223": {' 
Write(123,*) '      "Re": ',Real(GSRL_2223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2223) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_2233": {' 
Write(123,*) '      "Re": ',Real(GSRL_2233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_2233) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1113": {' 
Write(123,*) '      "Re": ',Real(GVLL_1113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1113) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1123": {' 
Write(123,*) '      "Re": ',Real(GVLL_1123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1123) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1133": {' 
Write(123,*) '      "Re": ',Real(GVLL_1133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1133) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1113": {' 
Write(123,*) '      "Re": ',Real(GVRL_1113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1113) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1123": {' 
Write(123,*) '      "Re": ',Real(GVRL_1123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1123) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1133": {' 
Write(123,*) '      "Re": ',Real(GVRL_1133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1133) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1113": {' 
Write(123,*) '      "Re": ',Real(GSLL_1113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1113) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1123": {' 
Write(123,*) '      "Re": ',Real(GSLL_1123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1123) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1133": {' 
Write(123,*) '      "Re": ',Real(GSLL_1133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1133) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1113": {' 
Write(123,*) '      "Re": ',Real(GSRL_1113,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1113) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1123": {' 
Write(123,*) '      "Re": ',Real(GSRL_1123,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1123) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1133": {' 
Write(123,*) '      "Re": ',Real(GSRL_1133,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1133) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1213": {' 
Write(123,*) '      "Re": ',Real(GVLL_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1213) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1223": {' 
Write(123,*) '      "Re": ',Real(GVLL_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1223) 
Write(123,*) '    },' 
Write(123,*) '    "GVLL_1233": {' 
Write(123,*) '      "Re": ',Real(GVLL_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVLL_1233) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1213": {' 
Write(123,*) '      "Re": ',Real(GVRL_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1213) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1223": {' 
Write(123,*) '      "Re": ',Real(GVRL_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1223) 
Write(123,*) '    },' 
Write(123,*) '    "GVRL_1233": {' 
Write(123,*) '      "Re": ',Real(GVRL_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GVRL_1233) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1213": {' 
Write(123,*) '      "Re": ',Real(GSLL_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1213) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1223": {' 
Write(123,*) '      "Re": ',Real(GSLL_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1223) 
Write(123,*) '    },' 
Write(123,*) '    "GSLL_1233": {' 
Write(123,*) '      "Re": ',Real(GSLL_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSLL_1233) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1213": {' 
Write(123,*) '      "Re": ',Real(GSRL_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1213) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1223": {' 
Write(123,*) '      "Re": ',Real(GSRL_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1223) 
Write(123,*) '    },' 
Write(123,*) '    "GSRL_1233": {' 
Write(123,*) '      "Re": ',Real(GSRL_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(GSRL_1233) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2311": {' 
Write(123,*) '      "Re": ',Real(FVLL_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2311) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2322": {' 
Write(123,*) '      "Re": ',Real(FVLL_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2322) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2333": {' 
Write(123,*) '      "Re": ',Real(FVLL_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2333) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2312": {' 
Write(123,*) '      "Re": ',Real(FVLL_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2312) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2313": {' 
Write(123,*) '      "Re": ',Real(FVLL_2313,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2313) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2323": {' 
Write(123,*) '      "Re": ',Real(FVLL_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2323) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_3212": {' 
Write(123,*) '      "Re": ',Real(FVLL_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_3212) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_3213": {' 
Write(123,*) '      "Re": ',Real(FVLL_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_3213) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_3223": {' 
Write(123,*) '      "Re": ',Real(FVLL_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_3223) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2311": {' 
Write(123,*) '      "Re": ',Real(FVRL_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2311) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2322": {' 
Write(123,*) '      "Re": ',Real(FVRL_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2322) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2333": {' 
Write(123,*) '      "Re": ',Real(FVRL_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2333) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2312": {' 
Write(123,*) '      "Re": ',Real(FVRL_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2312) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2313": {' 
Write(123,*) '      "Re": ',Real(FVRL_2313,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2313) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2323": {' 
Write(123,*) '      "Re": ',Real(FVRL_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2323) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_3212": {' 
Write(123,*) '      "Re": ',Real(FVRL_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_3212) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_3213": {' 
Write(123,*) '      "Re": ',Real(FVRL_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_3213) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_3223": {' 
Write(123,*) '      "Re": ',Real(FVRL_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_3223) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_1311": {' 
Write(123,*) '      "Re": ',Real(FVLL_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_1311) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_1322": {' 
Write(123,*) '      "Re": ',Real(FVLL_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_1322) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_1333": {' 
Write(123,*) '      "Re": ',Real(FVLL_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_1333) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_1312": {' 
Write(123,*) '      "Re": ',Real(FVLL_1312,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_1312) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_1313": {' 
Write(123,*) '      "Re": ',Real(FVLL_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_1313) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_1323": {' 
Write(123,*) '      "Re": ',Real(FVLL_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_1323) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_3112": {' 
Write(123,*) '      "Re": ',Real(FVLL_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_3112) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_3113": {' 
Write(123,*) '      "Re": ',Real(FVLL_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_3113) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_3123": {' 
Write(123,*) '      "Re": ',Real(FVLL_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_3123) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_1311": {' 
Write(123,*) '      "Re": ',Real(FVRL_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_1311) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_1322": {' 
Write(123,*) '      "Re": ',Real(FVRL_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_1322) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_1333": {' 
Write(123,*) '      "Re": ',Real(FVRL_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_1333) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_1312": {' 
Write(123,*) '      "Re": ',Real(FVRL_1312,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_1312) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_1313": {' 
Write(123,*) '      "Re": ',Real(FVRL_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_1313) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_1323": {' 
Write(123,*) '      "Re": ',Real(FVRL_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_1323) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_3112": {' 
Write(123,*) '      "Re": ',Real(FVRL_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_3112) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_3113": {' 
Write(123,*) '      "Re": ',Real(FVRL_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_3113) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_3123": {' 
Write(123,*) '      "Re": ',Real(FVRL_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_3123) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2111": {' 
Write(123,*) '      "Re": ',Real(FVLL_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2111) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2122": {' 
Write(123,*) '      "Re": ',Real(FVLL_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2122) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2133": {' 
Write(123,*) '      "Re": ',Real(FVLL_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2133) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2112": {' 
Write(123,*) '      "Re": ',Real(FVLL_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2112) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2113": {' 
Write(123,*) '      "Re": ',Real(FVLL_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2113) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_2123": {' 
Write(123,*) '      "Re": ',Real(FVLL_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_2123) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_1212": {' 
Write(123,*) '      "Re": ',Real(FVLL_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_1212) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_1213": {' 
Write(123,*) '      "Re": ',Real(FVLL_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_1213) 
Write(123,*) '    },' 
Write(123,*) '    "FVLL_1223": {' 
Write(123,*) '      "Re": ',Real(FVLL_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVLL_1223) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2111": {' 
Write(123,*) '      "Re": ',Real(FVRL_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2111) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2122": {' 
Write(123,*) '      "Re": ',Real(FVRL_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2122) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2133": {' 
Write(123,*) '      "Re": ',Real(FVRL_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2133) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2112": {' 
Write(123,*) '      "Re": ',Real(FVRL_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2112) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2113": {' 
Write(123,*) '      "Re": ',Real(FVRL_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2113) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_2123": {' 
Write(123,*) '      "Re": ',Real(FVRL_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_2123) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_1212": {' 
Write(123,*) '      "Re": ',Real(FVRL_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_1212) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_1213": {' 
Write(123,*) '      "Re": ',Real(FVRL_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_1213) 
Write(123,*) '    },' 
Write(123,*) '    "FVRL_1223": {' 
Write(123,*) '      "Re": ',Real(FVRL_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(FVRL_1223) 
Write(123,*) '    },' 
Write(123,*) '    "Q1R_23": {' 
Write(123,*) '      "Re": ',Real(Q1R_23,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q1R_23) 
Write(123,*) '    },' 
Write(123,*) '    "Q1R_32": {' 
Write(123,*) '      "Re": ',Real(Q1R_32,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q1R_32) 
Write(123,*) '    },' 
Write(123,*) '    "Q2R_23": {' 
Write(123,*) '      "Re": ',Real(Q2R_23,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q2R_23) 
Write(123,*) '    },' 
Write(123,*) '    "Q2R_32": {' 
Write(123,*) '      "Re": ',Real(Q2R_32,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q2R_32) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_2311": {' 
Write(123,*) '      "Re": ',Real(DVLL_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_2311) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_2322": {' 
Write(123,*) '      "Re": ',Real(DVLL_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_2322) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_2333": {' 
Write(123,*) '      "Re": ',Real(DVLL_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_2333) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1231": {' 
Write(123,*) '      "Re": ',Real(DVLL_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1231) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_2311": {' 
Write(123,*) '      "Re": ',Real(DVRR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_2322": {' 
Write(123,*) '      "Re": ',Real(DVRR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_2333": {' 
Write(123,*) '      "Re": ',Real(DVRR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1231": {' 
Write(123,*) '      "Re": ',Real(DVRR_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1231) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_2311": {' 
Write(123,*) '      "Re": ',Real(DVLR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_2322": {' 
Write(123,*) '      "Re": ',Real(DVLR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_2333": {' 
Write(123,*) '      "Re": ',Real(DVLR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "DVRL_2311": {' 
Write(123,*) '      "Re": ',Real(DVRL_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRL_2311) 
Write(123,*) '    },' 
Write(123,*) '    "DVRL_2322": {' 
Write(123,*) '      "Re": ',Real(DVRL_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRL_2322) 
Write(123,*) '    },' 
Write(123,*) '    "DVRL_2333": {' 
Write(123,*) '      "Re": ',Real(DVRL_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRL_2333) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1231": {' 
Write(123,*) '      "Re": ',Real(DVLR_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1231) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1321": {' 
Write(123,*) '      "Re": ',Real(DVLR_1321,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1321) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2311": {' 
Write(123,*) '      "Re": ',Real(DSRR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2322": {' 
Write(123,*) '      "Re": ',Real(DSRR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2333": {' 
Write(123,*) '      "Re": ',Real(DSRR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_3211": {' 
Write(123,*) '      "Re": ',Real(DSRR_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_3211) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_3222": {' 
Write(123,*) '      "Re": ',Real(DSRR_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_3222) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_3233": {' 
Write(123,*) '      "Re": ',Real(DSRR_3233,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_3233) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1231": {' 
Write(123,*) '      "Re": ',Real(DSRR_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1231) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1321": {' 
Write(123,*) '      "Re": ',Real(DSRR_1321,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1321) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_2311": {' 
Write(123,*) '      "Re": ',Real(EVLL_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_2311) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_2322": {' 
Write(123,*) '      "Re": ',Real(EVLL_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_2322) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_2333": {' 
Write(123,*) '      "Re": ',Real(EVLL_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_2333) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_2311": {' 
Write(123,*) '      "Re": ',Real(EVRR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_2322": {' 
Write(123,*) '      "Re": ',Real(EVRR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_2333": {' 
Write(123,*) '      "Re": ',Real(EVRR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2311": {' 
Write(123,*) '      "Re": ',Real(EVLR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2322": {' 
Write(123,*) '      "Re": ',Real(EVLR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2333": {' 
Write(123,*) '      "Re": ',Real(EVLR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_2311": {' 
Write(123,*) '      "Re": ',Real(EVRL_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_2311) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_2322": {' 
Write(123,*) '      "Re": ',Real(EVRL_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_2322) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_2333": {' 
Write(123,*) '      "Re": ',Real(EVRL_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_2333) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2311": {' 
Write(123,*) '      "Re": ',Real(ESRR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2322": {' 
Write(123,*) '      "Re": ',Real(ESRR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2333": {' 
Write(123,*) '      "Re": ',Real(ESRR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3211": {' 
Write(123,*) '      "Re": ',Real(ESRR_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3211) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3222": {' 
Write(123,*) '      "Re": ',Real(ESRR_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3222) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3233": {' 
Write(123,*) '      "Re": ',Real(ESRR_3233,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3233) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2311": {' 
Write(123,*) '      "Re": ',Real(ESLR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2322": {' 
Write(123,*) '      "Re": ',Real(ESLR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2333": {' 
Write(123,*) '      "Re": ',Real(ESLR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3211": {' 
Write(123,*) '      "Re": ',Real(ESLR_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3211) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3222": {' 
Write(123,*) '      "Re": ',Real(ESLR_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3222) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3233": {' 
Write(123,*) '      "Re": ',Real(ESLR_3233,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3233) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2311": {' 
Write(123,*) '      "Re": ',Real(ETRR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2322": {' 
Write(123,*) '      "Re": ',Real(ETRR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2333": {' 
Write(123,*) '      "Re": ',Real(ETRR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3211": {' 
Write(123,*) '      "Re": ',Real(ETRR_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3211) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3222": {' 
Write(123,*) '      "Re": ',Real(ETRR_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3222) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3233": {' 
Write(123,*) '      "Re": ',Real(ETRR_3233,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3233) 
Write(123,*) '    },' 
Write(123,*) '    "Q1R_13": {' 
Write(123,*) '      "Re": ',Real(Q1R_13,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q1R_13) 
Write(123,*) '    },' 
Write(123,*) '    "Q1R_31": {' 
Write(123,*) '      "Re": ',Real(Q1R_31,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q1R_31) 
Write(123,*) '    },' 
Write(123,*) '    "Q2R_13": {' 
Write(123,*) '      "Re": ',Real(Q2R_13,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q2R_13) 
Write(123,*) '    },' 
Write(123,*) '    "Q2R_31": {' 
Write(123,*) '      "Re": ',Real(Q2R_31,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q2R_31) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1311": {' 
Write(123,*) '      "Re": ',Real(DVLL_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1311) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1322": {' 
Write(123,*) '      "Re": ',Real(DVLL_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1322) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1333": {' 
Write(123,*) '      "Re": ',Real(DVLL_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1333) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_2132": {' 
Write(123,*) '      "Re": ',Real(DVLL_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_2132) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1311": {' 
Write(123,*) '      "Re": ',Real(DVRR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1322": {' 
Write(123,*) '      "Re": ',Real(DVRR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1333": {' 
Write(123,*) '      "Re": ',Real(DVRR_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1333) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_2132": {' 
Write(123,*) '      "Re": ',Real(DVRR_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_2132) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1311": {' 
Write(123,*) '      "Re": ',Real(DVLR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1322": {' 
Write(123,*) '      "Re": ',Real(DVLR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1333": {' 
Write(123,*) '      "Re": ',Real(DVLR_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1333) 
Write(123,*) '    },' 
Write(123,*) '    "DVRL_1311": {' 
Write(123,*) '      "Re": ',Real(DVRL_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRL_1311) 
Write(123,*) '    },' 
Write(123,*) '    "DVRL_1322": {' 
Write(123,*) '      "Re": ',Real(DVRL_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRL_1322) 
Write(123,*) '    },' 
Write(123,*) '    "DVRL_1333": {' 
Write(123,*) '      "Re": ',Real(DVRL_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRL_1333) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_2132": {' 
Write(123,*) '      "Re": ',Real(DVLR_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_2132) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_2312": {' 
Write(123,*) '      "Re": ',Real(DVLR_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_2312) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1311": {' 
Write(123,*) '      "Re": ',Real(DSRR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1322": {' 
Write(123,*) '      "Re": ',Real(DSRR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1333": {' 
Write(123,*) '      "Re": ',Real(DSRR_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1333) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_3111": {' 
Write(123,*) '      "Re": ',Real(DSRR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_3122": {' 
Write(123,*) '      "Re": ',Real(DSRR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_3133": {' 
Write(123,*) '      "Re": ',Real(DSRR_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_3133) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2132": {' 
Write(123,*) '      "Re": ',Real(DSRR_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2132) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2312": {' 
Write(123,*) '      "Re": ',Real(DSRR_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2312) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1311": {' 
Write(123,*) '      "Re": ',Real(EVLL_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1311) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1322": {' 
Write(123,*) '      "Re": ',Real(EVLL_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1322) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1333": {' 
Write(123,*) '      "Re": ',Real(EVLL_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1333) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1311": {' 
Write(123,*) '      "Re": ',Real(EVRR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1322": {' 
Write(123,*) '      "Re": ',Real(EVRR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1333": {' 
Write(123,*) '      "Re": ',Real(EVRR_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1333) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1311": {' 
Write(123,*) '      "Re": ',Real(EVLR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1322": {' 
Write(123,*) '      "Re": ',Real(EVLR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1333": {' 
Write(123,*) '      "Re": ',Real(EVLR_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1333) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1311": {' 
Write(123,*) '      "Re": ',Real(EVRL_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1311) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1322": {' 
Write(123,*) '      "Re": ',Real(EVRL_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1322) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1333": {' 
Write(123,*) '      "Re": ',Real(EVRL_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1333) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1311": {' 
Write(123,*) '      "Re": ',Real(ESRR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1322": {' 
Write(123,*) '      "Re": ',Real(ESRR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1333": {' 
Write(123,*) '      "Re": ',Real(ESRR_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1333) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3111": {' 
Write(123,*) '      "Re": ',Real(ESRR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3122": {' 
Write(123,*) '      "Re": ',Real(ESRR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3133": {' 
Write(123,*) '      "Re": ',Real(ESRR_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3133) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1311": {' 
Write(123,*) '      "Re": ',Real(ESLR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1322": {' 
Write(123,*) '      "Re": ',Real(ESLR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1333": {' 
Write(123,*) '      "Re": ',Real(ESLR_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1333) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3111": {' 
Write(123,*) '      "Re": ',Real(ESLR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3122": {' 
Write(123,*) '      "Re": ',Real(ESLR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3133": {' 
Write(123,*) '      "Re": ',Real(ESLR_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3133) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1311": {' 
Write(123,*) '      "Re": ',Real(ETRR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1322": {' 
Write(123,*) '      "Re": ',Real(ETRR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1333": {' 
Write(123,*) '      "Re": ',Real(ETRR_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1333) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3111": {' 
Write(123,*) '      "Re": ',Real(ETRR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3122": {' 
Write(123,*) '      "Re": ',Real(ETRR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3133": {' 
Write(123,*) '      "Re": ',Real(ETRR_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3133) 
Write(123,*) '    },' 
Write(123,*) '    "Q1R_12": {' 
Write(123,*) '      "Re": ',Real(Q1R_12,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q1R_12) 
Write(123,*) '    },' 
Write(123,*) '    "Q1R_21": {' 
Write(123,*) '      "Re": ',Real(Q1R_21,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q1R_21) 
Write(123,*) '    },' 
Write(123,*) '    "Q2R_12": {' 
Write(123,*) '      "Re": ',Real(Q2R_12,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q2R_12) 
Write(123,*) '    },' 
Write(123,*) '    "Q2R_21": {' 
Write(123,*) '      "Re": ',Real(Q2R_21,dp), ',' 
Write(123,*) '      "Im": ',AImag(Q2R_21) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1211": {' 
Write(123,*) '      "Re": ',Real(DVLL_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1211) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1222": {' 
Write(123,*) '      "Re": ',Real(DVLL_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1222) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_1233": {' 
Write(123,*) '      "Re": ',Real(DVLL_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_1233) 
Write(123,*) '    },' 
Write(123,*) '    "DVLL_3123": {' 
Write(123,*) '      "Re": ',Real(DVLL_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLL_3123) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1211": {' 
Write(123,*) '      "Re": ',Real(DVRR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1222": {' 
Write(123,*) '      "Re": ',Real(DVRR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_1233": {' 
Write(123,*) '      "Re": ',Real(DVRR_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_1233) 
Write(123,*) '    },' 
Write(123,*) '    "DVRR_3123": {' 
Write(123,*) '      "Re": ',Real(DVRR_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRR_3123) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1211": {' 
Write(123,*) '      "Re": ',Real(DVLR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1222": {' 
Write(123,*) '      "Re": ',Real(DVLR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_1233": {' 
Write(123,*) '      "Re": ',Real(DVLR_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_1233) 
Write(123,*) '    },' 
Write(123,*) '    "DVRL_1211": {' 
Write(123,*) '      "Re": ',Real(DVRL_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRL_1211) 
Write(123,*) '    },' 
Write(123,*) '    "DVRL_1222": {' 
Write(123,*) '      "Re": ',Real(DVRL_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRL_1222) 
Write(123,*) '    },' 
Write(123,*) '    "DVRL_1233": {' 
Write(123,*) '      "Re": ',Real(DVRL_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVRL_1233) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_3123": {' 
Write(123,*) '      "Re": ',Real(DVLR_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_3123) 
Write(123,*) '    },' 
Write(123,*) '    "DVLR_3213": {' 
Write(123,*) '      "Re": ',Real(DVLR_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(DVLR_3213) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1211": {' 
Write(123,*) '      "Re": ',Real(DSRR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1222": {' 
Write(123,*) '      "Re": ',Real(DSRR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_1233": {' 
Write(123,*) '      "Re": ',Real(DSRR_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_1233) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2111": {' 
Write(123,*) '      "Re": ',Real(DSRR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2122": {' 
Write(123,*) '      "Re": ',Real(DSRR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_2133": {' 
Write(123,*) '      "Re": ',Real(DSRR_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_2133) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_3123": {' 
Write(123,*) '      "Re": ',Real(DSRR_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_3123) 
Write(123,*) '    },' 
Write(123,*) '    "DSRR_3213": {' 
Write(123,*) '      "Re": ',Real(DSRR_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(DSRR_3213) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1211": {' 
Write(123,*) '      "Re": ',Real(EVLL_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1211) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1222": {' 
Write(123,*) '      "Re": ',Real(EVLL_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1222) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1233": {' 
Write(123,*) '      "Re": ',Real(EVLL_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1233) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1211": {' 
Write(123,*) '      "Re": ',Real(EVRR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1222": {' 
Write(123,*) '      "Re": ',Real(EVRR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1233": {' 
Write(123,*) '      "Re": ',Real(EVRR_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1233) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1211": {' 
Write(123,*) '      "Re": ',Real(EVLR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1222": {' 
Write(123,*) '      "Re": ',Real(EVLR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1233": {' 
Write(123,*) '      "Re": ',Real(EVLR_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1233) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1211": {' 
Write(123,*) '      "Re": ',Real(EVRL_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1211) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1222": {' 
Write(123,*) '      "Re": ',Real(EVRL_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1222) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1233": {' 
Write(123,*) '      "Re": ',Real(EVRL_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1233) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1211": {' 
Write(123,*) '      "Re": ',Real(ESRR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1222": {' 
Write(123,*) '      "Re": ',Real(ESRR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1233": {' 
Write(123,*) '      "Re": ',Real(ESRR_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1233) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2111": {' 
Write(123,*) '      "Re": ',Real(ESRR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2122": {' 
Write(123,*) '      "Re": ',Real(ESRR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2133": {' 
Write(123,*) '      "Re": ',Real(ESRR_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2133) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1211": {' 
Write(123,*) '      "Re": ',Real(ESLR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1222": {' 
Write(123,*) '      "Re": ',Real(ESLR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1233": {' 
Write(123,*) '      "Re": ',Real(ESLR_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1233) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2111": {' 
Write(123,*) '      "Re": ',Real(ESLR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2122": {' 
Write(123,*) '      "Re": ',Real(ESLR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2133": {' 
Write(123,*) '      "Re": ',Real(ESLR_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2133) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1211": {' 
Write(123,*) '      "Re": ',Real(ETRR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1222": {' 
Write(123,*) '      "Re": ',Real(ETRR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1233": {' 
Write(123,*) '      "Re": ',Real(ETRR_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1233) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2111": {' 
Write(123,*) '      "Re": ',Real(ETRR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2122": {' 
Write(123,*) '      "Re": ',Real(ETRR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2133": {' 
Write(123,*) '      "Re": ',Real(ETRR_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2133) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_2312": {' 
Write(123,*) '      "Re": ',Real(EVLL_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_2312) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_2312": {' 
Write(123,*) '      "Re": ',Real(EVRR_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_2312) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2312": {' 
Write(123,*) '      "Re": ',Real(EVLR_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2312) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_2312": {' 
Write(123,*) '      "Re": ',Real(EVRL_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_2312) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2312": {' 
Write(123,*) '      "Re": ',Real(ESRR_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2312) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3221": {' 
Write(123,*) '      "Re": ',Real(ESRR_3221,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3221) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2312": {' 
Write(123,*) '      "Re": ',Real(ESLR_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2312) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3221": {' 
Write(123,*) '      "Re": ',Real(ESLR_3221,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3221) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2312": {' 
Write(123,*) '      "Re": ',Real(ETRR_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2312) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3221": {' 
Write(123,*) '      "Re": ',Real(ETRR_3221,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3221) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_3212": {' 
Write(123,*) '      "Re": ',Real(EVLL_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_3212) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_3212": {' 
Write(123,*) '      "Re": ',Real(EVRR_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_3212) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_3212": {' 
Write(123,*) '      "Re": ',Real(EVLR_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_3212) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_3212": {' 
Write(123,*) '      "Re": ',Real(EVRL_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_3212) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3212": {' 
Write(123,*) '      "Re": ',Real(ESRR_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3212) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2321": {' 
Write(123,*) '      "Re": ',Real(ESRR_2321,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2321) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3212": {' 
Write(123,*) '      "Re": ',Real(ESLR_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3212) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2321": {' 
Write(123,*) '      "Re": ',Real(ESLR_2321,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2321) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3212": {' 
Write(123,*) '      "Re": ',Real(ETRR_3212,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3212) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2321": {' 
Write(123,*) '      "Re": ',Real(ETRR_2321,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2321) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_2313": {' 
Write(123,*) '      "Re": ',Real(EVLL_2313,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_2313) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_2313": {' 
Write(123,*) '      "Re": ',Real(EVRR_2313,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_2313) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2313": {' 
Write(123,*) '      "Re": ',Real(EVLR_2313,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2313) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_2313": {' 
Write(123,*) '      "Re": ',Real(EVRL_2313,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_2313) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2313": {' 
Write(123,*) '      "Re": ',Real(ESRR_2313,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2313) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3231": {' 
Write(123,*) '      "Re": ',Real(ESRR_3231,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3231) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2313": {' 
Write(123,*) '      "Re": ',Real(ESLR_2313,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2313) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3231": {' 
Write(123,*) '      "Re": ',Real(ESLR_3231,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3231) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2313": {' 
Write(123,*) '      "Re": ',Real(ETRR_2313,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2313) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3231": {' 
Write(123,*) '      "Re": ',Real(ETRR_3231,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3231) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_3213": {' 
Write(123,*) '      "Re": ',Real(EVLL_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_3213) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_3213": {' 
Write(123,*) '      "Re": ',Real(EVRR_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_3213) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_3213": {' 
Write(123,*) '      "Re": ',Real(EVLR_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_3213) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_3213": {' 
Write(123,*) '      "Re": ',Real(EVRL_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_3213) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3213": {' 
Write(123,*) '      "Re": ',Real(ESRR_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3213) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2331": {' 
Write(123,*) '      "Re": ',Real(ESRR_2331,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2331) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3213": {' 
Write(123,*) '      "Re": ',Real(ESLR_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3213) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2331": {' 
Write(123,*) '      "Re": ',Real(ESLR_2331,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2331) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3213": {' 
Write(123,*) '      "Re": ',Real(ETRR_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3213) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2331": {' 
Write(123,*) '      "Re": ',Real(ETRR_2331,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2331) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_2323": {' 
Write(123,*) '      "Re": ',Real(EVLL_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_2323) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_2323": {' 
Write(123,*) '      "Re": ',Real(EVRR_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_2323) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2323": {' 
Write(123,*) '      "Re": ',Real(EVLR_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2323) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_2323": {' 
Write(123,*) '      "Re": ',Real(EVRL_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_2323) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2323": {' 
Write(123,*) '      "Re": ',Real(ESRR_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2323) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3232": {' 
Write(123,*) '      "Re": ',Real(ESRR_3232,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3232) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2323": {' 
Write(123,*) '      "Re": ',Real(ESLR_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2323) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3232": {' 
Write(123,*) '      "Re": ',Real(ESLR_3232,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3232) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2323": {' 
Write(123,*) '      "Re": ',Real(ETRR_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2323) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3232": {' 
Write(123,*) '      "Re": ',Real(ETRR_3232,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3232) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_3223": {' 
Write(123,*) '      "Re": ',Real(EVLL_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_3223) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_3223": {' 
Write(123,*) '      "Re": ',Real(EVRR_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_3223) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_3223": {' 
Write(123,*) '      "Re": ',Real(EVLR_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_3223) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_3223": {' 
Write(123,*) '      "Re": ',Real(EVRL_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_3223) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3223": {' 
Write(123,*) '      "Re": ',Real(ESRR_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3223) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2332": {' 
Write(123,*) '      "Re": ',Real(ESRR_2332,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2332) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3223": {' 
Write(123,*) '      "Re": ',Real(ESLR_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3223) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2332": {' 
Write(123,*) '      "Re": ',Real(ESLR_2332,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2332) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3223": {' 
Write(123,*) '      "Re": ',Real(ETRR_3223,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3223) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2332": {' 
Write(123,*) '      "Re": ',Real(ETRR_2332,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2332) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1312": {' 
Write(123,*) '      "Re": ',Real(EVLL_1312,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1312) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1312": {' 
Write(123,*) '      "Re": ',Real(EVRR_1312,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1312) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1312": {' 
Write(123,*) '      "Re": ',Real(EVLR_1312,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1312) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1312": {' 
Write(123,*) '      "Re": ',Real(EVRL_1312,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1312) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1312": {' 
Write(123,*) '      "Re": ',Real(ESRR_1312,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1312) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3121": {' 
Write(123,*) '      "Re": ',Real(ESRR_3121,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3121) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1312": {' 
Write(123,*) '      "Re": ',Real(ESLR_1312,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1312) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3121": {' 
Write(123,*) '      "Re": ',Real(ESLR_3121,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3121) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1312": {' 
Write(123,*) '      "Re": ',Real(ETRR_1312,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1312) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3121": {' 
Write(123,*) '      "Re": ',Real(ETRR_3121,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3121) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_3112": {' 
Write(123,*) '      "Re": ',Real(EVLL_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_3112) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_3112": {' 
Write(123,*) '      "Re": ',Real(EVRR_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_3112) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_3112": {' 
Write(123,*) '      "Re": ',Real(EVLR_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_3112) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_3112": {' 
Write(123,*) '      "Re": ',Real(EVRL_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_3112) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3112": {' 
Write(123,*) '      "Re": ',Real(ESRR_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3112) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1321": {' 
Write(123,*) '      "Re": ',Real(ESRR_1321,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1321) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3112": {' 
Write(123,*) '      "Re": ',Real(ESLR_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3112) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1321": {' 
Write(123,*) '      "Re": ',Real(ESLR_1321,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1321) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3112": {' 
Write(123,*) '      "Re": ',Real(ETRR_3112,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3112) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1321": {' 
Write(123,*) '      "Re": ',Real(ETRR_1321,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1321) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1313": {' 
Write(123,*) '      "Re": ',Real(EVLL_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1313) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1313": {' 
Write(123,*) '      "Re": ',Real(EVRR_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1313) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1313": {' 
Write(123,*) '      "Re": ',Real(EVLR_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1313) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1313": {' 
Write(123,*) '      "Re": ',Real(EVRL_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1313) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1313": {' 
Write(123,*) '      "Re": ',Real(ESRR_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1313) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3131": {' 
Write(123,*) '      "Re": ',Real(ESRR_3131,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3131) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1313": {' 
Write(123,*) '      "Re": ',Real(ESLR_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1313) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3131": {' 
Write(123,*) '      "Re": ',Real(ESLR_3131,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3131) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1313": {' 
Write(123,*) '      "Re": ',Real(ETRR_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1313) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3131": {' 
Write(123,*) '      "Re": ',Real(ETRR_3131,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3131) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_3113": {' 
Write(123,*) '      "Re": ',Real(EVLL_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_3113) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_3113": {' 
Write(123,*) '      "Re": ',Real(EVRR_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_3113) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_3113": {' 
Write(123,*) '      "Re": ',Real(EVLR_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_3113) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_3113": {' 
Write(123,*) '      "Re": ',Real(EVRL_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_3113) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3113": {' 
Write(123,*) '      "Re": ',Real(ESRR_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3113) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1331": {' 
Write(123,*) '      "Re": ',Real(ESRR_1331,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1331) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3113": {' 
Write(123,*) '      "Re": ',Real(ESLR_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3113) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1331": {' 
Write(123,*) '      "Re": ',Real(ESLR_1331,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1331) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3113": {' 
Write(123,*) '      "Re": ',Real(ETRR_3113,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3113) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1331": {' 
Write(123,*) '      "Re": ',Real(ETRR_1331,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1331) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1323": {' 
Write(123,*) '      "Re": ',Real(EVLL_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1323) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1323": {' 
Write(123,*) '      "Re": ',Real(EVRR_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1323) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1323": {' 
Write(123,*) '      "Re": ',Real(EVLR_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1323) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1323": {' 
Write(123,*) '      "Re": ',Real(EVRL_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1323) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1323": {' 
Write(123,*) '      "Re": ',Real(ESRR_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1323) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3132": {' 
Write(123,*) '      "Re": ',Real(ESRR_3132,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3132) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1323": {' 
Write(123,*) '      "Re": ',Real(ESLR_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1323) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3132": {' 
Write(123,*) '      "Re": ',Real(ESLR_3132,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3132) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1323": {' 
Write(123,*) '      "Re": ',Real(ETRR_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1323) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3132": {' 
Write(123,*) '      "Re": ',Real(ETRR_3132,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3132) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_3123": {' 
Write(123,*) '      "Re": ',Real(EVLL_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_3123) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_3123": {' 
Write(123,*) '      "Re": ',Real(EVRR_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_3123) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_3123": {' 
Write(123,*) '      "Re": ',Real(EVLR_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_3123) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_3123": {' 
Write(123,*) '      "Re": ',Real(EVRL_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_3123) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_3123": {' 
Write(123,*) '      "Re": ',Real(ESRR_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_3123) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1332": {' 
Write(123,*) '      "Re": ',Real(ESRR_1332,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1332) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_3123": {' 
Write(123,*) '      "Re": ',Real(ESLR_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_3123) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1332": {' 
Write(123,*) '      "Re": ',Real(ESLR_1332,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1332) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_3123": {' 
Write(123,*) '      "Re": ',Real(ETRR_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_3123) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1332": {' 
Write(123,*) '      "Re": ',Real(ETRR_1332,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1332) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_2112": {' 
Write(123,*) '      "Re": ',Real(EVLL_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_2112) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_2112": {' 
Write(123,*) '      "Re": ',Real(EVRR_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_2112) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2112": {' 
Write(123,*) '      "Re": ',Real(EVLR_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2112) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_2112": {' 
Write(123,*) '      "Re": ',Real(EVRL_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_2112) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2112": {' 
Write(123,*) '      "Re": ',Real(ESRR_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2112) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1221": {' 
Write(123,*) '      "Re": ',Real(ESRR_1221,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1221) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2112": {' 
Write(123,*) '      "Re": ',Real(ESLR_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2112) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1221": {' 
Write(123,*) '      "Re": ',Real(ESLR_1221,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1221) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2112": {' 
Write(123,*) '      "Re": ',Real(ETRR_2112,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2112) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1221": {' 
Write(123,*) '      "Re": ',Real(ETRR_1221,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1221) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1212": {' 
Write(123,*) '      "Re": ',Real(EVLL_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1212) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1212": {' 
Write(123,*) '      "Re": ',Real(EVRR_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1212) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1212": {' 
Write(123,*) '      "Re": ',Real(EVLR_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1212) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1212": {' 
Write(123,*) '      "Re": ',Real(EVRL_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1212) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1212": {' 
Write(123,*) '      "Re": ',Real(ESRR_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1212) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2121": {' 
Write(123,*) '      "Re": ',Real(ESRR_2121,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2121) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1212": {' 
Write(123,*) '      "Re": ',Real(ESLR_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1212) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2121": {' 
Write(123,*) '      "Re": ',Real(ESLR_2121,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2121) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1212": {' 
Write(123,*) '      "Re": ',Real(ETRR_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1212) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2121": {' 
Write(123,*) '      "Re": ',Real(ETRR_2121,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2121) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_2113": {' 
Write(123,*) '      "Re": ',Real(EVLL_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_2113) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_2113": {' 
Write(123,*) '      "Re": ',Real(EVRR_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_2113) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2113": {' 
Write(123,*) '      "Re": ',Real(EVLR_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2113) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_2113": {' 
Write(123,*) '      "Re": ',Real(EVRL_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_2113) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2113": {' 
Write(123,*) '      "Re": ',Real(ESRR_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2113) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1231": {' 
Write(123,*) '      "Re": ',Real(ESRR_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1231) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2113": {' 
Write(123,*) '      "Re": ',Real(ESLR_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2113) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1231": {' 
Write(123,*) '      "Re": ',Real(ESLR_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1231) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2113": {' 
Write(123,*) '      "Re": ',Real(ETRR_2113,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2113) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1231": {' 
Write(123,*) '      "Re": ',Real(ETRR_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1231) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1213": {' 
Write(123,*) '      "Re": ',Real(EVLL_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1213) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1213": {' 
Write(123,*) '      "Re": ',Real(EVRR_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1213) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1213": {' 
Write(123,*) '      "Re": ',Real(EVLR_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1213) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1213": {' 
Write(123,*) '      "Re": ',Real(EVRL_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1213) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1213": {' 
Write(123,*) '      "Re": ',Real(ESRR_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1213) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2131": {' 
Write(123,*) '      "Re": ',Real(ESRR_2131,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2131) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1213": {' 
Write(123,*) '      "Re": ',Real(ESLR_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1213) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2131": {' 
Write(123,*) '      "Re": ',Real(ESLR_2131,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2131) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1213": {' 
Write(123,*) '      "Re": ',Real(ETRR_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1213) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2131": {' 
Write(123,*) '      "Re": ',Real(ETRR_2131,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2131) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_2123": {' 
Write(123,*) '      "Re": ',Real(EVLL_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_2123) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_2123": {' 
Write(123,*) '      "Re": ',Real(EVRR_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_2123) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2123": {' 
Write(123,*) '      "Re": ',Real(EVLR_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2123) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_2123": {' 
Write(123,*) '      "Re": ',Real(EVRL_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_2123) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2123": {' 
Write(123,*) '      "Re": ',Real(ESRR_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2123) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1232": {' 
Write(123,*) '      "Re": ',Real(ESRR_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1232) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2123": {' 
Write(123,*) '      "Re": ',Real(ESLR_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2123) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1232": {' 
Write(123,*) '      "Re": ',Real(ESLR_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1232) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2123": {' 
Write(123,*) '      "Re": ',Real(ETRR_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2123) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1232": {' 
Write(123,*) '      "Re": ',Real(ETRR_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1232) 
Write(123,*) '    },' 
Write(123,*) '    "EVLL_1223": {' 
Write(123,*) '      "Re": ',Real(EVLL_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLL_1223) 
Write(123,*) '    },' 
Write(123,*) '    "EVRR_1223": {' 
Write(123,*) '      "Re": ',Real(EVRR_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRR_1223) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1223": {' 
Write(123,*) '      "Re": ',Real(EVLR_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1223) 
Write(123,*) '    },' 
Write(123,*) '    "EVRL_1223": {' 
Write(123,*) '      "Re": ',Real(EVRL_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVRL_1223) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_1223": {' 
Write(123,*) '      "Re": ',Real(ESRR_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_1223) 
Write(123,*) '    },' 
Write(123,*) '    "ESRR_2132": {' 
Write(123,*) '      "Re": ',Real(ESRR_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESRR_2132) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_1223": {' 
Write(123,*) '      "Re": ',Real(ESLR_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_1223) 
Write(123,*) '    },' 
Write(123,*) '    "ESLR_2132": {' 
Write(123,*) '      "Re": ',Real(ESLR_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(ESLR_2132) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_1223": {' 
Write(123,*) '      "Re": ',Real(ETRR_1223,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_1223) 
Write(123,*) '    },' 
Write(123,*) '    "ETRR_2132": {' 
Write(123,*) '      "Re": ',Real(ETRR_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(ETRR_2132) 
Write(123,*) '    }' 
Write(123,*) "  }"
Write(123,*) "}"
    Close(123) 
   Open(123,file="WC.BGLNCS_2.json",status="unknown")
Write(123,*) "{"
Write(123,*) '  "eft": "WET",'
Write(123,*) '  "basis": "FlavorKit",'
Write(123,*) '  "scale": "91.",'
Write(123,*) '  "values": {'
Write(123,*) '    "K2R_21": {' 
Write(123,*) '      "Re": ',Real(K2R_21,dp), ',' 
Write(123,*) '      "Im": ',AImag(K2R_21) 
Write(123,*) '    },' 
Write(123,*) '    "K2R_12": {' 
Write(123,*) '      "Re": ',Real(K2R_12,dp), ',' 
Write(123,*) '      "Im": ',AImag(K2R_12) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_1121": {' 
Write(123,*) '      "Re": ',Real(AVLL_1121,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_1121) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_2221": {' 
Write(123,*) '      "Re": ',Real(AVLL_2221,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_2221) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_3321": {' 
Write(123,*) '      "Re": ',Real(AVLL_3321,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_3321) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_1121": {' 
Write(123,*) '      "Re": ',Real(AVRR_1121,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_1121) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_2221": {' 
Write(123,*) '      "Re": ',Real(AVRR_2221,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_2221) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_3321": {' 
Write(123,*) '      "Re": ',Real(AVRR_3321,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_3321) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_1121": {' 
Write(123,*) '      "Re": ',Real(AVLR_1121,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_1121) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2221": {' 
Write(123,*) '      "Re": ',Real(AVLR_2221,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2221) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_3321": {' 
Write(123,*) '      "Re": ',Real(AVLR_3321,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_3321) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2111": {' 
Write(123,*) '      "Re": ',Real(AVLR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2122": {' 
Write(123,*) '      "Re": ',Real(AVLR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2133": {' 
Write(123,*) '      "Re": ',Real(AVLR_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2133) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_3213": {' 
Write(123,*) '      "Re": ',Real(AVLR_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_3213) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_3123": {' 
Write(123,*) '      "Re": ',Real(AVLR_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_3123) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1121": {' 
Write(123,*) '      "Re": ',Real(ASRR_1121,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1121) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2221": {' 
Write(123,*) '      "Re": ',Real(ASRR_2221,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2221) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_3321": {' 
Write(123,*) '      "Re": ',Real(ASRR_3321,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_3321) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1112": {' 
Write(123,*) '      "Re": ',Real(ASRR_1112,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1112) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2212": {' 
Write(123,*) '      "Re": ',Real(ASRR_2212,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2212) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_3312": {' 
Write(123,*) '      "Re": ',Real(ASRR_3312,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_3312) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_3213": {' 
Write(123,*) '      "Re": ',Real(ASRR_3213,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_3213) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_3123": {' 
Write(123,*) '      "Re": ',Real(ASRR_3123,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_3123) 
Write(123,*) '    },' 
Write(123,*) '    "BVLL_2111": {' 
Write(123,*) '      "Re": ',Real(BVLL_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLL_2111) 
Write(123,*) '    },' 
Write(123,*) '    "BVLL_2122": {' 
Write(123,*) '      "Re": ',Real(BVLL_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLL_2122) 
Write(123,*) '    },' 
Write(123,*) '    "BVLL_2133": {' 
Write(123,*) '      "Re": ',Real(BVLL_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLL_2133) 
Write(123,*) '    },' 
Write(123,*) '    "BVRR_2111": {' 
Write(123,*) '      "Re": ',Real(BVRR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVRR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "BVRR_2122": {' 
Write(123,*) '      "Re": ',Real(BVRR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVRR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "BVRR_2133": {' 
Write(123,*) '      "Re": ',Real(BVRR_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVRR_2133) 
Write(123,*) '    },' 
Write(123,*) '    "BVLR_2111": {' 
Write(123,*) '      "Re": ',Real(BVLR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "BVLR_2122": {' 
Write(123,*) '      "Re": ',Real(BVLR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "BVLR_2133": {' 
Write(123,*) '      "Re": ',Real(BVLR_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLR_2133) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_2111": {' 
Write(123,*) '      "Re": ',Real(BSRL_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_2111) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_2122": {' 
Write(123,*) '      "Re": ',Real(BSRL_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_2122) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_2133": {' 
Write(123,*) '      "Re": ',Real(BSRL_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_2133) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_1211": {' 
Write(123,*) '      "Re": ',Real(BSRL_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_1211) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_1222": {' 
Write(123,*) '      "Re": ',Real(BSRL_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_1222) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_1233": {' 
Write(123,*) '      "Re": ',Real(BSRL_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_1233) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_2111": {' 
Write(123,*) '      "Re": ',Real(BSRR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_2122": {' 
Write(123,*) '      "Re": ',Real(BSRR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_2133": {' 
Write(123,*) '      "Re": ',Real(BSRR_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_2133) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_1211": {' 
Write(123,*) '      "Re": ',Real(BSRR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_1222": {' 
Write(123,*) '      "Re": ',Real(BSRR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_1233": {' 
Write(123,*) '      "Re": ',Real(BSRR_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_1233) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_2111": {' 
Write(123,*) '      "Re": ',Real(BTRR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_2122": {' 
Write(123,*) '      "Re": ',Real(BTRR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_2133": {' 
Write(123,*) '      "Re": ',Real(BTRR_2133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_2133) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_1211": {' 
Write(123,*) '      "Re": ',Real(BTRR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_1222": {' 
Write(123,*) '      "Re": ',Real(BTRR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_1233": {' 
Write(123,*) '      "Re": ',Real(BTRR_1233,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_1233) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1121": {' 
Write(123,*) '      "Re": ',Real(EVLR_1121,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1121) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2221": {' 
Write(123,*) '      "Re": ',Real(EVLR_2221,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2221) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_3321": {' 
Write(123,*) '      "Re": ',Real(EVLR_3321,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_3321) 
Write(123,*) '    },' 
Write(123,*) '    "CVLL_2111": {' 
Write(123,*) '      "Re": ',Real(CVLL_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLL_2111) 
Write(123,*) '    },' 
Write(123,*) '    "CVLL_2122": {' 
Write(123,*) '      "Re": ',Real(CVLL_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLL_2122) 
Write(123,*) '    },' 
Write(123,*) '    "CVRR_2111": {' 
Write(123,*) '      "Re": ',Real(CVRR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVRR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "CVRR_2122": {' 
Write(123,*) '      "Re": ',Real(CVRR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVRR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "CVLR_2111": {' 
Write(123,*) '      "Re": ',Real(CVLR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "CVLR_2122": {' 
Write(123,*) '      "Re": ',Real(CVLR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_2111": {' 
Write(123,*) '      "Re": ',Real(CSRL_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_2111) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_2122": {' 
Write(123,*) '      "Re": ',Real(CSRL_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_2122) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_1211": {' 
Write(123,*) '      "Re": ',Real(CSRL_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_1211) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_1222": {' 
Write(123,*) '      "Re": ',Real(CSRL_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_1222) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_2111": {' 
Write(123,*) '      "Re": ',Real(CSRR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_2122": {' 
Write(123,*) '      "Re": ',Real(CSRR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_1211": {' 
Write(123,*) '      "Re": ',Real(CSRR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_1222": {' 
Write(123,*) '      "Re": ',Real(CSRR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_2111": {' 
Write(123,*) '      "Re": ',Real(CTRR_2111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_2111) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_2122": {' 
Write(123,*) '      "Re": ',Real(CTRR_2122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_2122) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_1211": {' 
Write(123,*) '      "Re": ',Real(CTRR_1211,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_1211) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_1222": {' 
Write(123,*) '      "Re": ',Real(CTRR_1222,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_1222) 
Write(123,*) '    },' 
Write(123,*) '    "K2R_31": {' 
Write(123,*) '      "Re": ',Real(K2R_31,dp), ',' 
Write(123,*) '      "Im": ',AImag(K2R_31) 
Write(123,*) '    },' 
Write(123,*) '    "K2R_13": {' 
Write(123,*) '      "Re": ',Real(K2R_13,dp), ',' 
Write(123,*) '      "Im": ',AImag(K2R_13) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_1131": {' 
Write(123,*) '      "Re": ',Real(AVLL_1131,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_1131) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_2231": {' 
Write(123,*) '      "Re": ',Real(AVLL_2231,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_2231) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_3331": {' 
Write(123,*) '      "Re": ',Real(AVLL_3331,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_3331) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_1131": {' 
Write(123,*) '      "Re": ',Real(AVRR_1131,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_1131) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_2231": {' 
Write(123,*) '      "Re": ',Real(AVRR_2231,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_2231) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_3331": {' 
Write(123,*) '      "Re": ',Real(AVRR_3331,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_3331) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_1131": {' 
Write(123,*) '      "Re": ',Real(AVLR_1131,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_1131) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2231": {' 
Write(123,*) '      "Re": ',Real(AVLR_2231,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2231) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_3331": {' 
Write(123,*) '      "Re": ',Real(AVLR_3331,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_3331) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_3111": {' 
Write(123,*) '      "Re": ',Real(AVLR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_3122": {' 
Write(123,*) '      "Re": ',Real(AVLR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_3133": {' 
Write(123,*) '      "Re": ',Real(AVLR_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_3133) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2312": {' 
Write(123,*) '      "Re": ',Real(AVLR_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2312) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2132": {' 
Write(123,*) '      "Re": ',Real(AVLR_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2132) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1131": {' 
Write(123,*) '      "Re": ',Real(ASRR_1131,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1131) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2231": {' 
Write(123,*) '      "Re": ',Real(ASRR_2231,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2231) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_3331": {' 
Write(123,*) '      "Re": ',Real(ASRR_3331,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_3331) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1113": {' 
Write(123,*) '      "Re": ',Real(ASRR_1113,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1113) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2213": {' 
Write(123,*) '      "Re": ',Real(ASRR_2213,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2213) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_3313": {' 
Write(123,*) '      "Re": ',Real(ASRR_3313,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_3313) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2312": {' 
Write(123,*) '      "Re": ',Real(ASRR_2312,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2312) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2132": {' 
Write(123,*) '      "Re": ',Real(ASRR_2132,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2132) 
Write(123,*) '    },' 
Write(123,*) '    "BVLL_3111": {' 
Write(123,*) '      "Re": ',Real(BVLL_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLL_3111) 
Write(123,*) '    },' 
Write(123,*) '    "BVLL_3122": {' 
Write(123,*) '      "Re": ',Real(BVLL_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLL_3122) 
Write(123,*) '    },' 
Write(123,*) '    "BVLL_3133": {' 
Write(123,*) '      "Re": ',Real(BVLL_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLL_3133) 
Write(123,*) '    },' 
Write(123,*) '    "BVRR_3111": {' 
Write(123,*) '      "Re": ',Real(BVRR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVRR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "BVRR_3122": {' 
Write(123,*) '      "Re": ',Real(BVRR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVRR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "BVRR_3133": {' 
Write(123,*) '      "Re": ',Real(BVRR_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVRR_3133) 
Write(123,*) '    },' 
Write(123,*) '    "BVLR_3111": {' 
Write(123,*) '      "Re": ',Real(BVLR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "BVLR_3122": {' 
Write(123,*) '      "Re": ',Real(BVLR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "BVLR_3133": {' 
Write(123,*) '      "Re": ',Real(BVLR_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLR_3133) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_3111": {' 
Write(123,*) '      "Re": ',Real(BSRL_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_3111) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_3122": {' 
Write(123,*) '      "Re": ',Real(BSRL_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_3122) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_3133": {' 
Write(123,*) '      "Re": ',Real(BSRL_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_3133) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_1311": {' 
Write(123,*) '      "Re": ',Real(BSRL_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_1311) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_1322": {' 
Write(123,*) '      "Re": ',Real(BSRL_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_1322) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_1333": {' 
Write(123,*) '      "Re": ',Real(BSRL_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_1333) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_3111": {' 
Write(123,*) '      "Re": ',Real(BSRR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_3122": {' 
Write(123,*) '      "Re": ',Real(BSRR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_3133": {' 
Write(123,*) '      "Re": ',Real(BSRR_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_3133) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_1311": {' 
Write(123,*) '      "Re": ',Real(BSRR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_1322": {' 
Write(123,*) '      "Re": ',Real(BSRR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_1333": {' 
Write(123,*) '      "Re": ',Real(BSRR_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_1333) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_3111": {' 
Write(123,*) '      "Re": ',Real(BTRR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_3122": {' 
Write(123,*) '      "Re": ',Real(BTRR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_3133": {' 
Write(123,*) '      "Re": ',Real(BTRR_3133,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_3133) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_1311": {' 
Write(123,*) '      "Re": ',Real(BTRR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_1322": {' 
Write(123,*) '      "Re": ',Real(BTRR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_1333": {' 
Write(123,*) '      "Re": ',Real(BTRR_1333,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_1333) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1131": {' 
Write(123,*) '      "Re": ',Real(EVLR_1131,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1131) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2231": {' 
Write(123,*) '      "Re": ',Real(EVLR_2231,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2231) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_3331": {' 
Write(123,*) '      "Re": ',Real(EVLR_3331,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_3331) 
Write(123,*) '    },' 
Write(123,*) '    "CVLL_3111": {' 
Write(123,*) '      "Re": ',Real(CVLL_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLL_3111) 
Write(123,*) '    },' 
Write(123,*) '    "CVLL_3122": {' 
Write(123,*) '      "Re": ',Real(CVLL_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLL_3122) 
Write(123,*) '    },' 
Write(123,*) '    "CVRR_3111": {' 
Write(123,*) '      "Re": ',Real(CVRR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVRR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "CVRR_3122": {' 
Write(123,*) '      "Re": ',Real(CVRR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVRR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "CVLR_3111": {' 
Write(123,*) '      "Re": ',Real(CVLR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "CVLR_3122": {' 
Write(123,*) '      "Re": ',Real(CVLR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_3111": {' 
Write(123,*) '      "Re": ',Real(CSRL_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_3111) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_3122": {' 
Write(123,*) '      "Re": ',Real(CSRL_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_3122) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_1311": {' 
Write(123,*) '      "Re": ',Real(CSRL_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_1311) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_1322": {' 
Write(123,*) '      "Re": ',Real(CSRL_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_1322) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_3111": {' 
Write(123,*) '      "Re": ',Real(CSRR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_3122": {' 
Write(123,*) '      "Re": ',Real(CSRR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_1311": {' 
Write(123,*) '      "Re": ',Real(CSRR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_1322": {' 
Write(123,*) '      "Re": ',Real(CSRR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_3111": {' 
Write(123,*) '      "Re": ',Real(CTRR_3111,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_3111) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_3122": {' 
Write(123,*) '      "Re": ',Real(CTRR_3122,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_3122) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_1311": {' 
Write(123,*) '      "Re": ',Real(CTRR_1311,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_1311) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_1322": {' 
Write(123,*) '      "Re": ',Real(CTRR_1322,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_1322) 
Write(123,*) '    },' 
Write(123,*) '    "K2R_23": {' 
Write(123,*) '      "Re": ',Real(K2R_23,dp), ',' 
Write(123,*) '      "Im": ',AImag(K2R_23) 
Write(123,*) '    },' 
Write(123,*) '    "K2R_32": {' 
Write(123,*) '      "Re": ',Real(K2R_32,dp), ',' 
Write(123,*) '      "Im": ',AImag(K2R_32) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_1123": {' 
Write(123,*) '      "Re": ',Real(AVLL_1123,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_1123) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_2223": {' 
Write(123,*) '      "Re": ',Real(AVLL_2223,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_2223) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_3323": {' 
Write(123,*) '      "Re": ',Real(AVLL_3323,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_3323) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_1123": {' 
Write(123,*) '      "Re": ',Real(AVRR_1123,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_1123) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_2223": {' 
Write(123,*) '      "Re": ',Real(AVRR_2223,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_2223) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_3323": {' 
Write(123,*) '      "Re": ',Real(AVRR_3323,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_3323) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_1123": {' 
Write(123,*) '      "Re": ',Real(AVLR_1123,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_1123) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2223": {' 
Write(123,*) '      "Re": ',Real(AVLR_2223,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2223) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_3323": {' 
Write(123,*) '      "Re": ',Real(AVLR_3323,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_3323) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2311": {' 
Write(123,*) '      "Re": ',Real(AVLR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2322": {' 
Write(123,*) '      "Re": ',Real(AVLR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2333": {' 
Write(123,*) '      "Re": ',Real(AVLR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_1231": {' 
Write(123,*) '      "Re": ',Real(AVLR_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_1231) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_1321": {' 
Write(123,*) '      "Re": ',Real(AVLR_1321,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_1321) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1123": {' 
Write(123,*) '      "Re": ',Real(ASRR_1123,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1123) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2223": {' 
Write(123,*) '      "Re": ',Real(ASRR_2223,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2223) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_3323": {' 
Write(123,*) '      "Re": ',Real(ASRR_3323,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_3323) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1132": {' 
Write(123,*) '      "Re": ',Real(ASRR_1132,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1132) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2232": {' 
Write(123,*) '      "Re": ',Real(ASRR_2232,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2232) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_3332": {' 
Write(123,*) '      "Re": ',Real(ASRR_3332,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_3332) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1231": {' 
Write(123,*) '      "Re": ',Real(ASRR_1231,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1231) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1321": {' 
Write(123,*) '      "Re": ',Real(ASRR_1321,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1321) 
Write(123,*) '    },' 
Write(123,*) '    "BVLL_2311": {' 
Write(123,*) '      "Re": ',Real(BVLL_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLL_2311) 
Write(123,*) '    },' 
Write(123,*) '    "BVLL_2322": {' 
Write(123,*) '      "Re": ',Real(BVLL_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLL_2322) 
Write(123,*) '    },' 
Write(123,*) '    "BVLL_2333": {' 
Write(123,*) '      "Re": ',Real(BVLL_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLL_2333) 
Write(123,*) '    },' 
Write(123,*) '    "BVRR_2311": {' 
Write(123,*) '      "Re": ',Real(BVRR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVRR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "BVRR_2322": {' 
Write(123,*) '      "Re": ',Real(BVRR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVRR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "BVRR_2333": {' 
Write(123,*) '      "Re": ',Real(BVRR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVRR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "BVLR_2311": {' 
Write(123,*) '      "Re": ',Real(BVLR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "BVLR_2322": {' 
Write(123,*) '      "Re": ',Real(BVLR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "BVLR_2333": {' 
Write(123,*) '      "Re": ',Real(BVLR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(BVLR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_2311": {' 
Write(123,*) '      "Re": ',Real(BSRL_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_2311) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_2322": {' 
Write(123,*) '      "Re": ',Real(BSRL_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_2322) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_2333": {' 
Write(123,*) '      "Re": ',Real(BSRL_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_2333) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_3211": {' 
Write(123,*) '      "Re": ',Real(BSRL_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_3211) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_3222": {' 
Write(123,*) '      "Re": ',Real(BSRL_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_3222) 
Write(123,*) '    },' 
Write(123,*) '    "BSRL_3233": {' 
Write(123,*) '      "Re": ',Real(BSRL_3233,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRL_3233) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_2311": {' 
Write(123,*) '      "Re": ',Real(BSRR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_2322": {' 
Write(123,*) '      "Re": ',Real(BSRR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_2333": {' 
Write(123,*) '      "Re": ',Real(BSRR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_3211": {' 
Write(123,*) '      "Re": ',Real(BSRR_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_3211) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_3222": {' 
Write(123,*) '      "Re": ',Real(BSRR_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_3222) 
Write(123,*) '    },' 
Write(123,*) '    "BSRR_3233": {' 
Write(123,*) '      "Re": ',Real(BSRR_3233,dp), ',' 
Write(123,*) '      "Im": ',AImag(BSRR_3233) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_2311": {' 
Write(123,*) '      "Re": ',Real(BTRR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_2322": {' 
Write(123,*) '      "Re": ',Real(BTRR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_2333": {' 
Write(123,*) '      "Re": ',Real(BTRR_2333,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_2333) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_3211": {' 
Write(123,*) '      "Re": ',Real(BTRR_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_3211) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_3222": {' 
Write(123,*) '      "Re": ',Real(BTRR_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_3222) 
Write(123,*) '    },' 
Write(123,*) '    "BTRR_3233": {' 
Write(123,*) '      "Re": ',Real(BTRR_3233,dp), ',' 
Write(123,*) '      "Im": ',AImag(BTRR_3233) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_1123": {' 
Write(123,*) '      "Re": ',Real(EVLR_1123,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_1123) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_2223": {' 
Write(123,*) '      "Re": ',Real(EVLR_2223,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_2223) 
Write(123,*) '    },' 
Write(123,*) '    "EVLR_3323": {' 
Write(123,*) '      "Re": ',Real(EVLR_3323,dp), ',' 
Write(123,*) '      "Im": ',AImag(EVLR_3323) 
Write(123,*) '    },' 
Write(123,*) '    "CVLL_2311": {' 
Write(123,*) '      "Re": ',Real(CVLL_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLL_2311) 
Write(123,*) '    },' 
Write(123,*) '    "CVLL_2322": {' 
Write(123,*) '      "Re": ',Real(CVLL_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLL_2322) 
Write(123,*) '    },' 
Write(123,*) '    "CVRR_2311": {' 
Write(123,*) '      "Re": ',Real(CVRR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVRR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "CVRR_2322": {' 
Write(123,*) '      "Re": ',Real(CVRR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVRR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "CVLR_2311": {' 
Write(123,*) '      "Re": ',Real(CVLR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "CVLR_2322": {' 
Write(123,*) '      "Re": ',Real(CVLR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(CVLR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_2311": {' 
Write(123,*) '      "Re": ',Real(CSRL_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_2311) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_2322": {' 
Write(123,*) '      "Re": ',Real(CSRL_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_2322) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_3211": {' 
Write(123,*) '      "Re": ',Real(CSRL_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_3211) 
Write(123,*) '    },' 
Write(123,*) '    "CSRL_3222": {' 
Write(123,*) '      "Re": ',Real(CSRL_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRL_3222) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_2311": {' 
Write(123,*) '      "Re": ',Real(CSRR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_2322": {' 
Write(123,*) '      "Re": ',Real(CSRR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_3211": {' 
Write(123,*) '      "Re": ',Real(CSRR_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_3211) 
Write(123,*) '    },' 
Write(123,*) '    "CSRR_3222": {' 
Write(123,*) '      "Re": ',Real(CSRR_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(CSRR_3222) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_2311": {' 
Write(123,*) '      "Re": ',Real(CTRR_2311,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_2311) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_2322": {' 
Write(123,*) '      "Re": ',Real(CTRR_2322,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_2322) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_3211": {' 
Write(123,*) '      "Re": ',Real(CTRR_3211,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_3211) 
Write(123,*) '    },' 
Write(123,*) '    "CTRR_3222": {' 
Write(123,*) '      "Re": ',Real(CTRR_3222,dp), ',' 
Write(123,*) '      "Im": ',AImag(CTRR_3222) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_1313": {' 
Write(123,*) '      "Re": ',Real(AVLL_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_1313) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_1313": {' 
Write(123,*) '      "Re": ',Real(AVRR_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_1313) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_1313": {' 
Write(123,*) '      "Re": ',Real(AVLR_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_1313) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1313": {' 
Write(123,*) '      "Re": ',Real(ASRR_1313,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1313) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_3131": {' 
Write(123,*) '      "Re": ',Real(ASRR_3131,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_3131) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_2323": {' 
Write(123,*) '      "Re": ',Real(AVLL_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_2323) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_2323": {' 
Write(123,*) '      "Re": ',Real(AVRR_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_2323) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2323": {' 
Write(123,*) '      "Re": ',Real(AVLR_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2323) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2323": {' 
Write(123,*) '      "Re": ',Real(ASRR_2323,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2323) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_3232": {' 
Write(123,*) '      "Re": ',Real(ASRR_3232,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_3232) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_1212": {' 
Write(123,*) '      "Re": ',Real(AVLL_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_1212) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_1212": {' 
Write(123,*) '      "Re": ',Real(AVRR_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_1212) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_1212": {' 
Write(123,*) '      "Re": ',Real(AVLR_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_1212) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1212": {' 
Write(123,*) '      "Re": ',Real(ASRR_1212,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1212) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2121": {' 
Write(123,*) '      "Re": ',Real(ASRR_2121,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2121) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_1232": {' 
Write(123,*) '      "Re": ',Real(AVLL_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_1232) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_1232": {' 
Write(123,*) '      "Re": ',Real(AVRR_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_1232) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_1232": {' 
Write(123,*) '      "Re": ',Real(AVLR_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_1232) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2321": {' 
Write(123,*) '      "Re": ',Real(AVLR_2321,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2321) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1232": {' 
Write(123,*) '      "Re": ',Real(ASRR_1232,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1232) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2123": {' 
Write(123,*) '      "Re": ',Real(ASRR_2123,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2123) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_1213": {' 
Write(123,*) '      "Re": ',Real(AVLL_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_1213) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_1213": {' 
Write(123,*) '      "Re": ',Real(AVRR_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_1213) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_1213": {' 
Write(123,*) '      "Re": ',Real(AVLR_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_1213) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_1312": {' 
Write(123,*) '      "Re": ',Real(AVLR_1312,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_1312) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1213": {' 
Write(123,*) '      "Re": ',Real(ASRR_1213,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1213) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_2131": {' 
Write(123,*) '      "Re": ',Real(ASRR_2131,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_2131) 
Write(123,*) '    },' 
Write(123,*) '    "AVLL_1323": {' 
Write(123,*) '      "Re": ',Real(AVLL_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLL_1323) 
Write(123,*) '    },' 
Write(123,*) '    "AVRR_1323": {' 
Write(123,*) '      "Re": ',Real(AVRR_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVRR_1323) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_1323": {' 
Write(123,*) '      "Re": ',Real(AVLR_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_1323) 
Write(123,*) '    },' 
Write(123,*) '    "AVLR_2313": {' 
Write(123,*) '      "Re": ',Real(AVLR_2313,dp), ',' 
Write(123,*) '      "Im": ',AImag(AVLR_2313) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_1323": {' 
Write(123,*) '      "Re": ',Real(ASRR_1323,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_1323) 
Write(123,*) '    },' 
Write(123,*) '    "ASRR_3132": {' 
Write(123,*) '      "Re": ',Real(ASRR_3132,dp), ',' 
Write(123,*) '      "Im": ',AImag(ASRR_3132) 
Write(123,*) '    }' 
Write(123,*) "  }"
Write(123,*) "}"
    Close(123) 
End Subroutine WriteWCXF 
Subroutine Read_GAUGEIN(io,i_c,i_model,set_mod_par,kont) 
Implicit None 
Integer,Intent(in)::io,i_c,i_model 
Integer,Intent(inout)::kont,set_mod_par(:) 
Integer::i_par 
Real(dp)::wert 
Character(len=80)::read_line 
Do 
Read(io,*,End=200) read_line 
If (read_line(1:1).Eq."#") Cycle! this loop 
Backspace(io)! resetting to the beginning of the line 
If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b")) Exit! this loop 
Read(io,*) i_par,wert!,read_line 
If (i_par.Eq.1) Then 
g1IN= wert 
InputValueforg1= .True. 
Else If (i_par.Eq.2) Then 
g2IN= wert 
InputValueforg2= .True. 
Else If (i_par.Eq.3) Then 
g3IN= wert 
InputValueforg3= .True. 
Else
Write(ErrCan,*) "Error in routine "//NameOfUnit(Iname)
If (i_c.Eq.0) Write(ErrCan,*) "Unknown entry for Block GAUGEIN ",i_par
If (i_c.Eq.1) Write(ErrCan,*) "Unknown entry for Block IMGAUGEIN ",i_par
If (i_c.Eq.0) Write(*,*) "Unknown entry for Block GAUGEIN ",i_par
If (i_c.Eq.1) Write(*,*) "Unknown entry for Block IMGAUGEIN ",i_par
Call AddError(304)
If (ErrorLevel.Eq.2) Call TerminateProgram
End If
End Do! i_par
200 Return
End Subroutine Read_GAUGEIN 
 
 
Subroutine Read_HMIXIN(io,i_c,i_model,set_mod_par,kont) 
Implicit None 
Integer,Intent(in)::io,i_c,i_model 
Integer,Intent(inout)::kont,set_mod_par(:) 
Integer::i_par 
Real(dp)::wert 
Character(len=80)::read_line 
Do 
Read(io,*,End=200) read_line 
If (read_line(1:1).Eq."#") Cycle! this loop 
Backspace(io)! resetting to the beginning of the line 
If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b")) Exit! this loop 
Read(io,*) i_par,wert!,read_line 
If (i_par.Eq.31) Then 
If (i_c.Eq.0) Lam1IN= Cmplx(wert,Aimag(Lam1IN),dp) 
If (i_c.Eq.1) Lam1IN= Cmplx(Real(Lam1IN,dp),wert,dp) 
InputValueforLam1= .True. 
Else If (i_par.Eq.33) Then 
If (i_c.Eq.0) Lam3IN= Cmplx(wert,Aimag(Lam3IN),dp) 
If (i_c.Eq.1) Lam3IN= Cmplx(Real(Lam3IN,dp),wert,dp) 
InputValueforLam3= .True. 
Else If (i_par.Eq.34) Then 
If (i_c.Eq.0) Lam4IN= Cmplx(wert,Aimag(Lam4IN),dp) 
If (i_c.Eq.1) Lam4IN= Cmplx(Real(Lam4IN,dp),wert,dp) 
InputValueforLam4= .True. 
Else If (i_par.Eq.32) Then 
If (i_c.Eq.0) Lam2IN= Cmplx(wert,Aimag(Lam2IN),dp) 
If (i_c.Eq.1) Lam2IN= Cmplx(Real(Lam2IN,dp),wert,dp) 
InputValueforLam2= .True. 
Else If (i_par.Eq.35) Then 
If (i_c.Eq.0) Lam1DashIN= Cmplx(wert,Aimag(Lam1DashIN),dp) 
If (i_c.Eq.1) Lam1DashIN= Cmplx(Real(Lam1DashIN,dp),wert,dp) 
InputValueforLam1Dash= .True. 
Else If (i_par.Eq.36) Then 
If (i_c.Eq.0) Lam2DashIN= Cmplx(wert,Aimag(Lam2DashIN),dp) 
If (i_c.Eq.1) Lam2DashIN= Cmplx(Real(Lam2DashIN,dp),wert,dp) 
InputValueforLam2Dash= .True. 
Else If (i_par.Eq.37) Then 
If (i_c.Eq.0) Lam3DashIN= Cmplx(wert,Aimag(Lam3DashIN),dp) 
If (i_c.Eq.1) Lam3DashIN= Cmplx(Real(Lam3DashIN,dp),wert,dp) 
InputValueforLam3Dash= .True. 
Else If (i_par.Eq.42) Then 
If (i_c.Eq.0) Aa3IN= Cmplx(wert,Aimag(Aa3IN),dp) 
If (i_c.Eq.1) Aa3IN= Cmplx(Real(Aa3IN,dp),wert,dp) 
InputValueforAa3= .True. 
Else If (i_par.Eq.43) Then 
If (i_c.Eq.0) Aa4IN= Cmplx(wert,Aimag(Aa4IN),dp) 
If (i_c.Eq.1) Aa4IN= Cmplx(Real(Aa4IN,dp),wert,dp) 
InputValueforAa4= .True. 
Else If (i_par.Eq.62) Then 
If (i_c.Eq.0) Y1d11IN= Cmplx(wert,Aimag(Y1d11IN),dp) 
If (i_c.Eq.1) Y1d11IN= Cmplx(Real(Y1d11IN,dp),wert,dp) 
InputValueforY1d11= .True. 
Else If (i_par.Eq.63) Then 
If (i_c.Eq.0) Y1d12IN= Cmplx(wert,Aimag(Y1d12IN),dp) 
If (i_c.Eq.1) Y1d12IN= Cmplx(Real(Y1d12IN,dp),wert,dp) 
InputValueforY1d12= .True. 
Else If (i_par.Eq.64) Then 
If (i_c.Eq.0) Y1d13IN= Cmplx(wert,Aimag(Y1d13IN),dp) 
If (i_c.Eq.1) Y1d13IN= Cmplx(Real(Y1d13IN,dp),wert,dp) 
InputValueforY1d13= .True. 
Else If (i_par.Eq.65) Then 
If (i_c.Eq.0) Y1d21IN= Cmplx(wert,Aimag(Y1d21IN),dp) 
If (i_c.Eq.1) Y1d21IN= Cmplx(Real(Y1d21IN,dp),wert,dp) 
InputValueforY1d21= .True. 
Else If (i_par.Eq.66) Then 
If (i_c.Eq.0) Y1d22IN= Cmplx(wert,Aimag(Y1d22IN),dp) 
If (i_c.Eq.1) Y1d22IN= Cmplx(Real(Y1d22IN,dp),wert,dp) 
InputValueforY1d22= .True. 
Else If (i_par.Eq.67) Then 
If (i_c.Eq.0) Y1d23IN= Cmplx(wert,Aimag(Y1d23IN),dp) 
If (i_c.Eq.1) Y1d23IN= Cmplx(Real(Y1d23IN,dp),wert,dp) 
InputValueforY1d23= .True. 
Else If (i_par.Eq.77) Then 
If (i_c.Eq.0) Y2d31IN= Cmplx(wert,Aimag(Y2d31IN),dp) 
If (i_c.Eq.1) Y2d31IN= Cmplx(Real(Y2d31IN,dp),wert,dp) 
InputValueforY2d31= .True. 
Else If (i_par.Eq.78) Then 
If (i_c.Eq.0) Y2d32IN= Cmplx(wert,Aimag(Y2d32IN),dp) 
If (i_c.Eq.1) Y2d32IN= Cmplx(Real(Y2d32IN,dp),wert,dp) 
InputValueforY2d32= .True. 
Else If (i_par.Eq.79) Then 
If (i_c.Eq.0) Y2d33IN= Cmplx(wert,Aimag(Y2d33IN),dp) 
If (i_c.Eq.1) Y2d33IN= Cmplx(Real(Y2d33IN,dp),wert,dp) 
InputValueforY2d33= .True. 
Else If (i_par.Eq.44) Then 
If (i_c.Eq.0) Y1u11IN= Cmplx(wert,Aimag(Y1u11IN),dp) 
If (i_c.Eq.1) Y1u11IN= Cmplx(Real(Y1u11IN,dp),wert,dp) 
InputValueforY1u11= .True. 
Else If (i_par.Eq.45) Then 
If (i_c.Eq.0) Y1u12IN= Cmplx(wert,Aimag(Y1u12IN),dp) 
If (i_c.Eq.1) Y1u12IN= Cmplx(Real(Y1u12IN,dp),wert,dp) 
InputValueforY1u12= .True. 
Else If (i_par.Eq.47) Then 
If (i_c.Eq.0) Y1u21IN= Cmplx(wert,Aimag(Y1u21IN),dp) 
If (i_c.Eq.1) Y1u21IN= Cmplx(Real(Y1u21IN,dp),wert,dp) 
InputValueforY1u21= .True. 
Else If (i_par.Eq.48) Then 
If (i_c.Eq.0) Y1u22IN= Cmplx(wert,Aimag(Y1u22IN),dp) 
If (i_c.Eq.1) Y1u22IN= Cmplx(Real(Y1u22IN,dp),wert,dp) 
InputValueforY1u22= .True. 
Else If (i_par.Eq.61) Then 
If (i_c.Eq.0) Y2u33IN= Cmplx(wert,Aimag(Y2u33IN),dp) 
If (i_c.Eq.1) Y2u33IN= Cmplx(Real(Y2u33IN,dp),wert,dp) 
InputValueforY2u33= .True. 
Else If (i_par.Eq.80) Then 
If (i_c.Eq.0) Y1e11IN= Cmplx(wert,Aimag(Y1e11IN),dp) 
If (i_c.Eq.1) Y1e11IN= Cmplx(Real(Y1e11IN,dp),wert,dp) 
InputValueforY1e11= .True. 
Else If (i_par.Eq.81) Then 
If (i_c.Eq.0) Y1e12IN= Cmplx(wert,Aimag(Y1e12IN),dp) 
If (i_c.Eq.1) Y1e12IN= Cmplx(Real(Y1e12IN,dp),wert,dp) 
InputValueforY1e12= .True. 
Else If (i_par.Eq.83) Then 
If (i_c.Eq.0) Y1e21IN= Cmplx(wert,Aimag(Y1e21IN),dp) 
If (i_c.Eq.1) Y1e21IN= Cmplx(Real(Y1e21IN,dp),wert,dp) 
InputValueforY1e21= .True. 
Else If (i_par.Eq.84) Then 
If (i_c.Eq.0) Y1e22IN= Cmplx(wert,Aimag(Y1e22IN),dp) 
If (i_c.Eq.1) Y1e22IN= Cmplx(Real(Y1e22IN,dp),wert,dp) 
InputValueforY1e22= .True. 
Else If (i_par.Eq.97) Then 
If (i_c.Eq.0) Y2e33IN= Cmplx(wert,Aimag(Y2e33IN),dp) 
If (i_c.Eq.1) Y2e33IN= Cmplx(Real(Y2e33IN,dp),wert,dp) 
InputValueforY2e33= .True. 
Else If (i_par.Eq.98) Then 
If (i_c.Eq.0) Y1n11IN= Cmplx(wert,Aimag(Y1n11IN),dp) 
If (i_c.Eq.1) Y1n11IN= Cmplx(Real(Y1n11IN,dp),wert,dp) 
InputValueforY1n11= .True. 
Else If (i_par.Eq.99) Then 
If (i_c.Eq.0) Y1n12IN= Cmplx(wert,Aimag(Y1n12IN),dp) 
If (i_c.Eq.1) Y1n12IN= Cmplx(Real(Y1n12IN,dp),wert,dp) 
InputValueforY1n12= .True. 
Else If (i_par.Eq.101) Then 
If (i_c.Eq.0) Y1n21IN= Cmplx(wert,Aimag(Y1n21IN),dp) 
If (i_c.Eq.1) Y1n21IN= Cmplx(Real(Y1n21IN,dp),wert,dp) 
InputValueforY1n21= .True. 
Else If (i_par.Eq.102) Then 
If (i_c.Eq.0) Y1n22IN= Cmplx(wert,Aimag(Y1n22IN),dp) 
If (i_c.Eq.1) Y1n22IN= Cmplx(Real(Y1n22IN,dp),wert,dp) 
InputValueforY1n22= .True. 
Else If (i_par.Eq.115) Then 
If (i_c.Eq.0) Y2n33IN= Cmplx(wert,Aimag(Y2n33IN),dp) 
If (i_c.Eq.1) Y2n33IN= Cmplx(Real(Y2n33IN,dp),wert,dp) 
InputValueforY2n33= .True. 
Else If (i_par.Eq.136) Then 
If (i_c.Eq.0) C13IN= Cmplx(wert,Aimag(C13IN),dp) 
If (i_c.Eq.1) C13IN= Cmplx(Real(C13IN,dp),wert,dp) 
InputValueforC13= .True. 
Else If (i_par.Eq.139) Then 
If (i_c.Eq.0) C23IN= Cmplx(wert,Aimag(C23IN),dp) 
If (i_c.Eq.1) C23IN= Cmplx(Real(C23IN,dp),wert,dp) 
InputValueforC23= .True. 
Else If (i_par.Eq.140) Then 
If (i_c.Eq.0) C31IN= Cmplx(wert,Aimag(C31IN),dp) 
If (i_c.Eq.1) C31IN= Cmplx(Real(C31IN,dp),wert,dp) 
InputValueforC31= .True. 
Else If (i_par.Eq.141) Then 
If (i_c.Eq.0) C32IN= Cmplx(wert,Aimag(C32IN),dp) 
If (i_c.Eq.1) C32IN= Cmplx(Real(C32IN,dp),wert,dp) 
InputValueforC32= .True. 
Else If (i_par.Eq.125) Then 
If (i_c.Eq.0) BB11IN= Cmplx(wert,Aimag(BB11IN),dp) 
If (i_c.Eq.1) BB11IN= Cmplx(Real(BB11IN,dp),wert,dp) 
InputValueforBB11= .True. 
Else If (i_par.Eq.126) Then 
If (i_c.Eq.0) BB12IN= Cmplx(wert,Aimag(BB12IN),dp) 
If (i_c.Eq.1) BB12IN= Cmplx(Real(BB12IN,dp),wert,dp) 
InputValueforBB12= .True. 
Else If (i_par.Eq.128) Then 
If (i_c.Eq.0) BB21IN= Cmplx(wert,Aimag(BB21IN),dp) 
If (i_c.Eq.1) BB21IN= Cmplx(Real(BB21IN,dp),wert,dp) 
InputValueforBB21= .True. 
Else If (i_par.Eq.129) Then 
If (i_c.Eq.0) BB22IN= Cmplx(wert,Aimag(BB22IN),dp) 
If (i_c.Eq.1) BB22IN= Cmplx(Real(BB22IN,dp),wert,dp) 
InputValueforBB22= .True. 
Else If (i_par.Eq.40) Then 
If (i_c.Eq.0) Aa1IN= Cmplx(wert,Aimag(Aa1IN),dp) 
If (i_c.Eq.1) Aa1IN= Cmplx(Real(Aa1IN,dp),wert,dp) 
InputValueforAa1= .True. 
Else If (i_par.Eq.41) Then 
If (i_c.Eq.0) Aa2IN= Cmplx(wert,Aimag(Aa2IN),dp) 
If (i_c.Eq.1) Aa2IN= Cmplx(Real(Aa2IN,dp),wert,dp) 
InputValueforAa2= .True. 
Else If (i_par.Eq.20) Then 
Mu1IN= wert 
InputValueforMu1= .True. 
Else If (i_par.Eq.21) Then 
Mu2IN= wert 
InputValueforMu2= .True. 
Else If (i_par.Eq.22) Then 
MuDashIN= wert 
InputValueforMuDash= .True. 
Else If (i_par.Eq.39) Then 
If (i_c.Eq.0) MubIN= Cmplx(wert,Aimag(MubIN),dp) 
If (i_c.Eq.1) MubIN= Cmplx(Real(MubIN,dp),wert,dp) 
InputValueforMub= .True. 
Else If (i_par.Eq.38) Then 
If (i_c.Eq.0) Mu3IN= Cmplx(wert,Aimag(Mu3IN),dp) 
If (i_c.Eq.1) Mu3IN= Cmplx(Real(Mu3IN,dp),wert,dp) 
InputValueforMu3= .True. 
Else
Write(ErrCan,*) "Error in routine "//NameOfUnit(Iname)
If (i_c.Eq.0) Write(ErrCan,*) "Unknown entry for Block HMIXIN ",i_par
If (i_c.Eq.1) Write(ErrCan,*) "Unknown entry for Block IMHMIXIN ",i_par
If (i_c.Eq.0) Write(*,*) "Unknown entry for Block HMIXIN ",i_par
If (i_c.Eq.1) Write(*,*) "Unknown entry for Block IMHMIXIN ",i_par
Call AddError(304)
If (ErrorLevel.Eq.2) Call TerminateProgram
End If
End Do! i_par
200 Return
End Subroutine Read_HMIXIN 
 
 
Subroutine Read_VEVSIN(io,i_c,i_model,set_mod_par,kont) 
Implicit None 
Integer,Intent(in)::io,i_c,i_model 
Integer,Intent(inout)::kont,set_mod_par(:) 
Integer::i_par 
Real(dp)::wert 
Character(len=80)::read_line 
Do 
Read(io,*,End=200) read_line 
If (read_line(1:1).Eq."#") Cycle! this loop 
Backspace(io)! resetting to the beginning of the line 
If ((read_line(1:1).Eq."B").Or.(read_line(1:1).Eq."b")) Exit! this loop 
Read(io,*) i_par,wert!,read_line 
If (i_par.Eq.2) Then 
v1IN= wert 
Else If (i_par.Eq.3) Then 
v2IN= wert 
Else If (i_par.Eq.4) Then 
v3IN= wert 
Else If (i_par.Eq.2) Then 
v1IN= wert 
Else If (i_par.Eq.3) Then 
v2IN= wert 
Else If (i_par.Eq.4) Then 
v3IN= wert 
Else
Write(ErrCan,*) "Error in routine "//NameOfUnit(Iname)
If (i_c.Eq.0) Write(ErrCan,*) "Unknown entry for Block VEVSIN ",i_par
If (i_c.Eq.1) Write(ErrCan,*) "Unknown entry for Block IMVEVSIN ",i_par
If (i_c.Eq.0) Write(*,*) "Unknown entry for Block VEVSIN ",i_par
If (i_c.Eq.1) Write(*,*) "Unknown entry for Block IMVEVSIN ",i_par
Call AddError(304)
If (ErrorLevel.Eq.2) Call TerminateProgram
End If
End Do! i_par
200 Return
End Subroutine Read_VEVSIN 
 
 
Subroutine Switch_to_superCKM(Y_d, Y_u, Ad_in, Au_in, MD_in, MQ_in, MU_in &
                      &, Ad_out, Au_out, MD_out, MQ_out, MU_out, tr        &
                      &, RSd_in, RSu_in, RSd_out, RSu_out, CKM_out, Yd_out, Yu_out )
 !---------------------------------------------------------------------------
 ! shifts the parameter from the electroweak basis to the super CKM basis
 ! written by werner Porod, 12.03.08
 !---------------------------------------------------------------------------
 Implicit None
  Complex(dp), Intent(in), Dimension(3,3) :: Y_d, Y_u, Au_in, Ad_in, MD_in &
        & , MQ_in, MU_in
  Complex(dp), Optional, Intent(in), Dimension(6,6) :: RSu_in, RSd_in
  Logical, Intent(in) :: tr  ! if true, then the matrices are transposed 
                             ! compared to low energy definition
  Complex(dp), Intent(out), Dimension(3,3) :: Au_out, Ad_out, MD_out, MQ_out &
        & , MU_out, Yd_out, Yu_out
  Complex(dp), Optional, Intent(out), Dimension(6,6) :: RSu_out, RSd_out
  Complex(dp), Optional, Intent(out) :: CKM_out(3,3)

  Complex(dp), Dimension(3,3) :: uU_L, uU_R, uD_L, uD_R, CKM_Q
  Complex(dp) :: rot(6,6), Ephi

  Real(dp) :: mf(3), s12, s23, aR, aI, s13, c13
  Integer :: ierr

  !------------------------------------------
  ! diagonalizing d- and u-Yukawa couplings
  ! I am only interested in the mixing matrices
  !------------------------------------------

   Call FermionMass(Y_u, 1._dp, mf, uU_L, uU_R, ierr)
   Call FermionMass(Y_d, 1._dp, mf, uD_L, uD_R, ierr)
   Yu_out = MatMul(MatMul(conjg(uU_L),Y_u),Transpose(conjg(uU_R)))
   Yd_out = MatMul(MatMul(conjg(uD_L),Y_d),Transpose(conjg(uD_R)))

  !---------------------------------------------------------
  ! CKM matrix at Q, shifting phases according to PDG form
  !---------------------------------------------------------
  CKM_Q =  Matmul(uU_R, Transpose(Conjg(ud_R)) )
  uD_L(1,:) = uD_L(1,:) / Conjg(CKM_Q(1,1)) * Abs(CKM_Q(1,1))
  uD_L(2,:) = uD_L(2,:) / Conjg(CKM_Q(1,2)) * Abs(CKM_Q(1,2))
  uU_L(2,:) = uU_L(2,:) / CKM_Q(2,3) * Abs(CKM_Q(2,3))
  uU_L(3,:) = uU_L(3,:) / CKM_Q(3,3) * Abs(CKM_Q(3,3))
  !-------------------------------------------------------------------
  ! also the right quark must be multiplied with the conjugate phase
  ! as otherwise the masses get complex
  !-------------------------------------------------------------------
  uD_R(1,:) = uD_R(1,:) / CKM_Q(1,1) * Abs(CKM_Q(1,1))
  uD_R(2,:) = uD_R(2,:) / CKM_Q(1,2) * Abs(CKM_Q(1,2))
  uU_R(2,:) = uU_R(2,:) * Abs(CKM_Q(2,3)) / Conjg(CKM_Q(2,3))
  uU_R(3,:) = uU_R(3,:) * Abs(CKM_Q(3,3)) / Conjg(CKM_Q(3,3))
  CKM_Q =  Matmul(uU_L, Transpose(Conjg(ud_L)) )

  !--------------------------------------------------------------
  ! one more freedom left
  !--------------------------------------------------------------
  s13 = Abs(CKM_Q(1,3))
  c13 = sqrt(1._dp - s13**2)
  s23 = Abs(CKM_Q(2,3))/c13
  s12 = Abs(CKM_Q(1,2))/c13

  aR = Real(CKM_Q(2,2),dp) + s12 * s23 * Real(CKM_Q(1,3),dp)
  aI =  s12 * s23 * Aimag(CKM_Q(1,3)) - Aimag(CKM_Q(2,2))
  Ephi = Cmplx(aR/Sqrt(aR**2+aI**2),aI/Sqrt(aR**2+aI**2),dp)

  uU_L(2:3,:) = Ephi * uU_L(2:3,:)
  uD_L(3,:) = Ephi * uD_L(3,:)
  Ephi = Conjg(Ephi)
  uU_R(2:3,:) = Ephi * uU_R(2:3,:)
  uD_R(3,:) = Ephi * uD_R(3,:)


  CKM_Q =  Matmul(uU_R, Transpose(Conjg(ud_R)) )

  If (Present(CKM_out)) CKM_out = CKM_Q
  !-------------------------------------------------------------------
  ! shifting the parameters to the super CKM basis
  !-------------------------------------------------------------------

   Au_out = Matmul( Matmul(Conjg(uU_L), Au_in), Conjg(Transpose(uU_R)))

   Ad_out = Matmul( Matmul(Conjg(uD_L), Ad_in), Conjg(Transpose(uD_R)))


  MD_out = Matmul( Matmul( Conjg(uD_L), Transpose(MD_in)), Transpose(uD_L))
  MU_out = Matmul( Matmul( Conjg(uU_L), Transpose( MU_in)), Transpose(uU_L))
  MQ_out = Matmul( Matmul( uD_R, MQ_in), Transpose(Conjg(uD_R)) )

   If (Present(RSu_in).And.Present(RSu_out)) Then
    rot = 0._dp
    rot(1:3,1:3) = Conjg(uU_L)
    rot(4:6,4:6) = uU_R
    RSu_out = Matmul(RSu_in,Transpose(rot))
   End If
   If (Present(RSd_in).And.Present(RSd_out)) Then
    rot = 0._dp
    rot(1:3,1:3) = Conjg(uD_L)
    rot(4:6,4:6) = uD_R
    RSd_out = Matmul(RSd_in,Transpose(rot))
   End If

 End Subroutine Switch_to_superCKM




 Subroutine Switch_to_superPMNS(Y_l, uN_L, Al_in, ME_in, ML_in &
                      &, Al_out, ME_out, ML_out, tr            &
                      &, RSl_in, RSn_in, RSl_out, RSn_out, PMNS_out, Yl )
 !---------------------------------------------------------------------------
 ! shifts the parameter from the electroweak basis to the super PMNS basis
 ! written by werner Porod, 12.03.08
 !---------------------------------------------------------------------------
 Implicit None
  Complex(dp), Intent(in), Dimension(3,3) :: Y_l, uN_L, Al_in, ME_in, ML_in
  Complex(dp), Optional, Intent(in) :: RSl_in(6,6), RSn_in(3,3)
  Logical, Intent(in) :: tr  ! if true, then the matrices are transposed 
                             ! compared to low energy definition
  Complex(dp), Intent(out), Dimension(3,3) :: Al_out, ME_out, ML_out
  Complex(dp), Optional, Intent(out), Dimension(6,6) :: RSl_out(6,6)
  Complex(dp), Optional, Intent(out) :: PMNS_out(3,3), RSn_out(3,3)
  Complex(dp), Optional, Intent(out) :: Yl(3,3)

  Complex(dp), Dimension(3,3) :: uL_L, uL_R, PMNS_Q
  Complex(dp) :: rot(6,6)

  Real(dp) :: mf(3)
  Integer :: ierr

  !------------------------------------------
  ! diagonalizing d- and u-Yukawa couplings
  ! I am only interested in the mixing matrices
  !------------------------------------------



   If (tr) Then
   Call FermionMass(Transpose(Y_l), 1._dp, mf, uL_L, uL_R, ierr)
  Else
   Call FermionMass(Y_l, 1._dp, mf, uL_L, uL_R, ierr)
  End If

  If (Present(Yl)) Then 
    Yl = 0._dp
    Yl(1,1) = sqrt2 * mf(1)
    Yl(2,2) = sqrt2 * mf(2)
    Yl(3,3) = sqrt2 * mf(3)
  End if

  !---------------------------------------------------------
  ! PMNS matrix at Q, shifting phases according to PDG form
  !---------------------------------------------------------
  PMNS_Q =  Matmul(uL_L, uN_L)

  If (Present(PMNS_out)) PMNS_out = PMNS_Q
  !-------------------------------------------------------------------
  ! shifting the parameters to the super PMNS basis
  !-------------------------------------------------------------------
  If (tr) Then
   Al_out = Matmul( Matmul(uL_R, Al_in), Transpose(Conjg(uL_L)))

   ME_out = Matmul( Matmul( uL_R, ME_in), Transpose(Conjg(uL_R)))
   ML_out = Matmul( Matmul( Transpose(uL_L), ML_in), Conjg(uL_L) )

  Else
   Al_out = Matmul( Matmul(Conjg(uL_L), Al_in), Transpose(uL_R))

   ME_out = Matmul( Matmul( Conjg(uL_R), ME_in), Transpose(uL_R))
   ML_out = Matmul( Matmul( uL_L, ML_in), Transpose(Conjg(uL_L)) )

  End If
  !------------------------------------------------------------------
  ! to avoid numerical problems ensure that matrices are hermitian
  !-----------------------------------------------------------------
  ME_out = 0.5_dp * ( ME_out + Conjg(Transpose(ME_out)) )
  ML_out = 0.5_dp * ( ML_out + Conjg(Transpose(ML_out)) )

   If (Present(RSn_in).And.Present(RSn_out)) Then
    RSn_out = Matmul(RSn_in, Conjg(uN_L) )
   End If
   If (Present(RSl_in).And.Present(RSl_out)) Then
    rot = 0._dp
    rot(1:3,1:3) = Transpose(uL_L)
    rot(4:6,4:6) = Transpose(Conjg(uL_R))
    RSl_out = Matmul(RSl_in, rot)
   End If

 End Subroutine Switch_to_superPMNS

Subroutine SLHA1converter(MSd,MSd2, MSu,MSu2, MSe, MSe2, MSv, MSv2, &
   & ZD,ZU,ZE,ZV,Ztop,Zbottom,Ztau,  &
   & PDGd, PDGu, PDGe, PDGv, NamesD, NamesU, NamesE, NamesV)
Implicit None
Real(dp), Intent(inout) :: MSd(6), MSu(6), MSe(6), MSv(3), MSd2(6), MSu2(6), MSe2(6), MSv2(3)
Character(len=30),Dimension(6), Intent(inout) :: NamesD, NamesU, NamesE
Character(len=30),Dimension(3), Intent(inout) :: NamesV
Complex(dp), Intent(inout) :: PDGd(6), PDGu(6), PDGe(6), PDGv(3)
Complex(dp), Intent(in) :: ZU(6,6), ZD(6,6), ZE(6,6), ZV(3,3)
Complex(dp), Intent(out) :: Ztop(2,2), Zbottom(2,2), Ztau(2,2)
Real(dp) :: MSdt(6), MSut(6), MSet(6), MSvt(3)
Character(len=30),Dimension(6) :: NamesDt, NamesUt, NamesEt
Character(len=30),Dimension(3) :: NamesVt
Complex(dp) :: PDGdt(6), PDGut(6), PDGet(6), PDGvt(3)


Integer :: i1, i2, i3, ii, jj, i_zaehl


! Down-Squark

Call CheckMixingMatrix6(ZD, Zbottom,MSd,MSdt, PDGd, PDGdt, NamesD, NamesDt, 1)
!GammaD = GammaDt
!BrD = BrDt
PDGd = PDGdt
NamesD = NamesDt
! MSd = MSdt
! MSd2 = MSdt**2
   

! Up-Squark

Call CheckMixingMatrix6(ZU, Ztop,MSu,MSut, PDGu, PDGut, NamesU, NamesUt,2)
!GammaU = GammaUt
!BrU = BrUt
PDGu = PDGut
NamesU = NamesUt
! MSu = MSut
! MSu2 = MSut**2


! Selectron

Call CheckMixingMatrix6(ZE, Ztau,MSe,MSet, PDGe, PDGet, NamesE, NamesEt,3)
!GammaE = GammaEt
PDGe = PDGet
NamesE = NamesEt
!BrE = BrEt
! MSe = MSet
! MSe2 = MSet**2

! Sneutrino

Call CheckMixingMatrix3(ZV, MSv,MSvt, PDGv, PDGvt, NamesV, NamesVt)
!GammaV = GammaVt
PDGv = PDGvt
NamesV = NamesVt
!BrV = BrVt
! MSv = MSvt
! MSv2 = MSvt**2

Contains 

Subroutine CheckMixingMatrix6(Z,Z_out,m_in,m_out, PDG_in, PDG_out, Names_in, Names_out,particle)
Implicit None
Complex(dp), Intent(in) :: Z(6,6)
Complex(dp), Intent(out) :: Z_out(2,2)
Character(len=30),Dimension(6), Intent(in) :: Names_in
Character(len=30),Dimension(6), Intent(out) :: Names_out
Character(len=30) :: Names_temp, Names_save(6)
Complex(dp), Intent(in) :: PDG_in(6)
Complex(dp), Intent(out) :: PDG_out(6)
Complex(dp) :: PDG_temp, PDG_save(6)
Real(dp), Intent(in) :: m_in(6)
Real(dp) :: mat6R(6,6), mtemp, Maxcont, Z_temp(2)
Integer, Intent(in) ::particle
Real(dp), Intent(out) :: m_out(6)
Integer :: i1, ii, jj, PDG3, PDG6

      Select Case(particle)
       Case(1)  ! d-squark
          Names_save(1) = "SdL"
          Names_save(2) = "SsL"
          Names_save(3) = "Sb1" 
          Names_save(4) = "SdR"
          Names_save(5) = "SsR"
          Names_save(6) = "Sb2" 
          PDG_save(1) = 1000001
          PDG_save(2) = 1000003
          PDG_save(3) = 1000005
          PDG_save(4) = 2000001
          PDG_save(5) = 2000003
          PDG_save(6) = 2000005
       Case(2)  ! u-squark
          Names_save(1) = "SuL"
          Names_save(2) = "ScL"
          Names_save(3) = "St1" 
          Names_save(4) = "SuR"
          Names_save(5) = "ScR"
          Names_save(6) = "St2"
          PDG_save(1) = 1000002
          PDG_save(2) = 1000004
          PDG_save(3) = 1000006
          PDG_save(4) = 2000002
          PDG_save(5) = 2000004
          PDG_save(6) = 2000006 
       Case(3)  ! selectron
          Names_save(1) = "SeL"
          Names_save(2) = "SmuL"
          Names_save(3) = "Stau1" 
          Names_save(4) = "SeR"
          Names_save(5) = "SmuR"
          Names_save(6) = "Stau2"
          PDG_save(1) = 1000011
          PDG_save(2) = 1000013
          PDG_save(3) = 1000015
          PDG_save(4) = 2000011
          PDG_save(5) = 2000013
          PDG_save(6) = 2000015 
      End Select


     mat6R = Abs(Z)
     Do i1=1,6
      jj = Maxloc(mat6R(i1,:),1)
      m_out(jj) = m_in(i1)
      PDG_out(i1) = PDG_save(jj)
      Names_out(i1) = Names_save(jj)
      If (jj.eq.3) Then
         Z_out(1,1) = MaxVal(mat6R(i1,:))
         PDG3 = i1
      End if
      If (jj.eq.6) Then
          Z_out(2,2) = MaxVal(mat6R(i1,:)) 
          PDG6 = i1
      End if
!       mat6R(ii,:) = 0._dp
!       mat6R(:,jj) = 0._dp
     End Do
      Z_out(1,2) = sqrt(1._dp - Z_out(1,1)**2)
      Z_out(2,1) = -Z_out(1,2)
     If (M_out(3).gt.M_out(6)) Then

      Names_out(PDG3) = Names_save(6)
      Names_out(PDG6) = Names_save(3) 

      PDG_out(PDG3) = PDG_save(6)
      PDG_out(PDG6) = PDG_save(3) 

      Z_temp = Z_out(1,:)
      Z_out(1,:) = Z_out(2,:)
      Z_out(2,:) = Z_temp
     End if






End Subroutine CheckMixingMatrix6


Subroutine CheckMixingMatrix3(Z,m_in,m_out, PDG_in, PDG_out, Names_in, Names_out)
Implicit None
Complex(dp), Intent(in) :: Z(3,3)
Real(dp), Intent(in) :: m_in(3)
Character(len=30),Dimension(3), Intent(in) :: Names_in
Character(len=30),Dimension(3), Intent(out) :: Names_out
Character(len=30),Dimension(3) :: Names_save
Complex(dp), Intent(in) :: PDG_in(3)
Complex(dp), Intent(out) :: PDG_out(3)
Complex(dp) :: PDG_save(3)
Real(dp), Intent(out) :: m_out(3)
Real(dp) :: mat6R(3,3), mtemp, Maxcont
Integer :: i1, ii, jj

          PDG_save(1) = 1000012
          PDG_save(2) = 1000014
          PDG_save(3) = 1000016
          Names_save(1) = "Snu_e"
          Names_save(2) = "Snu_mu"
          Names_save(3) = "Snu_tau" 

     mat6R = Abs(Z)
     Do i1=1,3
      jj = Maxloc(mat6R(i1,:),1)
      m_out(jj) = m_in(i1)
      PDG_out(i1) = PDG_save(jj)
      Names_out(i1) = Names_save(jj)
!       mat6R(ii,:) = 0._dp
!       mat6R(:,jj) = 0._dp
     End Do

End Subroutine CheckMixingMatrix3


End Subroutine SLHA1converter
End Module InputOutput_BGLNCS 
 
