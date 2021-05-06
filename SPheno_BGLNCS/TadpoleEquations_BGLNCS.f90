! -----------------------------------------------------------------------------  
! This file was automatically created by SARAH version 4.14.3 
! SARAH References: arXiv:0806.0538, 0909.2863, 1002.0840, 1207.0906, 1309.7223  
! (c) Florian Staub, 2013  
! ------------------------------------------------------------------------------  
! File created at 11:18 on 6.5.2021   
! ----------------------------------------------------------------------  
 
 
Module Tadpoles_BGLNCS 
 
Use Model_Data_BGLNCS 
Use TreeLevelMasses_BGLNCS 
Use RGEs_BGLNCS 
Use Control 
Use Settings 
Use Mathematics 

Contains 


Subroutine SolveTadpoleEquations(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,               & 
& Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,             & 
& Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,               & 
& Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,Tad1Loop)

Implicit None
Real(dp),Intent(inout) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp),Intent(inout) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mub,Mu3

Complex(dp), Intent(in) :: Tad1Loop(3)

! For numerical routines 
Real(dp) :: gC(85)
logical :: broycheck 
Real(dp) :: broyx(3)

If (HighScaleModel.Eq."LOW") Then 
Mu1 = -(4*Lam1*v1**3 + 2*Mu3*v2 + 2*Lam3*v1*v2**2 + 2*Lam4*v1*v2**2 + sqrt(2._dp)*Aa1*v2*v3 +& 
&  sqrt(2._dp)*Aa2*v2*v3 + 2*Lam2Dash*v1*v3**2 + Aa3*v2*v3**2 + Aa4*v2*v3**2 +           & 
&  sqrt(2._dp)*v2*v3*Conjg(Aa1) + sqrt(2._dp)*v2*v3*Conjg(Aa2) + v2*v3**2*Conjg(Aa3) +   & 
&  v2*v3**2*Conjg(Aa4) + 2*v2*Conjg(Mu3) - 4*Tad1Loop(1))/(4._dp*v1)
Mu2 = -(2*Mu3*v1 + 2*Lam3*v1**2*v2 + 2*Lam4*v1**2*v2 + 4*Lam2*v2**3 + sqrt(2._dp)*Aa1*v1*v3 +& 
&  sqrt(2._dp)*Aa2*v1*v3 + Aa3*v1*v3**2 + Aa4*v1*v3**2 + 2*Lam3Dash*v2*v3**2 +           & 
&  sqrt(2._dp)*v1*v3*Conjg(Aa1) + sqrt(2._dp)*v1*v3*Conjg(Aa2) + v1*v3**2*Conjg(Aa3) +   & 
&  v1*v3**2*Conjg(Aa4) + 2*v1*Conjg(Mu3) - 4*Tad1Loop(2))/(4._dp*v2)
MuDash = -(sqrt(2._dp)*Aa1*v1*v2 + sqrt(2._dp)*Aa2*v1*v2 + 2*Mub*v3 + 2*Lam2Dash*v1**2*v3 +    & 
&  2*Aa3*v1*v2*v3 + 2*Aa4*v1*v2*v3 + 2*Lam3Dash*v2**2*v3 + 4*Lam1Dash*v3**3 +            & 
&  sqrt(2._dp)*v1*v2*Conjg(Aa1) + sqrt(2._dp)*v1*v2*Conjg(Aa2) + 2*v1*v2*v3*Conjg(Aa3) + & 
&  2*v1*v2*v3*Conjg(Aa4) + 2*v3*Conjg(Mub) - 4*Tad1Loop(3))/(4._dp*v3)

 ! ----------- Check solutions for consistency  -------- 

 ! Check for NaNs 
If (Mu1.ne.Mu1) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for Mu1" 
   Call TerminateProgram  
 End If 
 If (Mu2.ne.Mu2) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for Mu2" 
   Call TerminateProgram  
 End If 
 If (MuDash.ne.MuDash) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for MuDash" 
   Call TerminateProgram  
 End If 
 Else 
Mu1 = -(4*Lam1*v1**3 + 2*Mu3*v2 + 2*Lam3*v1*v2**2 + 2*Lam4*v1*v2**2 + sqrt(2._dp)*Aa1*v2*v3 +& 
&  sqrt(2._dp)*Aa2*v2*v3 + 2*Lam2Dash*v1*v3**2 + Aa3*v2*v3**2 + Aa4*v2*v3**2 +           & 
&  sqrt(2._dp)*v2*v3*Conjg(Aa1) + sqrt(2._dp)*v2*v3*Conjg(Aa2) + v2*v3**2*Conjg(Aa3) +   & 
&  v2*v3**2*Conjg(Aa4) + 2*v2*Conjg(Mu3) - 4*Tad1Loop(1))/(4._dp*v1)
Mu2 = -(2*Mu3*v1 + 2*Lam3*v1**2*v2 + 2*Lam4*v1**2*v2 + 4*Lam2*v2**3 + sqrt(2._dp)*Aa1*v1*v3 +& 
&  sqrt(2._dp)*Aa2*v1*v3 + Aa3*v1*v3**2 + Aa4*v1*v3**2 + 2*Lam3Dash*v2*v3**2 +           & 
&  sqrt(2._dp)*v1*v3*Conjg(Aa1) + sqrt(2._dp)*v1*v3*Conjg(Aa2) + v1*v3**2*Conjg(Aa3) +   & 
&  v1*v3**2*Conjg(Aa4) + 2*v1*Conjg(Mu3) - 4*Tad1Loop(2))/(4._dp*v2)
MuDash = -(sqrt(2._dp)*Aa1*v1*v2 + sqrt(2._dp)*Aa2*v1*v2 + 2*Mub*v3 + 2*Lam2Dash*v1**2*v3 +    & 
&  2*Aa3*v1*v2*v3 + 2*Aa4*v1*v2*v3 + 2*Lam3Dash*v2**2*v3 + 4*Lam1Dash*v3**3 +            & 
&  sqrt(2._dp)*v1*v2*Conjg(Aa1) + sqrt(2._dp)*v1*v2*Conjg(Aa2) + 2*v1*v2*v3*Conjg(Aa3) + & 
&  2*v1*v2*v3*Conjg(Aa4) + 2*v3*Conjg(Mub) - 4*Tad1Loop(3))/(4._dp*v3)

 ! ----------- Check solutions for consistency  -------- 

 ! Check for NaNs 
If (Mu1.ne.Mu1) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for Mu1" 
   Call TerminateProgram  
 End If 
 If (Mu2.ne.Mu2) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for Mu2" 
   Call TerminateProgram  
 End If 
 If (MuDash.ne.MuDash) Then 
   Write(*,*) "NaN appearing in solution of tadpole equations for MuDash" 
   Call TerminateProgram  
 End If 
 End if 
End Subroutine SolveTadpoleEquations

Subroutine CalculateTadpoles(g1,g2,g3,Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,          & 
& Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,          & 
& Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,               & 
& C23,C32,BB11,Aa1,Aa2,Mu1,Mu2,MuDash,Mub,Mu3,v1,v2,v3,Tad1Loop,TadpoleValues)

Real(dp),Intent(in) :: g1,g2,g3,Mu1,Mu2,MuDash,v1,v2,v3

Complex(dp),Intent(in) :: Lam1,Lam3,Lam4,Lam2,Lam1Dash,Lam2Dash,Lam3Dash,Aa3,Aa4,Y1d11,Y1d12,Y1d13,             & 
& Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,               & 
& Y1e22,Y1e21,Y2e33,Y1n11,Y1n22,Y1n12,Y2n33,C23,C32,BB11,Aa1,Aa2,Mub,Mu3

Complex(dp), Intent(in) :: Tad1Loop(3)

Real(dp), Intent(out) :: TadpoleValues(3)

TadpoleValues(1) = Real((4*Lam1*v1**3 + 2*v1*(2._dp*(Mu1) + (Lam3 + Lam4)             & 
& *v2**2 + Lam2Dash*v3**2) + v2*(2._dp*(Mu3) + sqrt(2._dp)*v3*(Aa1 + Aa2 + Conjg(Aa1)    & 
&  + Conjg(Aa2)) + v3**2*(Aa3 + Aa4 + Conjg(Aa3) + Conjg(Aa4)) + 2*Conjg(Mu3)))          & 
& /4._dp - Tad1Loop(1),dp) 
TadpoleValues(2) = Real((2*(Lam3 + Lam4)*v1**2*v2 + 2*v2*(2._dp*(Mu2) + 2*Lam2*v2**2 + Lam3Dash*v3**2)& 
&  + v1*(2._dp*(Mu3) + sqrt(2._dp)*v3*(Aa1 + Aa2 + Conjg(Aa1) + Conjg(Aa2))              & 
&  + v3**2*(Aa3 + Aa4 + Conjg(Aa3) + Conjg(Aa4)) + 2*Conjg(Mu3)))/4._dp - Tad1Loop(2),dp) 
TadpoleValues(3) = Real((2*Lam2Dash*v1**2*v3 + v1*v2*(sqrt(2._dp)*Aa1 + sqrt(2._dp)   & 
& *Aa2 + 2*Aa3*v3 + 2*Aa4*v3 + sqrt(2._dp)*Conjg(Aa1) + sqrt(2._dp)*Conjg(Aa2)           & 
&  + 2*v3*Conjg(Aa3) + 2*v3*Conjg(Aa4)) + 2*v3*(Mub + 2._dp*(MuDash) + Lam3Dash*v2**2 + 2*Lam1Dash*v3**2 + Conjg(Mub)))& 
& /4._dp - Tad1Loop(3),dp) 
End Subroutine CalculateTadpoles 

End Module Tadpoles_BGLNCS 
 
