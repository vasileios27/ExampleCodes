(* ::Package:: *)

(* ::Input::Initialization:: *)
Off[General::spell]


(* ::Input::Initialization:: *)
Model`Name="BGLNCS";
Model`NameLaTeX="BGL with Type-I Seesaw and Complex Singlet";
Model`Authors="M.Nowak";
Model`Date="2021-02-10";


(* ::Input::Initialization:: *)
(*Gauge Symmetries*)
Gauge[[1]]={B,U[1], hypercharge,g1,False, 0};
Gauge[[2]]={WB,SU[2],left,g2, True, 0};
Gauge[[3]]={G, SU[3], color, g3, False, 0};


(* ::Input::Initialization:: *)
(*Global Symmetries*)
Global[[1]]={U[1], family};


(* ::Input::Initialization:: *)
(*Matter Fields*)
(*U(1)_F charge assignaments: x=1 y=1/3 *)

 (*Quarks, L*)
 FermionFields[[1]]={q1,1,{u1L,d1L},1/6,2,3,1};
 FermionFields[[2]]={q2,1,{u2L,d2L},1/6,2,3,1};
 FermionFields[[3]]={q3,1,{u3L,d3L},1/6,2,3,-19/3};

 (*Leptons, L*)
 FermionFields[[4]]={l1,1,{v1L,e1L},-1/2,2,1, 2/3};
 FermionFields[[5]]={l2,1,{v2L,e2L},-1/2,2,1, -20/3};
 FermionFields[[6]]={l3,1,{v3L,e3L},-1/2,2,1, 19};

 (*Down-type Quarks, R*)
 FermionFields[[7]]={d1,1,conj[d1R],1/3,1,-3, -5/3};
 FermionFields[[8]]={d2,1,conj[d2R],1/3,1,-3, -5/3};
 FermionFields[[9]]={d3,1,conj[d3R],1/3,1,-3, -5/3};

 (*Up-type Quarks, R*)
 FermionFields[[10]]={uu1,1,conj[u1R],-2/3,1,-3,-1/3};
 FermionFields[[11]]={uu2,1,conj[u2R],-2/3,1,-3,-1/3};
 FermionFields[[12]]={uu3,1,conj[u3R],-2/3,1,-3, 43/3};

 (*Charged Leptons, R*)
 FermionFields[[13]]={ee1,1,conj[e1R],1,1,1,-4/3};
 FermionFields[[14]]={ee2,1,conj[e2R],1,1,1,6};
 FermionFields[[15]]={ee3,1,conj[e3R],1,1,1,-27};

 (*Sterile Neutrinos*)
 FermionFields[[16]] = {v1R, 1, conj[v01R],   0, 1,  1, 0};
 FermionFields[[17]] = {v2R, 1, conj[v02R],   0, 1,  1, 22/3};
 FermionFields[[18]] = {v3R, 1, conj[v03R],   0, 1,  1, -11};

 (*Scalar Fields*)
 ScalarFields[[1]]={H1,1,{H1p,H10},1/2,2,1,-2/3};
 ScalarFields[[2]]={H2,1,{H2p,H20},1/2,2,1,-8};
 ScalarFields[[3]]={S,1,S0,0,1,1,-11/3};


(* ::Input::Initialization:: *)
(*--------------------------------------------------------------------------*)
NameOfStates={GaugeES, EWSB};

(* ----- Before EWSB ----- *)

DEFINITION[GaugeES][Additional]= {
{LagFerD, {AddHC -> True}},
  {LagFerU, {AddHC ->True}},
{LagFerE, {AddHC -> True}},
{LagFerN, {AddHC -> True}},
{LagMajorana, {AddHC -> True}},
{LagPureHiggsTerms, {AddHC ->False}},
  {LagCSTerms, {AddHC ->False}},
  {LagMixedTerms, {AddHC -> False}},
  {LagSymBrTerms, {AddHC -> True}}
};


(* ::Input::Initialization:: *)
(*Lagrangian pieces*)
LagFerD = -(Y1d11 conj[H1].q1.d1 +Y1d12 conj[H1].q1.d2+Y1d13 conj[H1].q1.d3+Y1d21 conj[H1].q2.d1 \
+ Y1d22 conj[H1].q2.d2+Y1d23 conj[H1].q2.d3)\
-(Y2d31 conj[H2].q3.d1 +Y2d32 conj[H2].q3.d2+Y2d33 conj[H2].q3.d3);


LagFerU =-(Y1u11 H1.q1.uu1 +Y1u12 H1.q1.uu2+Y1u21 H1.q2.uu1+Y1u22 H1.q2.uu2)-(Y2u33 H2.q3.uu3);


LagFerE =-(Y1e11 conj[H1].l1.ee1 + Y1e22 conj[H1].l2.ee2)\
 - ( Y1e21 conj[H2].l2.ee1 + Y2e33 conj[H2].l3.ee3);

LagFerN =(Y1n11 H1.l1.v1R  + Y1n22 H1.l2.v2R) + (Y2n33 H2.l3.v3R + Y1n12 H2.l1.v2R);

LagMajorana =
 - 1/2 ( BB11 v1R.v1R + C23 conj[S].v2R.v3R + C32 conj[S].v3R.v2R );


LagPureHiggsTerms =-(Mu1 conj[H1].H1+Mu2 conj[H2].H2+Lambda1 conj[H1].H1.conj[H1].H1+\
Lambda2 conj[H2].H2.conj[H2].H2+Lambda3 conj[H1].H1.conj[H2].H2+Lambda4 conj[H1].H2.conj[H2].H1);


LagCSTerms=-(MuDash conj[S].S+Lambda1Dash conj[S].S.conj[S].S);

LagMixedTerms=-(Lambda2Dash conj[H1].H1.conj[S].S+Lambda3Dash conj[H2].H2.conj[S].S);

LagSymBrTerms=-(Mu3 conj[H2].H1+1/2 Mub S.S+Aa1 conj[H1].H2.S+Aa2 conj[H1].H2.conj[S]+\
Aa3 conj[H1].H2.S.S+Aa4 conj[H1].H2.conj[S].conj[S]);







(* ::Input::Initialization:: *)
(* Gauge Sector *)

DEFINITION[EWSB][GaugeSector] =
{
  {{VB,VWB[3]},{VP,VZ},ZZ},
  {{VWB[1],VWB[2]},{VWm,conj[VWm]},ZW}
};

(* ----- VEVs ---- *)

DEFINITION[EWSB][VEVs]=
{    {H10, {v1, 1/Sqrt[2]}, {sigma1, I/Sqrt[2]},{phi1, 1/Sqrt[2]}},
	  {H20, {v2, 1/Sqrt[2]}, {sigma2, I/Sqrt[2]}, {phi2, 1/Sqrt[2]}},
	   {S0,  {v3, 1/Sqrt[2]}, {sigma3, I/Sqrt[2]}, {phi3, 1/Sqrt[2]}}
};


DEFINITION[EWSB][MatterSector]=
{
	{{phi1, phi2, phi3}, {hh, ZH}},
    {{sigma1, sigma2, sigma3}, {Ah, ZA}},
    {{conj[H1p],conj[H2p]},{Hm,ZP}},
    {{{d1L, d2L, d3L}, {conj[d1R],conj[d2R],conj[d3R]}}, {{DL,Vd}, {DR,Ud}}},
    {{{u1L, u2L, u3L}, {conj[u1R],conj[u2R],conj[u3R]}}, {{UL,Vu}, {UR,Uu}}},
    {{{e1L, e2L, e3L}, {conj[e1R],conj[e2R],conj[e3R]}}, {{EL,Ve}, {ER,Ue}}},
    {{v1L, v2L, v3L, conj[v01R], conj[v02R], conj[v03R]},{VL, Vv}}
};


(* ::Input::Initialization:: *)
(*------------------------------------------------------*)
(* Dirac-Spinors *)
(*------------------------------------------------------*)

DEFINITION[EWSB][DiracSpinors]=
{
	 Fd ->{  DL, conj[DR]},
     Fe ->{  EL, conj[ER]},
     Fu ->{  UL, conj[UR]},
     Fv ->{  VL, conj[VL]}
};
(*Fvs \[Rule]{  XO, conj[XO]}*)

DEFINITION[EWSB][GaugeES]={
FDLd1 -> { d1L, 0 },
       FDLd2 -> { d2L, 0 },
       FDLd3 -> { d3L, 0 },
       FDRd1 -> { 0, d1R },
       FDRd2 -> { 0, d2R },
       FDRd3 -> { 0, d3R },
       FDLu1 -> { u1L, 0 },
       FDLu2 -> { u2L, 0 },
       FDLu3 -> { u3L, 0 },
       FDRu1 -> { 0, u1R },
       FDRu2 -> { 0, u2R },
       FDRu3 -> { 0, u3R },
       FDLe1 -> { e1L, 0 },
       FDLe2 -> { e2L, 0 },
       FDLe3 -> { e3L, 0 },
       FDRe1 -> { 0, e1R },
       FDRe2 -> { 0, e2R },
       FDRe3 -> { 0, e3R },
       FDLv1  -> { v1L, 0 },
       FDLv2  -> { v2L, 0 },
       FDLv3  -> { v3L, 0 },
       FDRv1 -> { 0,v01R },
       FDRv2 -> { 0,v02R },
       FDRv3 -> { 0,v03R }
};



(* ::Input::Initialization:: *)
