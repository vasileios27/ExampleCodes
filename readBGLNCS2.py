#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os
import subprocess
import sys
import glob
import numpy as np
import flavio
from wilson import Wilson
import csv


# In[2]:


cluster = False # If you run the scrip on the cluster
from_batch_nu = 1
to_batch_nu = 2
run_analysis = True


# In[3]:


if not cluster:

    #HiggsBound folder
    HiggsBounds_Path = '~/higgsbounds/build'
    HiggsBounds = os.path.join(HiggsBounds_Path,'HiggsBounds')
    #HiggsSignal folder
    HiggsSignals_Path = '~/higgssignals/build'
    HiggsSignals = os.path.join(HiggsSignals_Path,'HiggsSignals')

if cluster:

    #HiggsBound folder
    HiggsBounds_Path = '~/opt/higgsbounds/build'
    HiggsBounds = os.path.join(HiggsBounds_Path,'HiggsBounds')
    #HiggsSignal folder
    HiggsSignals_Path = '~/opt/higgssignals/build'
    HiggsSignals = os.path.join(HiggsSignals_Path,'HiggsSignals')


# In[4]:


def SPheno_reader(i,j,SPheno_spc_File):
    LSTU,BRhh1,LMass,Lcouplings,Imcouplings,Lvev= [],[],[],[],[],[]
    Pass1,Pass2,Pass3,Pass4,Pass5,Pass6 = False,False,False,False,False,False
    data_spheno ='batch_{}/Result_data/Spheno_output_Folder/point_{}/{}'.format(i,j,SPheno_spc_File)
    R = open(data_spheno, 'r')
    for line in R:
        if line.startswith('Block MINPAR'):Pass4 = True
        if line.startswith('Block IMMINPAR'):Pass4 = False
        if line.startswith('Block IMMINPAR'):Pass5 = True
        if line.startswith('Block gaugeGUT'):Pass5 =False
        if line.startswith('Block MASS'):Pass3 = True
        if line.startswith('Block ZH_SCALARMIXs'):Pass3=False
        if line.startswith('Block SPhenoLowEnergy'):Pass1 = True
        if line.startswith('Block VEVS'): Pass6 = True
        if line.startswith('Block MASS  # Mass spectrum'):Pass6 = False
        if line.startswith('Block FlavorKitQFV # quark flavor violating observables'):Pass1=False
        if line.startswith('DECAY        25'): Pass2 = True
        if line.startswith('DECAY        26'): Pass2 = False
        if Pass1:
            LSTU.append(line.strip('\n'))
        if Pass2 :
            BRhh1.append(line.strip('\n'))
        if Pass3:
            LMass.append(line.strip('\n'))
        if Pass4:
            Lcouplings.append(line.strip('\n'))
        if Pass5:
            Imcouplings.append(line.strip('\n'))
        if Pass6:
            Lvev.append(line.strip('\n'))
    Lambda1,Lambda2,Lambda3,Lambda4 =float(Lcouplings[1].split()[1]),float(Lcouplings[2].split()[1]),float(Lcouplings[3].split()[1]),float(Lcouplings[4].split()[1])
    Lambda1Dash,Lambda2Dash,Lambda3Dash =float(Lcouplings[5].split()[1]),float(Lcouplings[6].split()[1]),float(Lcouplings[7].split()[1])
    Mu3,Mub,alpha1,alpha2,alpha3,alpha4 =float(Lcouplings[8].split()[1]),float(Lcouplings[9].split()[1]),float(Lcouplings[10].split()[1]),float(Lcouplings[11].split()[1]),float(Lcouplings[12].split()[1]),float(Lcouplings[13].split()[1])
    Y1d11,Y1d12,Y1d13 =float(Lcouplings[15].split()[1]),float(Lcouplings[16].split()[1]),float(Lcouplings[17].split()[1])
    Y1d21,Y1d22,Y1d23 =float(Lcouplings[18].split()[1]),float(Lcouplings[19].split()[1]),float(Lcouplings[20].split()[1])
    Y2d31,Y2d32,Y2d33 =float(Lcouplings[21].split()[1]),float(Lcouplings[22].split()[1]),float(Lcouplings[23].split()[1])
    Y1u11,Y1u12,Y1u21,Y1u22,Y2u33 =float(Lcouplings[24].split()[1]),float(Lcouplings[25].split()[1]),float(Lcouplings[26].split()[1]),float(Lcouplings[27].split()[1]),float(Lcouplings[28].split()[1])
    Y1e11,Y1e12,Y1e22,Y1e33 =float(Lcouplings[29].split()[1]),float(Lcouplings[30].split()[1]),float(Lcouplings[32].split()[1]),float(Lcouplings[33].split()[1])
    Y1n11,Y1n12,Y1n22,Y1n33 =float(Lcouplings[34].split()[1]),float(Lcouplings[35].split()[1]),float(Lcouplings[37].split()[1]),float(Lcouplings[38].split()[1])
    B11,B12,B21,B22,C13,C23,C31,C32 = float(Lcouplings[39].split()[1]),float(Lcouplings[40].split()[1]),float(Lcouplings[41].split()[1]),float(Lcouplings[42].split()[1]),float(Lcouplings[43].split()[1]),float(Lcouplings[44].split()[1]),float(Lcouplings[45].split()[1]),float(Lcouplings[46].split()[1])

    imY1d11,imY1d12,imY1d13 =float(Imcouplings[1].split()[1]),float(Imcouplings[2].split()[1]),float(Imcouplings[3].split()[1])
    imY1d21,imY1d22,imY1d23  =float(Imcouplings[4].split()[1]),float(Imcouplings[5].split()[1]),float(Imcouplings[6].split()[1])
    imY2d31,imY2d32,imY2d33  =float(Imcouplings[7].split()[1]),float(Imcouplings[8].split()[1]),float(Imcouplings[9].split()[1])
    imY1u11,imY1u12,imY1u21,imY1u22=float(Imcouplings[10].split()[1]),float(Imcouplings[11].split()[1]),float(Imcouplings[12].split()[1]),float(Imcouplings[13].split()[1])
    imY1e11,imY1e12,imY1e21,imY1e22   =float(Imcouplings[14].split()[1]),float(Imcouplings[15].split()[1]),float(Imcouplings[16].split()[1]),float(Imcouplings[17].split()[1])

    V1,V2,VS = float(Lvev[1].split()[1]),float(Lvev[2].split()[1]),float(Lvev[3].split()[1])

    if float(LMass[2].split()[0])==25:Mhh1 =float(LMass[2].split()[1])
    if float(LMass[3].split()[0])==26:Mhh2 =float(LMass[3].split()[1])
    if float(LMass[4].split()[0])==27:Mhh3 =float(LMass[4].split()[1])
    if float(LMass[5].split()[0])==35:MAh2 =float(LMass[5].split()[1])
    if float(LMass[6].split()[0])==36:MAh3 =float(LMass[6].split()[1])
    if float(LMass[7].split()[0])==37:MHm2 =float(LMass[7].split()[1])
    if float(LMass[19].split()[0])==12: MFv1 =float(LMass[19].split()[1])
    if float(LMass[20].split()[0])==14:MFv2 =float(LMass[20].split()[1])
    if float(LMass[21].split()[0])==16:MFv3 =float(LMass[21].split()[1])
    if float(LMass[22].split()[0])==8810012:MFv4 =float(LMass[22].split()[1])
    if float(LMass[23].split()[0])==8810014:MFv5 =float(LMass[23].split()[1])
    if float(LMass[24].split()[0])==8810016:MFv6 =float(LMass[24].split()[1])
    if float(LSTU[1].split()[0] )==1  :T_parameter = float(LSTU[1].split()[1] )
    if float(LSTU[2].split()[0] )==2  :S_parameter = float(LSTU[2].split()[1] )
    if float(LSTU[3].split()[0] )==3  :U_parameter = float(LSTU[3].split()[1] )
    if float(BRhh1[6].split()[2])==35 and float(BRhh1[6].split()[3])==35: BRhh2toAh2Ah2 =  float(BRhh1[6].split()[0])

    SphenoDataList = [Lambda1,Lambda2,Lambda3,Lambda4,Lambda1Dash,Lambda2Dash,Lambda3Dash,Mu3,Mub,alpha1,alpha2,alpha3,alpha4,\
    Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33,Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1e11,Y1e12,Y1e22,Y1e33,\
    Y1n11,Y1n12,Y1n22,Y1n33,B11,B12,B21,B22,C13,C23,C31,C32,\
    imY1d11,imY1d12,imY1d13,imY1d21,imY1d22,imY1d23,imY2d31,imY2d32,imY2d33,imY1u11,imY1u12,imY1u21,imY1u22,imY1e11,imY1e12,imY1e21,imY1e22,\
    V1,V2,VS,Mhh1,Mhh2,Mhh3,MAh2,MAh3,MHm2,MFv1,MFv2,MFv3,MFv4,MFv5,MFv6,MHm2,\
    T_parameter,S_parameter,U_parameter,BRhh2toAh2Ah2]

    return SphenoDataList


# In[5]:


def EW_precision_function(T_parameter,S_parameter,U_parameter):

    EW_T,EW_S,EW_U = float(T_parameter),float(S_parameter),float(U_parameter)

    InvSig = [[1720.26, -1288.92, 1027.36],[-1288.92,1142.91, -639.825],[1027.36, -639.825, 832.293]]
    Obsref = [0.07, 0.02, 0.00]
    delta_chi = "%10.8F" % ((EW_T-Obsref[0])*InvSig[0][0]*(EW_T-Obsref[0]) +
                              (EW_S-Obsref[1])*InvSig[1][0]*(EW_T-Obsref[0]) +
                              (EW_U-Obsref[2])*InvSig[2][0]*(EW_T-Obsref[0]) +
                              (EW_T-Obsref[0])*InvSig[0][1]*(EW_S-Obsref[1]) +
                              (EW_S-Obsref[1])*InvSig[1][1]*(EW_S-Obsref[1]) +
                              (EW_U-Obsref[2])*InvSig[2][1]*(EW_S-Obsref[1]) +
                              (EW_T-Obsref[0])*InvSig[0][2]*(EW_U-Obsref[2]) +
                              (EW_S-Obsref[1])*InvSig[1][2]*(EW_U-Obsref[2]) +
                              (EW_U-Obsref[2])*InvSig[2][2]*(EW_U-Obsref[2]))
    if float(delta_chi) < 7.815:
        EW_precision = True
    if float(delta_chi) > 7.815:
        EW_precision = False
    return EW_precision,delta_chi


# In[6]:


def HiggsBounds_reader_limit_function(i,j):
    data_higgsbounds = 'batch_{}/Result_data/Spheno_output_Folder/point_{}/HiggsBounds_results.dat'.format(i,j)
    R = open( data_higgsbounds, 'r')
 #   HBresult   : scenario allowed flag (1: allowed, 0: excluded, -1: unphysical)
 #   chan       : most sensitive channel (see below). chan=0 if no channel applies
 #   obsratio   : ratio [sig x BR]_model/[sig x BR]_limit (<1: allowed, >1: excluded)
 #   ncomb      : number of Higgs bosons combined in most sensitive channel
    for line in R:
        if not line.lstrip().startswith('#'):
            HBresult,chanel,obsratio,ncomb = float(line.split()[-4]),float(line.split()[-3]),float(line.split()[-2]),float(line.split()[-1])
            if float(line.split()[-2]) < 1 and HBresult == 1:
                HiggsBounds_pass = True
            else :  HiggsBounds_pass = False
    return HiggsBounds_pass,HBresult,chanel,obsratio, ncomb


# In[7]:


#There is a problem all the Pvalues are zero. But that might be a problem of SPheno
def HiggsSignals_reader_limit_function(i,j):
    data_higgssignal = 'batch_{}/Result_data/Spheno_output_Folder/point_{}/HiggsSignals_results.dat'.format(i,j)
    R = open( data_higgssignal, 'r')
 #   csq(mu)    : Chi^2 from the signal strengths observables
 #   csq(mh)    : Chi^2 from the Higgs mass observables
 #   csq(tot)   : total Chi^2
 #   nobs(mu)   : number of signal strength observables
 #   nobs(mh)   : number of Higgs mass observables
 #   nobs(tot)  : total number of observables
#   Pvalue     : Probability, given csq(tot) and ndf=nobs(tot)-  0
    for line in R:
        if not line.lstrip().startswith('#'):
            Pvalue,nobs_tot,nobs_h = float(line.split()[-1]),float(line.split()[-2]),float(line.split()[-3])
            nobs_mu,csq_tot,csq_mh,csq_mu = float(line.split()[-4]),float(line.split()[-5]),float(line.split()[-6]),float(line.split()[-7])
            if 80 > csq_tot and csq_tot < 100:
                HiggsSignal_pass = True
            else :  HiggsSignal_pass = False
    #return HiggsBounds_pass
    return HiggsSignal_pass,Pvalue,nobs_tot,nobs_h,nobs_mu,csq_tot,csq_mh,csq_mu


# In[8]:


def FlavioFun(i,j):
    with open('batch_{}/Result_data/Spheno_output_Folder/point_{}/WC.BGLNCS_1.json'.format(i,j), 'r') as f:
        myw = Wilson.load_wc(f)
    BRBXsgammaNP,BRBXsgammaSM = flavio.np_prediction('BR(B->Xsgamma)', myw), flavio.sm_prediction('BR(B->Xsgamma)')
    RatioBXsgamma = BRBXsgammaNP/BRBXsgammaSM

    BRB0eeNP,BRB0eeSM = flavio.np_prediction('BR(B0->ee)', myw),flavio.sm_prediction('BR(B0->ee)')
    RatioB0ee = BRB0eeNP / BRB0eeSM

    BRBseeNP,BRBseeSM  = flavio.np_prediction('BR(Bs->ee)', myw), flavio.sm_prediction('BR(Bs->ee)')
    RatioBsee = BRBseeNP / BRBseeSM

    BRB0mumuNP,BRB0mumuSM = flavio.np_prediction('BR(B0->mumu)', myw),flavio.sm_prediction('BR(B0->mumu)')
    RatioB0mumu = BRB0mumuNP / BRB0mumuSM

    BRBsmumuNP,BRBsmumuSM = flavio.np_prediction('BR(Bs->mumu)', myw),flavio.sm_prediction('BR(Bs->mumu)')
    RatioBsmumu = BRBsmumuNP / BRBsmumuSM

    BRB0tautauNP,BRB0tautauSM = flavio.np_prediction('BR(B0->tautau)', myw),flavio.sm_prediction('BR(B0->tautau)')
    RatioB0tautau = BRB0tautauNP / BRB0tautauSM

    BRBstautauNP,BRBstautauSM = flavio.np_prediction('BR(Bs->tautau)', myw),flavio.sm_prediction('BR(Bs->tautau)')
    RatioBstautau = BRBstautauNP / BRBstautauSM

    DeltaMdNP,DeltaMdSM = flavio.np_prediction('DeltaM_d', myw),flavio.sm_prediction('DeltaM_d')
    RatioDeltaMd = DeltaMdNP / DeltaMdSM

    DeltaMsNP,DeltaMsSM = flavio.np_prediction('DeltaM_s', myw),flavio.sm_prediction('DeltaM_s')
    RatioDeltaMs = DeltaMsNP / DeltaMsSM

    RBpmueNP,RBpmueSM = flavio.np_prediction('Rmue(B+->K*ll)', myw, q2=3),flavio.sm_prediction('Rmue(B+->K*ll)', q2=3)
    RatioRBpmue = RBpmueNP / RBpmueSM

    RB0mueNP,RB0mueSM = flavio.np_prediction('Rmue(B0->K*ll)', myw, q2=3),flavio.sm_prediction('Rmue(B0->K*ll)', q2=3)
    RatioRB0mue = RB0mueNP / RB0mueSM

    eps_KNP,eps_KSM = flavio.np_prediction('eps_K', myw),flavio.sm_prediction('eps_K')
    Ratioeps_K = eps_KNP / eps_KSM

    BRBpKnunuNP,BRBpKnunuSM = flavio.np_prediction('BR(B+->Knunu)', myw),flavio.sm_prediction('BR(B+->Knunu)')
    RatioBpKnunu = BRBpKnunuNP / BRBpKnunuSM

    BRBppinunuNP,BRBppinunuSM = flavio.np_prediction('BR(B+->pinunu)', myw),flavio.sm_prediction('BR(B+->pinunu)')
    RatioBppinunu = BRBppinunuNP / BRBppinunuSM

    BRB0pinunuNP,BRB0pinunuSM = flavio.np_prediction('BR(B0->pinunu)', myw),flavio.sm_prediction('BR(B0->pinunu)')
    RatioB0pinunu = BRB0pinunuNP / BRB0pinunuSM

    BRB0KnunuNP,BRB0KnunuSM = flavio.np_prediction('BR(B0->Knunu)', myw),flavio.sm_prediction('BR(B0->Knunu)')
    RatioB0Knunu = BRB0KnunuNP / BRB0KnunuSM

    BRBXsllNP,BRBXsllSM = flavio.np_prediction('<BR>(B->Xsll)', myw, q2min=2.0 , q2max=4.3),flavio.sm_prediction('<BR>(B->Xsll)', q2min=2.0 , q2max=4.3)
    RatioBRBXsll = BRBXsllNP / BRBXsllSM

    BRBpmunuNP,BRBpmunuSM  = flavio.np_prediction('BR(B+->munu)', myw),flavio.sm_prediction('BR(B+->munu)')
    RatioBpmunu = BRBpmunuNP / BRBpmunuSM

    BRBptaunuNP,BRBptaunuSM = flavio.np_prediction('BR(B+->taunu)', myw),flavio.sm_prediction('BR(B+->taunu)')
    RatioBptaunu = BRBptaunuNP / BRBptaunuSM

    BRBpDlnuNP,BRBpDlnuSM = flavio.np_prediction('BR(B+->Dlnu)', myw),flavio.sm_prediction('BR(B+->Dlnu)')
    RatioBpDlnu = BRBpDlnuNP / BRBpDlnuSM

    BRDpmunuNP,BRDpmunuSM = flavio.np_prediction('BR(D+->munu)', myw),flavio.sm_prediction('BR(D+->munu)')
    RatioDpmunu = BRDpmunuNP / BRDpmunuSM

    BRDptaunuNP,BRDptaunuSM = flavio.np_prediction('BR(D+->taunu)', myw),flavio.sm_prediction('BR(D+->taunu)')
    RatioDptaunu = BRDptaunuNP / BRDptaunuSM

    BRKLmumuNP,BRKLmumuSM = flavio.np_prediction('BR(KL->mumu)', myw),flavio.sm_prediction('BR(KL->mumu)')
    RatioKLmumu = BRKLmumuNP / BRKLmumuSM

    BRKLeeNP,BRKLeeSM = flavio.np_prediction('BR(KL->ee)', myw),flavio.sm_prediction('BR(KL->ee)')
    RatioKLee = BRKLeeNP / BRKLeeSM

    BRKppinunuNP,BRKppinunuSM = flavio.np_prediction('BR(K+->pinunu)', myw), flavio.sm_prediction('BR(K+->pinunu)')
    RatioKppinunu = BRKppinunuNP / BRKppinunuSM

    BRKLpinunuNP,BRKLpinunuSM = flavio.np_prediction('BR(KL->pinunu)', myw),flavio.sm_prediction('BR(KL->pinunu)')
    RatioKLpinunu = BRKLpinunuNP / BRKLpinunuSM

    epsp_over_epsNP,epsp_over_epsSM = flavio.np_prediction('epsp/eps', myw),flavio.sm_prediction('epsp/eps')
    Ratioepsp_over_eps = epsp_over_epsNP / epsp_over_epsSM

    RmueB0KllNP,RmueB0KllSM = flavio.np_prediction('Rmue(B0->Kll)', myw, q2=3),flavio.sm_prediction('Rmue(B0->Kll)', q2=3)
    RatioRmueB0Kll = RmueB0KllNP / RmueB0KllSM

    RmueBpKllNP,RmueBpKllSM = flavio.np_prediction('Rmue(B+->Kll)', myw, q2=3),flavio.sm_prediction('Rmue(B+->Kll)', q2=3)
    RatioRmueBpKll = RmueBpKllNP / RmueBpKllSM

    RatioList = [RatioBXsgamma,RatioB0ee,RatioBsee,RatioB0mumu,RatioBsmumu,RatioB0tautau,                RatioBstautau,RatioDeltaMd,RatioDeltaMs,RatioRBpmue,RatioRB0mue,                Ratioeps_K,RatioBpKnunu,RatioBppinunu,RatioB0pinunu,RatioB0Knunu,                RatioBRBXsll,RatioBpmunu,RatioBptaunu,RatioBpDlnu,RatioDpmunu,RatioDptaunu,                RatioKLmumu,RatioKLee,RatioKppinunu,RatioKLpinunu,Ratioepsp_over_eps,RatioRmueB0Kll,                RatioRmueBpKll]
    return RatioList


# In[9]:


def WdataSPheno(name,SphenoDataList):
    with open('{}.txt'.format(name), 'a') as csv_file:
        csv_writer = csv.writer(csv_file, delimiter=',')
        csv_writer.writerow(SphenoDataList)


# # Main Part of the code

# ## Read data from batches

# In[10]:


SPhenoFileName = 'SPheno.spc.BGLNCS'


# In[11]:


n_tot,n_EW = 0,0
if run_analysis:
    for i in range(from_batch_nu,to_batch_nu):
        dir = 'batch_{}/Result_data/Spheno_output_Folder'.format(i)
        list = os.listdir(dir) # dir is your directory path
        number_points = len(list)
        print("start with batch:{}".format(i))
        for j in range(1,number_points+1):
            n_tot += 1
            #Function that return S,T and U parameters
            SphenoDataList = SPheno_reader(i,j,SPhenoFileName )
            T_parameter,S_parameter,U_parameter = SphenoDataList[-4],SphenoDataList[-3],SphenoDataList[-2]
            #Function that test the S,T and U
            EW_precision,delta_chi = EW_precision_function(T_parameter,S_parameter,U_parameter)


            if EW_precision:
                n_EW += 1
                print('The point {1} in batch {0} bassed EWPT'.format(i,j))
                data_spheno = 'batch_{}/Result_data/Spheno_output_Folder/point_{}/'.format(i,j)
                #This line, creates an HiggsBounds output
                os.system(HiggsBounds + ' ' + 'LandH effC 5 1' + ' ' + data_spheno )
                #This line, creates an HiggsSigan output
                os.system(HiggsSignals + ' ' + 'latestresults 2 effC 5 1' + ' ' + data_spheno )

                #Reads HB output
                HiggsBounds_pass,HBresult,chanel,obsratio, ncomb = HiggsBounds_reader_limit_function(i,j)
                #Reads HS output
                HiggsSignal_pass,Pvalue,nobs_tot,nobs_h,nobs_mu,csq_tot,csq_mh,csq_mu=HiggsSignals_reader_limit_function(i,j)
                #Runs Flavio
                RatioList = FlavioFun(i,j)

                HBHS = [HiggsBounds_pass,HBresult,chanel,obsratio, ncomb,HiggsSignal_pass,Pvalue,nobs_tot,nobs_h,nobs_mu,csq_tot,csq_mh,csq_mu]

                NewDatalist = SphenoDataList + HBHS + RatioList
                WdataSPheno('EWdata',NewDatalist)
                #print(len(NewDatalist),len(SphenoDataList),len(HBHS),len(RatioList))
            else:
                WdataSPheno('Ddata',SphenoDataList)

    print('Total points {0}, points passed EWPT {1}'.format(n_tot,n_EW))


# In[ ]:
