#!/usr/bin/env python
# coding: utf-8

# In[1]:


import os
import subprocess
import sys
import glob
import numpy as np

cluster = True # If you run the scrip on the cluster
from_batch_nu = 1
to_batch_nu = 2
run_analysis = True

if not cluster:

    #FlexibleSUSY folder
    FlexibleSUSY_Path= '/Users/vasileios_vatellis/PhaseTracer/FlexibleSUSY'
    FlexibleSUSY_BGL = os.path.join(FlexibleSUSY_Path,'models/THDMSBGL/run_THDMSBGL.x')

    #HiggsBound folder
    HiggsBounds_Path = '/Users/vasileios_vatellis/higgsbounds/build'
    HiggsBounds = os.path.join(HiggsBounds_Path,'HiggsBounds')


    #HiggsSignal folder
    HiggsSignals_Path = '/Users/vasileios_vatellis/higgssignals/build'
    HiggsSignals = os.path.join(HiggsSignals_Path,'HiggsSignals')

if cluster:


    #HiggsBound folder
    HiggsBounds_Path = '/home/vasileios.vatellis@UA.PT/opt/higgsbounds/build'
    HiggsBounds = os.path.join(HiggsBounds_Path,'HiggsBounds')


    #HiggsSignal folder
    HiggsSignals_Path = '/home/vasileios.vatellis@UA.PT/opt/higgssignals/build'
    HiggsSignals = os.path.join(HiggsSignals_Path,'HiggsSignals')

# ## Functions

# In[2]:


# In[2]:


def SPheno_reader(i,j,SPheno_spc_File):
    A = []
    EW_P_O_info = False
    data_spheno ='batch_{}/Result_data/Spheno_output_Folder/point_{}/{}'.format(i,j,SPheno_spc_File)
    R = open(data_spheno, 'r')
    for line in R:
        if line.startswith('Block SPhenoLowEnergy'):
            #Electroweak precision observables
            EW_P_O_info = True
        if line.startswith('Block FlavorKitQFV # quark flavor violating observables'):EW_P_O_info=False
        if EW_P_O_info:
            A.append(line.strip('\n'))
            l0 = line.strip('\n')
    T_parameter = float(A[1].split()[1] )
    S_parameter = float(A[2].split()[1] )
    U_parameter = float(A[3].split()[1] )
    return  T_parameter,S_parameter,U_parameter


# In[3]:


def VEV_reader(i,j,SPheno_spc_File):
    A = []
    EW_P_O_info = False
    data_spheno ='batch_{}/Result_data/Spheno_output_Folder/point_{}/{}'.format(i,j,SPheno_spc_File)
    R = open(data_spheno, 'r')
    for line in R:
        if line.startswith('Block VEVS'):
            #Electroweak precision observables
            EW_P_O_info = True
        if line.startswith('Block MASS  # Mass spectrum'):EW_P_O_info=False
        if EW_P_O_info:
            A.append(line.strip('\n'))
            l0 = line.strip('\n')
    V1 = float(A[1].split()[1] )
    V2 = float(A[2].split()[1] )
    VS = float(A[3].split()[1] )
    return  V1,V2,VS


# In[4]:


def Masses_SPheno_reader(i,j,SPheno_spc_File):
    A = []
    EW_P_O_info = False
    data_spheno = 'batch_{}/Result_data/Spheno_output_Folder/point_{}/{}'.format(i,j,SPheno_spc_File)
    R = open(data_spheno, 'r')
    for line in R:
        if line.startswith('Block MASS'):
            #Electroweak precision observables
            EW_P_O_info = True
        if line.startswith('Block ZH_SCALARMIXs'):EW_P_O_info=False
        if EW_P_O_info:
            A.append(line.strip('\n'))
            l0 = line.strip('\n')
    Mhh1 =float(A[2].split()[1])
    Mhh2 =float(A[3].split()[1])
    Mhh3 =float(A[4].split()[1])
    MAh2 =float(A[5].split()[1])
    MAh3 =float(A[6].split()[1])
    MHm2 =float(A[7].split()[1])
    MVZ  =float(A[8].split()[1])
    MVWM =float(A[9].split()[1])
    MFd1 =float(A[10].split()[1])
    MFd2 =float(A[11].split()[1])
    MFd3 =float(A[12].split()[1])
    MFu1 =float(A[13].split()[1])
    MFu2 =float(A[14].split()[1])
    MFu3 =float(A[15].split()[1])
    MFe1 =float(A[16].split()[1])
    MFe2 =float(A[17].split()[1])
    MFe3 =float(A[18].split()[1])
    MFv1 =float(A[19].split()[1])
    MFv2 =float(A[20].split()[1])
    MFv3 =float(A[21].split()[1])
    MFv4 =float(A[22].split()[1])
    MFv5 =float(A[23].split()[1])
    MFv6 =float(A[24].split()[1])

    Listmasses = [Mhh1,Mhh2,Mhh3,MAh2,MAh3,MHm2,MVZ,MVWM,MFd1,MFd2,MFd3,MFu1,MFu2,MFu3,MFe1,MFe2,MFe3                 ,MFv1,MFv2,MFv3,MFv4,MFv5,MFv6]
    return Listmasses


# In[5]:


def MINPAR_SPheno_reader(i,j,SPheno_spc_File):
    A = []
    EW_P_O_info = False
    data_spheno = 'batch_{}/Result_data/Spheno_output_Folder/point_{}/{}'.format(i,j,SPheno_spc_File)
    R = open(data_spheno, 'r')
    for line in R:
        if line.startswith('Block MINPAR'):
            #Electroweak precision observables
            EW_P_O_info = True
        if line.startswith('Block IMMINPAR'):EW_P_O_info=False
        if EW_P_O_info:
            A.append(line.strip('\n'))
            l0 = line.strip('\n')
    Lambda1 =float(A[1].split()[1])
    Lambda2 =float(A[2].split()[1])
    Lambda3 =float(A[3].split()[1])
    Lambda4 =float(A[4].split()[1])
    Lambda1Dash =float(A[5].split()[1])
    Lambda2Dash =float(A[6].split()[1])
    Lambda3Dash  =float(A[7].split()[1])

    Mu3 =float(A[8].split()[1])
    Mub =float(A[9].split()[1])
    alpha1 =float(A[10].split()[1])
    alpha2 =float(A[11].split()[1])
    alpha3 =float(A[12].split()[1])
    alpha4 =float(A[13].split()[1])

    Y1d11 =float(A[15].split()[1])
    Y1d12 =float(A[16].split()[1])
    Y1d13 =float(A[17].split()[1])
    Y1d21 =float(A[18].split()[1])
    Y1d22 =float(A[19].split()[1])
    Y1d23 =float(A[20].split()[1])
    Y2d31 =float(A[21].split()[1])
    Y2d32 =float(A[22].split()[1])
    Y2d33 =float(A[23].split()[1])

    Y1u11 =float(A[24].split()[1])
    Y1u12 =float(A[25].split()[1])
    Y1u21 =float(A[26].split()[1])
    Y1u22 =float(A[27].split()[1])
    Y2u33 =float(A[28].split()[1])

    Y1e11 =float(A[29].split()[1])
    Y1e12 =float(A[30].split()[1])
    Y1e22 =float(A[32].split()[1])
    Y1e33 =float(A[33].split()[1])

    Y1n11 =float(A[34].split()[1])
    Y1n12 =float(A[35].split()[1])
    Y1n22 =float(A[37].split()[1])
    Y1n33 =float(A[38].split()[1])

    B11 = float(A[39].split()[1])
    C23 = float(A[44].split()[1])
    C32 = float(A[46].split()[1])


    quartics = [Lambda1,Lambda2,Lambda3,Lambda4,Lambda1Dash,Lambda2Dash,Lambda3Dash]
    ancouplings = [Mu3,Mub,alpha1,alpha2,alpha3,alpha4]
    YDre = [Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,Y2d31,Y2d32,Y2d33]
    YUre = [Y1u11,Y1u12,Y1u21,Y1u22,Y2u33]
    YRre = [Y1e11,Y1e12,Y1e22,Y1e33]
    YlightN = [Y1n11,Y1n12,Y1n22,Y1n33]
    YheavyN = [B11,C23,C32]

    return quartics,ancouplings,YDre,YUre,YRre,YlightN,YheavyN




# In[6]:


def imMINPAR_SPheno_reader(i,j,SPheno_spc_File):
    A = []
    EW_P_O_info = False
    data_spheno = 'batch_{}/Result_data/Spheno_output_Folder/point_{}/{}'.format(i,j,SPheno_spc_File)
    R = open(data_spheno, 'r')
    for line in R:
        if line.startswith('Block IMMINPAR'):
            #Electroweak precision observables
            EW_P_O_info = True
        if line.startswith('Block gaugeGUT'):EW_P_O_info=False
        if EW_P_O_info:
            A.append(line.strip('\n'))
            l0 = line.strip('\n')
    imY1d11 =float(A[1].split()[1])
    imY1d12 =float(A[2].split()[1])
    imY1d13 =float(A[3].split()[1])
    imY1d21 =float(A[4].split()[1])
    imY1d22 =float(A[5].split()[1])
    imY1d23 =float(A[6].split()[1])
    imY2d31  =float(A[7].split()[1])
    imY2d32  =float(A[8].split()[1])
    imY2d33  =float(A[9].split()[1])
    imY2e33  =float(A[10].split()[1])


    imYukawas = [imY1d11,imY1d12,imY1d13,imY1d21,imY1d22,imY1d23,imY2d31,imY2d32,imY2d33,imY2e33]


    return imYukawas


# In[7]:




# In[4]:


def HiggsBounds_reader_limit_function(i,j):

    data_higgsbounds = 'batch_{}/Result_data/Spheno_output_Folder/point_{}/HiggsBounds_results.dat'.format(i,j)
    R = open( data_higgsbounds, 'r')
    for line in R:
        if not line.lstrip().startswith('#'):
            obsratio = float(line.split()[-2])
            HBresult = float(line.split()[-4])
            if float(line.split()[-2]) < 1 and HBresult == 1:
                HiggsBounds_pass = True
            else :  HiggsBounds_pass = False
    return HiggsBounds_pass, obsratio, HBresult


# In[5]:


#There is a problem all the Pvalues are zero. But that might be a problem of SPheno
def HiggsSignals_reader_limit_function(i,j):
    data_higgssignal = 'batch_{}/Result_data/Spheno_output_Folder/point_{}/HiggsSignals_results.dat'.format(i,j)
    R = open( data_higgssignal, 'r')
    for line in R:
        if not line.lstrip().startswith('#'):
            Pvalue = float(line.split()[-1])
            chi_2_tot = float(line.split()[-5])
            if 80 > chi_2_tot and chi_2_tot < 100:
                HiggsSignal_pass = True
            else :  HiggsSignal_pass = False
    #return HiggsBounds_pass
    return Pvalue,chi_2_tot,HiggsSignal_pass








# In[10]:


def EW_precision_function(T_parameter,S_parameter,U_parameter):
    #
    EW_T = float(T_parameter)
    EW_S = float(S_parameter)
    EW_U = float(U_parameter)
    #
    InvSig = [[1720.26, -1288.92, 1027.36],
              [-1288.92,1142.91, -639.825],
              [1027.36, -639.825, 832.293]]
    Obsref = [0.07, 0.02, 0.00]
    #
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



def write_couplings_txt(Mylist,name):
    with open('{}.txt'.format(name), 'a') as f:
        for item in Mylist:
            f.write("%s\n" % item)


# In[14]:


def Data_loader_txt(name):
    R = open('{}.txt'.format(name), 'r')
    A,B = [],[]
    for line in R:
        A.append(line.strip('\n').strip('[]').split(','))
    for i in range(len(A)):
        sublist = []
        for j in range(len(A[0])):
            sublist.append(float(A[i][j]))
        B.append(sublist)
    return(B)


# In[15]:


def clean_F():
    list_output_files = glob.glob('*.txt')
    for cleaning_folder in list_output_files:
        delete_compant = 'rm'+ ' ' + '{}'.format(cleaning_folder)
        os.system(delete_compant)


# In[16]:


#EW_STU.append([T_parameter,S_parameter,U_parameter])
def separate_stu(list_STU):
    MU,MS,MT = [],[],[]
    for k in range(len(list_STU)):
        MT.append(list_STU[k][0])
        MS.append(list_STU[k][1])
        MU.append(list_STU[k][2])
    return(MT,MS,MU)


# In[17]:


#D_Masses.append([VZ_M,VZp_M,hh_1_M,hh_2_M,hh_3_M,Ah_3_M,Hm_2_M])
def separate_masses(list_masses):
    MVZ,MVZp,Mh1,Mh2,Mh3,MAh,MHm = [],[],[],[],[],[],[]
    for k in range(len(list_masses)):
        MVZ.append(list_masses[k][0])
        MVZp.append(list_masses[k][1])
        Mh1.append(list_masses[k][2])
        Mh2.append(list_masses[k][3])
        Mh3.append(list_masses[k][4])
        MAh.append(list_masses[k][5])
        MHm.append(list_masses[k][6])
    return(MVZ,MVZp,Mh1,Mh2,Mh3,MAh,MHm)


# In[18]:


#D_couplings.append([AL1,AL2,AL3,AL4,Ad2,AD1,AD2,AD3])
def separate_quartics(list_m):
    AL1,AL2,AL3,AL4,Ad2,AD1,AD2,AD3 = [],[],[],[],[],[],[],[]
    for k in range(len(list_m)):
        AL1.append(list_m[k][0])
        AL2.append(list_m[k][1])
        AL3.append(list_m[k][2])
        AL4.append(list_m[k][3])
        Ad2.append(list_m[k][4])
        AD1.append(list_m[k][5])
        AD2.append(list_m[k][6])
        AD3.append(list_m[k][7])
    return(AL1,AL2,AL3,AL4,Ad2,AD1,AD2,AD3)


# In[19]:


#D_vevg4.append([V1,V2,VS,g4])
def separate_vevg4(list_m):
    V1,V2,VS,g4 = [],[],[],[]
    for k in range(len(list_m)):
        V1.append(list_m[k][0])
        V2.append(list_m[k][1])
        VS.append(list_m[k][2])
        g4.append(list_m[k][3])
    return(V1,V2,VS,g4)

def write_couplings_dat(Mylist,name):
    with open('{}.dat'.format(name), 'a') as f:
        for item in Mylist:
            f.write("%s\n" % item)

# ## Read Points



# In[23]:


# In[8]:


SPhenoFileName = 'SPheno.spc.BGLNCS'


# In[10]:


if run_analysis:
    n_tot,n_EW,n_HB = 0,0,0

    EW_pass = [] # The List contains the points that pass EWPT

    #Lists with D are the ones that do  not pass
    D_vevg4 = [] # contains [V1,V2,VS,g4]
    D_Masses = [] # contains [VZ_M,VZp_M,hh_1_M,hh_2_M,hh_3_M,Ah_3_M,Hm_2_M]
    D_couplings = [] # contains [AL1,AL2,AL3,AL4,Ad2,AD1,AD2,AD3]
    D_STU = [] # contains [T_parameter,S_parameter,U_parameter]
    #Lists with EW are the ones that pass EW precition test (EWPT)
    EW_vevg4 = [] # [V1,V2,VS,g4])
    EW_Masses = [] # [VZ_M,VZp_M,hh_1_M,hh_2_M,hh_3_M,Ah_3_M,Hm_2_M]
    EW_couplings = [] # [AL1,AL2,AL3,AL4,Ad2,AD1,AD2,AD3]
    EW_STU = [] #[T_parameter,S_parameter,U_parameter]
    #Lists with HB are the ones that pass EWPT and Higgs observables
    HB_vevg4 = [] # [V1,V2,VS,g4])
    HB_Masses = [] # [VZ_M,VZp_M,hh_1_M,hh_2_M,hh_3_M,Ah_3_M,Hm_2_M]
    HB_couplings = [] # [AL1,AL2,AL3,AL4,Ad2,AD1,AD2,AD3]
    HB_STU = [] #[T_parameter,S_parameter,U_parameter]
    HB_data = []


    for i in range(from_batch_nu,to_batch_nu):
        dir = 'batch_{}/Result_data/Spheno_output_Folder'.format(i)
        list = os.listdir(dir) # dir is your directory path
        number_points = len(list)
        print("start with batch:{}".format(i))
        for j in range(1,number_points+1):
            n_tot += 1
            #Function that return S,T and U parameters
            T_parameter,S_parameter,U_parameter = SPheno_reader(i,j,SPhenoFileName )

            #Function that test the S,T and U
            EW_precision,delta_chi = EW_precision_function(T_parameter,S_parameter,U_parameter)

            #Listmasses = [Mhh1,Mhh2,Mhh3,MAh2,MAh3,MHm2,MVZ,MVWM,MFd1,MFd2,MFd3,MFu1,MFu2,MFu3,MFe1,MFe2,MFe3\
            #     ,MFv1,MFv2,MFv3,MFv4,MFv5,MFv6]
            Listmasses = Masses_SPheno_reader(i,j,SPhenoFileName)

            #function that returns vevs
            V1,V2,VS = VEV_reader(i,j,SPhenoFileName)

            #function returning MINBAR inputs
            quartics,ancouplings,YDre,YUre,YRre,YlightN,YheavyN = MINPAR_SPheno_reader(i,j,SPhenoFileName)
            imYukawas = imMINPAR_SPheno_reader(i,j,SPhenoFileName)


            if EW_precision:
                n_EW += 1
                print('The point {1} in batch {0} bassed EWPT'.format(i,j))
                EW_pass.append([i,j]) #saves batch : i & point number j which pass the EW
                # EW_pass[k][0] = i batch  and EW_pass[k][1] = j point number
                data_spheno = 'batch_{}/Result_data/Spheno_output_Folder/point_{}/'.format(i,j)
                #This line, creates an HiggsBounds output
                os.system(HiggsBounds + ' ' + 'LandH effC 5 1' + ' ' + data_spheno )

                #This line, creates an HiggsSigan output
                os.system(HiggsSignals + ' ' + 'latestresults 2 effC 5 1' + ' ' + data_spheno )
                #print("Finished runing for batch:{} and point:{}".format(i,j))

                HiggsBounds_pass, obsratio, HBresult = HiggsBounds_reader_limit_function(i,j)

                Pvalue,chi_2_tot,HiggsSignal_pass=HiggsSignals_reader_limit_function(i,j)


                if HiggsBounds_pass and HiggsSignal_pass:
                    n_HB += 1
                    print('The point {1} in batch {0} bassed EWPT and HB'.format(i,j))
                    HB_vevg4.append([V1,V2,VS])
                    HB_Masses.append(Listmasses)
                    HB_couplings.append([quartics,ancouplings,YDre,YUre,YRre,YlightN,YheavyN,imYukawas])
                    HB_STU.append([T_parameter,S_parameter,U_parameter])

                EW_vevg4.append([V1,V2,VS])
                EW_Masses.append(Listmasses)
                EW_couplings.append([quartics,ancouplings,YDre,YUre,YRre,YlightN,YheavyN,imYukawas])
                EW_STU.append([T_parameter,S_parameter,U_parameter])

            else:

                D_vevg4.append([V1,V2,VS])
                D_Masses.append(Listmasses)
                D_couplings.append([quartics,ancouplings,YDre,YUre,YRre,YlightN,YheavyN,imYukawas])
                D_STU.append([T_parameter,S_parameter,U_parameter])

    print('Total points {0}, points passed EWPT {1}, points passed HB {2}'.format(n_tot,n_EW,n_HB))



# In[ ]:


EW_pass


# ## Save couplings in files

# In[ ]:



# In[ ]:





# In[ ]:


if run_analysis:
    write_couplings_dat(EW_pass,"EW_pass")
    write_couplings_dat(HB_data,"HB_data")
    #STU Values
    write_couplings_txt(D_STU,"D_STU")
    write_couplings_txt(EW_STU,"EW_STU")
    write_couplings_txt(HB_STU,"HB_STU")
    # Masses
    write_couplings_txt(D_Masses,"D_Masses")
    write_couplings_txt(EW_Masses,"EW_Masses")
    write_couplings_txt(HB_Masses,"HB_Masses")
    # quartic couplings
    write_couplings_txt(D_couplings,"D_couplings")
    write_couplings_txt(EW_couplings,"EW_couplings")
    write_couplings_txt(HB_couplings,"HB_couplings")
    # VEVs and g4
    write_couplings_txt(D_vevg4,"D_vevg4")
    write_couplings_txt(EW_vevg4,"EW_vevg4")
    write_couplings_txt(HB_vevg4,"HB_vevg4")
    print("The shell was executed")


# In[ ]:


len(D_vevg4[0])


# In[ ]:
