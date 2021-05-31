#!/usr/bin/env python
# coding: utf-8

# In[ ]:





# In[1]:


import os


# In[2]:


cluster = False


# In[3]:


if not cluster:

    #HiggsBound folder
    HiggsBounds_Path = '~/higgsbounds/build'
    HiggsBounds = os.path.join(HiggsBounds_Path,'HiggsBounds')


    #HiggsSignal folder
    HiggsSignals_Path = '~/higgssignals/build'
    HiggsSignals = os.path.join(HiggsSignals_Path,'HiggsSignals')
    
    #Spheno folder
    SPheno_Path= '~/SPheno-4.0.4'
    spheno_BGL = os.path.join(SPheno_Path,'bin/SPhenoBGLNCS')

if cluster:


    #HiggsBound folder
    HiggsBounds_Path = '~/opt/higgsbounds/build'
    HiggsBounds = os.path.join(HiggsBounds_Path,'HiggsBounds')


    #HiggsSignal folder
    HiggsSignals_Path = '~/opt/higgssignals/build'
    HiggsSignals = os.path.join(HiggsSignals_Path,'HiggsSignals')


# In[ ]:





# In[4]:



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

SPhenoFileName = 'SPheno.spc.BGLNCS'


# In[10]:



# In[5]:


def HiggsBounds_reader_limit_function(i,j):

    data_higgsbounds = 'batch_{}/Result_data/Spheno_output_Folder/point_{}/HiggsBounds_results.dat'.format(i,j)
    R = open( data_higgsbounds, 'r')
    for line in R:
        if not line.lstrip().startswith('#'):
            obsratio = float(line.split()[-2])
            HBresult = float(line.split()[-4])
            chanel = float(line.split()[-3])
            if float(line.split()[-2]) < 1 and HBresult == 1:
                HiggsBounds_pass = True
            else :  HiggsBounds_pass = False
    return HiggsBounds_pass, obsratio, HBresult,chanel


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


from_batch_nu = 40
to_batch_nu = 51
run_analysis = True


# In[ ]:


if run_analysis:
    n_tot,n_EW,n_HB = 0,0,0

    EW_pass = [] # The List contains the points that pass EWPT


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

                HiggsBounds_pass, obsratio, HBresult,chanel = HiggsBounds_reader_limit_function(i,j)

                Pvalue,chi_2_tot,HiggsSignal_pass=HiggsSignals_reader_limit_function(i,j)



    print('Total points {0}, points passed EWPT {1}'.format(n_tot,n_EW))


# In[ ]:





# In[ ]:




