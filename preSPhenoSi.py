#!/usr/bin/env python
# coding: utf-8

# In[1]:


cluster = False # If you run the scrip on the cluster you need to add
nupoints = 100 #number of points simulated


# This code aims to create a working system for the BGL like two Higgs doublet models with a complex singlet and to calculate points passing the unitarity constraints and running them through SPheno.  All the points will be saved in one folder

# In[2]:


import os
import subprocess
import sys
import numpy as np
import math
from sympy import *
from scipy.optimize import fsolve
from scipy.stats import unitary_group
import glob
import scipy.linalg as la
import matplotlib.pyplot as plt
import csv
import gzip


# In[3]:


print("Hello, this will take a while")
counter = 0


# In[4]:


# The code tries to be as automatic as it could be. The command below gives the path where the code is saved
os.getcwd()


# ## Path definitions

# 
# based on the path where the code is saved, I tried to create a workign tree of
# files, which will make easier in the end to find the data. In case that someone
# else wants to use this code, he needs to change the path of the softwares below.

# In[5]:


#The path where all the programs are
Working_Folder= os.getcwd()

if not cluster:
    #Spheno folder
    SPheno_Path= '~/SPheno-4.0.4'
    spheno_BGL = os.path.join(SPheno_Path,'bin/SPhenoBGLNCS')
    
    
    #HiggsBound folder
    HiggsBounds_Path = '~/higgsbounds/build'
    HiggsBounds = os.path.join(HiggsBounds_Path,'HiggsBounds')


    #HiggsSignal folder
    HiggsSignals_Path = '~/higgssignals/build'
    HiggsSignals = os.path.join(HiggsSignals_Path,'HiggsSignals')
    
    #MadGraph
    MG_Path = '~/MG5_aMC_v3_1_1/bin/mg5_aMC'

if cluster:
    #SPheno cluster
    SPheno_Path= '~/opt/SPheno-4.0.4'
    spheno_BGL = os.path.join(SPheno_Path,'bin/SPhenoBGLNCS')
    
    #HiggsBound folder
    HiggsBounds_Path = '~/opt/higgsbounds/build'
    HiggsBounds = os.path.join(HiggsBounds_Path,'HiggsBounds')


    #HiggsSignal folder
    HiggsSignals_Path = '~/opt/higgssignals/build'
    HiggsSignals = os.path.join(HiggsSignals_Path,'HiggsSignals')
    
    #MadGraph
    MG_Path = '~/opt/MG5_aMC_v3_1_1/bin/mg5_aMC'


# In[6]:


Running_Env = Working_Folder +'/Running_Env'
Resutls_Env = Working_Folder +'/Resutls_Env'
Result_data = Working_Folder +'/Result_data'
Spheno_output_Folder = Result_data + '/Spheno_output_Folder'


# In[7]:


#create the working folders if they don't exist
if not os.path.exists(Running_Env):
    os.makedirs(Running_Env)

if not os.path.exists(Resutls_Env):
    os.makedirs(Resutls_Env)

if not os.path.exists(Result_data):
    os.makedirs(Result_data)


if not os.path.exists(Spheno_output_Folder):
    os.makedirs(Spheno_output_Folder)


#creates a folder where will be saved the LesHouches.in.BGL for SPheno
if not os.path.exists(Running_Env + '/spheno'):
    os.makedirs(Running_Env + '/spheno')

#creates a folder where will be saved the LesHouches.in.BGL FlexibleSUSY
if not os.path.exists(Running_Env + '/FlexibleSUSY'):
    os.makedirs(Running_Env + '/FlexibleSUSY')


# In[8]:


os.chdir(Resutls_Env)
os.getcwd()


# ## variables definitions
# 

# in a case that we want to use this script for another model, all the next shells until the for loop need be changed, especialy the part which calculates the couplings.
# 

# In[9]:


# Trigonometri equations
def Csc(x):
    return 1/np.sin(x)
def Cos(x):
    return np.cos(x)
def Sin(x):
    return np.sin(x)
def Sec(x):
    return 1/np.cos(x)
def Cot(x):
    return 1/np.tan(x)
def Tan(x):
    return np.tan(x)
def Sqrt(x):
    return np.sqrt(x)


# In[10]:


# lambda_1
def Fuction_l1(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,a2,a3):
    l1 = (1/(2*v**2))*Sec(beta)**2*(mH1sq* Cos(a2)**2* Cos(delta)**2 -     Sin(beta)**2* (mAh2sq* Cos(gamma1)**2 + mAh3sq* Sin(gamma1)**2) +     Cos(a3)**2* (mH3sq* Cos(delta)**2* Sin(a2)**2 + mH2sq* Sin(delta)**2) +     Sin(a3)**2* (mH2sq* Cos(delta)**2* Sin(a2)**2 + mH3sq* Sin(delta)**2)    + (mH2sq - mH3sq)* Cos(a3)* Sin(a2)* Sin(a3)* Sin(2* delta))
    return l1

# lambda_2
def Fuction_l2(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,a2,a3):
    l2 =  (1/(2*v**2))*Csc(beta)**2* (mH3sq* Cos(delta)**2* Sin(a3)**2 -     Cos(beta)**2* (mAh2sq* Cos(gamma1)**2 + mAh3sq* Sin(gamma1)**2) + (mH1sq* Cos(a2)**2 +     mH2sq* Sin(a2)**2* Sin(a3)**2)* Sin(delta)**2 + Cos(a3)**2* (mH2sq* Cos(delta)**2 +     mH3sq* Sin(a2)**2* Sin(delta)**2) + (-mH2sq + mH3sq)* Cos(a3)* Sin(a2)*    Sin(a3)* Sin(2* delta))
    return l2

# lambda_3
def Fuction_l3(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3):
    l3 = (1/(8* v**2))*(-4* (mAh2sq + mAh3sq - 4* mCh) +     4*(-mAh2sq + mAh3sq)* Cos(2* gamma1) + Csc(beta)* Sec(beta)*     (4* (mH2sq - mH3sq)* Cos(2* delta)* Sin(a2)* Sin(2* a3) + (2*     (-2* mH1sq + mH2sq + mH3sq)* Cos(a2)**2 - (mH2sq - mH3sq)* (-3 + Cos(2* a2))*     Cos(2* a3))* Sin(2* delta)))
    return l3

# lambda_4
def Fuction_l4(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3):
    l4 = (mAh2sq + mAh3sq - 2* mCh + (mAh2sq - mAh3sq)* Cos(2*gamma1))/v**2
    return l4
# lambda_{sigma 1}
def Fuction_l_s1(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    ls1 = (2*v3*(mH1sq* Sin(a2)**2 + Cos(a2)**2* (mH3sq* Cos(a3)**2 + mH2sq* Sin(a3)**2)) +    Sqrt(2)* v**2* Cos(beta)* Sin(beta)* (alpha_1 + alpha_2))/(4* v3**3)
    return ls1

# lambda_{sigma 2}
def Fuction_l_s2(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    ls2 = (1/(v*v3))*(Cos(a2)* Sec(beta)* (Cos(delta)* Sin(a2)*     (mH1sq - mH3sq* Cos(a3)**2 - mH2sq* Sin(a3)**2) + (-mH2sq + mH3sq)* Cos(a3)*    Sin(a3)* Sin(delta)) + (mAh2sq - mAh3sq)* Cos(gamma1)* Sin(gamma1)* Tan(beta) -    v* (Sqrt(2)* alpha_1 + 2* v3* alpha_3)* Tan(beta))
    return ls2

# lambda_{sigma 3}
def Fuction_l_s3(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    ls3 = (1/(v*v3))*((mAh2sq - mAh3sq)* Cos(gamma1)* Cot(beta)* Sin(gamma1) +     Cos(a2)* Csc(beta)* ((-mH2sq + mH3sq)* Cos(a3)* Cos(delta)* Sin(a3) + Sin(a2)*    (-mH1sq + mH3sq* Cos(a3)**2 + mH2sq* Sin(a3)**2)* Sin(delta)) -    v* Cot(beta)* (Sqrt(2)* alpha_1 + 2* v3* alpha_3))
    return ls3

# alpha_4
def Fuction_alpha4(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    alpha_4 =  ((-mAh2sq + mAh3sq)* Sin(2* gamma1) + Sqrt(2)* v* alpha_1 - Sqrt(2)* v*    alpha_2 + 2* v* v3* alpha_3)/(2* v* v3)
    return alpha_4

# mu_3
def Fuction_mu3(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    mu3 = (-v *(mAh2sq + mAh3sq + (mAh2sq - mAh3sq)* Cos(2* gamma1))* Sin(    2 *beta) + (mAh2sq - mAh3sq)* v3* Sin(2* gamma1) + v* v3* (-Sqrt(2)* (3* alpha_1 + alpha_2) -    4* v3* alpha_3))/(4* v)
    return mu3

# mu_{sigma b}
def Fuction_musb(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    musb = (-(mAh2sq + mAh3sq)*v3 + (mAh2sq - mAh3sq)*(v3* Cos(2* gamma1) + v* Sin(2* beta)* Sin(2* gamma1)) +     v**2* Cos(beta)* Sin(beta)* (-3*Sqrt(2)* alpha_1 + Sqrt(2)* alpha_2 - 8* v3* alpha_3))/(4* v3)
    return musb


# In[11]:


def nineRandomAngles():
    phi1,phi2,phi3 = np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi)
    phi4,phi5,phi6 = np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi)
    phi7,phi8,phi9 = np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi)
    return phi1,phi2,phi3,phi4,phi5,phi6,phi7,phi8,phi9
def FiveRandomAngles():
    phi1,phi2,phi3,phi4,phi5 = np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi)
    return phi1,phi2,phi3,phi4,phi5


# In[12]:


def Yukawa_alaysis(beta,v3):
    v = 246
    num_v_1 = np.cos(beta)*v
    num_v_2 = np.sin(beta)*v
    num_v_3 = v3
    """Yukawas calculation
    -------------------------------------------------------------------------------
    v_sm = 246
    experimental values taken from PDG 2020
    https://pdg.lbl.gov/2020/reviews/rpp2020-rev-ckm-matrix.pdf  : """
    #----------------------------------------------------------------------------
    lam_value =  0.22650
    lam_error_value = 0.00048
    A_value = 0.790
    A_error_plus_value = 0.017
    A_error_minus_value = 0.012
    rho_value = 0.141
    rho_error_plus_value = 0.016
    rho_error_minus_value = 0.017
    etha_value = 0.357
    etha_error_value = 0.0011
    #-----------------------------------------------------------------------------
    # Random sampling of the mixing parameters within allowed bounds:
    lam = np.random.uniform(lam_value - lam_error_value, lam_value + lam_error_value)
    A = np.random.uniform(A_value - A_error_minus_value, A_value + A_error_plus_value)
    rho = np.random.uniform(rho_value - rho_error_minus_value, rho_value + rho_error_plus_value)
    etha = np.random.uniform(etha_value - etha_error_value, etha_value + etha_error_value)
    limit = 5      # limit for the couplings
    Y_lim = np.sqrt(4*np.pi) # limit for the Yukawa couplings
    limit = 5      # limit for the couplings
    Y_lim = np.sqrt(4*np.pi) # limit for the Yukawa couplings


    # Mass parameters data from
    #https://pdg.lbl.gov/2021/tables/contents_tables.html
    #------------------
    Mu = np.array(2.16)*pow(10,-3) #2.16 Mev
    Mc = np.array(1.27)
    Mt = np.array(172.76)
    Md = np.array(4.67)*pow(10,-3)
    Ms = np.array(93)*pow(10,-3)
    Mb = np.array(4.18)

    # Angles used to diagonalize up-quarks
    #------------------
    theta_1,theta_2,phi_1,phi_2,phi_3,phi_4,phi_5,phi_6,phi_7 = nineRandomAngles()

    #Yukawa calculations
    #-------------------------------------------------------------------------------
    #Unitary U^u_L matrix
    mat1 = np.array([[1., 0.], [0., np.exp(1j*(phi_1 - phi_2))]])
    mat2 = np.array([[np.cos(theta_1), np.sin(theta_1)], [-np.sin(theta_1), np.cos(theta_1)]])
    mat3 = np.array([[np.exp(1j*phi_3), 0.], [0., np.exp(1j*phi_2)]])

    matProd1 = np.matmul(mat1, np.matmul(mat2, mat3))

    vlu = np.block([[matProd1, np.zeros((2, 1))], [np.zeros((1, 2)), 1.0]])

    #Unitary U^u_R matrix
    mat4 = np.array([[1., 0.], [0., np.exp(1j*(phi_4 - phi_5))]])
    mat5 = np.array([[np.cos(theta_2), np.sin(theta_2)], [-np.sin(theta_2), np.cos(theta_2)]])
    mat6 = np.array([[np.exp(1j*phi_6), 0.], [0., np.exp(1j*phi_5)]])
    matProd2 = np.matmul(mat4, np.matmul(mat5, mat6))

    # Note the complex phase in the (3,3) entry.
    vru = np.block([[matProd2, np.zeros((2, 1))], [np.zeros((1, 2)), np.exp(1j*phi_7)]])

    #CKM matrix
    vckm = np.array([[1-1/2*lam**2                , lam         , A*lam**3*(rho - 1j*etha)],
                             [-lam                        , 1-1/2*lam**2,                     A*lam**2],
                             [A*lam**3*(1-rho-1j*etha), -A*lam**2   ,                         1. ]])

    vckmdagger = np.transpose(np.conjugate(vckm))

    #Unitary U^d_L matrix
    vld = np.matmul(vckmdagger,vlu)

    # Angles to digonalize down-type quarks (U^d_R matrix)
    theta_12,theta_13,theta_23,alpha_11,alpha_12,alpha_13,alpha_23,alpha_33,delta = nineRandomAngles()

    #Unitary U^u_R matrix

    mat7 = np.array([[1., 0., 0.], [0., np.exp(1j*(alpha_23 - alpha_13)), 0.], [0., 0., np.exp(1j*(alpha_33 - alpha_13))]])
    mat8 = np.array([[
                    np.cos(theta_12)*np.cos(theta_13),
                    np.sin(theta_12)*np.cos(theta_13),
                    np.sin(theta_13)*np.exp(-1j*delta)
                    ],
                    [
                    - np.sin(theta_12)*np.cos(theta_23) - np.cos(theta_12)*np.sin(theta_23)*np.sin(theta_13)*np.exp(1j*delta),
                    np.cos(theta_12)*np.cos(theta_23) - np.sin(theta_12)*np.sin(theta_23)*np.sin(theta_13)*np.exp(1j*delta),
                    np.sin(theta_23)*np.cos(theta_13)
                    ],
                    [
                    np.sin(theta_12)*np.cos(theta_23) - np.cos(theta_12)*np.sin(theta_23)*np.sin(theta_13)*np.exp(1j*delta),
                    - np.cos(theta_12)*np.sin(theta_23) - np.sin(theta_12)*np.cos(theta_23)*np.sin(theta_13)*np.exp(1j*delta),
                    np.cos(theta_23)*np.cos(theta_13)
                    ]]
                    )
    mat9 = np.array([[np.exp(1j*alpha_11), 0., 0.], [0., np.exp(1j*alpha_12), 0.], [0., 0., np.exp(1j*alpha_13)]])

    vrd = np.matmul(mat7, np.matmul(mat8, mat9))

    Du = np.array([[Mu,0,0],
                    [0,Mc,0],
                    [0,0,Mt]])
    Dd = np.array([[Md,0,0],
                    [0,Ms,0],
                    [0,0,Mb]])

    #
    #-------------------------------------------------------------------------------
    #

    # The actual inversion:
    MdT = np.transpose(np.matmul(np.transpose(vrd),np.matmul(Dd, vld)))
    MuT = np.transpose(np.matmul(np.transpose(vru),np.matmul(Du, vlu)))

    #print("Up-Quark mass texture: ", MuT)
    #print("Down-Quark mass texture: ", MdT)

    # Extracting the values of the Yukawa couplings:
    Y1d11 = (np.sqrt(2)/num_v_1)*MdT[0, 0]
    Y1d12 = (np.sqrt(2)/num_v_1)*MdT[0, 1]
    Y1d13 = (np.sqrt(2)/num_v_1)*MdT[0, 2]
    Y1d21 = (np.sqrt(2)/num_v_1)*MdT[1, 0]
    Y1d22 = (np.sqrt(2)/num_v_1)*MdT[1, 1]
    Y1d23 = (np.sqrt(2)/num_v_1)*MdT[1, 2]
    Y2d31 = (np.sqrt(2)/num_v_2)*MdT[2, 0]
    Y2d32 = (np.sqrt(2)/num_v_2)*MdT[2, 1]
    Y2d33 = (np.sqrt(2)/num_v_2)*MdT[2, 2]

    Y1u11 = (np.sqrt(2)/num_v_1)*MuT[0, 0]
    Y1u12 = (np.sqrt(2)/num_v_1)*MuT[0, 1]
    Y1u21 = (np.sqrt(2)/num_v_1)*MuT[1, 0]
    Y1u22 = (np.sqrt(2)/num_v_1)*MuT[1, 1]
    Y2u33 = (np.sqrt(2)/num_v_2)*MuT[2, 2]

    ANGLES = [0,0,0,0,0,0,0,0,0,0,0,0]
    return Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,            Y2d31,Y2d32,Y2d33,vlu,vru,vld,vrd,ANGLES,vckm


# #Need to change function below

# # Start Neutrino Part

# In[13]:


def neutrinosMatrix(v1,v2,v3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,BB11,BB22,BB12,BB21,C13,C31,C23,C32):
    el14,el15 = - v1*Y1n11/np.sqrt(2),- v1*Y1n12/np.sqrt(2)
    el24,el25 = - v1*Y1n21/np.sqrt(2),- v1*Y1n22/np.sqrt(2)
    el36 = - v2*Y2n33/np.sqrt(2)
    el44,el55 = BB11*v3/np.sqrt(2),-BB22*v3/np.sqrt(2)
    el45 = -( BB12 + BB21)*v3/(2*np.sqrt(2))
    el46 = -( C13 + C31)*v3/(2*np.sqrt(2))
    el56 = -( C23 + C32)*v3/(2*np.sqrt(2))
    NutrinoMatrix = np.array([[0,0,0,el14,el15,0 ],
                      [0,0,0,el24,el25,0],
                      [0,0,0,0,0,el36],
                      [el14,el24,0,el44,el45,el46],
                      [el15,el25,0,el45,el55,el56],
                      [0,0,el36,el46,el56,0]])
    return NutrinoMatrix


# In[14]:


def NutrinoF(v1,v2,v3):    
    Y1n11,Y1n12,Y1n21,Y1n22,Y2n33 = np.random.uniform(low = pow(10,-7) ,high = pow(10,-4) ,size = 5 )
    BB11,BB22,BB12,BB21,C13,C31,C23,C32 = np.random.uniform(low = 0.1  ,high = 10 ,size = 8 )
        
    NutrinoMatrix = neutrinosMatrix(v1,v2,v3,Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,BB11,BB22,BB12,BB21,C13,C31,C23,C32)
    eigvals,eigvecs = la.eig(NutrinoMatrix)
    Abseigvals= abs(eigvals)
    vals = Abseigvals
    Abseigvals_index = sorted(range(len(vals)), key=vals.__getitem__)
    list_eigvals = [eigvals[i] for i in Abseigvals_index]
    list_eigvecs = [eigvecs.T[i] for i in Abseigvals_index]
    UmixNu = np.array(list_eigvecs)

    return Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,BB11,BB22,BB12,BB21,C13,C31,C23,C32,list_eigvals,UmixNu      


# In[15]:


def Umatrix_F(phi1,phi2,phi3,theta1):
    # Unitari matrix that will be used to diagonalize the mass matrices
    #mat1 = np.array([[1., 0.], [0., np.exp(1j*(phi1))]])
    #mat2 = np.array([[np.cos(theta1), np.sin(theta1)], [-np.sin(theta1), np.cos(theta1)]])
    #mat3 = np.array([[np.exp(1j*phi3), 0.], [0., np.exp(1j*phi2)]])
    #mat3= np.diag([1,1])
    #matProd1 = np.matmul(mat1, np.matmul(mat2, mat3))

    #Umatrix = np.block([[matProd1, np.zeros((2, 1))], [np.zeros((1, 2)), 1.0]])
    mat1 = np.array([[np.cos(phi1),-np.sin(phi1),0],[np.sin(phi1),np.cos(phi1),0],[0,0,1]])
    mat2 = np.diag([np.exp(1.j*theta1),1,1])
    Umatrix = np.dot(mat1,mat2)
    return Umatrix
def ReArangeEigeV(OeL,eigVel,OeR):    
    vals= abs(eigVel)
    Abseigvals_index = sorted(range(len(vals)), key=vals.__getitem__)
    list_eigvals = [eigVel[i] for i in Abseigvals_index]
    list_OeL = [OeL.T[i] for i in Abseigvals_index]
    list_OeR = [OeR[i] for i in Abseigvals_index]
    OeL = np.array(list_OeL)
    OeR = np.array(list_OeR)
    return OeL,list_eigvals,OeR

def PMNS_numCheck(OeL,UmixNu,UELMIX,IMUELMIX):
    PMNS = np.dot(np.block([OeL, np.zeros((3, 3))]),UmixNu.conj().T)
    SPhenoPMNS = np.dot(np.block([UELMIX + 1.j*IMUELMIX, np.zeros((3, 3))]),UmixNu.conj().T)
    return np.round(abs(PMNS),3)==np.round(abs(SPhenoPMNS),3),PMNS,SPhenoPMNS


# In[16]:


def mneutrinos_check(list_eigvals):
    Mnum =  np.array([abs(list_eigvals[0]) < pow(10,-9),abs(list_eigvals[1]) < pow(10,-9),abs(list_eigvals[2]) < pow(10,-9)])
    return sum(Mnum)
def PMNS_check(num_OMNS):
    Jcp = np.imag(num_OMNS[0,0]*num_OMNS[0,1].conj()*num_OMNS[1,0].conj()*num_OMNS[1,1])
    num1 = np.array([0.797 < abs(num_OMNS[0,0]) < 0.842, 0.518 < abs(num_OMNS[0,1]) < 0.585,
             0.143 < abs(num_OMNS[0,2]) < 0.156, 0.233 < abs(num_OMNS[1,0]) < 0.495,
              0.448 < abs(num_OMNS[1,1]) < 0.679,0.639< abs(num_OMNS[1,2]) < 0.783,
              0.287 < abs(num_OMNS[2,0]) < 0.532,0.486 < abs(num_OMNS[2,1]) < 0.706,
              0.604 < abs(num_OMNS[2,2]) < 0.754,-0.012452964877619483>Jcp> -0.02266807824167488])
    num2 = sum(num1)
    return num2
# the num1 array contains alll the requierments for the num_OMNS (the calculated PMNS) to fit in the experimental PMNS
#if the sum is 10 that means all the restrictions passed


# # end Neutrino Part

# In[17]:


def Lepton_neutrino_Yukawa_couplings_BGL(v_s,v_u,v_d):
    v_sm = 246
    #definitios
    #mass parameters data from
    #https://pdg.lbl.gov/2021/tables/contents_tables.html
    #------------------
    Me = np.array(0.5109989461)*pow(10,-3) #0.5109989461 MeV
    Mmu =np.array(105.6583745)*pow(10,-3) # 105.6583745 Mev
    Mtau = np.array(1776.86)*pow(10,-3) #1776.86 MeV

    me1,me2,me3 = Me,Mmu,Mtau
    #vevs
    #------------------
    vs = v_s
    v1 = v_u
    v2 = v_d
    v3 = vs
    #------------------
    NuPass2=0
    while NuPass2 <1000:
        if NuPass2 ==999:
            NuPass2 = 0
        NuPass2 +=1
        #Yukawa calculations
        #------------------
        theta_1,theta_2,phi_1,phi_2,phi_3,phi_4,phi_5,phi_6,phi_7 = nineRandomAngles()
        #-------------------------------------------------------------------------------
        #Unitary U^u_L matrix
        vle = Umatrix_F(phi_1,phi_2,phi_3,theta_1)

        #Unitary U^u_R matrix
        vre = Umatrix_F(phi_4,phi_5,phi_6,theta_2)
        De = np.array([[me1,0,0],
                [0,me2,0],
                [0,0,me3]])
        MeT = np.transpose(np.matmul(np.transpose(vre),np.matmul(De, vle)))

        Y1e11 = (np.sqrt(2)/v1)*MeT[0, 0]
        Y1e12 = (np.sqrt(2)/v1)*MeT[0, 1]
        Y1e21 = (np.sqrt(2)/v1)*MeT[1, 0]
        Y1e22 = (np.sqrt(2)/v1)*MeT[1, 1]
        Y2e33 = (np.sqrt(2)/v2)*MeT[2, 2]
        Y1l11, Y1l12, Y1l21, Y1l22, Y2l33 = Y1e11, Y1e12, Y1e21, Y1e22, Y2e33

        NuPass= 0
        while NuPass < 2000:
            #Neutrinos 
            #------------------
            Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,BB11,BB22,BB12,BB21,C13,C31,C23,C32,list_eigvals,UmixNu= NutrinoF(v1,v2,v3)

            OeL,eigVel,OeR=np.linalg.svd(MeT, full_matrices=True)
            OeL,eigVel,OeR = ReArangeEigeV(OeL,eigVel,OeR)
            #OeR might be the transpose in SPheno 

            OeL3x6 = np.block([OeL, np.zeros((3, 3))])

            num_OMNS = np.dot(OeL3x6,UmixNu.conj().T)
            NuPass += 1
            if abs(list_eigvals[0]) < pow(10,-9) and abs(list_eigvals[1]) < pow(10,-9) and abs(list_eigvals[2]) < pow(10,-9):
                num2 =PMNS_check(num_OMNS)
                if num2 ==10:
                    return OeL,eigVel,OeR,Y1l11, Y1l12, Y1l21, Y1l22, Y2l33,                    Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,BB11,BB22,BB12,BB21,C13,C31,C23,C32,num_OMNS,list_eigvals,UmixNu,eigVel


# In[18]:


#creation of a function like the SLHA file
def write_spheno_LesHouches(gl1, gl2, gl3, gl4, gl_s1,gl_s2, gl_s3,gmu_3,gmusb,galpha_1,galpha_2,galpha_3,galpha_4,v3,Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23,Y2d31, Y2d32, Y2d33, Y1u11, Y1u12, Y1u21, Y1u22, Y2u33,Y1l11, Y1l12, Y1l21, Y1l22,Y2l33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, BB11,BB22,BB12,BB21, C13, C23, C31, C32,v_u,v_d):
#creation of a function like the write_mg_cards fo the LesHouches file
     with open(os.path.join(Running_Env,'spheno','LesHouches.in.BGLNCS'),'w') as f:
        f.write("""
Block MODSEL      #
 1 0               #  1/0: High/low scale input
 2 1              # Boundary Condition
 6 1               # Generation Mixing
 5 2               # 0 CP conserved, 1 CP violated (only CKM phase), 2 CP generally violated
 12 9.118870E+01   # Renormalization energy scale
Block SMINPUTS    # Standard Model inputs
 2 1.166370E-05    # G_F,Fermi constant
 3 1.187000E-01    # alpha_s(MZ) SM MSbar
 4 9.118870E+01    # Z-boson pole mass
 5 4.180000E+00    # m_b(mb) SM MSbar
 6 1.735000E+02    # m_top(pole)
 7 1.776690E+00    # m_tau(pole)""")
        f.write("""
Block MINPAR  # Input parameters
 1   {0}    # Lambda1Input
 2   {1}    # Lambda2Input
 3   {2}    # Lambda3Input
 4   {3}    # Lambda4Input
 5   {4}    # Lambda1DashInput
 6   {5}    # Lambda2DashInput
 7   {6}    # Lambda3DashInput""".format(gl1, gl2, gl3, gl4, gl_s1,gl_s2, gl_s3))
        f.write("""
 8   {0}    # Mu3Input
 9   {1}   # MubInput
 10  {2}    # Aa1Input
 11  {3}    # Aa2Input
 12  {4}    # Aa3Input
 13  {5}    # Aa4Input
 14  {6}   # v3input""".format(gmu_3,gmusb,galpha_1,galpha_2,galpha_3,galpha_4,v3))
        f.write("""
15   {0}    # Y1d11input
16   {1}    # Y1d12input
17   {2}    # Y1d13input
18   {3}    # Y1d21input
19   {4}    # Y1d22input
20   {5}    # Y1d23input
21   {6}    # Y2d31input
22   {7}    # Y2d32input
23   {8}    # Y2d33input""".format(np.real(Y1d11),np.real(Y1d12),np.real(Y1d13),\
np.real(Y1d21),np.real(Y1d22),np.real(Y1d23),np.real(Y2d31),np.real(Y2d32),np.real(Y2d33)))
        f.write("""
24   {0}    # Y1u11input
25   {1}    # Y1u12input
26   {2}    # Y1u21input
27   {3}    # Y1u22input
28   {4}    # Y2u33input""".format(np.real(Y1u11),np.real(Y1u12),np.real(Y1u21),np.real(Y1u22),np.real(Y2u33)))
        f.write("""
 29   {0}    # Y1e11input
 30   {1}    # Y1e12input
 31   {2}    # Y1e21input
 32   {3}    # Y1e22input
 33   {4}    # Y2e33input""".format(np.real(Y1l11),np.real(Y1l12),np.real(Y1l21),\
 np.real(Y1l22),np.real(Y2l33)))
        f.write("""
 34   {0}    # Y1n11input
 35   {1}    # Y1n12input
 36   {2}    # Y1n21input
 37   {3}    # Y1n22input
 38   {4}    # Y2n33input
 39   {5}    # B11input
 40   {6}    # B12input
 41   {7}    # B21input
 42   {8}    # B22input
 43   {9}    # C13input
 44   {10}    # C23input
 45   {11}    # C31input
 46   {12}    # C32input""".format(Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, BB11, BB12, \
 BB21, BB22, C13, C23, C31, C32))
        f.write("""
 47   {}    # v1input
 48   {}    # v2input""".format(v_u,v_d))
        f.write("""
Block IMMINPAR  # Input parameters
 15   {0}    # Y1d11input
 16   {1}    # Y1d12input
 17   {2}    # Y1d13input
 18   {3}    # Y1d21input
 19   {4}    # Y1d22input
 20   {5}    # Y1d23input
 21   {6}    # Y2d31input
 22   {7}    # Y2d32input
 23   {8}    # Y2d33input""".format(np.imag(Y1d11),np.imag(Y1d12),np.imag(Y1d13),\
 np.imag(Y1d21),np.imag(Y1d22),np.imag(Y1d23),np.imag(Y2d31),np.imag(Y2d32),np.imag(Y2d33)))
        f.write("""
 24    {0}  # Y1u11input
 25    {1}    # Y1u12input
 26    {2}    # Y1u21input
 27    {3}    # Y1u22input
 28    {4}    # Y2u33input""".format(np.imag(Y1u11),np.imag(Y1u12),np.imag(Y1u21),np.imag(Y1u22),np.imag(Y2u33)))
        f.write("""
 29   {0}    # Y1e11input
 30   {1}    # Y1e12input
 31   {2}    # Y1e21input
 32   {3}    # Y1e22input
 33   {4}    # Y2e33input""".format(np.imag(Y1l11),np.imag(Y1l12),np.imag(Y1l21),\
 np.imag(Y1l22),np.imag(Y2l33)))
        f.write("""
 34   {0}    # Y1n11input
 35   {1}    # Y1n12input
 36   {2}    # Y1n21input
 37   {3}    # Y1n22input
 38   {4}    # Y2n33input
 39   {5}    # B11input
 40   {6}    # B12input
 41   {7}    # B21input
 42   {8}    # B22input
 43   {9}    # C13input
 44   {10}    # C23input
 45   {11}    # C31input
 46   {12}    # C32input""".format(np.imag(Y1n11), np.imag(Y1n12), np.imag(Y1n21), np.imag(Y1n22), np.imag(Y2n33), np.imag(BB11), np.imag(BB12), \
 np.imag(BB21), np.imag(BB22), np.imag(C13), np.imag(C23), np.imag(C31), np.imag(C32)))
        f.write("""
Block SPhenoInput   # SPheno specific input
  1 -1              # error level
  2  0              # SPA conventions
  7  1              # Skip 2-loop Higgs corrections
  8  3              # Method used for two-loop calculation
  9  1              # Gaugeless limit used at two-loop
 10  1              # safe-mode used at two-loop
 11 1               # calculate branching ratios
 13 0               # 3-Body decays: none (0), fermion (1), scalar (2), both (3)
 14 0               # Run couplings to scale of decaying particle
 12 1.000E-30       # write only branching ratios larger than this value
 15 1.000E-30       # write only decay if width larger than this value
 16 0               # One-loop decays
 19 -2              # Matching order (-2:automatic, -1:pole, 0-2: tree, one- & two-loop)
 31 -1              # fixed GUT scale (-1: dynamical GUT scale)
 32 0               # Strict unification
 34 1.000E-04       # Precision of mass calculation
 35 40              # Maximal number of iterations
 36 5               # Minimal number of iterations before discarding points
 37 1               # Set Yukawa scheme
 38 1               # 1- or 2-Loop RGEs
 50 0               # Majorana phases: use only positive masses (put 0 to use file with CalcHep/Micromegas!)
 51 0               # Write Output in CKM basis
 52 0               # Write spectrum in case of tachyonic states
 55 0               # Calculate loop corrected masses
 57 1               # Calculate low energy constraints
 65 1               # Solution tadpole equation
 66 1               # Two-Scale Matching
 67 1               # effective Higgs mass calculation
 75 0               # Write WHIZARD files
 76 2               # Write HiggsBounds file
 77 0               # Output for MicrOmegas (running masses for light quarks; real mixing matrices)
 78 0               # Output for MadGraph (writes also vanishing blocks)
 79 1               # Write WCXF files (exchange format for Wilson coefficients)
 86 0.              # Maximal width to be counted as invisible in Higgs decays; -1: only LSP
 440 1               # Tree-level unitarity constraints (limit s->infinity)
 441 1               # Full tree-level unitarity constraints
 442 1000.           # sqrt(s_min)
 443 2000.           # sqrt(s_max)
 444 5               # steps
 445 0               # running
 510 1.              # Write tree level values for tadpole solutions
 515 0               # Write parameter values at GUT scale
 520 1.              # Write effective Higgs couplings (HiggsBounds blocks): put 0 to use file with MadGraph!
 521 0.              # Diphoton/Digluon widths including higher order
 525 0.              # Write loop contributions to diphoton decay of Higgs
 530 0.              # Write Blocks for Vevacious
Block DECAYOPTIONS   # Options to turn on/off specific decays
1    1     # Calc 3-Body decays of Fu. I have turned them off/on to make it work for HiggsBounds (only NDA=2 allowed)
2    1     # Calc 3-Body decays of Fe.
3    1     # Calc 3-Body decays of Fd.
1001 0     # Loop Decay of Fu
1002 0     # Loop Decay of Fe
1003 0     # Loop Decay of Fd
1004 0     # Loop Decay of hh
1005 0     # Loop Decay of Ah
1006 0     # Loop Decay of Hm
1114 0.     # U-factors (0: off, 1:p2_i=m2_i, 2:p2=0, p3:p2_i=m2_1)
1115 0.     # Use loop-corrected masses for external states
1116 0.     # OS values for W,Z and fermions (0: off, 1:g1,g2,v 2:g1,g2,v,Y_i)
1117 0.     # Use defined counter-terms
1118 0.     # Use everywhere loop-corrected masses for loop-induced decays
""")


# In[19]:


#creation of a function like the SLHA file for MadGraph
def write_MG_LesHouches(gl1, gl2, gl3, gl4, gl_s1,gl_s2, gl_s3,gmu_3,gmusb,galpha_1,galpha_2,galpha_3,galpha_4,v3,Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23,Y2d31, Y2d32, Y2d33, Y1u11, Y1u12, Y1u21, Y1u22, Y2u33,Y1l11, Y1l12, Y1l21, Y1l22,Y2l33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, BB11,BB22,BB12,BB21, C13, C23, C31, C32,v_u,v_d):
#creation of a function like the write_mg_cards fo the LesHouches file
     with open(os.path.join(Running_Env,'spheno','LesHouches.in.BGLNCSmg'),'w') as f:
        f.write("""
Block MODSEL      #
 1 0               #  1/0: High/low scale input
 2 1              # Boundary Condition
 6 1               # Generation Mixing
 5 2               # 0 CP conserved, 1 CP violated (only CKM phase), 2 CP generally violated
 12 9.118870E+01   # Renormalization energy scale
Block SMINPUTS    # Standard Model inputs
 2 1.166370E-05    # G_F,Fermi constant
 3 1.187000E-01    # alpha_s(MZ) SM MSbar
 4 9.118870E+01    # Z-boson pole mass
 5 4.180000E+00    # m_b(mb) SM MSbar
 6 1.735000E+02    # m_top(pole)
 7 1.776690E+00    # m_tau(pole)""")
        f.write("""
Block MINPAR  # Input parameters
 1   {0}    # Lambda1Input
 2   {1}    # Lambda2Input
 3   {2}    # Lambda3Input
 4   {3}    # Lambda4Input
 5   {4}    # Lambda1DashInput
 6   {5}    # Lambda2DashInput
 7   {6}    # Lambda3DashInput""".format(gl1, gl2, gl3, gl4, gl_s1,gl_s2, gl_s3))
        f.write("""
 8   {0}    # Mu3Input
 9   {1}   # MubInput
 10  {2}    # Aa1Input
 11  {3}    # Aa2Input
 12  {4}    # Aa3Input
 13  {5}    # Aa4Input
 14  {6}   # v3input""".format(gmu_3,gmusb,galpha_1,galpha_2,galpha_3,galpha_4,v3))
        f.write("""
15   {0}    # Y1d11input
16   {1}    # Y1d12input
17   {2}    # Y1d13input
18   {3}    # Y1d21input
19   {4}    # Y1d22input
20   {5}    # Y1d23input
21   {6}    # Y2d31input
22   {7}    # Y2d32input
23   {8}    # Y2d33input""".format(np.real(Y1d11),np.real(Y1d12),np.real(Y1d13),\
np.real(Y1d21),np.real(Y1d22),np.real(Y1d23),np.real(Y2d31),np.real(Y2d32),np.real(Y2d33)))
        f.write("""
24   {0}    # Y1u11input
25   {1}    # Y1u12input
26   {2}    # Y1u21input
27   {3}    # Y1u22input
28   {4}    # Y2u33input""".format(np.real(Y1u11),np.real(Y1u12),np.real(Y1u21),np.real(Y1u22),np.real(Y2u33)))
        f.write("""
 29   {0}    # Y1e11input
 30   {1}    # Y1e12input
 31   {2}    # Y1e21input
 32   {3}    # Y1e22input
 33   {4}    # Y2e33input""".format(np.real(Y1l11),np.real(Y1l12),np.real(Y1l21),\
 np.real(Y1l22),np.real(Y2l33)))
        f.write("""
 34   {0}    # Y1n11input
 35   {1}    # Y1n12input
 36   {2}    # Y1n21input
 37   {3}    # Y1n22input
 38   {4}    # Y2n33input
 39   {5}    # B11input
 40   {6}    # B12input
 41   {7}    # B21input
 42   {8}    # B22input
 43   {9}    # C13input
 44   {10}    # C23input
 45   {11}    # C31input
 46   {12}    # C32input""".format(np.real(Y1n11), np.real(Y1n12), np.real(Y1n21), np.real(Y1n22), np.real(Y2n33), np.real(BB11), np.real(BB12), \
 np.real(BB21), np.real(BB22), np.real(C13), np.real(C23), np.real(C31), np.real(C32)))
        f.write("""
 47   {}    # v1input
 48   {}    # v2input""".format(v_u,v_d))
        f.write("""
Block IMMINPAR  # Input parameters
 15   {0}    # Y1d11input
 16   {1}    # Y1d12input
 17   {2}    # Y1d13input
 18   {3}    # Y1d21input
 19   {4}    # Y1d22input
 20   {5}    # Y1d23input
 21   {6}    # Y2d31input
 22   {7}    # Y2d32input
 23   {8}    # Y2d33input""".format(np.imag(Y1d11),np.imag(Y1d12),np.imag(Y1d13),\
 np.imag(Y1d21),np.imag(Y1d22),np.imag(Y1d23),np.imag(Y2d31),np.imag(Y2d32),np.imag(Y2d33)))
        f.write("""
 24    {0}  # Y1u11input
 25    {1}    # Y1u12input
 26    {2}    # Y1u21input
 27    {3}    # Y1u22input
 28    {4}    # Y2u33input""".format(np.imag(Y1u11),np.imag(Y1u12),np.imag(Y1u21),np.imag(Y1u22),np.imag(Y2u33)))
        f.write("""
 29   {0}    # Y1e11input
 30   {1}    # Y1e12input
 31   {2}    # Y1e21input
 32   {3}    # Y1e22input
 33   {4}    # Y2e33input""".format(np.imag(Y1l11),np.imag(Y1l12),np.imag(Y1l21),\
 np.imag(Y1l22),np.imag(Y2l33)))
        f.write("""
 34   {0}    # Y1n11input
 35   {1}    # Y1n12input
 36   {2}    # Y1n21input
 37   {3}    # Y1n22input
 38   {4}    # Y2n33input
 39   {5}    # B11input
 40   {6}    # B12input
 41   {7}    # B21input
 42   {8}    # B22input
 43   {9}    # C13input
 44   {10}    # C23input
 45   {11}    # C31input
 46   {12}    # C32input""".format(np.imag(Y1n11), np.imag(Y1n12), np.imag(Y1n21), np.imag(Y1n22), np.imag(Y2n33), np.imag(BB11), np.imag(BB12), \
 np.imag(BB21), np.imag(BB22), np.imag(C13), np.imag(C23), np.imag(C31), np.imag(C32)))
        f.write("""
Block SPhenoInput   # SPheno specific input
  1 -1              # error level
  2  0              # SPA conventions
  7  1              # Skip 2-loop Higgs corrections
  8  3              # Method used for two-loop calculation
  9  1              # Gaugeless limit used at two-loop
 10  1              # safe-mode used at two-loop
 11 1               # calculate branching ratios
 13 0               # 3-Body decays: none (0), fermion (1), scalar (2), both (3)
 14 0               # Run couplings to scale of decaying particle
 12 1.000E-30       # write only branching ratios larger than this value
 15 1.000E-30       # write only decay if width larger than this value
 16 0               # One-loop decays
 19 -2              # Matching order (-2:automatic, -1:pole, 0-2: tree, one- & two-loop)
 31 -1              # fixed GUT scale (-1: dynamical GUT scale)
 32 0               # Strict unification
 34 1.000E-04       # Precision of mass calculation
 35 40              # Maximal number of iterations
 36 5               # Minimal number of iterations before discarding points
 37 1               # Set Yukawa scheme
 38 1               # 1- or 2-Loop RGEs
 50 0               # Majorana phases: use only positive masses (put 0 to use file with CalcHep/Micromegas!)
 51 0               # Write Output in CKM basis
 52 0               # Write spectrum in case of tachyonic states
 55 0               # Calculate loop corrected masses
 57 1               # Calculate low energy constraints
 65 1               # Solution tadpole equation
 66 1               # Two-Scale Matching
 67 1               # effective Higgs mass calculation
 75 0               # Write WHIZARD files
 76 2               # Write HiggsBounds file
 77 0               # Output for MicrOmegas (running masses for light quarks; real mixing matrices)
 78 1               # Output for MadGraph (writes also vanishing blocks)
 79 1               # Write WCXF files (exchange format for Wilson coefficients)
 86 0.              # Maximal width to be counted as invisible in Higgs decays; -1: only LSP
 440 1               # Tree-level unitarity constraints (limit s->infinity)
 441 1               # Full tree-level unitarity constraints
 442 1000.           # sqrt(s_min)
 443 2000.           # sqrt(s_max)
 444 5               # steps
 445 0               # running
 510 1.              # Write tree level values for tadpole solutions
 515 0               # Write parameter values at GUT scale
 520 0.              # Write effective Higgs couplings (HiggsBounds blocks): put 0 to use file with MadGraph!
 521 0.              # Diphoton/Digluon widths including higher order
 525 0.              # Write loop contributions to diphoton decay of Higgs
 530 0.              # Write Blocks for Vevacious
Block DECAYOPTIONS   # Options to turn on/off specific decays
1    1     # Calc 3-Body decays of Fu. I have turned them off/on to make it work for HiggsBounds (only NDA=2 allowed)
2    1     # Calc 3-Body decays of Fe.
3    1     # Calc 3-Body decays of Fd.
1001 0     # Loop Decay of Fu
1002 0     # Loop Decay of Fe
1003 0     # Loop Decay of Fd
1004 0     # Loop Decay of hh
1005 0     # Loop Decay of Ah
1006 0     # Loop Decay of Hm
1114 0.     # U-factors (0: off, 1:p2_i=m2_i, 2:p2=0, p3:p2_i=m2_1)
1115 0.     # Use loop-corrected masses for external states
1116 0.     # OS values for W,Z and fermions (0: off, 1:g1,g2,v 2:g1,g2,v,Y_i)
1117 0.     # Use defined counter-terms
1118 0.     # Use everywhere loop-corrected masses for loop-induced decays
""")


# In[20]:


def write_input_LHE(Mhh1,Mhh2,Mhh3,MAh2,M_Ah3,MHm,beta,a2,a3,delta,gamma1,OeL,OeR,Oa,vlu,vru,vld,vrd,ANGLES,UCKM,OZA,num_OMNS,list_eigvals,UmixNu,eigVel):
#creation of a function like the write_mg_cards fo the LesHouches file
     with open(os.path.join(Resutls_Env,'input_LHE.BGLNCS'),'w') as f:
        f.write("""
#----------------------------------------------------------
# Higgs masses information:
#----------------------------------------------------------
hh1: {0} GeV
hh2: {1} GeV
hh3: {2} GeV
Ah2: {3} GeV
Ah3: {4} GeV
Hm: {5} GeV""".format(Mhh1,Mhh2,Mhh3,MAh2,M_Ah3,MHm))
        f.write("""
#----------------------------------------------------------
# Lepton masses:
#----------------------------------------------------------
Fe_1: {0} GeV
Fe_2: {1} GeV
Fe_3: {2} GeV""".format(eigVel[0],eigVel[1],eigVel[2]))
        f.write("""
#----------------------------------------------------------
# Neutrino masses information:
#----------------------------------------------------------
light neutrino 1: {0}
light neutrino 2: {1}
light neutrino 3: {2}
heavy neutrino 1: {3}
heavy neutrino 2: {4}
heavy neutrino 3: {5}""".format(list_eigvals[0],list_eigvals[1],list_eigvals[2],list_eigvals[3],list_eigvals[4],list_eigvals[5]))
        f.write("""
#----------------------------------------------------------
# Angles & vevs information:
#----------------------------------------------------------
beta:  {0}
a2: {1}
a3: {2}
delta: {3}
gamma1: {4}
z1:  {5}
z2:  {6}
phi1:  {7}
phi2:  {8}
theta1:  {9}
theta2:  {10}
theta3:  {11}
Yuka_delta: {12}""".format(beta,a2,a3,delta,gamma1,ANGLES[0],ANGLES[1],ANGLES[2]\
,ANGLES[3],ANGLES[4],ANGLES[5],ANGLES[6],ANGLES[6]))
        f.write("""
#----------------------------------------------------------
#Mixing Matrices
#----------------------------------------------------------""")
        f.write("""
ZH_SCALARMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(Oa[0,0],Oa[0,1],Oa[0,2],Oa[1,0],Oa[1,1],Oa[1,2],Oa[2,0],Oa[2,1],Oa[2,2]))
        f.write("""
ZA_SCALARMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(OZA[0,0],OZA[0,1],OZA[0,2],OZA[1,0],OZA[1,1],OZA[1,2],OZA[2,0],OZA[2,1],OZA[2,2]))
        f.write("""
UDLMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(np.real(vld[0,0]),np.real(vld[0,1]),np.real(vld[0,2]),\
            np.real(vld[1,0]),np.real(vld[1,1]),np.real(vld[1,2]),\
            np.real(vld[2,0]),np.real(vld[2,1]),np.real(vld[2,2])))
        f.write("""
IMUDLMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(np.imag(vld[0,0]),np.imag(vld[0,1]),np.imag(vld[0,2]),\
            np.imag(vld[1,0]),np.imag(vld[1,1]),np.imag(vld[1,2]),\
            np.imag(vld[2,0]),np.imag(vld[2,1]),np.imag(vld[2,2])))
        f.write("""
UDRMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(np.real(vrd[0,0]),np.real(vrd[0,1]),np.real(vrd[0,2]),\
    np.real(vrd[1,0]),np.real(vrd[1,1]),np.real(vrd[1,2]),\
    np.real(vrd[2,0]),np.real(vrd[2,1]),np.real(vrd[2,2])))
        f.write("""
IMUDRMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(np.imag(vrd[0,0]),np.imag(vrd[0,1]),np.imag(vrd[0,2]),\
    np.imag(vrd[1,0]),np.imag(vrd[1,1]),np.imag(vrd[1,2]),\
    np.imag(vrd[2,0]),np.imag(vrd[2,1]),np.imag(vrd[2,2])))
        f.write("""
UULMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(np.real(vlu[0,0]),np.real(vlu[0,1]),np.real(vlu[0,2]),\
            np.real(vlu[1,0]),np.real(vlu[1,1]),np.real(vlu[1,2]),\
            np.real(vlu[2,0]),np.real(vlu[2,1]),np.real(vlu[2,2])))
        f.write("""
IMUULMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(np.imag(vlu[0,0]),np.imag(vlu[0,1]),np.imag(vlu[0,2]),\
            np.imag(vlu[1,0]),np.imag(vlu[1,1]),np.imag(vlu[1,2]),\
            np.imag(vlu[2,0]),np.imag(vlu[2,1]),np.imag(vlu[2,2])))
        f.write("""
UURMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(np.real(vru[0,0]),np.real(vru[0,1]),np.real(vru[0,2]),\
    np.real(vru[1,0]),np.real(vru[1,1]),np.real(vru[1,2]),\
    np.real(vru[2,0]),np.real(vru[2,1]),np.real(vru[2,2])))
        f.write("""
UELMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(np.real(OeL[0,0]),np.real(OeL[0,1]),np.real(OeL[0,2]),
                    np.real(OeL[1,0]),np.real(OeL[1,1]),np.real(OeL[1,2]),
                    np.real(OeL[2,0]),np.real(OeL[2,1]),np.real(OeL[2,2])))
        f.write("""
IMUELMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(np.imag(OeL[0,0]),np.imag(OeL[0,1]),np.imag(OeL[0,2]),
                    np.imag(OeL[1,0]),np.imag(OeL[1,1]),np.imag(OeL[1,2]),
                    np.imag(OeL[2,0]),np.imag(OeL[2,1]),np.imag(OeL[2,2])))
        f.write("""
UERMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(np.real(OeR[0,0]),np.real(OeR[0,1]),np.real(OeR[0,2]),
                    np.real(OeR[1,0]),np.real(OeR[1,1]),np.real(OeR[1,2]),
                    np.real(OeR[2,0]),np.real(OeR[2,1]),np.real(OeR[2,2])))
        f.write("""
IMUERMIX :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(np.imag(OeR[0,0]),np.imag(OeR[0,1]),np.imag(OeR[0,2]),
                    np.imag(OeR[1,0]),np.imag(OeR[1,1]),np.imag(OeR[1,2]),
                    np.imag(OeR[2,0]),np.imag(OeR[2,1]),np.imag(OeR[2,2])))
        f.write("""
UCKM :
1 1: {0}
1 2: {1}
1 3: {2}
2 1: {3}
2 2: {4}
2 3: {5}
3 1: {6}
3 2: {7}
3 3: {8}""".format(UCKM[0,0],UCKM[0,1],UCKM[0,2],
                UCKM[1,0],UCKM[1,1],UCKM[1,2],
                UCKM[2,0],UCKM[2,1],UCKM[2,2]))
        f.write("""
PMNS:
1 1: {0}
1 2: {1}
1 3: {2}
1 4: {3}
1 5: {4}
1 6: {5}
2 1: {6}
2 2: {7}
2 3: {8}
2 4: {9}
2 5: {10}
2 6: {11}
3 1: {12}
3 2: {13}
3 3: {14}
3 4: {15}
3 5: {16}
3 6: {17}
""".format(num_OMNS[0,0],num_OMNS[0,1],num_OMNS[0,2],num_OMNS[0,3],num_OMNS[0,4],num_OMNS[0,5],
                num_OMNS[1,0],num_OMNS[1,1],num_OMNS[1,2],num_OMNS[1,3],num_OMNS[1,4],num_OMNS[1,5],
                num_OMNS[2,0],num_OMNS[2,1],num_OMNS[2,2],num_OMNS[2,3],num_OMNS[2,4],num_OMNS[2,5]))        
        f.write("""
Neutrino Mixing:
1 1: {0}
1 2: {1}
1 3: {2}
1 4: {3}
1 5: {4}
1 6: {5}
2 1: {6}
2 2: {7}
2 3: {8}
2 4: {9}
2 5: {10}
2 6: {11}
3 1: {12}
3 2: {13}
3 3: {14}
3 4: {15}
3 5: {16}
3 6: {17}
4 1: {18}
4 2: {19}
4 3: {20}
4 4: {21}
4 5: {22}
4 6: {23}
5 1: {24}
5 2: {25}
5 3: {26}
5 4: {27}
5 5: {28}
5 6: {29}
6 1: {30}
6 2: {31}
6 3: {32}
6 4: {33}
6 5: {34}
6 6: {35}""".format(UmixNu[0,0],
UmixNu[0,1],
UmixNu[0,2],
UmixNu[0,3],
UmixNu[0,4],
UmixNu[0,5],
UmixNu[1,0],
UmixNu[1,1],
UmixNu[1,2],
UmixNu[1,3],
UmixNu[1,4],
UmixNu[1,5],
UmixNu[2,0],
UmixNu[2,1],
UmixNu[2,2],
UmixNu[2,3],
UmixNu[2,4],
UmixNu[2,5],
UmixNu[3,0],
UmixNu[3,1],
UmixNu[3,2],
UmixNu[3,3],
UmixNu[3,4],
UmixNu[3,5],
UmixNu[4,0],
UmixNu[4,1],
UmixNu[4,2],
UmixNu[4,3],
UmixNu[4,4],
UmixNu[4,5],
UmixNu[5,0],
UmixNu[5,1],
UmixNu[5,2],
UmixNu[5,3],
UmixNu[5,4],
UmixNu[5,5]))        



# # MadGraph Function

# In[21]:


def RunMG(MG_Path):
    with open('RMGcommands.txt', 'w') as f:
        f.write(""" set automatic_html_opening False 
import model BGLNCS -modelname  
generate g g > ah2
add process g g > h1
add process g g > h2
add process g g > h3
add process g g > d3bar d3 ah2
add process g g > d3bar d3 ah3
add process g g > d3bar d3 h1
add process g g > d3bar d3 h2
add process g g > d3bar d3 h3
output MGgg
y 
launch
0 
SPheno.spc.BGLNCS
0
quit  """)

    os.system(MG_Path + ' RMGcommands.txt')


# In[22]:


def ZH_matrix(delta,beta,a2,a3):
    a1 = beta + delta
    Oa4 = Matrix([[cos(beta), sin(beta), 0],
                [-sin(beta) , cos(beta), 0],
                [0, 0, 1]])

    Oa1 = Matrix([[cos(a1), -sin(a1), 0],
                [sin(a1) , cos(a1), 0],
                [0, 0, 1]])


    Oa2 =  Matrix([[cos(a2), 0, sin(a2)],
                   [0, 1, 0],
                   [- sin(a2) ,0, cos(a2)]])

    Oa3 =  Matrix([[1,0,0],
                   [0, cos(a3), -sin(a3)],
                   [0, sin(a3), cos(a3)]])
    Oa = simplify(np.dot(np.dot(Oa3,Oa2),np.dot(Oa1,Oa4)))

    return Oa

def ZA_matrix(beta,gamma1):
    Oa4 = Matrix([[cos(beta), sin(beta), 0],
                [-sin(beta) , cos(beta), 0],
                [0, 0, 1]])

    Oa3 =  Matrix([[1,0,0],
                   [0, cos(gamma1), -sin(gamma1)],
                   [0, sin(gamma1), cos(gamma1)]])
    Oa = simplify(np.dot(Oa3,Oa4))

    return Oa


# In[23]:


def readMGgz(bath):
    Pass= False
    MGdata = []
    with gzip.open(bath, 'rt') as f:
         for line in f:
            if line.startswith('<init>'):Pass = True        
            if line.startswith('</init>'):Pass = False
            if Pass:
                MGdata.append(line.strip('\n'))
    for l in range(1,len(MGdata)-1):
        if  float(MGdata[l].split()[-1])==2: p2X,p2xD = float(MGdata[l].split()[0]),float(MGdata[l].split()[1]) 
        if  float(MGdata[l].split()[-1])==3: p3X,p3xD = float(MGdata[l].split()[0]),float(MGdata[l].split()[1]) 
        if  float(MGdata[l].split()[-1])==4: p4X,p4xD = float(MGdata[l].split()[0]),float(MGdata[l].split()[1])
        if  float(MGdata[l].split()[-1])==5: p5X,p5xD = float(MGdata[l].split()[0]),float(MGdata[l].split()[1]) 
        if  float(MGdata[l].split()[-1])==6: p6X,p6xD = float(MGdata[l].split()[0]),float(MGdata[l].split()[1]) 
        if  float(MGdata[l].split()[-1])==7: p7X,p7xD = float(MGdata[l].split()[0]),float(MGdata[l].split()[1]) 
        if  float(MGdata[l].split()[-1])==8: p8X,p8xD = float(MGdata[l].split()[0]),float(MGdata[l].split()[1]) 
        if  float(MGdata[l].split()[-1])==9: p9X,p9xD = float(MGdata[l].split()[0]),float(MGdata[l].split()[1]) 
    
    lisyMG = [p2X,p2xD,p3X,p3xD,p4X,p4xD,p5X,p5xD,p6X,p6xD,p7X,p7xD,p8X,p8xD,p9X,p9xD]
    return lisyMG

def WdataSPheno(name,SphenoDataList):
    with open('{}.txt'.format(name), 'w') as csv_file:
        csv_writer = csv.writer(csv_file, delimiter=',')
        csv_writer.writerow(SphenoDataList)


# In[24]:


def cleaning_functiont():
    #Running_Env
    #Resutls_Env
    #Result_data
    list_output_files=[Resutls_Env,Running_Env]
    for cleaning_folder in list_output_files:
        delete_compant = 'rm -r'+ ' ' +'/{}'.format(cleaning_folder)
        os.system(delete_compant)


# ## Running the main program

# In[25]:


list = os.listdir(Spheno_output_Folder) # dir is your directory path
counter = len(list)
#print("number of points",counter)
# W_counter will stop the loop after that number of points


# In[26]:


break_code1 = 'go'
break_code2 = 'go'

W_counter = counter + nupoints


# In[27]:


def RanAngles():
    #a2 = np.random.uniform(-np.pi/2,np.pi/2)
    a3 = np.random.uniform(-np.pi/2,np.pi/2)
    beta = np.random.uniform(np.arctan(0.5),np.arctan(35))
    delta1 = np.random.uniform(np.pi/2 -0.8,np.pi/2 + 0.8)
    a2 = beta + delta1
    delta = np.random.uniform(np.pi/2 -0.8,np.pi/2 + 0.8)
    gamma1 = np.random.uniform(np.pi/2 - 0.5,np.pi/2 + 0.5) #np.random.uniform(np.arccos(1),np.arccos(0.9))
    return a2,a3,beta,delta,gamma1
def higgsSQ_masses():
    mass = np.random.uniform(200**2,700**2)
    mH1sq = 125.09**2
    mH2sq = np.random.uniform(pow(125.09,2),pow(800,2))
    mH3sq = np.random.uniform(pow(125.09,2),pow(800,2))
    mCh = np.random.uniform(pow(20,2),pow(800,2))
    mAh2sq = np.random.uniform(pow(10,-7),pow(10,-5))
    mAh3sq = np.random.uniform(pow(20,2),pow(800,2))
    if mAh2sq > mAh3sq:
        mAh2sq , mAh3sq = mAh3sq , mAh2sq
    if mH2sq > mH3sq:
        mH2sq , mH3sq = mH3sq , mH2sq
    return mH1sq,mH2sq,mH3sq,mCh,mAh2sq,mAh3sq
def higgs_masses(mH1sq,mH2sq,mH3sq,mCh,mAh2sq,mAh3sq):
    Mhh1,Mhh2,Mhh3 = np.sqrt(mH1sq),np.sqrt(mH2sq),np.sqrt(mH3sq)
    MAh2,M_Ah3 = np.sqrt(mAh2sq),np.sqrt(mAh3sq)
    MHm = np.sqrt(mCh)
    return Mhh1,Mhh2,Mhh3,MAh2,M_Ah3,MHm
def VEVs(beta):
    v = 246
    v_u = np.cos(beta)*v
    v_d = np.sin(beta)*v
    v3 = np.random.uniform(7*pow(10,2),7*pow(10,3))
    return v,v_u,v_d,v3

def Fanction_alphas():
    galpha_1 = np.random.uniform(-1*pow(10,-4),+1*pow(10,-4))
    galpha_2 = np.random.uniform(-1*pow(10,-4),+1*pow(10,-4))
    galpha_3 = np.random.uniform(-1*pow(10,-4),+1*pow(10,-4))
    return galpha_1,galpha_2,galpha_3


# In[ ]:


while W_counter > counter:
#if break_code1=='go':
    #BSM squared masses
    #=================================================
    mH1sq,mH2sq,mH3sq,mCh,mAh2sq,mAh3sq  = higgsSQ_masses()
    #=================================================
    #BSM masses
    Mhh1,Mhh2,Mhh3,MAh2,M_Ah3,MHm = higgs_masses(mH1sq,mH2sq,mH3sq,mCh,mAh2sq,mAh3sq)
    #=================================================
    # angles
    a2,a3,beta,delta,gamma1 = RanAngles()
    #=================================================
    v,v_u,v_d,v3 = VEVs(beta)
    #=================================================
    print("Got Random inputs")
    if break_code1=='go':
        # the g symbols the numerical values of the couplings, rundom values for alphas exept alpha_4 which is given below
        #=================================================
        galpha_1,galpha_2,galpha_3 = Fanction_alphas()
        alpha_1,alpha_2,alpha_3=galpha_1,galpha_2,galpha_3
        #=================================================

        # the g symbols the numerical values of the couplings based on the inverted equations
        #=================================================
        gl1 =  Fuction_l1(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,a2,a3)
        gl2 =  Fuction_l2(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,a2,a3)
        gl3 =  Fuction_l3(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3)
        gl4 = Fuction_l4(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3)
        gl_s1= Fuction_l_s1(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3)
        gl_s2= Fuction_l_s2(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3)
        gl_s3= Fuction_l_s3(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3)
        galpha_4= Fuction_alpha4(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3)
        gmu_3 =  Fuction_mu3(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3)
        gmusb = Fuction_musb(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3)
        #=================================================
        coupli_list = [gl1,gl2,gl3,gl_s1,gl_s2,gl_s3,galpha_1,galpha_2,galpha_3,galpha_4,gmu_3,gmusb]
        #=================================================
        Alim1 = 2.
        Alim2 = -2.
        gb2 = 0
        #Upl = 8*3.14
        Upl = 5

        if abs(gl1) < Upl and abs(gl2) < Upl and abs(gl3) < Upl and abs(gl4) < Upl :
            if abs(gl_s1) < Upl and abs(gl_s2) < Upl and abs(gl_s3) < Upl :
                if abs(galpha_4) < Upl:
                    if break_code1=='go':
                        if break_code1=='go':
                            if break_code1=='go':
                                print("Got Lagrangian parameters")
                                #The line below assigns the Yukawa couplings to variables for quarks.
                                Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,                                Y2d31,Y2d32,Y2d33,vlu,vru,vld,vrd,ANGLES,UCKM= Yukawa_alaysis(beta,v3)

                                #The line below assigns the Lepton & Neutrino Yukawa couplings to variables.
                                OeL,eigVel,OeR,Y1l11, Y1l12, Y1l21, Y1l22, Y2l33,                                Y1n11,Y1n12,Y1n21,Y1n22,Y2n33,BB11,BB22,BB12,BB21,C13,C31,C23,C32,num_OMNS,list_eigvals,UmixNu,eigVel= Lepton_neutrino_Yukawa_couplings_BGL(v3,v_u,v_d)
                                print("Got Yukawas")
                                
                                #SLHA for MG
                                write_MG_LesHouches(gl1, gl2, gl3, gl4, gl_s1,gl_s2, gl_s3,gmu_3,gmusb,                                galpha_1,galpha_2,galpha_3,galpha_4,v3,Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23,                                Y2d31, Y2d32, Y2d33, Y1u11, Y1u12, Y1u21, Y1u22, Y2u33,Y1l11, Y1l12, Y1l21, Y1l22,                                Y2l33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, BB11,BB22,BB12,BB21, C13, C23, C31, C32,v_u,v_d)

                                # create the SPheno output for MG
                                MG_BGL_Running_comand = spheno_BGL + ' ' + os.path.join(Running_Env,'spheno','LesHouches.in.BGLNCSmg')
                                os.system(MG_BGL_Running_comand)
                                #Runs MG
                                RunMG(MG_Path)
                                
                                if os.path.exists('MGgg/Events/run_01/unweighted_events.lhe.gz'):
    
                                    #This line, creates an spheno_LesHouches, which will be latter used into SPheno
                                    write_spheno_LesHouches(gl1, gl2, gl3, gl4, gl_s1,gl_s2, gl_s3,gmu_3,gmusb,                                    galpha_1,galpha_2,galpha_3,galpha_4,v3,Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23,                                    Y2d31, Y2d32, Y2d33, Y1u11, Y1u12, Y1u21, Y1u22, Y2u33,Y1l11, Y1l12, Y1l21, Y1l22,                                    Y2l33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, BB11,BB22,BB12,BB21, C13, C23, C31, C32,v_u,v_d)
                                    #This line, creates an FS_LesHouches, which will be latter used into SPheno
                                    #write_FlexibleSUSY_LesHouches(gl1, gl2, gl3, gl4, gAd2, gd1, gd2, gd3, g4, g41, g14, v3,Yd1_R,Yd2_R,Yu1,Yu2,Ye,Yv,Yvv,v_u,v_d,beta)
                                    
                                    # create the SPheno output
                                    Spheno_BGL_Running_comand = spheno_BGL + ' ' + os.path.join(Running_Env,'spheno','LesHouches.in.BGLNCS')
                                    os.system(Spheno_BGL_Running_comand)


                                    #FS_BGL_Running_comand = FlexibleSUSY_BGL + ' ' + '--slha-input-file='+os.path.join(Running_Env,'FlexibleSUSY','LesHouches.in.THDMSBGL') + ' ' + '--slha-output-file='+os.path.join(Resutls_Env,'LesHouches.out.THDMSBGL')
                                    #os.system(FS_BGL_Running_comand)


                                    Oa = ZH_matrix(delta,beta,a2,a3)
                                    OZA = ZA_matrix(beta,gamma1)
                                    write_input_LHE(Mhh1,Mhh2,Mhh3,MAh2,M_Ah3,MHm,beta,a2,a3,delta,gamma1,OeL,OeR,Oa,vlu,vru,vld,vrd,ANGLES,UCKM,OZA,num_OMNS,list_eigvals,UmixNu,eigVel)
                                    
                                    #MG in txt
                                    
                                    lisyMG = readMGgz('MGgg/Events/run_01/unweighted_events.lhe.gz')
                                    WdataSPheno("MGdata",lisyMG)
                                    
                                    #This line, creates an HiggBounds output
                                    os.system(HiggsBounds + ' ' + 'LandH effC 5 1' + ' ' + Resutls_Env+"/")

                                    #This line, creates an HiggsSigan output
                                    os.system(HiggsSignals + ' ' + 'latestresults 2 effC 5 1' + ' '+Resutls_Env+"/" )
                                    
                                    #cleaning files
                                    if os.path.exists('Key.dat'): os.system('rm Key.dat')
                                    if os.path.exists('ME5_debug'): os.system('rm ME5_debug')
                                    if os.path.exists('peakobservables.dat'): os.system('rm peakobservables.dat')
                                    if os.path.exists('py.py'): os.system('rm py.py')
                                    if os.path.exists('MGgg'): os.system('rm -rf MGgg')
                                    if os.path.exists('BR_H_NP.dat'):os.system('rm BR_H_NP.dat')
                                    if os.path.exists('BR_Hplus.dat'):os.system('rm BR_Hplus.dat')
                                    if os.path.exists('BR_t.dat'):os.system('rm BR_t.dat')
                                    if os.path.exists('effC.dat'):os.system('rm effC.dat')
                                    if os.path.exists('Messages.out'):os.system('rm Messages.out')
                                    if os.path.exists('MH_GammaTot.dat'):os.system('rm MH_GammaTot.dat')
                                    if os.path.exists('MHplus_GammaTot.dat'):os.system('rm MHplus_GammaTot.dat')
                                    if os.path.exists('SPheno.out'):os.system('rm SPheno.out')
                                    if os.path.exists('fort.31'):os.system('rm fort.31')
                                    if os.path.exists('fort.38'):os.system('rm fort.38')
                                    if os.path.exists('fort.39'):os.system('rm fort.39')
                                    if os.path.exists('debug_channels.txt'):os.system('rm debug_channels.txt')
                                    if os.path.exists('debug_predratio.txt'):os.system('rm debug_predratio.txt')
                                    if os.path.exists('HS_correlations.txt'):os.system('rm HS_correlations.txt')
                                    if os.path.exists('STXS_analyses.txt'):os.system('rm STXS_analyses.txt')
                                    if os.path.exists('STXS_correlations.txt'):os.system('rm STXS_correlations.txt')
                                    if os.path.exists('RMGcommands.txt'):os.system('rm RMGcommands.txt')
                                    
                                    if break_code2=='go':
                                    #if float(delta_chi) < 7.815:

                                        counter += 1
                                        print("counter: {}".format(counter))

                                        os.system('cp' + ' '+ os.path.join(Running_Env,'spheno','LesHouches.in.BGLNCS')+ ' ' + Resutls_Env)
                                        os.system('cp' + ' '+ os.path.join(Running_Env,'spheno','LesHouches.in.BGLNCSmg')+ ' ' + Resutls_Env)

                                        # copy comant that saves the spheno output to a folder
                                        spc_BGL = Resutls_Env
                                        copy_comand_spheno = 'cp -r ' + spc_BGL + ' '+ Spheno_output_Folder +'/point_{}'.format(counter)

                                        os.system(copy_comand_spheno )
                                        #cleaning Resutls_Env
                                        os.system("rm -r {}/*".format(Resutls_Env))
                                        
                                        #if HiggsBounds_pass:
                                        #            if HiggsSignal_pass:
                                        #                print("The point passed all the requirments")

                                    if break_code2=='Not_go':
                                        counter=2*W_counter
                                        print('break_code2')
    if break_code1=='Not_go':
        counter=2*W_counter
        print("break_code1")


# In[45]:

print("The END")
cleaning_functiont()

