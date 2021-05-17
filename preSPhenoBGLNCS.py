#!/usr/bin/env python
# coding: utf-8

cluster = True # If you run the scrip on the cluster you need to add
nupoints = 1000 #number of points simulated
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
#from math import cos,sin,tan
import glob
import scipy.linalg as la


# In[3]:


print("Hello, this will take a while")

counter = 0


# The code tries to be as automatic as it could be. The command below gives the path where the code is saved
os.getcwd()


# # Path definitions

# based on the path where the code is saved, I tried to create a workign tree of
#files, which will make easier in the end to find the data. In case that someone
#else wants to use this code, he needs to change the path of the softwares below.

#The path where all the programs are
Working_Folder= os.getcwd()

if not cluster:
    #Spheno folder
    SPheno_Path= '~/SPheno-4.0.4'
    spheno_BGL = os.path.join(SPheno_Path,'bin/SPhenoBGLNCS')

if cluster:
    #SPheno cluster
    SPheno_Path= '~/opt/SPheno-4.0.4'
    spheno_BGL = os.path.join(SPheno_Path,'bin/SPhenoBGLNCS')

Running_Env = Working_Folder +'/Running_Env'
Resutls_Env = Working_Folder +'/Resutls_Env'
Result_data = Working_Folder +'/Result_data'

Spheno_output_Folder = Result_data + '/Spheno_output_Folder'

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


# In[7]:

os.chdir(Resutls_Env)

# In[8]:

os.getcwd()

# ## variables definitions

# in a case that we want to use this script for another model, all the next shells until the for loop need be changed, especialy the part which calculates the couplings. Important this script is using the sympy library alot

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

# couplings

# lambda_1
def Fuction_l1(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,a2,a3):
    l1 = (1/(2*v**2))*Sec(beta)**2*(mH1sq* Cos(a2)**2* Cos(delta)**2 - \
    Sin(beta)**2* (mAh2sq* Cos(gamma1)**2 + mAh3sq* Sin(gamma1)**2) + \
    Cos(a3)**2* (mH3sq* Cos(delta)**2* Sin(a2)**2 + mH2sq* Sin(delta)**2) + \
    Sin(a3)**2* (mH2sq* Cos(delta)**2* Sin(a2)**2 + mH3sq* Sin(delta)**2)\
    + (mH2sq - mH3sq)* Cos(a3)* Sin(a2)* Sin(a3)* Sin(2* delta))
    return l1

# lambda_2
def Fuction_l2(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,a2,a3):
    l2 =  (1/(2*v**2))*Csc(beta)**2* (mH3sq* Cos(delta)**2* Sin(a3)**2 - \
    Cos(beta)**2* (mAh2sq* Cos(gamma1)**2 + mAh3sq* Sin(gamma1)**2) + (mH1sq* Cos(a2)**2 + \
    mH2sq* Sin(a2)**2* Sin(a3)**2)* Sin(delta)**2 + Cos(a3)**2* (mH2sq* Cos(delta)**2 + \
    mH3sq* Sin(a2)**2* Sin(delta)**2) + (-mH2sq + mH3sq)* Cos(a3)* Sin(a2)*\
    Sin(a3)* Sin(2* delta))
    return l2

# lambda_3
def Fuction_l3(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3):
    l3 = (1/(8* v**2))*(-4* (mAh2sq + mAh3sq - 4* mCh) + \
    4*(-mAh2sq + mAh3sq)* Cos(2* gamma1) + Csc(beta)* Sec(beta)* \
    (4* (mH2sq - mH3sq)* Cos(2* delta)* Sin(a2)* Sin(2* a3) + (2* \
    (-2* mH1sq + mH2sq + mH3sq)* Cos(a2)**2 - (mH2sq - mH3sq)* (-3 + Cos(2* a2))* \
    Cos(2* a3))* Sin(2* delta)))
    return l3

# lambda_4
def Fuction_l4(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3):
    l4 = (mAh2sq + mAh3sq - 2* mCh + (mAh2sq - mAh3sq)* Cos(2*gamma1))/v**2
    return l4
# lambda_{sigma 1}
def Fuction_l_s1(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    ls1 = (2*v3*(mH1sq* Sin(a2)**2 + Cos(a2)**2* (mH3sq* Cos(a3)**2 + mH2sq* Sin(a3)**2)) +\
    Sqrt(2)* v**2* Cos(beta)* Sin(beta)* (alpha_1 + alpha_2))/(4* v3**3)
    return ls1

# lambda_{sigma 2}
def Fuction_l_s2(v,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    ls2 = (1/(v*v3))*(Cos(a2)* Sec(beta)* (Cos(delta)* Sin(a2)* \
    (mH1sq - mH3sq* Cos(a3)**2 - mH2sq* Sin(a3)**2) + (-mH2sq + mH3sq)* Cos(a3)*\
    Sin(a3)* Sin(delta)) + (mAh2sq - mAh3sq)* Cos(gamma1)* Sin(gamma1)* Tan(beta) -\
    v* (Sqrt(2)* alpha_1 + 2* v3* alpha_3)* Tan(beta))
    return ls2

# lambda_{sigma 3}
def Fuction_l_s3(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    ls3 = (1/(v*v3))*((mAh2sq - mAh3sq)* Cos(gamma1)* Cot(beta)* Sin(gamma1) + \
    Cos(a2)* Csc(beta)* ((-mH2sq + mH3sq)* Cos(a3)* Cos(delta)* Sin(a3) + Sin(a2)*\
    (-mH1sq + mH3sq* Cos(a3)**2 + mH2sq* Sin(a3)**2)* Sin(delta)) -\
    v* Cot(beta)* (Sqrt(2)* alpha_1 + 2* v3* alpha_3))
    return ls3

# alpha_4
def Fuction_alpha4(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    alpha_4 =  ((-mAh2sq + mAh3sq)* Sin(2* gamma1) + Sqrt(2)* v* alpha_1 - Sqrt(2)* v*\
    alpha_2 + 2* v* v3* alpha_3)/(2* v* v3)
    return alpha_4

# mu_3
def Fuction_mu3(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    mu3 = (-v *(mAh2sq + mAh3sq + (mAh2sq - mAh3sq)* Cos(2* gamma1))* Sin(\
    2 *beta) + (mAh2sq - mAh3sq)* v3* Sin(2* gamma1) + v* v3* (-Sqrt(2)* (3* alpha_1 + alpha_2) -\
    4* v3* alpha_3))/(4* v)
    return mu3

# mu_{sigma b}
def Fuction_musb(v,v3,beta,delta,mH1sq,mH2sq,mH3sq,mAh2sq,mAh3sq,mCh,a2,a3,alpha_1,alpha_2,alpha_3):
    musb = (-(mAh2sq + mAh3sq)*v3 + (mAh2sq - mAh3sq)*(v3* Cos(2* gamma1) + v* Sin(2* beta)* Sin(2* gamma1)) + \
    v**2* Cos(beta)* Sin(beta)* (-3*Sqrt(2)* alpha_1 + Sqrt(2)* alpha_2 - 8* v3* alpha_3))/(4* v3)
    return musb

def Unitary3b3matrix(phi1,phi2,phi3,z1,delta1,z2,delta2,z3,delta3):
    mat1 = np.diag([np.exp(1.j*phi1),np.exp(1.j*phi2),np.exp(1.j*phi3)])

    mat2 = np.array([[Cos(z1),Sin(z1)*np.exp(1.j*delta1),0],
                     [-Sin(z1)*np.exp(-1.j*delta1),Cos(z1),0],
                    [0,0,1]])

    mat3 = np.array([[Cos(z2),0,-Sin(z2)*np.exp(-1.j*delta2)],
                    [0,1,0],
                    [Sin(z2)*np.exp(1.j*delta2),0,Cos(z2)]])

    mat4 = np.array([[1,0,0],
                    [0,Cos(z3),Sin(z3)*np.exp(1.j*delta3)],
                    [0,-Sin(z3)*np.exp(-1.j*delta3),Cos(z3)]])
    UniMatrix = np.dot(np.dot(mat1,mat2),np.dot(mat3,mat4))
    return UniMatrix

def Unitary2b2matrix(phi1,phi2,phi3,z1,delta1):
        #Unitary U^u_L matrix
     #Unitary U^u_L matrix
    mat1 = np.array([[1., 0.], [0., np.exp(1j*(phi1 - phi2))]])
    mat2 = np.array([[np.cos(z1), np.sin(z1)], [-np.sin(z1), np.cos(z1)]])
    mat3 = np.array([[np.exp(1j*phi3), 0.], [0., np.exp(1j*phi2)]])
    UniMatrix = np.matmul(mat1, np.matmul(mat2, mat3))
    UniMatrix = np.block([[UniMatrix, np.zeros((2, 1))], [np.zeros((1, 2)), 1.0]])
    return UniMatrix

def Unitary3x3(alpha_11,alpha_12,alpha_13,alpha_23,alpha_33,theta_12,theta_13,theta_23,delta):
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
                    np.sin(theta_12)*np.sin(theta_23) - np.cos(theta_12)*np.cos(theta_23)*np.sin(theta_13)*np.exp(1j*delta),
                    - np.cos(theta_12)*np.sin(theta_23) - np.sin(theta_12)*np.cos(theta_23)*np.sin(theta_13)*np.exp(1j*delta),
                    np.cos(theta_23)*np.cos(theta_13)
                    ]]
                    )
    mat9 = np.array([[np.exp(1j*alpha_11), 0., 0.], [0., np.exp(1j*alpha_12), 0.], [0., 0., np.exp(1j*alpha_13)]])

    unitary3x3 = np.matmul(mat7, np.matmul(mat8, mat9))
    return unitary3x3
def nineRandomAngles():
    phi1,phi2,phi3 = np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi)
    phi4,phi5,phi6 = np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi)
    phi7,phi8,phi9 = np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi)
    return phi1,phi2,phi3,phi4,phi5,phi6,phi7,phi8,phi9
def FiveRandomAngles():
    phi1,phi2,phi3,phi4,phi5 = np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi),np.random.uniform(0,2*np.pi)
    return phi1,phi2,phi3,phi4,phi5

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


    # Mass parameters
    #------------------
    Mu = np.array(0.00122)
    Mc = np.array(0.590)
    Mt = np.array(162.9)
    Md = np.array(0.00276)
    Ms = np.array(0.052)
    Mb = np.array(2.75)

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
    return Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,\
            Y2d31,Y2d32,Y2d33,vlu,vru,vld,vrd,ANGLES,vckm

def numeri_neutrino_masses(v1,v2,v3):
    #Ya = np.random.uniform(0,1)*10**3
    #Yb = np.random.uniform(0,1)
    #Yb1 = np.random.uniform(0,1)
    #Yv1 = np.random.uniform(0,1)*10**(-1)
    #Yv2 = np.random.uniform(0,1)*10**(-1)
    #Yv3 = np.random.uniform(0,1)*10**(-1)
    #Yv4 = np.random.uniform(0,1)*10**(-1)
    Y = np.random.uniform(0,1)*10**(-5)
    Ya = np.random.uniform(0,1)*10**3
    Yb = np.random.uniform(0,1)
    Yb1 = Yb
    Yv1 = Y
    Yv2 = Y
    Yv3 = Y
    Yv4 = Y
    Fv = (1/np.sqrt(2))*np.array([[0,0,0,v1*Yv1, v2*Yv3,0],
                                  [0,0,0,0,v1*Yv2,0],
                                  [0,0,0,0,0,v2*Yv4],
                                  [v1*Yv1,0,0,np.sqrt(2)* Ya,0,0],
                                  [v2*Yv3,v1*Yv2,0,0,0,(v3*Yb)/2 + (v3*Yb1)/2],
                                  [0,0,v2*Yv4,0,(v3*Yb)/2 + (v3*Yb1)/2,0]])

    SQFv = np.dot(Fv.T,Fv)

    tt = np.sqrt(SQFv)
    a,b =la.eig(tt)
    idx = abs(a).argsort()[::-1]
    eigenValues = a[idx]
    eigenVectors = b[idx].T

    Mhe_1,Mhe_2,Mhe_3,Ml_1,Ml_2,Ml_3=eigenValues

    return Ml_1,Ml_2,Ml_3,Mhe_1,Mhe_2,Mhe_3,Yv1,Yv2,Yv3,Yv4,Ya,Yb1,Yb

def old_neutrino_masses(v1,v2,v3):

    vs= v3
    Ya = np.random.uniform(0,1)*10**3
    Yb = np.random.uniform(0,1)
    Yb1 = Yb

    Y = np.random.uniform(0,1)*10**(-5)
    Yv1=Y
    Yv2=Y
    Yv3=Y
    Yv4=Y


    Yv = np.array([[Y,Y,0],
                  [0,Y,0],
                  [0,0,Y]])
    Yvv = np.array([[Ya,0,0],
                   [0,0,Yb],
                   [0,Yb,0]])

    #Heavy neutrinos masses
    Mhe_1 = Ya
    Mhe_2 = Yb*vs/np.sqrt(2)
    Mhe_3 = Yb*vs/np.sqrt(2)

    #Light neutrino masses
    Ml_1 = (v1**4*Y**2)/(2*Ya*v1**2 + 2*Ya*v2**2)
    Ml_2 = (v2*np.sqrt(2*Ya*(v1**2 + v2**2) + (np.sqrt(2)*Yb*v1**2*v2*vs)/np.sqrt(v1**2 + v2**2))*Y**2)/(2*np.sqrt(Ya)*Yb*vs)
    Ml_3 = (v2*np.sqrt(2*Ya**2 * (v1**2 + v2**2)**(7/2) - np.sqrt(2)*Ya*Yb*v1**2*v2*(v1**2 + v2**2)**2*vs + 1/2*Yb**2*v1**4*np.sqrt(v1**2 + v2**2)*(2*v1**2 + v2**2) * vs**2)*Y**2)/(2 * Ya*Yb*(v1**2 + v2**2)**(5/4)*vs)

    return Ml_1,Ml_2,Ml_3,Mhe_1,Mhe_2,Mhe_3,Yv1,Yv2,Yv3,Yv4,Ya,Yb1,Yb



def Lepton_neutrino_Yukawa_couplings_BGL(v_s,v_u,v_d):
    v_sm = 246
    #definitios
    #mass parameters
    #------------------
    Me = np.array(0.000485289396)
    Mmu =np.array( 0.1024673155)
    Mtau = np.array(1.74215)

    m1,m2,m3 = Me,Mmu,Mtau
    #vevs
    #------------------
    vs = v_s
    v1 = v_u
    v2 = v_d
    v3 = vs
    #------------------


    z1 = np.random.uniform(0,2*np.pi)
    z2 = np.random.uniform(0,2*np.pi)
    phi1 = np.random.uniform(0,2*np.pi)
    phi2 = np.random.uniform(0,2*np.pi)
    #phi4 = np.random.uniform(0,2*np.pi)

    #U_e_L

    OeL =np.array([[Cos(z1), -Cos(phi1)*Sin(z1) - 1j* Sin(z1)*Sin(phi1), 0],
                    [Cos(phi1)*Sin(z1) - 1j* Sin(z1)*Sin(phi1),Cos(z1), 0],
                    [0, 0, 1]])


    OeR =np.array([[Cos(z2), -Cos(phi2)*Sin(z2) - 1j* Sin(z2)*Sin(phi2), 0],
                    [Cos(phi2)*Sin(z2) - 1j* Sin(z2)*Sin(phi2),Cos(z2), 0],
                    [0, 0, 1]])

    Y1e11 = (1/v1)*(Sqrt(2)* (m1* Cos(z1)* Cos(z2) + m2* Cos(phi1 - phi2)* \
    Sin(z1)* Sin(z2)) + 1.j *Sqrt(2)*m2* Sin(z1)* Sin(z2)* Sin(phi1 - phi2))

    Y1e22 = (1/v1)*(Sqrt(2)*(m2*Cos(z1)*Cos(z2) + m1*Cos(phi1 - phi2)* Sin(z1)*\
    Sin(z2)) - 1.j* Sqrt(2)*m1* Sin(z1)* Sin(z2)* Sin(phi1 - phi2))

    Y1e12 = (1/v1)*(-Sqrt(2)*(m2* Cos(z2)* Cos(phi1)* Sin(z1) - m1* Cos(z1)* \
    Cos(phi2)* Sin(z2)) - 1.j* Sqrt(2)* (m2* Cos(z2)* Sin(z1)* Sin(phi1) - m1* \
    Cos(z1)* Sin(z2)* Sin(phi2)))

    Y1e21 = (1/v1)*(Sqrt(2)* (m1* Cos(z2)* Cos(phi1)* Sin(z1) - m2* Cos(z1)* \
    Cos(phi2)* Sin(z2)) - 1.j* Sqrt(2)* (m1* Cos(z2)* Sin(z1)* Sin(phi1) - m2*\
    Cos(z1)* Sin(z2)* Sin(phi2)))

    Y2e33 = ((Sqrt(2)* m3)/v2)

    Y1l11, Y1l12, Y1l21, Y1l22, Y2l33 = Y1e11, Y1e12, Y1e21, Y1e22, Y2e33

    #heavy neutrino masses
    #------------------
    mheavy1 = pow(10,4)
    mheavy2 = 2*pow(10,4)
    Yb = (mheavy1 - mheavy2)/(Sqrt(2)* vs)
    Yc = -((Sqrt(mheavy1)*Sqrt(mheavy2))/vs)

    Y1n11 = pow(10,-6)
    Y1n12 = pow(10,-6)
    Y1n21 = pow(10,-6)
    Y1n22 = pow(10,-6)
    Y2n33 = pow(10,-6)
    B11,B12,B21,B22 = Yb,Yb,Yb,Yb
    C13,C23,C31,C32 = Yc,Yc,Yc,Yc

    return OeL,OeR,Y1l11, Y1l12, Y1l21, Y1l22, Y2l33, Y1n11, Y1n12, Y1n21, Y1n22,\
     Y2n33, B11, B12, B21, B22, C13, C23, C31, C32



# In[27]:


#for some reason seams to break the loop code this function (is not used for that)
def cleaning_function_SPheno_output():
    list_output_files = glob.glob('*')
    for cleaning_folder in list_output_files:
        delete_compant = 'rm'+ ' ' + Resutls_Env + '/{}'.format(cleaning_folder)
        os.system(delete_compant)



# In[28]:


#There is a problem all the Pvalues are zero. But that might be a problem of SPheno
def HiggsSignals_reader_limit_function():
    data_higgssignal = Resutls_Env + '/HiggsSignals_results.dat'
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


# In[29]:


def HiggsBounds_reader_limit_function():
    data_higgsbounds = Resutls_Env + '/HiggsBounds_results.dat'
    R = open( data_higgsbounds, 'r')
    for line in R:
        if not line.lstrip().startswith('#'):
            obsratio = float(line.split()[-2])
            HBresult = float(line.split()[-4])
            if float(line.split()[-2]) < 1 and HBresult == 1:
                HiggsBounds_pass = True
            else :  HiggsBounds_pass = False
    return HiggsBounds_pass, obsratio, HBresult



# In[30]:


def SPheno_reader():
    A = []
    EW_P_O_info = False
    data_spheno = Resutls_Env + '/SPheno.spc.THDMSBGL'
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


# In[31]:


def Masses_SPheno_reader():
    A = []
    EW_P_O_info = False
    data_spheno = Resutls_Env + '/SPheno.spc.THDMSBGL'
    R = open(data_spheno, 'r')
    for line in R:
        if line.startswith('Block MASS'):
            #Electroweak precision observables
            EW_P_O_info = True
        if line.startswith('Block ZH_SCALARMIXs'):EW_P_O_info=False
        if EW_P_O_info:
            A.append(line.strip('\n'))
            l0 = line.strip('\n')
    VZ_M=float(A[7].split()[1])
    VZp_M=float(A[8].split()[1])
    VW_M=float(A[9].split()[1])
    return VZ_M,VZp_M,VW_M


# In[32]:


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


# In[33]:


#for some reason seams to break the loop code this function (is not used for that)
def cleaning_function_SPheno_output():
    list_output_files = glob.glob('*')
    for cleaning_folder in list_output_files:
        delete_compant = 'rm'+ ' ' + Resutls_Env + '/{}'.format(cleaning_folder)
        os.system(delete_compant)



# In[34]:


#for some reason seams to break the loop code this function (is not used for that)
def cleaning_functiont():
    #Running_Env
    #Resutls_Env
    #Result_data
    list_output_files=[Resutls_Env,Running_Env]
    for cleaning_folder in list_output_files:
        delete_compant = 'rm -r'+ ' ' +'/{}'.format(cleaning_folder)
        os.system(delete_compant)


# # Creation of LesHouse files and run cards of MadGraph

# In[35]:


#creation of a function like the write_mg_cards fo the LesHouches file
def write_spheno_LesHouches(gl1, gl2, gl3, gl4, gl_s1,gl_s2, gl_s3,gmu_3,gmusb,\
galpha_1,galpha_2,galpha_3,galpha_4,v3,Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23,\
Y2d31, Y2d32, Y2d33, Y1u11, Y1u12, Y1u21, Y1u22, Y2u33,Y1l11, Y1l12, Y1l21, Y1l22,\
Y2l33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, B11, B12, B21, B22, C13, C23, C31, C32,v_u,v_d):
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
 46   {12}    # C32input""".format(Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, B11, B12, \
 B21, B22, C13, C23, C31, C32))
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
 67 0               # effective Higgs mass calculation
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


# In[36]:





#creation of a function like the write_mg_cards fo the LesHouches file
def write_input_LHE(Mhh1,Mhh2,Mhh3,MAh2,M_Ah3,MHm,beta,a2,a3,delta,gamma1,\
OeL,OeR,Oa,vlu,vru,vld,vrd,ANGLES,UCKM):
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
# Neutrino masses information:
#----------------------------------------------------------
light neutrino 1: {0}
light neutrino 2: {1}
light neutrino 3: {2}
heavy neutrino 1: {3}
heavy neutrino 2: {4}
heavy neutrino 3: {5}""".format(0,0,0,0,0,0))
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


# # Running the main program

list = os.listdir(Spheno_output_Folder) # dir is your directory path
counter = len(list)
#print("number of points",counter)
# W_counter will stop the loop after that number of points

break_code1 = 'go'
break_code2 = 'go'


# In[42]:


W_counter = counter + nupoints


# In[43]:


def RanAngles():
    a2 = np.random.uniform(0,np.pi)
    a3 = np.random.uniform(0,np.pi)
    beta = np.random.uniform(np.arctan(1),np.arctan(15))
    delta = np.random.uniform(np.arccos(1),np.arccos(0.9))
    gamma1 = np.random.uniform(np.arccos(1),np.arccos(0.9))
    return a2,a3,beta,delta,gamma1
def higgsSQ_masses():
    mass = np.random.uniform(200**2,700**2)
    mH1sq = 125.09**2
    mH2sq = np.random.uniform(pow(130,2),pow(500,2))
    mH3sq = np.random.uniform(pow(130,2),pow(500,2))
    mCh = np.random.uniform(pow(80,2),pow(500,2))
    mAh2sq = np.random.uniform(pow(10,-20),pow(10,-12))
    mAh3sq = np.random.uniform(pow(80,2),pow(500,2))
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
    v3 = np.random.uniform(pow(10,2),9*pow(10,2))
    return v,v_u,v_d,v3

def Fanction_alphas():
    galpha_1 = np.random.uniform(-1*pow(10,-4),-1*pow(10,-4))
    galpha_2 = np.random.uniform(-1*pow(10,-4),-1*pow(10,-4))
    galpha_3 = np.random.uniform(-1*pow(10,-4),-1*pow(10,-4))
    return galpha_1,galpha_2,galpha_3


# In[44]:


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
        Upl = 10

        if abs(gl1) < Upl and abs(gl2) < Upl and abs(gl3) < Upl and abs(gl4) < Upl :
            if abs(gl_s1) < Upl and abs(gl_s2) < Upl and abs(gl_s3) < Upl :
                if abs(galpha_4) < Upl:
                    if break_code1=='go':
                        if break_code1=='go':
                            if break_code1=='go':

                                #The line below assigns the Yukawa couplings to variables for quarks.
                                Y1u11,Y1u12,Y1u21,Y1u22,Y2u33,Y1d11,Y1d12,Y1d13,Y1d21,Y1d22,Y1d23,\
                                Y2d31,Y2d32,Y2d33,vlu,vru,vld,vrd,ANGLES,UCKM= Yukawa_alaysis(beta,v3)

                                #The line below assigns the Lepton & Neutrino Yukawa couplings to variables.
                                OeL,OeR,Y1l11, Y1l12, Y1l21, Y1l22, Y2l33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, B11, B12, \
                                B21, B22, C13, C23, C31, C32= Lepton_neutrino_Yukawa_couplings_BGL(v3,v_u,v_d)


                                #This line, creates an spheno_LesHouches, which will be latter used into SPheno
                                write_spheno_LesHouches(gl1, gl2, gl3, gl4, gl_s1,gl_s2, gl_s3,gmu_3,gmusb,\
                                galpha_1,galpha_2,galpha_3,galpha_4,v3,Y1d11, Y1d12, Y1d13, Y1d21, Y1d22, Y1d23,\
                                Y2d31, Y2d32, Y2d33, Y1u11, Y1u12, Y1u21, Y1u22, Y2u33,Y1l11, Y1l12, Y1l21, Y1l22,\
                                Y2l33, Y1n11, Y1n12, Y1n21, Y1n22, Y2n33, B11, B12, B21, B22, C13, C23, C31, C32,v_u,v_d)
                                #This line, creates an FS_LesHouches, which will be latter used into SPheno
                                #write_FlexibleSUSY_LesHouches(gl1, gl2, gl3, gl4, gAd2, gd1, gd2, gd3, g4, g41, g14, v3,Yd1_R,Yd2_R,Yu1,Yu2,Ye,Yv,Yvv,v_u,v_d,beta)

                                # create the SPheno output
                                Spheno_BGL_Running_comand = spheno_BGL + ' ' + os.path.join(Running_Env,'spheno','LesHouches.in.BGLNCS')
                                os.system(Spheno_BGL_Running_comand)


                                #FS_BGL_Running_comand = FlexibleSUSY_BGL + ' ' + '--slha-input-file='+os.path.join(Running_Env,'FlexibleSUSY','LesHouches.in.THDMSBGL') + ' ' + '--slha-output-file='+os.path.join(Resutls_Env,'LesHouches.out.THDMSBGL')
                                #os.system(FS_BGL_Running_comand)


                                Oa = ZH_matrix(delta,beta,a2,a3)
                                write_input_LHE(Mhh1,Mhh2,Mhh3,MAh2,M_Ah3,MHm,beta,a2,a3,delta,gamma1,OeL,OeR,Oa,vlu,vru,vld,vrd,ANGLES,UCKM)



                                if break_code2=='go':
                                #if float(delta_chi) < 7.815:

                                    counter += 1

                                    os.system('cp' + ' '+ os.path.join(Running_Env,'spheno','LesHouches.in.BGLNCS')+ ' ' + Resutls_Env)

                                    # copy comant that saves the spheno output to a folder
                                    spc_BGL = Resutls_Env
                                    copy_comand_spheno = 'cp -r ' + spc_BGL + ' '+ Spheno_output_Folder +'/point_{}'.format(counter)

                                    os.system(copy_comand_spheno )
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


cleaning_functiont()
