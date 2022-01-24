# -*- coding: utf-8 -*-
"""
Created on Wed Jan  5 12:00:04 2022

@author: stevo

A code to translate tidy excel files into RAMP input files
Grazie a J per la spinta necessaria

"""

import pandas as pd

def window_start(x):
    if (x == '-'):
        x = 0
    else:
        x = x.hour*60+x.minute    
    return x

def window_end(x):
    if (x == '-'):
        y = 0
    else:
        y = x.hour*60+x.minute
        if (y == 0):
            y = 1440
    return y

def thermal_P_var_check(x):
    if (x == 'no'):
        x = 0
    else:
        x = x
    return x
    
    
Input = pd.read_excel('Input_File_1.xlsx')

number_users = 1
user_name = Input['User_Name'][0]
user_code = Input['User_Code'][0]
input_file = Input['Input_File'][0]

name = Input['App_Name']
number = Input['Number']
power = Input['Power [W]']
windows = Input['N_windows'] # rimane problema specific cycle
daily_time = Input['Daily Time [min]']
daily_time_var = Input['Time %']
min_time = Input['Min Time [min]']
win_1_start = Input['W1 Start'].apply(window_start)
win_1_end = Input['W1 End'].apply(window_end)
win_2_start = Input['W2 Start'].apply(window_start)
win_2_end = Input['W2 End'].apply(window_end)
win_3_start = Input['W3 Start'].apply(window_start)
win_3_end = Input['W3 End'].apply(window_end) 
win_var = Input['Window %']
occasional = Input['Occasional Use']
week = Input['we/wd'].replace(('wd','we','no'),(0,1,2))
thermal_P_var = Input['Thermal_P_Var'].apply(thermal_P_var_check)

Intro = ['# -*- coding: utf-8 -*-','','# 05 December 2022','','# Nicolò Stevanato - Politecnico di Milano','', 
'#%% Definition of the inputs for Tier 2 Health Center in Uganda','## In collaboration with WRI, Uganda MoH','',
'from ramp.core.core import User, np','User_list = []','']

User_def = ['#Create new user classes','','{} = User("{}",{})'.format(user_code,user_name,number_users),'','User_list.append({})'.format(user_code),'',
            '#Create new appliances']

with open('{}.py'.format(input_file),'w',encoding='utf-8') as f:
     for line in Intro:
            f.write(line)
            f.write('\n')
     for line in User_def:
            f.write(line)
            f.write('\n')
            
App_def = {}            

for a in range(len(name)):
    App_def[a] = ['{} = {}.Appliance({},{},{},{},{},{},{},occasional_use = {}, wd_we_type = {}, thermal_P_var = {})'
               .format(name[a],user_code,user_code,number[a],power[a],windows[a],daily_time[a],daily_time_var[a],min_time[a],occasional[a],week[a],thermal_P_var[a]),
               '{}.windows([{},{}],[{},{}],{},[{},{}]'
               .format(name[a],win_1_start[a],win_1_end[a],win_2_start[a],win_2_end[a],win_var[a],win_3_start[a],win_3_end[a]),'']
    
    with open('{}.py'.format(input_file),'a',encoding='utf-8') as ff:
        for line in App_def[a]:
            ff.write(line)
            ff.write('\n')



'''
App_def = ['{} = {}.Appliance({},{},{},{},{},{},{},occasional_use = {}, wd_we_type = 0, thermal_P_var = {})'
           .format(name[0],user_code,user_code,number[0],power[0],windows[0],daily_time[0],daily_time_var[0],min_time[0],occasional[0],thermal_P_var[0]),
           '{}.windows([{},{}],[{},{}],{},[{},{}]'
           .format(name[0],win_1_start[0],win_1_end[0],win_2_start[0],win_2_end[0],win_var[0],win_3_start[0],win_3_end[0])]


with open('{}.py'.format(input_file),'w',encoding='utf-8') as f:
    for line in Intro:
        f.write(line)
        f.write('\n')
    for line in User_def:
        f.write(line)
        f.write('\n')
    for line in App_def:
        f.write(line)
        f.write('\n')
        
'''
