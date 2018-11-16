# -*- coding: utf-8 -*-
"""
Spyder Editor

This is a temporary script file.
"""
import os
import re

os.makedirs("start")
for filename in os.listdir(os.getcwd()):
    if filename[-3:]=="Rmd":
        #print(filename)
        with open(filename, "r") as f:
            RMD_file = f.read()
            #print(RMD_file)
            pattern = re.compile(r'<span class="answer">.*?</span>',re.S)
            write_file = "./start/s_"+filename
            with open(write_file,"w") as wf:
                print(len(re.findall(pattern, RMD_file)))
                wf.write(re.sub(pattern,"",RMD_file))


    