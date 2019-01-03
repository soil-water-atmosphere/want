# -*- coding: utf-8 -*-
"""
Parser for the WANT practicals
"""

import os
import re
import copy

def parse(filename, output_directory  = "++",
                    start  = True , start_name  = "",
                    answer = False, answer_name = "",
                    origin = False, origin_name = "",
                    use_name_as_prefix = True,
                    css = False, js = False):   
    # filename of Rmd file, this can be a pathname if required
    #
    # output_directory where files will be written to
    # output_directory = "." means it will be written to the current directory
    # output_directory = "++" creates the files in the same folder as the Rmd file
    #
    # start,answer,origin -> toggles for creation of files (start is True by default)
    # _name -> output file name
    # if blank this will create the following files:
    #   start_filename.Rmd
    #   answer_filename.Rmd
    #   origin_filename.Rmd
    #
    # js = True will replace the first <script> tag with it's source file
    # css = True will replace the css entry in the head into its source file
    
    if output_directory == "++" and "/" in filename:
        output_directory = "/".join(filename.split("/")[:-1])
    elif output_directory == "++":
        output_directory = "."
    else:
        if not output_directory == "":
            if not os.path.exists(output_directory):
                os.makedirs(output_directory)
    
    if filename[-3:]=="Rmd":
        #print(filename)
        with open(filename, "r") as f:
            RMD_file = f.read()
            list_classes = ("answer", "student_answer", "question", "comment")
            for html_class in list_classes:
                pattern = re.compile(r'<[^\n<]*?"{}"[^\n]*?>'.format(html_class),re.S)
                RMD_file = re.sub(pattern,'<div class="{}">'.format(html_class),RMD_file)
            
        
        # set closing tags
        pattern = re.compile(r'</[^\n<]*?{}[^\n]*?>'.format("span"),re.S)    
        RMD_file = re.sub(pattern,"</div>",RMD_file)
        
        pattern = re.compile(r'</[^\n<]*?{}[^\n]*?>'.format("div"),re.S)    
        RMD_file = re.sub(pattern,"</div>",RMD_file)
        
        if css:
            #find css file insertion
            pattern = re.compile(r'css.*?css',re.S)
            css     = re.findall(pattern,RMD_file)
            if len(css)>0:
                css_file = css[0].split(" ")[-1]
                path     = css_file.split("/")
                
                back     = path.count("..")
                file     = path[-1]
                
                folders = filename.split("/")
                count_folders = len(folders)
                
                css_file = ("/"+"/".join(folders[:count_folders-back-1])+"/" + file)[1:]
                with open(css_file,"r") as f:
                    css_text = f.read()
                    
            RMD_file = re.sub(pattern,"",RMD_file)
            
            pattern = re.compile(r'---',re.S)
            head    = [m.span() for m in re.finditer(pattern,RMD_file)]
            css_write_text = "<style>\n"+css_text+ "\n</style>"
            
            RMD_file = RMD_file[:head[1][1]+1] + '\n' + css_write_text + RMD_file[head[1][1]:]
        
        if js:
            #find css file insertion
            pattern = re.compile(r'<script.*?</script>',re.S)
            js      = re.findall(pattern,RMD_file)
            
            if len(js)>0:
                file_pattern = re.compile(r'src=".*?"',re.S)
                js_file      = re.findall(file_pattern,js[0])[0][5:-1]
                
                path     = js_file.split("/")
                
                back     = path.count("..")
                file     = path[-1]
                
                folders = filename.split("/")
                count_folders = len(folders)
                
                js_file = ("/"+"/".join(folders[:count_folders-back-1])+"/" + file)[1:]
                with open(js_file,"r") as f:
                    js_text = f.read()
                
            RMD_file = re.sub(pattern,"",RMD_file)
            
            pattern = re.compile(r'---',re.S)
            head    = [m.span() for m in re.finditer(pattern,RMD_file)]
            js_write_text = "<script>\n"+js_text+ "\n</script>"
            
            RMD_file = RMD_file[:head[1][1]+1] + "\n" + js_write_text + RMD_file[head[1][1]:]
            
        # remove path from filename
        filename = filename.split("/")[-1]
            
        if(start):  
            ####### make start version #####    
            if start_name == "": start_name = "start_" + filename
            else: start_name = start_name + ".Rmd"
            
            # Create copy of the Rmd file 
            RMD_file_start = copy.copy(RMD_file)
                
            # remove comment class
            pattern = re.compile(r'<div class="comment">.*?</div>',re.S)
            RMD_file_start = re.sub(pattern, "", RMD_file_start)
            
            # remove answer class 
            pattern = re.compile(r'<div class="answer">.*?</div>',re.S)
            RMD_file_start = re.sub(pattern, "", RMD_file_start)
                
            # write file
            with open(output_directory + "/" + start_name, "w") as f:
                f.write(RMD_file_start)
            
        if(answer):    
            ####### make answer version #####
            if answer_name == "": answer_name = "answer_" + filename
            else: answer_name = answer_name + ".Rmd"
            
            # Create copy of the Rmd file 
            RMD_file_answer = copy.copy(RMD_file)
            
            # remove comment class
            pattern = re.compile(r'<div class="comment">.*?</div>',re.S)
            RMD_file_answer = re.sub(pattern, "", RMD_file_answer)
            
            # remove student_answer class 
            pattern = re.compile(r'<div class="student_answer">.*?</div>',re.S)
            RMD_file_answer = re.sub(pattern, "", RMD_file_answer)
                
            # write file
            with open(output_directory + "/" + answer_name, "w") as f:
                f.write(RMD_file_answer)
                
        if(origin):
            ####### create original version without comments ###### 
            if origin_name == "": origin_name = "origin_" + filename
            else: origin_name = origin_name + ".Rmd" 
            
            # Create copy of the Rmd file 
            RMD_file_origin = copy.copy(RMD_file)    
        
            # remove comment class
            pattern = re.compile(r'<div class="comment">.*?</div>',re.S)
            RMD_file_origin = re.sub(pattern, "", RMD_file_origin)
                
            # write file
            with open(output_directory + "/" + origin_name, "w") as f:
                f.write(RMD_file_origin)

def find_all_Rmd():                
    # routine to find all files in the directory and subdirectories
    files = []
    current_dir = os.getcwd()
    l = len(current_dir) + 1
    for (dirpath, dirnames, filenames) in os.walk(current_dir):
        files += [os.path.join(dirpath, file) for file in filenames]
    files = [file[l:] for file in files if file[-3:]=="Rmd"]
    for f in files:
        print(f)
    return(files)
    
#### EXAMPLES
# create start versions of all RMD's in the base folder
#os.chdir("/Users/jroebroek/WANT")
#for f in find_all_Rmd():
#    parse(f,".")

# create start version of FD_time in the folder of the parser with css and js 
# baked into the RMD
#parse("/Users/jroebroek/WANT/finite_difference/Time/Finite_differences_1.Rmd",
#      output_directory = ".", js = True, css = True)

# create start versions of all RMD's in their own folder
#os.chdir("/Users/jroebroek/WANT")
#for f in find_all_Rmd():
#    parse(f) 
    
# create start, original and answer version in a folder named versions in the base folder
os.chdir("/Users/jroebroek/WANT")
for f in find_all_Rmd():
    parse(f, "versions", answer=True) 