###
### remove all the doc strings for *.tdl files 
### write the dedoc-ed file into *-for-pet.tdl
###
import re
import os


for thing in os.listdir("."):
    if thing.endswith(".tdl"):
        ## write to the same file with '-for-pet.tdl'
        output = thing[:-4]+'-for-pet.tdl'
        raw = open(thing).read()

        ### remove the comments, assume quotes are balanced
        cooked = re.sub('""".*?"""', '', raw, flags = re.DOTALL)
        fh = open (output, 'w')
        fh.write(cooked)
