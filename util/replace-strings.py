#!/usr/bin/env python
# encoding: utf-8
"""
replace-strings.py

Utility script to make all the substitions from the subs_file passed with -s arg on the command line (whitespace-separated
source-target pairs).

Runs on the filename argument passed it, backing up the original file with the 'orig' suffix.

Created by Andrew MacKinlay on 2010-02-22.
Copyright (c) 2010 __MyCompanyName__. All rights reserved.
"""
from __future__ import with_statement

import sys
import getopt
import re
import os
from contextlib import nested

help_message = '''
Usage:
replace-strings.py -s subs_file filename
'''


class Usage(Exception):
    def __init__(self, msg):
        self.msg = msg


def read_subs_and_replace(subs_fname, replaceable_fnames):
    sub_delim = None
    backup_suff = '.orig'
    subs = {}
    orig_in_order = []
    with open(subs_fname) as subs_file:
        for line in subs_file:
            try:
                orig, repl = line.split()
                subs[orig] = repl
                orig_in_order.append(orig)
            except ValueError:
                print >> sys.stderr, "WARNING: Ignoring line in substitutions file '%s' as it has an incorrect number of items. Syntax is a one whitespace-separated word-pair per line" % line
    terms_re_component = r'|'.join(re.escape(orig) for orig in orig_in_order)
    sub_search_re = re.compile(r'''
        (?:\A|(?<=[^\w-])) # zero-width lookbehind: require a string boundary or 
                           # any character that's not in [A-Za-z0-9_-] before the replaced string
        (%s)      # the matched string itself
        (?:(?=[^\w-])|\Z) # similar to beginniing boundary: zero-width lookahead assertion 
    ''' % terms_re_component, re.VERBOSE)
    # would be nice to use zero-width '\b' here but that would allow eg '-' at boundaries which is not what we want, I don't think.
    
    def get_repl(found_match):
        return subs[found_match.group(0)]
    
    for fname in replaceable_fnames:
        fname_orig = fname + backup_suff
        os.rename(fname, fname_orig)
        with nested(open(fname_orig), open(fname, 'w')) as (replaceable_file_in, replaceable_file_out):
            for line in replaceable_file_in:
                new_line = sub_search_re.sub(get_repl, line)
                replaceable_file_out.write(new_line)
    

def main(argv=None):
    if argv is None:
        argv = sys.argv
    try:
        try:
            opts, args = getopt.getopt(argv[1:], "ho:vs:", ["help", "output=", "substitutions="])
        except getopt.error, msg:
            raise Usage(msg)
        
        subs_fname = None
        # option processing
        for option, value in opts:
            if option == "-v":
                verbose = True
            if option in ("-h", "--help"):
                raise Usage(help_message)
            if option in ("-o", "--output"):
                output = value
            if option in ("-s", "--substitutions"):
                subs_fname = value
        
        if not args:
            raise Usage("Must supply file(s) to operate on")
        replaceable_fnames = args
        read_subs_and_replace(subs_fname, replaceable_fnames)
            
    
    except Usage, err:
        print >> sys.stderr, sys.argv[0].split("/")[-1] + ": " + str(err.msg)
        print >> sys.stderr, "\t for help use --help"
        return 2


if __name__ == "__main__":
    sys.exit(main())
