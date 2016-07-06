#!/usr/bin/env python

import sys
import fileinput

if __name__ == '__main__':

    d = {}

    def median(list):
        sortedList = sorted(list)
        listLen = len(list)
        index = (listLen - 1) // 2
    
        if (listLen % 2):
            return sortedList[index]
        else:
            return (sortedList[index] + sortedList[index + 1])/2.0
    
    def mean(list):
        return float(sum(list))/float(len(list))
        

    # check for input filename given as first argument
    sys.argv = ['stream_stats.py', 'sample_input.tsv']
    if len(sys.argv) < 2:
        sys.stderr.write('reading input from stdin\n')
        

    # read input one line at a time
    for line in fileinput.input():
        # split on tab to get the key and value for each line
        fields = line.rstrip('\n').split('\t')
        
        # update statistics for the group correponding to this key
        # (minimum, median, mean, and maximum)
        if d.has_key(fields[0]):
            d[fields[0]].append(int(fields[1]))
            d[fields[0]+ ' stats'] = [int(fields[1]), int(fields[1]), int(fields[1]), int(fields[1])]
        else:
            d[fields[0]] = []
            d[fields[0]].append(int(fields[1]))
            d[fields[0]+ ' stats'] = [min(d[fields[0]]), median(d[fields[0]]), mean(d[fields[0]]), max(d[fields[0]])]
            
            
    fileinput.close()
        
        

    # loop over each group and output the group's key and statistics