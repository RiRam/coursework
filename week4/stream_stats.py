#!/usr/bin/env python

import sys
import fileinput

if __name__ == '__main__':

    d = {}
    measures = {}

    # Computing the median (like this) has added an additional memory requirement
    # of storing a sorted copy and then determine the length of the list and an
    # index at which the median is. If there is an even number of items, an average
    # of the two middle values is calculated.
    def median(list):
        sortedList = sorted(list)
        listLen = len(list)
        index = (listLen - 1) // 2
    
        if (listLen % 2):
            return sortedList[index]
        else:
            return (sortedList[index] + sortedList[index + 1])/2.0
            
    def mean(list):
        total = sum(list)
        mean = total / float(len(list))
        return mean
        
    # check for input filename given as first argument
    sys.argv = ['stream_stats.py', 'sample_input.tsv']
    if len(sys.argv) < 2:
        sys.stderr.write('reading input from stdin\n')
        

    # read input one line at a time
    for line in fileinput.input():
        # split on tab to get the key and value for each line
        fields = line.rstrip('\n').split('\t')

        k = fields[0]
        v = float(fields[1])        
        
        # update statistics for the group correponding to this key
        # (minimum, median, mean, and maximum)
        if k in d:
            d[k].append(v)
            #measures[k] = [min(d[k]), median(d[k]), mean(d[k]), max(d[k])]
            measures[k] = [min(d[k]), mean(d[k]), max(d[k])]
        else:
            d[k] = []
            d[k].append(v)
            #measures[k] = [v, v, v, v]
            measures[k] = [v, v, v]
            
            
    fileinput.close()
    
    # loop over each group and output the group's key and statistics
    filewriter = open("output.tsv", "w")
    
    for k in d: 
        string = k + "\t" + '\t'.join([str(x) for x in measures[k]]) + "\n"
        filewriter.write(string)
        
    filewriter.close()    
    
    # Assuming that all the provided data was sorted by key, the minimum memory
    # footprint would change because one wouldn't need to store the values in a 
    # dictionary by key, one could instead store them in a list, and each set of 
    # measures can be calculated at the end of each section.
    # Also, instead of recalculating the measures at each line read, they can be 
    # calculated once, removing the need to update them in the dictionary. 