#!/usr/bin/env python3
import matplotlib.pyplot as plt
import h5py, argparse 
import numpy as np
from maps import plotMapHist
def read_h5(filename):
    h5 = h5py.File(filename,'r')
    keys = list(h5.keys())
    d = {}
    for k in keys:
        d[k] = np.asarray(h5.get(k))
    return d

if __name__ == '__main__':
    parser = argparse.ArgumentParser( description = 'Plot profile of statistics from IFS/CADS output.')
    parser.add_argument('--input', help = 'path to first input file', required = True, dest = 'input')
    a = parser.parse_args()
    d = read_h5(a.input)
    omb = d['Observation']-d['Background']

    for i,c in enumerate(d['Channels']):
       # if(c>1180 and c<1200):
        if(c==1939 or c==2175):
            idx, = np.where(d['Cloud_Flags'][:,i]<1)
            #mx = np.max(np.abs(omb[idx,i]))
            mx=1
            plotMapHist(d['Latitude'][idx], d['Longitude'][idx], omb[idx,i], title='Channel {}'.format(c), plotRange=[-1*mx,mx], graphicName='omb_{}.png'.format(c),  units='', colorScheme = 'coolwarm',  showFig=False, figureSize = (16,9),graphicDPI=100)
  
