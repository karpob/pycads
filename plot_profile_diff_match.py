#!/usr/bin/env python3
import matplotlib.pyplot as plt
import h5py,argparse 
import numpy as np

def read_h5(filename):
    h5 = h5py.File(filename,'r')
    keys = list(h5.keys())
    d = {}
    for k in keys:
        d[k] = np.asarray(h5.get(k))
    return d
def plotProfile(chan_lw,chan_mw,chan_sw,Channels, heights,\
                cld_counts_match,\
                clr_counts_match,\
                cld_counts_fp,\
                cld_counts_miss,\
                cld_omba_match,\
                clr_omba_match,\
                cld_omba_fp,\
                cld_omba_miss,\
                cld_ombs_match,\
                clr_ombs_match,\
                cld_ombs_fp,\
                cld_ombs_miss, plt_name):


    idx_height = heights.argsort()
    chans = Channels[idx_height]
    str_chan = [str(x) for x in chans]
    lev = np.arange(0,len(Channels))

    fig, ax = plt.subplots(ncols=3,nrows=1,figsize=(9,32))
    #ax[0].plot(cld_counts_match[idx_height],lev,marker='x',label='Cloud Match',color='green')
    #ax[0].plot(clr_counts_match[idx_height],lev,marker='x',label='Clear Match',color='lightgreen')
    ax[0].plot(cld_counts_fp[idx_height],lev,marker='x',label='Cloud False',color='orange')
    ax[0].plot(cld_counts_miss[idx_height],lev,marker='x',label='Cloud Miss',color='red')
    ax[0].set_yticks(lev)
    ax[0].set_yticklabels(str_chan)
    ax[0].set_ylabel('Channel Number')
    ax[0].set_xlabel('Count')
    ax[0].set_xscale('log')
    ax[0].legend()
    idx_lw = np.where( d['Channels']<chan_lw[-1] )
    idx_mw = np.where( (d['Channels']>=chan_mw[0] ) & ( d['Channels']<chan_mw[-1] ) )
    idx_sw = np.where( (d['Channels']>=chan_sw[0] ) & ( d['Channels']<chan_sw[-1] ) )


    #ax[1].plot(cld_omba_match[idx_height],lev,marker = 'x',color='green') 
    ax[1].plot(clr_omba_match[idx_height],lev,marker = 'x',color='lightgreen') 
    ax[1].plot(cld_omba_fp[idx_height],lev,marker = 'x',color='orange') 
    ax[1].plot(cld_omba_miss[idx_height],lev,marker = 'x',color='red') 
    ax[1].set_xlabel('O-B (Mean) [K]')
    ax[1].set_yticks(lev)
    ax[1].set_yticklabels(str_chan)

    #ax[2].plot(cld_ombs_match[idx_height],lev,marker = 'x',color='green') 
    ax[2].plot(clr_ombs_match[idx_height],lev,marker = 'x',color='lightgreen') 
    ax[2].plot(cld_ombs_fp[idx_height],lev,marker = 'x',color='orange') 
    ax[2].plot(cld_ombs_miss[idx_height],lev,marker = 'x',color='red') 
    ax[2].set_xlabel('O-B (Std) [K]')
    ax[2].set_yticks(lev)
    ax[2].set_yticklabels(str_chan)

    for iii in range(3):
        cnt = len(ax[iii].get_yticklabels()[:])
        for ii in range(cnt):
            if chans[ii] <= chan_lw[-1]:
                ax[iii].get_yticklabels()[ii].set_color('grey')
            elif (chans[ii] >=chan_mw[0] ) & ( chans[ii] <=chan_mw[-1]) :
                ax[iii].get_yticklabels()[ii].set_color('blue')
            elif (chans[ii] >=chan_sw[0] ) & ( chans[ii] <=chan_sw[-1]) :
                ax[iii].get_yticklabels()[ii].set_color('orange')
    

    for i in range(3):
        ax[i].set_ylim(ax[i].get_ylim()[::-1])
    fig.tight_layout()
    plt.savefig(plt_name)


def processData(d):
    omb = d['Observation']-d['Background']
    heights = np.zeros(d['Channels'].shape[0])
    cloud_counts = np.zeros(d['Channels'].shape[0])
    omb_ave = np.zeros(d['Channels'].shape[0])
    omb_std = np.zeros(d['Channels'].shape[0])
    omb_std_clr = np.zeros(d['Channels'].shape[0])
    omb_ave_clr = np.zeros(d['Channels'].shape[0])
    for i,c in enumerate(d['Channels']):
        good_idx, = np.where( d['Cloud_Flags'][:,i] == 0 )
        bad_idx, = np.where( d['Cloud_Flags'][:,i] == 1 )
        cloud_counts[i] = len(bad_idx)
        omb_ave[i] = omb[:,i].mean()
        omb_std[i] = omb[:,i].std()
        omb_ave_clr[i] = omb[good_idx,i].mean()
        omb_std_clr[i] = omb[good_idx,i].std()
        heights[i] = d['Height'][:,i].mean() 
    return heights, cloud_counts, omb_ave, omb_std, omb_ave_clr, omb_std_clr

def processDataMatch(dr,de):
    refCldFlg = dr['Cloud_Flags']
    expCldFlg = de['Cloud_Flags']
    omb = dr['Observation']-dr['Background']
    aomb = np.abs(omb)
    #idxMatchCld = np.where( (refCldFlg == 1) & (expCldFlg ==1))
    #idxMatchClr = np.where( (refCldFlg == 0) & (expCldFlg ==0))
    #idxRefClrExpCld = np.where( (refCldFlg ==0) & (expCldFlg ==1))
    #idxRefCldExpClr = np.where( (refCldFlg ==1) & (expCldFlg ==0))
    cld_counts_match = np.zeros(d['Channels'].shape[0])
    clr_counts_match = np.zeros(d['Channels'].shape[0])
    cld_counts_fp = np.zeros(d['Channels'].shape[0])
    cld_counts_miss = np.zeros(d['Channels'].shape[0])
    cld_omba_match = np.zeros(d['Channels'].shape[0])
    clr_omba_match = np.zeros(d['Channels'].shape[0])
    cld_omba_fp = np.zeros(d['Channels'].shape[0])
    cld_omba_miss =np.zeros(d['Channels'].shape[0])
    cld_ombs_match =np.zeros(d['Channels'].shape[0])
    clr_ombs_match = np.zeros(d['Channels'].shape[0])
    cld_ombs_fp = np.zeros(d['Channels'].shape[0])
    cld_ombs_miss =np.zeros(d['Channels'].shape[0])
    
    
    cnts = dr['Cloud_Flags'].shape[0]
    for i,c in enumerate(d['Channels']):
        idx_match_cld, = np.where((dr['Cloud_Flags'][:,i]==1) & (de['Cloud_Flags'][:,i]==1))
        idx_match_clr, = np.where((dr['Cloud_Flags'][:,i]==0) & (de['Cloud_Flags'][:,i]==0))
        idx_cld_fp, = np.where((dr['Cloud_Flags'][:,i]==0) & (de['Cloud_Flags'][:,i]==1))
        idx_cld_miss, = np.where((dr['Cloud_Flags'][:,i]==1) & (de['Cloud_Flags'][:,i]==0))

        cld_counts_match[i] = len(idx_match_cld)
        clr_counts_match[i] = len(idx_match_clr)
        cld_counts_fp[i] = len(idx_cld_fp)
        cld_counts_miss[i] = len(idx_cld_miss)

        cld_omba_match[i] = omb[idx_match_cld,i].mean()
        clr_omba_match[i] = omb[idx_match_clr,i].mean()

        cld_omba_fp[i] = 100*(( aomb[idx_match_clr,i].mean()-aomb[idx_cld_fp,i].mean())/aomb[idx_match_clr,i].mean())
        cld_omba_miss[i] = 100*((aomb[idx_match_clr,i].mean()-aomb[idx_cld_miss,i].mean())/aomb[idx_match_clr,i].mean())

        cld_ombs_match[i] = omb[idx_match_cld,i].std()
        clr_ombs_match[i] = omb[idx_match_clr,i].std()
        cld_ombs_fp[i] = 100*((omb[idx_match_clr,i].std()-omb[idx_cld_fp,i].std())/omb[idx_match_clr,i].std())
        cld_ombs_miss[i] = 100*((omb[idx_match_clr,i].std()-omb[idx_cld_miss,i].std())/omb[idx_match_clr,i].std())
    return cld_counts_match,\
        clr_counts_match,\
        cld_counts_fp,\
        cld_counts_miss,\
        cld_omba_match,\
        clr_omba_match,\
        cld_omba_fp,\
        cld_omba_miss,\
        cld_ombs_match,\
        clr_ombs_match,\
        cld_ombs_fp,\
        cld_ombs_miss




if __name__ == '__main__':
    parser = argparse.ArgumentParser( description = 'Plot profile of statistics from IFS/CADS output.')
    parser.add_argument('--inputa', help = 'path to first input file', required = True, dest = 'inputa')
    parser.add_argument('--inputb', help = 'path to first input file', required = True, dest = 'inputb')
    parser.add_argument('--output', help = 'output prefix (will append h5 and txt)', required = True, dest = 'output')
    parser.add_argument('--img', help = 'suffix for plot type default png.', required = False, dest = 'img',default='png')
    a = parser.parse_args()


    d = read_h5(a.inputa)
    d1 = read_h5(a.inputb)

    heights, cloud_counts, omb_ave, omb_std, omb_ave_clr, omb_std_clr = processData(d)
    heights1, cloud_counts1, omb_ave1, omb_std1, omb_ave_clr1, omb_std_clr1 = processData(d1)
    # hard wired for cris, should make more generic
    chan_lw = np.arange(1,1+713,1)
    # 200 added to only label R branch as "shortwave"
    chan_mw = np.arange(714,714+865,1)
    chan_sw = np.arange(714+865,714+865+633,1)
    h5out= h5py.File('chans.h5','w')
    h5out.create_dataset('Channels',data=np.asarray(d['Channels']))
    h5out.close()

    cld_counts_match,\
    clr_counts_match,\
    cld_counts_fp,\
    cld_counts_miss,\
    cld_omba_match,\
    clr_omba_match,\
    cld_omba_fp,\
    cld_omba_miss,\
    cld_ombs_match,\
    clr_ombs_match,\
    cld_ombs_fp,\
    cld_ombs_miss = processDataMatch(d,d1)
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'], heights,\
                cld_counts_match,\
                clr_counts_match,\
                cld_counts_fp,\
                cld_counts_miss,\
                cld_omba_match,\
                clr_omba_match,\
                cld_omba_fp,\
                cld_omba_miss,\
                cld_ombs_match,\
                clr_ombs_match,\
                cld_ombs_fp,\
                cld_ombs_miss, a.output+'_diff_all.'+a.img)

    ave_trop = np.asarray(d['trop_level']).mean() 
    ave_bound = np.asarray(d['bl_level']).mean()
 
    idx_bl, = np.where( heights > ave_bound)
    idx_trop, = np.where( (heights > ave_bound) & (heights <= ave_trop) ) 
    idx_strat, = np.where( (heights < ave_trop ) ) 
    """
    # break up by boundary layer, tropopause
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_bl], heights[idx_bl],\
                cld_counts_match[idx_bl],\
                clr_counts_match[idx_bl],\
                cld_counts_fp[idx_bl],\
                cld_counts_miss[idx_bl],\
                cld_omba_match[idx_bl],\
                clr_omba_match[idx_bl],\
                cld_omba_fp[idx_bl],\
                cld_omba_miss[idx_bl],\
                cld_ombs_match[idx_bl],\
                clr_ombs_match[idx_bl],\
                cld_ombs_fp[idx_bl],\
                cld_ombs_miss[idx_bl], a.output+'diff__bnd_lyr_.'+a.img)
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_trop], heights[idx_trop],\
                cld_counts_match[idx_trop],\
                clr_counts_match[idx_trop],\
                cld_counts_fp[idx_trop],\
                cld_counts_miss[idx_trop],\
                cld_omba_match[idx_trop],\
                clr_omba_match[idx_trop],\
                cld_omba_fp[idx_trop],\
                cld_omba_miss[idx_trop],\
                cld_ombs_match[idx_trop],\
                clr_ombs_match[idx_trop],\
                cld_ombs_fp[idx_trop],\
                cld_ombs_miss[idx_trop], a.output+'_diff_trop.'+a.img)
 
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_strat], heights[idx_strat],\
                cld_counts_match[idx_strat],\
                clr_counts_match[idx_strat],\
                cld_counts_fp[idx_strat],\
                cld_counts_miss[idx_strat],\
                cld_omba_match[idx_strat],\
                clr_omba_match[idx_strat],\
                cld_omba_fp[idx_strat],\
                cld_omba_miss[idx_strat],\
                cld_ombs_match[idx_strat],\
                clr_ombs_match[idx_strat],\
                cld_ombs_fp[idx_strat],\
                cld_ombs_miss[idx_strat], a.output+'_diff_strat.'+a.img)
    """


    # break up by longwave, midwave, and shortwave band
    idx_lw = np.where( d['Channels']<=chan_lw[-1] )
    idx_mw = np.where( (d['Channels']>=chan_mw[0] ) & ( d['Channels']<=chan_mw[-1] ) )
    idx_sw = np.where( (d['Channels']>=chan_sw[0] ) & ( d['Channels']<=chan_sw[-1] ) ) 
    chans_use = np.asarray([1939, 1940, 1941, 1942, 1943, 1944, 1945, 1946, 1947, 1948,\
        1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958,\
        1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968,\
        1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978,\
        1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 2119,\
        2140, 2143, 2147, 2153, 2158, 2161, 2168, 2171, 2175, 2182])
 
    idx_sw, = np.nonzero(np.in1d(d['Channels'],chans_use))
    print ('sw  plot')
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_sw], heights[idx_sw],\
                cld_counts_match[idx_sw],\
                clr_counts_match[idx_sw],\
                cld_counts_fp[idx_sw],\
                cld_counts_miss[idx_sw],\
                cld_omba_match[idx_sw],\
                clr_omba_match[idx_sw],\
                cld_omba_fp[idx_sw],\
                cld_omba_miss[idx_sw],\
                cld_ombs_match[idx_sw],\
                clr_ombs_match[idx_sw],\
                cld_ombs_fp[idx_sw],\
                cld_ombs_miss[idx_sw], a.output+'_diff_sw.'+a.img)
  
    print('mw plot')
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_mw], heights[idx_mw],\
                cld_counts_match[idx_mw],\
                clr_counts_match[idx_mw],\
                cld_counts_fp[idx_mw],\
                cld_counts_miss[idx_mw],\
                cld_omba_match[idx_mw],\
                clr_omba_match[idx_mw],\
                cld_omba_fp[idx_mw],\
                cld_omba_miss[idx_mw],\
                cld_ombs_match[idx_mw],\
                clr_ombs_match[idx_mw],\
                cld_ombs_fp[idx_mw],\
                cld_ombs_miss[idx_mw], a.output+'_diff_mw.'+a.img)
    print('lw plot') 
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_lw], heights[idx_lw],\
                cld_counts_match[idx_lw],\
                clr_counts_match[idx_lw],\
                cld_counts_fp[idx_lw],\
                cld_counts_miss[idx_lw],\
                cld_omba_match[idx_lw],\
                clr_omba_match[idx_lw],\
                cld_omba_fp[idx_lw],\
                cld_omba_miss[idx_lw],\
                cld_ombs_match[idx_lw],\
                clr_ombs_match[idx_lw],\
                cld_ombs_fp[idx_lw],\
                cld_ombs_miss[idx_lw], a.output+'_diff_lw.'+a.img)
 
