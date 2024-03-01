#!/usr/bin/env python3
import matplotlib.pyplot as plt
import h5py, argparse 
import numpy as np

def read_h5(filename):
    h5 = h5py.File(filename,'r')
    keys = list(h5.keys())
    d = {}
    for k in keys:
        d[k] = np.asarray(h5.get(k))
    return d

def plotProfile(chan_lw, chan_mw, chan_sw, Channels, hists, hists1, edges, heights, cloud_counts, omb_ave, omb_std, omb_ave_clr, omb_std_clr, total, plt_name):
    """
    valid = ~np.isnan(omb_ave)
    heights = heights[valid]
    Channels = Channels[valid]
    cloud_counts = cloud_counts[valid]
    omb_ave  = omb_ave[valid]
    omb_std = omb_std[valid]
    omb_ave_clr = omb_ave_clr[valid]
    """
    from matplotlib.colors import LogNorm
    from matplotlib.colors import Normalize
    from mpl_toolkits.axes_grid1 import make_axes_locatable
    idx_height = heights.argsort()
    chans = Channels[idx_height]
    print(hists.shape)
    hists_sorted = hists[idx_height,:]
    hists1_sorted = hists1[idx_height,:]
    str_chan = [str(x) for x in chans]
    lev = np.arange(0,len(Channels))
    bins = np.arange(0,3,1)
    fig, ax = plt.subplots(ncols=3,nrows=1,figsize=(32,32))
    """
    ax[0].barh(lev,cloud_counts[idx_height],label='Cloudy',color='lightgrey')
    ax[0].barh(lev,total-cloud_counts[idx_height],label='Clear',color='lightskyblue',left=cloud_counts[idx_height])
    ax[0].set_yticks(lev)
    ax[0].set_yticklabels(str_chan)
    ax[0].set_ylabel('Channel Number')
    ax[0].set_xlabel('Count')
    ax[0].legend()
    idx_lw = np.where( d['Channels']<chan_lw[-1] )
    idx_mw = np.where( (d['Channels']>=chan_mw[0] ) & ( d['Channels']<chan_mw[-1] ) )
    idx_sw = np.where( (d['Channels']>=chan_sw[0] ) & ( d['Channels']<chan_sw[-1] ) )
    """
    vvmin = 1
    vvmax = max(np.max(hists_sorted),np.max(hists1_sorted))

    im =  ax[0].imshow(hists_sorted,cmap='plasma',norm=LogNorm(vmin=vvmin,vmax=vvmax))
    ax[0].set_xlabel('O-B Bins')
    ax[0].set_yticks(lev)
    middleIndex = (len(edges) - 1)//2
    print(middleIndex)
    bins = [0,middleIndex,len(edges)]
    ax[0].set_xticks(bins)
    ax[0].set_xticklabels([edges[0],edges[middleIndex],edges[-1]])
    ax[0].set_yticklabels(str_chan)
    divider = make_axes_locatable(ax[0])
    cax = divider.new_vertical(size="5%", pad=0.7, pack_start=True)
    fig.add_axes(cax)
    fig.colorbar(im, cax=cax,label='Counts', orientation="horizontal")


    im =  ax[1].imshow(hists1_sorted,cmap='plasma',norm=LogNorm(vmin=vvmin,vmax=vvmax))
    ax[1].set_xlabel('O-B Bins')
    ax[1].set_yticks(lev)
    middleIndex = (len(edges) - 1)//2
    print(middleIndex)
    bins = [0,middleIndex,len(edges)]
    ax[1].set_xticks(bins)
    ax[1].set_xticklabels([edges[0],edges[middleIndex],edges[-1]])
    ax[1].set_yticklabels(str_chan)
    divider = make_axes_locatable(ax[1])
    cax = divider.new_vertical(size="5%", pad=0.7, pack_start=True)
    fig.add_axes(cax)
    fig.colorbar(im, cax=cax,label='Counts', orientation="horizontal")


    diff = hists_sorted-hists1_sorted
    neg_idx = np.where(diff<=0)
    pos_idx = np.where(diff>0)
    log_diff = np.zeros(diff.shape)
    log_diff[neg_idx] = -1.0* np.log10(np.abs(diff[neg_idx]))
    log_diff[pos_idx] = np.log10(diff[pos_idx])
    mdiff = np.max(np.abs(diff))
    norm = Normalize(-1.0*np.log10(mdiff),1.0*np.log10(mdiff))
    im =  ax[2].imshow(log_diff, cmap='coolwarm',norm=norm)
    ax[2].set_xlabel('O-B Bins')
    ax[2].set_yticks(lev)
    middleIndex = (len(edges) - 1)//2
    print(middleIndex)
    bins = [0,middleIndex,len(edges)]
    ax[2].set_xticks(bins)
    ax[2].set_xticklabels([edges[0],edges[middleIndex],edges[-1]])
    ax[2].set_yticklabels(str_chan)
    divider = make_axes_locatable(ax[2])
    cax = divider.new_vertical(size="5%", pad=0.7, pack_start=True)
    fig.add_axes(cax)
    fig.colorbar(im, cax=cax,label='Counts Difference', orientation="horizontal")



    """
    ax[2].plot(omb_ave_clr[idx_height],lev,'bx',label='Mean') 
    ax[2].plot(omb_std_clr[idx_height],lev,'rx',label='Std')
    ax[2].set_xlabel('O-B (clear) [K]')
    ax[2].set_yticks(lev)
    ax[2].set_yticklabels(str_chan)
    ax[2].legend()
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
    """
    plt.savefig(plt_name)
if __name__ == '__main__':
    parser = argparse.ArgumentParser( description = 'Plot profile of statistics from IFS/CADS output.')
    parser.add_argument('--input', help = 'path to first input file', required = True, dest = 'input')
    parser.add_argument('--inputb', help = 'path to first input file', required = True, dest = 'inputb')
    parser.add_argument('--output', help = 'output prefix (will append h5 and txt)', required = True, dest = 'output')
    parser.add_argument('--img', help = 'suffix for plot type default png.', required = False, dest = 'img',default='png')
    a = parser.parse_args()
    nval = 4
    x = 5
    nhist= x*nval+1

    stepRem = 1./nval
    histRange = (-1*x-stepRem,x+stepRem)
    d = read_h5(a.input)
    omb = d['Observation']-d['Background']
    heights = np.zeros(d['Channels'].shape[0])
    cloud_counts = np.zeros(d['Channels'].shape[0])
    bl = np.zeros(d['Channels'].shape[0])
    tropl = np.zeros(d['Channels'].shape[0])
    omb_ave = np.zeros(d['Channels'].shape[0])
    omb_std = np.zeros(d['Channels'].shape[0])
    omb_std_clr = np.zeros(d['Channels'].shape[0])
    omb_ave_clr = np.zeros(d['Channels'].shape[0])
    hists = []
 
    for i,c in enumerate(d['Channels']):
        good_idx, = np.where( d['Cloud_Flags'][:,i] == 0 )
        bad_idx, = np.where( d['Cloud_Flags'][:,i] == 1 )
        cloud_counts[i] = len(bad_idx)
        omb_ave[i] = omb[:,i].mean()
        omb_std[i] = omb[:,i].std()
        omb_ave_clr[i] = omb[good_idx,i].mean()
        omb_std_clr[i] = omb[good_idx,i].std()
        heights[i] = d['Height'][:,i].mean() 
        hist,edges = np.histogram(omb[good_idx,i],bins=nhist,range=histRange)
        hists.append(hist)
    # hard wired for cris, should make more generic
    hists = np.asarray(hists)
    nval = 4
    x = 5
    nhist= x*nval+1

    stepRem = 1./nval
    histRange = (-1*x-stepRem,x+stepRem)
    d = read_h5(a.inputb)
    omb = d['Observation']-d['Background']
    heights = np.zeros(d['Channels'].shape[0])
    cloud_counts = np.zeros(d['Channels'].shape[0])
    bl = np.zeros(d['Channels'].shape[0])
    tropl = np.zeros(d['Channels'].shape[0])
    omb_ave = np.zeros(d['Channels'].shape[0])
    omb_std = np.zeros(d['Channels'].shape[0])
    omb_std_clr = np.zeros(d['Channels'].shape[0])
    omb_ave_clr = np.zeros(d['Channels'].shape[0])
    hists1 = []
 
    for i,c in enumerate(d['Channels']):
        good_idx, = np.where( d['Cloud_Flags'][:,i] == 0 )
        bad_idx, = np.where( d['Cloud_Flags'][:,i] == 1 )
        cloud_counts[i] = len(bad_idx)
        omb_ave[i] = omb[:,i].mean()
        omb_std[i] = omb[:,i].std()
        omb_ave_clr[i] = omb[good_idx,i].mean()
        omb_std_clr[i] = omb[good_idx,i].std()
        heights[i] = d['Height'][:,i].mean() 
        hist,edges = np.histogram(omb[good_idx,i],bins=nhist,range=histRange)
        hists1.append(hist)
    # hard wired for cris, should make more generic
    hists1 = np.asarray(hists1)





    chan_lw = np.arange(1,1+713,1)
    # add 200 to take out the longer wavelength water stuff at the beginning of shortwave stick to R branch for screwing around.
    chan_mw = np.arange(714,714+865,1)
    chan_sw = np.arange(200+714+865,714+865+633,1)
    chan_sw_mw = np.arange(714,714+865+633,1)
    binMids = 0.5 * (edges[1:] + edges[:-1])
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'], hists, hists1, binMids, heights, cloud_counts, omb_ave, omb_std, omb_ave_clr, omb_std_clr, d['Observation'].shape[0], a.output+'_all.'+a.img)
    """ 
    ave_trop = np.asarray(d['trop_level']).mean() 
    ave_bound = np.asarray(d['bl_level']).mean()
 
    idx_bl, = np.where( heights > ave_bound)
    idx_trop, = np.where( (heights > ave_bound) & (heights <= ave_trop) ) 
    idx_strat, = np.where( (heights < ave_trop ) ) 

    # break up by boundary layer, tropopause
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_bl], heights[idx_bl], cloud_counts[idx_bl],\
                omb_ave[idx_bl], omb_std[idx_bl], omb_ave_clr[idx_bl], omb_std_clr[idx_bl],\
                d['Observation'].shape[0], a.output+'_bnd_lyr.'+a.img)

    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_trop], heights[idx_trop], cloud_counts[idx_trop],\
                omb_ave[idx_trop], omb_std[idx_trop], omb_ave_clr[idx_trop], omb_std_clr[idx_trop],\
                d['Observation'].shape[0], a.output+'_bnd2trop.'+a.img)

    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_strat], heights[idx_strat], cloud_counts[idx_strat],\
                omb_ave[idx_strat], omb_std[idx_strat], omb_ave_clr[idx_strat], omb_std_clr[idx_strat],\
                d['Observation'].shape[0], a.output+'_strat.'+a.img)
    """
    # break up by longwave, midwave, and shortwave band
    idx_lw, = np.where( d['Channels']<=chan_lw[-1] )
    idx_mw, = np.where( (d['Channels']>=chan_mw[0] ) & ( d['Channels']<=chan_mw[-1] ) )
    idx_sw, = np.where( (d['Channels']>=chan_sw[0] ) & ( d['Channels']<=chan_sw[-1] ) ) 
    print(hists[idx_lw,:].shape)
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_lw],hists[idx_lw,:], hists1[idx_lw,:], binMids,  heights[idx_lw], cloud_counts[idx_lw],\
                omb_ave[idx_lw], omb_std[idx_lw], omb_ave_clr[idx_lw], omb_std_clr[idx_lw],\
                d['Observation'].shape[0], a.output+'_lw.'+a.img)

    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_mw], hists[idx_mw,:],  hists1[idx_mw,:], binMids, heights[idx_mw], cloud_counts[idx_mw],\
                omb_ave[idx_mw], omb_std[idx_mw], omb_ave_clr[idx_mw], omb_std_clr[idx_mw],\
                d['Observation'].shape[0], a.output+'_mw.'+a.img)
    
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_sw], hists[idx_sw,:],  hists1[idx_sw,:], binMids, heights[idx_sw], cloud_counts[idx_sw],\
                omb_ave[idx_sw], omb_std[idx_sw], omb_ave_clr[idx_sw], omb_std_clr[idx_sw],\
                d['Observation'].shape[0], a.output+'_sw.'+a.img)

    chans_use = np.asarray([1939, 1940, 1941, 1942, 1943, 1944, 1945, 1946, 1947, 1948,\
        1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958,\
        1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968,\
        1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978,\
        1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 2119,\
        2140, 2143, 2147, 2153, 2158, 2161, 2168, 2171, 2175, 2182])

    idx_sw, = np.nonzero(np.in1d(d['Channels'],chans_use))
    idx_mw, = np.nonzero(np.in1d(d['Channels'],chan_mw))
    ii = idx_sw.tolist()
    ii.extend(idx_mw.tolist())
    cc = d['Channels'][ii]
    hh = heights[ii]
    idx_hh = hh.argsort()
    hhh = hh[idx_hh]
    ccc = cc[idx_hh]
    print('whir!')
    for i,cccc in enumerate(ccc):
        print(cccc,hhh[i])
    print('end whir!')    
    plotProfile(chan_lw,chan_mw,chan_sw,d['Channels'][idx_sw], hists[idx_sw,:], hists1[idx_sw,:],binMids, heights[idx_sw], cloud_counts[idx_sw],\
                omb_ave[idx_sw], omb_std[idx_sw], omb_ave_clr[idx_sw], omb_std_clr[idx_sw],\
                d['Observation'].shape[0], a.output+'_sub_sw.'+a.img)


