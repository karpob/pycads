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
    parser.add_argument('--input', help = 'path to first input file', required = True, dest = 'input')
    a = parser.parse_args()


    d = read_h5(a.input)

    heights, cloud_counts, omb_ave, omb_std, omb_ave_clr, omb_std_clr = processData(d)
    # hard wired for cris, should make more generic
    chan_lw = np.arange(1,1+713,1)
    # 200 added to only label R branch as "shortwave"
    chan_mw = np.arange(714,714+865,1)
    chan_sw = np.arange(714+865,714+865+633,1)
    h5out= h5py.File('chans.h5','w')
    h5out.create_dataset('Channels',data=np.asarray(d['Channels']))
    h5out.close()

    chan_sw = np.arange(200+714+865,714+865+633,1)
    idx_lw = np.where( d['Channels']<=chan_lw[-1] )
    idx_mw = np.where( (d['Channels']>=chan_mw[0] ) & ( d['Channels']<=chan_mw[-1] ) )
    idx_sw = np.where( (d['Channels']>=chan_sw[0] ) & ( d['Channels']<=chan_sw[-1] ) ) 
    lw_chans_b1 =np.asarray([  1, 5, 9, 13, 17, 18, 19, 20, 21, 22,\
                 23, 24, 25, 26, 27, 28, 29, 30, 31, 32,\
                 33, 34, 35, 36, 37, 38, 39, 40, 41, 42,\
                 43, 44, 45, 46, 47, 48, 49, 50, 51, 52,\
                 53, 54, 55, 56, 57, 58, 59, 60, 61, 62,\
                 63, 64, 65, 66, 67, 68, 69, 70, 71, 72,\
                 73, 74, 75, 76, 77, 78, 79, 80, 81, 82,\
                 83, 84, 85, 86, 87, 88, 91, 92, 93, 94,\
                 95, 96, 97, 99, 101, 105, 107, 109, 111, 113,\
                 115, 116, 117, 118, 119, 120, 121, 122, 123, 124,\
                 125, 133, 135, 137, 139, 141, 144, 147, 161, 173,\
                 177, 181, 185, 195, 210, 221, 225, ]) 
                 
                 
    idx_lw_b1 = np.nonzero(np.in1d(lw_chans_b1, d['Channels']))
    print('lw idx',idx_lw_b1)
    lw_b1_heights = heights[idx_lw_b1]
    sw_heights = heights[idx_sw]
    matching_heights = []
    matching_chans = []
    sw_heights_m = np.ma.array(sw_heights,mask=False)
    drop_idx, = np.where( (chan_sw>=1900) & (chan_sw<1961)  )
    sw_heights_m.mask[drop_idx] = True
    drop_idx = np.where((chan_sw>1987))
    sw_heights_m.mask[drop_idx] = True
    drop_idx = np.where(chan_sw==1780)
    sw_heights_m.mask[drop_idx] = True

    drop_idx = np.where(chan_sw==1782)
    sw_heights_m.mask[drop_idx] = True

    drop_idx = np.where(chan_sw==1784)
    sw_heights_m.mask[drop_idx] = True

    drop_idx = np.where(chan_sw==1786)
    sw_heights_m.mask[drop_idx] = True


    drop_idx = np.where(chan_sw==1788)
    sw_heights_m.mask[drop_idx] = True


    drop_idx = np.where(chan_sw==1791)
    sw_heights_m.mask[drop_idx] = True

    drop_idx = np.where(chan_sw==1792)
    sw_heights_m.mask[drop_idx] = True

    for i,h in enumerate(lw_b1_heights):
        ii = np.abs(sw_heights_m-h).argmin()
        cmatch = chan_sw[ii]
        if(cmatch not in matching_chans):
            matching_chans.append(cmatch)
            sw_heights_m.mask[ii] = True
            matching_heights.append(sw_heights_m[ii])
    print(matching_chans)
    matching_chans.sort() 
    batches = len(matching_chans)//10
    extra = False
    if(batches%10!=0):
        extra=True
    iiii=0           
    for iii in range(batches):
        print(matching_chans[iiii:iiii+10])
        iiii+=10
    if(extra):
        print(matching_chans[iiii:len(matching_chans)])
    matching_chans=[]
    lw_band2= [3, 6, 7, 8, 10, 12, 14, 15, 16, 89,
              90, 102, 103, 104, 106, 108, 110, 114, 126, 127,\
              129, 132, 134, 138, 140, 143, 145, 146, 148, 149,\
              150, 151, 153, 155, 156, 157, 158, 159, 162, 163,\
              164, 165, 166, 169, 170, 171, 172, 175, 180, 189,\
              200, 201, 205, 206, 214, 217, 218, 226, 228, 230,\
              231, 233, 236, 237, 240, 241, 245, 248, 252, 264,\
              265, 281, 285, 297, 324, 327, 361, 378, 389, 392,\
              400, 473, 493, 500, 503, 511, 527, 528, 529, 530,\
              531, 534, 538, 542, 544, 545, 547, 550, 553, 555,\
              590, 594, 598, 602, 606, 610, 614, 618, 622, 626,\
              645, 649, 653, 657, 661, 665, 685, 702, 703, 704,\
              706, 707, 713]
    idx_lw_b2 = np.nonzero(np.in1d(lw_chans_b1, d['Channels']))
    print('lw idx',idx_lw_b1)
    lw_b2_heights = heights[idx_lw_b2]
    for i,h in enumerate(lw_b2_heights):
        ii = np.abs(sw_heights_m-h).argmin()
        cmatch = chan_sw[ii]
        if(cmatch not in matching_chans):
            matching_chans.append(cmatch)
            sw_heights_m.mask[ii] = True
            matching_heights.append(sw_heights_m[ii])
    matching_chans.sort() 
    batches = len(matching_chans)//10
    extra = False
    if(batches%10!=0):
        extra=True
    iiii=0           
    for iii in range(batches):
        print(matching_chans[iiii:iiii+10])
        iiii+=10
    if(extra):
        print(matching_chans[iiii:len(matching_chans)])

    mw_bands = [717, 725, 728, 729, 730, 731, 732, 733, 734, 735,\
    736, 741, 749, 757, 765, 773, 781, 789, 794, 797,\
    805, 806, 815, 822, 829, 839, 845, 853, 861, 868,\
    869, 872, 877, 885, 887, 893, 898, 900, 909, 912,\
    915, 917, 921, 929, 933, 941, 949, 957, 963, 965,\
    973, 975, 978, 981, 989, 991, 993, 996, 1005, 1014,\
    1025, 1029, 1037, 1042, 1053, 1061, 1073, 1077, 1085, 1093,\
    1101, 1109, 1117, 1125, 1133, 1141, 1149, 1157, 1164, 1165,\
    1173, 1181, 1189, 1197, 1205, 1213, 1221, 1251]

    idx_mw = np.nonzero(np.in1d(mw_bands, d['Channels']))

    idx_mw = np.where( (d['Channels']>=chan_mw[0] ) & ( d['Channels']<=chan_mw[-1] ) )
    print('lw mw',idx_mw)
    mw_heights = heights[idx_mw]
    matching_chans = []
    for i,h in enumerate(mw_heights):
        ii = np.abs(sw_heights_m-h).argmin()
        cmatch = chan_sw[ii]
        if(cmatch not in matching_chans):
            matching_chans.append(cmatch)
            sw_heights_m.mask[ii] = True
            matching_heights.append(sw_heights_m[ii])
    matching_chans.sort() 
    batches = len(matching_chans)//10
    extra = False
    print(matching_chans)
    if(batches%10!=0):
        extra=True
    iiii=0           
    for iii in range(batches):
        print(matching_chans[iiii:iiii+10])
        iiii+=10
    if(extra):
        print(matching_chans[iiii:len(matching_chans)])

    used = [1779, 1781, 1783, 1785, 1787, 1789, 1790, 1793, 1794, 1795,\
            1796, 1797, 1798, 1799, 1800, 1801, 1802, 1803, 1804, 1805,\
            1806, 1807, 1808, 1809, 1810, 1811, 1812, 1813, 1814, 1815,\
            1816, 1817, 1818, 1819, 1820, 1821, 1822, 1823, 1824, 1825,\
            1826, 1827, 1828, 1829, 1830, 1831, 1832, 1833, 1834, 1835,\
            1836, 1837, 1838, 1839, 1840, 1841, 1842, 1843, 1844, 1845,\
            1846, 1847, 1848, 1849, 1850, 1851, 1852, 1853, 1854, 1855,\
            1856, 1858, 1859, 1861, 1862, 1864, 1867, 1869, 1874, 1883,\
            1885, 1886, 1887, 1888, 1889, 1890, 1891, 1892, 1893, 1894,\
            1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968, 1969, 1970,\
            1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980,\
            1981, 1982, 1983, 1984, 1985, 1986, 1987, 2119, 2120, 2121,\
            2122, 2127, 2140, 2143, 2147, 2153, 2158, 2161, 2167, 2168,\
            2170, 2171, 2175, 1788, 1782, 1780, 1786, 1784, 1739, 1866,\
            1742, 1744, 1745, 1732, 1713, 1650, 1673, 1678, 1691, 1695,\
            1703, 1705, 1711, 1721, 1757, 1769, 1857, 1860, 1863, 1865,\
            1868, 1870, 1871, 1872, 1873, 1875, 1876, 1877, 1878, 1879,\
            1880, 1881, 1882, 1884, 1896, 1897, 1899, 1939, 1940, 1941,\
            1942, 1943, 1944, 1945, 1947, 1948, 1949, 1950, 1951, 1952,\
            1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960]
    used= np.asarray(np.unique(used))
    used.sort()
    matching_chans = used    
    batches = len(matching_chans)//10
    extra = False
    print(matching_chans)
    if(batches%10!=0):
        extra=True
    iiii=0           
    for iii in range(batches):
        print(matching_chans[iiii:iiii+10])
        iiii+=10
    if(extra):
        print(matching_chans[iiii:len(matching_chans)])


    idx = np.nonzero(np.in1d(used, d['Channels']))
    used_heights = heights[idx]
    iidx = np.where(used==1950)
    h50 = used_heights[iidx]
    lower_idx = np.where(used_heights> h50)
    upper_idx = np.where(used_heights< h50)
    print(used[lower_idx])
    print(used[upper_idx])

    matching_chans = used[lower_idx].tolist()    
    batches = len(matching_chans)//10
    extra = False
    print(matching_chans)
    if(batches%10!=0):
        extra=True
    iiii=0           
    for iii in range(batches):
        print(matching_chans[iiii:iiii+10])
        iiii+=10
    if(extra):
        print(matching_chans[iiii:len(matching_chans)])

    matching_chans = used[upper_idx].tolist()    
    batches = len(matching_chans)//10
    extra = False
    print(matching_chans)
    if(batches%10!=0):
        extra=True
    iiii=0           
    for iii in range(batches):
        print(matching_chans[iiii:iiii+10])
        iiii+=10
    if(extra):
        print(matching_chans[iiii:len(matching_chans)])


