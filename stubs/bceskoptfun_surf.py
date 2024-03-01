#!/usr/bin/env python3
from namelist_io import getParmAllSwMw, getParmLwMw, writeNamelist, getParmSwMw3band
import numpy as np
import codc, argparse,h5py
import fortranformat as ftn
from useful_stuff import odb_list_master, odb_list_imager, init_all, init_im, \
                         print_rows, sensors, odb_dir, ascii_dir

from scipy.optimize import minimize
from skopt.space import Real
from skopt.space import Integer
from skopt import gp_minimize
from skopt.utils import use_named_args
import skopt
from collections import namedtuple
#from keras.losses import BinaryCrossentropy as BCE
import cads_wrap,os
def BinaryCrossEntropy(y_true, y_pred):
    y_pred = np.clip(y_pred, 1e-7, 1 - 1e-7)
    term_0 = (1-y_true) * np.log(1-y_pred + 1e-7)
    term_1 = y_true * np.log(y_pred + 1e-7)
    return -np.mean(term_0+term_1, axis=0)

def read_ofb(filename, imager=False, max_seqno=6e23,sensor='cris' ):
    sensor_id=sensors[sensor]

    # Prepare list of ODB columns to extract
    odb_list=odb_list_master
    if imager:
        odb_list=odb_list+odb_list_imager

    # Read ODB
    df = codc.read_odb(filename, single=True,columns=odb_list)

    # Populate dictionaries with values from ODB
    all_main=init_all(df)
    if imager:
        all_im=init_im(df)


    # Process for all channels in OFB
    chans=np.unique(all_main['ch'])
    n_chans=len(chans)
    n_seqnos = len(all_main['seqno']) / n_chans
    # Unique seqnos represent distinct spectra
    unique_seqnos=np.unique(all_main['seqno'])
    n_seqnos=len(unique_seqnos)
    # Either process all the seqnos in the ODB, or max_seqno; whichever is smaller
    n_profs=np.minimum(max_seqno,n_seqnos)

    items = ['Latitude','Longitude','lsm','bl_level','trop_level','ob','bkg','Height','chan','counter' ]
    dout = {}
   
    if imager:
        img_items = ['fracs','means','stds','im_bkg']
        all_items = items+img_items
    else:
        all_items = items
    for k in all_items:
        dout[k] = []
 


    # Loop over unique seqnos
    sort_idx = np.lexsort([all_main['ch'],all_main['seqno']])
    for k in list(all_main.keys()):
        tmp = all_main[k][sort_idx].reshape(n_seqnos,n_chans)
        all_main[k] = tmp
    idx_w_null,_ = np.where((all_main['datum_event1'] == 6) | (all_main['solar_zenith']<90.0)| (all_main['lsm']!=0.0))
    for k in list(all_main.keys()):
        all_main[k] = np.delete(all_main[k],idx_w_null,0)
    # Calculate background and apply bias correction to obs
    bgvalue=all_main['obsvalue']-(all_main['fg_depar']+all_main['bc'])
    obsvalue_corr=all_main['obsvalue']-all_main['bc']
    counter=0
    n_seqnos = bgvalue.shape[0]
    for i in np.arange(n_seqnos):
        if counter>n_profs:
            break

        # Select values that exist for each seqno (order by channel index)
        this_ch = all_main['ch'][i,:]
        this_ob = obsvalue_corr[i,:]
        this_bg = bgvalue[i,:]
        this_rank_cld = all_main['rank_cld'][i,:]

        # Select and write one-per-spectrum values
        this_lat = all_main['lat'][i,0]
        this_lon=all_main['lon'][i,0]
        this_lsm=all_main['lsm'][i,0]
        this_bl=all_main['bl_level'][i,0]
        this_trop=all_main['trop_level'][i,0]
        dout['Latitude'].append(this_lat)
        dout['Longitude'].append(this_lon)
        dout['lsm'].append(this_lsm)
        dout['bl_level'].append(this_bl)
        dout['trop_level'].append(this_trop)
        dout['chan'].append(this_ch)
        dout['ob'].append(this_ob)
        dout['bkg'].append(this_bg)
        dout['Height'].append(this_rank_cld)
 
        # Write imager information
        if imager:
            im_fracs=np.array([all_im['im_frac1'][i,0],all_im['im_frac2'][i,0],
                               all_im['im_frac3'][i,0],all_im['im_frac4'][i,0],
                               all_im['im_frac5'][i,0],all_im['im_frac6'][i,0],
                               all_im['im_frac7'][i,0]])
            im_means=np.array([all_im['im_m1_c1'][i,0],all_im['im_m2_c1'][i,0],
                               all_im['im_m1_c2'][i,0],all_im['im_m2_c2'][i,0],
                               all_im['im_m1_c3'][i,0],all_im['im_m2_c3'][i,0],
                               all_im['im_m1_c4'][i,0],all_im['im_m2_c4'][i,0],
                               all_im['im_m1_c5'][i,0],all_im['im_m2_c5'][i,0],
                               all_im['im_m1_c6'][i,0],all_im['im_m2_c6'][i,0],
                               all_im['im_m1_c7'][i,0],all_im['im_m2_c7'][i,0]])
            # Overall standard deviations for each IR channel
            im_stds=np.array([all_im['im_stdev1'][i,0],all_im['im_stdev1'][i,0]])
            # Background BT for each IR channel
            im_fgs=np.array([all_im['im_fg1'][i,0],all_im['im_fg2'][i,0]])
            dout['fracs'].append(im_fracs)
            dout['means'].append(im_means)
            dout['stds'].append(im_stds)
            im_fgs=np.array([all_im['im_fg1'][mask][0],all_im['im_fg2'][mask][0]])
            dout['im_bkg'].append(im_fgs)

        dout['counter'].append(counter)
        counter+=1
    return dout



def replaceParms(x,d):
    #1.3859235346353243
    d['R__BT_Threshold'][1] = x[0]  
    d['N__Window_Width'][1] = int(x[1])
    return d

def output_h5(filename, flags, dout, Sensor):
    h5 = h5py.File(filename,'w')
    h5.create_dataset('Cloud_Flags',data=np.asarray(flags))
    for k in list(dout.keys()):
        if k == 'ob':
            h5.create_dataset('Observation', data=np.asarray(dout['ob']) )
        elif k == 'bkg':
            h5.create_dataset('Background', data=np.asarray(dout['bkg']) )
        elif k == 'chan':
            h5.create_dataset('Channels', data=np.asarray(dout['chan'][0]) )
        else:
            h5.create_dataset(k ,data=np.asarray(dout[k]) )
    h5.attrs['Sensor'] = Sensor
    h5.close()    
def read_h5(filename):
    h5 = h5py.File(filename,'r')
    keys = list(h5.keys())
    d = {}
    for k in keys:
        d[k] = h5.get(k)
    return d




if __name__ == "__main__":
    parser = argparse.ArgumentParser( description = 'Run cads using traditional CADS ascii input file. Will output stardard cads output ascii, and h5 with output prefix')
    parser.add_argument('--input', help = 'path to first input file', required = True, dest = 'input')
    a = parser.parse_args()
    # Run CADS for refrence longwave midwave case
    print("Reading ODB")
    inputOdb = read_ofb(a.input)
    print("Writing LWMW namelist")
    d = getParmLwMw()
    writeNamelist('CRIS_CLDDET.NL',d) 
    print("Running CADS Cloud detect")
    omb_lw = np.asarray(inputOdb['ob']) - np.asarray(inputOdb['bkg'])
    all_cloud_flags_lw_mw = cads_wrap.wrap_cads_detect_cloud(inputOdb['chan'][0],\
                                                 np.asarray(inputOdb['bl_level']),\
                                                 np.asarray(inputOdb['trop_level']),\
                                                 np.asarray(inputOdb['ob']),\
                                                 np.asarray(inputOdb['bkg']),\
                                                 np.asarray(inputOdb['Height']),\
                                                27) # 27 is CrIS

    #output_h5('ctl.h5',all_cloud_flags_lw_mw,inputOdb, 27)
    x00 = Real(name='x00',low=0.1, high=2.0)
    x01 = Integer(name='x01',low=1,high=20)

    dimensions =[x00,x01]

    # Maybe this could be made less "weird" by using partial function (not sure about the name) library?
    # new objective passed to the optimizer
    @use_named_args(dimensions)
    def objective( x00, x01 ):
        xx= np.zeros(2)
        xx[0] = x00 
        xx[1] = x01 
        os.environ['CADS_THRESH'] = '{}'.format(xx[0])
        os.environ['CADS_WIN'] = '{}'.format(int(xx[1])) 
        for xxx in xx:
            print(xxx)
        #d = replaceParms(xx,d_init)
        #writeNamelist('CRIS_CLDDET.NL',d)
        os.system('go_cloud_odb.py --input whir --output tmp')
        d_init = getParmAllSwMw() 
        d = replaceParms(x,d_init)
        writeNamelist('CRIS_CLDDET.NL',d)
        new_flags = cads_wrap.wrap_cads_detect_cloud(inputOdb['chan'][0],\
                                                 np.asarray(inputOdb['lsm']),\
                                                 np.asarray(inputOdb['bl_level']),\
                                                 np.asarray(inputOdb['ob']),\
                                                 np.asarray(inputOdb['bkg']),\
                                                 np.asarray(inputOdb['Height']),\
                                                27) # 27 is CrIS

 
        flags_in = all_cloud_flags_lw_mw
        chans = inputOdb['chan'][0]
        chans_use= np.asarray([ 2140, 2143, 2147, 2153, 2158, 2161, 2168, 2171, 2175, 2182])
        #chans_use= np.asarray([2140,2143]) 
        #       1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958,\
        #        1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968,\
        #        1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978,\
        #        1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 2119,\
        #        2140, 2143, 2147, 2153, 2158, 2161, 2168, 2171, 2175, 2182])
 

        consider_idx, = np.nonzero(np.in1d(chans,chans_use))
        print('chan idx used',consider_idx)
        print('chans used.',chans[consider_idx])
         
        ytrue = flags_in[:,consider_idx]
        ypred = new_flags[:,consider_idx]
        omb_sub = omb_lw[:,consider_idx]
        idxsw = np.where(new_flags[:,consider_idx]==0)
        idxlw = np.where(flags_in[:,consider_idx]==0)
        
        somb_sw = omb_sub[idxsw].std()
        somb_lw = omb_sub[idxlw].std()
        nomb = np.abs((somb_sw-somb_lw)/somb_lw)*100.0
        print(ypred.shape,ytrue.shape)
        perfect = np.zeros([1,ytrue.shape[1]])
        print(np.where(ytrue == perfect))
        idx_tp = np.where(ytrue == perfect)
        idx_pp = np.where(ypred == perfect)
        uidx_tp = np.unique(idx_tp[0])
        uidx_pp = np.unique(idx_pp[0])
        print('whir', np.nonzero(np.in1d(uidx_pp, uidx_tp)))
        print('uidx_tp', len(uidx_tp))
        print('uidx_pp', len(uidx_pp))
        cnt_perfecta = np.nonzero(np.in1d(uidx_pp, uidx_tp))
        cnt_perfect = len(cnt_perfecta[0])
        #bce =  BCE()
        # cost = 1e3*bce(ytrue,ypred).numpy()
        #iidx, = np.where(ytrue<1.0)

        #idx, = np.where((ytrue<1.0) & (ypred<1.0))
        #cnt0 = len(idx)

        #idx, = np.where((ytrue>0.0) & (ypred>0.0))
        #cnt1 = len(idx)

        #idx, = np.nonzero(ytrue==ypred)
        #cnt2 = len(idx)
        #bce = BinaryCrossEntropy(ytrue[iidx].reshape(-1,1),ypred[iidx].reshape(-1,1))
        print('matches,1/cost,cost,average omb difference',cnt_perfect,1/cnt_perfect,nomb)
        #cost = 1/cnt_perfect 
        return nomb
    # use this to get around weird numpy thing with deprecated use on np.int
    np.int = int
    # run search with new objective
    result = gp_minimize(
        func=objective,
        dimensions=dimensions,
        x0=[1.2,5],
        n_calls=100
    )
    print(result)
    #x = [0.9677610718637996, 1, 2, 8, 2, 10, 5, 10, 3, 2.0, 0.4809850343108193, 0.14373484737766287, 1.1913949398289987, 0.001, 1.6441282551758343, 1.5054393681397513, 1.8486690073442311]

    replaceParms(result.x,d)
    writeNamelist('CRIS_CLDDET.NL_FINAL_BCE',d)
    skopt.dump(result, 'results_bce.pkl')

