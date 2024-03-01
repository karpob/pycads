#!/usr/bin/env python3
import numpy as np
import codc, argparse,h5py
import cads_wrap
import fortranformat as ftn
from useful_stuff import odb_list_master, odb_list_imager, init_all, init_im, \
                         print_rows, sensors, odb_dir, ascii_dir

def output_ascii(filename, flags, Longitude, Latitude, counter):
    fout = open(filename, 'w')
    counter.sort()
    for i,ii in enumerate(counter):
        k__cloud_flag = flags[i]
        linez = np.size(k__cloud_flag)//20
        extraz = np.size(k__cloud_flag)%20
        startz = 0
        end = 20
        fout.write('Longitude Latitude ObNumber\n')
        line = ftn.FortranRecordWriter('2(1X,F9.4),1X,I8')
        ll = line.write(np.asarray([Longitude[i],Latitude[i],i+1]))
        fout.write(ll+"\n")
        fout.write('Cloud flags\n') 
        for ii in list(range(0,linez)):
            tmp = []
            for tt in k__cloud_flag[startz:end]:
                tmp.append(tt)
            joined_string = ' '.join([str(v) for v in tmp])
            now = joined_string
            fout.write(" "+now+'\n')
            startz+=20
            end+=20
        if(extraz>0):
            tmp = []
            for tt in k__cloud_flag[end-20:end-20+extraz]:
                tmp.append(tt)
            joined_string = ' '.join([str(v) for v in tmp])
            now = joined_string
            fout.write(" "+now+'\n')
    fout.close()
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
    idx_w_null,_ = np.where(all_main['datum_event1'] == 6 )
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
    parser.add_argument('--output', help = 'output prefix (will append h5 and txt)', required = True, dest = 'output')
    parser.add_argument('--sensor', help = 'sensor (cris, iasi, etc..)', required = False, dest = 'sensor',default='cris')
    parser.add_argument('--ascii', help="Output ASCII", action="store_true", required=False, dest='ascii')
    a = parser.parse_args()

    print("Reading ODB")
    inputOdb = read_ofb(a.input, sensor=a.sensor)
    all_cloud_flags = []
    all_cloud_flags = np.zeros(np.asarray(inputOdb['ob'][:]).shape)
    print("Running CADS Cloud detect")
    all_cloud_flags = cads_wrap.wrap_cads_detect_cloud(inputOdb['chan'][0],\
                                                 np.asarray(inputOdb['bl_level']),\
                                                 np.asarray(inputOdb['trop_level']),\
                                                 np.asarray(inputOdb['ob']),\
                                                 np.asarray(inputOdb['bkg']),\
                                                 np.asarray(inputOdb['Height']),\
                                                27) # 27 is CrIS
    if(a.ascii):
        print("Writing ASCII {}.txt".format(a.output))
        output_ascii(a.output+".txt",all_cloud_flags,inputOdb['Longitude'],inputOdb['Latitude'],inputOdb['counter'])
    print("Writing H5 {}.h5".format(a.output))
    output_h5(a.output+'.h5',all_cloud_flags,inputOdb, 27)
    dd = read_h5(a.output+'.h5')
    print("Quick test of H5 read in and print fields/dimensions.")
    dd = read_h5(a.output+'.h5')
    for ddd in list(dd.keys()):
        print(ddd,dd[ddd])

