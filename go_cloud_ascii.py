#!/usr/bin/env python3
import numpy as np
import  cads_wrap
import fortranformat as ftn
import argparse, h5py

def read_cads_input(ff):
    with open(ff,'r') as f:
        data = f.readlines()

    I_SENSOR_ID = int(data[0].split('!')[0])
    I__Num_Chans = int(data[1].strip('\n'))
    #chans = np.zeros(I__Num_Chans,dtype='int')
    lines = (I__Num_Chans)//10
    extra = (I__Num_Chans)%10
    dstart = 2
    chans = []
    for i in list(range(dstart,dstart+lines)):
        cc = data[i].strip('\n').split()
        chans.extend(cc)
    if(extra>0):
        cc = data[dstart+lines].strip('\n').split()
        chans.extend(cc)
        cur = dstart+lines+1
    else:
        cur = dstart+lines
    chans = np.asarray(chans).astype('int')
    I__Num_Observations = int(float(data[cur].strip('\n')))
    cur+=1
    C_Record = data[cur]
    if("!" in C_Record):
        I__Num_Imager_Chans = int(data[cur].split('!')[0].strip(' '))
        cur+=1
        I__Chan_ID_Imager = data[cur].strip('\n').split()
        I__Chan_ID_Imager = np.asarray(I__Chan_ID_Imager).astype('int')
        cur+=1
        I__Num_Imager_Cluster = int(data[cur].strip('\n').split('!')[0])
        Imager=True
        cur+=1
    else:
        Imager = False
    Longitude = []
    Latitude = []
    Land_Fraction = []
    Min_Level = []
    Max_Level = []
    BT_Obser = []
    BT_Model = []
    Chan_Height = []
    Cluster_Fraction = []
    BT_in_Cluster = []
    BT_Overall_Std = []
    BT_Model_Imager = []
    for ii in list(range(0,I__Num_Observations)):
        Z__Longitude, Z__Latitude, Z__Land_Fraction, I__Min_Level, I__Max_Level,_ = data[cur].strip('\n').split()
        Z__Longiutde = float(Z__Longitude)
        Z__Latitude = float(Z__Latitude)
        Z__Land_Fraaction = float(Z__Land_Fraction)
        I__Min_Level = int(I__Min_Level)
        I__Max_Level = int(I__Max_Level)
        cur+=1
        bonus=0
        lines = I__Num_Chans//10
        extra = I__Num_Chans%10
        if(extra>0):
            bonus=1
        Z_BT_Obser = []
        for i in list(range(cur,cur+lines+bonus)):
            cc = data[i].strip('\n').split()
            Z_BT_Obser.extend(cc)
        Z_BT_Obser = np.asarray(Z_BT_Obser).astype('float')
        cur = cur+lines+bonus
        Z_BT_Model = []
        for i in list(range(cur,cur+lines+bonus)):
            cc = data[i].strip('\n').split()
            Z_BT_Model.extend(cc)
        Z_BT_Model = np.asarray(Z_BT_Model).astype('float')
        cur = cur+lines+bonus
        Z_Chan_Height = []
        for i in list(range(cur,cur+lines+bonus)):
            cc = data[i].strip('\n').split()
            Z_Chan_Height.extend(cc)
        Z_Chan_Height = np.asarray(Z_Chan_Height).astype('float')
        cur = cur+lines+bonus
        if(Imager):
            Z__Cluster_Fraction = np.asarray(data[cur].strip('\n').split()).astype('float')
            cur+=1
            bonus=0
            tot = I__Num_Imager_Cluster + I__Num_Imager_Chans
            lines = (I__Num_Imager_Cluster * I__Num_Imager_Chans)//10 
            extra = (I__Num_Imager_Cluster * I__Num_Imager_Chans)%10
            if(extra!=0):
                bonus=1
            Z__BT_in_Cluster = []
            for i in list(range(cur,cur+lines+bonus)):
                cc = data[i].strip('\n').split()
                Z__BT_in_Cluster.extend(cc)
            Z__BT_in_Cluster = np.asarray(Z__BT_in_Cluster).astype('float').reshape(I__Num_Imager_Cluster,I__Num_Imager_Chans)
            cur=cur+lines+bonus
            Z__BT_Overall_SDev = np.asarray(data[cur].strip('\n').split()).astype('float')
            cur+=1
            Z__BT_Model_Imager = np.asarray(data[cur].strip('\n').split()).astype('float')
        Longitude.append(Z__Longitude)
        Latitude.append(Z__Latitude)
        Land_Fraction.append(Z__Land_Fraction)
        Min_Level.append(I__Min_Level)
        Max_Level.append(I__Max_Level)
        BT_Obser.append(Z_BT_Obser)
        BT_Model.append(Z_BT_Model)
        Chan_Height.append(Z_Chan_Height)
        if(Imager):
            Cluster_Fraction.append(Z__Cluster_Fraction)
            BT_in_Cluster.append(Z__BT_in_Cluster)
            BT_Overall_Std.append(Z__BT_Overall_SDev)
            BT_Model_Imager.append(Z__BT_Model_Imager)
    Longitude = np.asarray(Longitude).astype('float')
    Latitude = np.asarray(Latitude).astype('float')
    Land_Fraction = np.asarray(Land_Fraction).astype('float')
    Min_Level = np.asarray(Min_Level).astype('int')
    Max_Level = np.asarray(Max_Level).astype('int')
    BT_Obser = np.asarray(BT_Obser).astype('float')
    BT_Model = np.asarray(BT_Model).astype('float')
    Chan_Height = np.asarray(Chan_Height).astype('float')
    Cluster_Fraction = np.asarray(Cluster_Fraction).astype('float')
    BT_in_Cluster = np.asarray(BT_in_Cluster).astype('float')
    BT_Overall_Std = np.asarray(BT_Overall_Std).astype('float')
    BT_Model_Imager = np.asarray(BT_Model_Imager).astype('float')
    return Longitude,Latitude,Land_Fraction,Min_Level,Max_Level,BT_Obser,BT_Model,Chan_Height,Cluster_Fraction,BT_in_Cluster,BT_Overall_Std,BT_Model_Imager,I_SENSOR_ID, chans 
def output_ascii(filename, flags, Longitude, Latitude):
    fout = open(filename, 'w')
    for i,k__cloud_flag in enumerate(flags):
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
def output_h5(filename, flags, Longitude, Latitude, Height, Ob, Bkg, Sensor, Channels):
    h5 = h5py.File(filename,'w')
    h5.create_dataset('Cloud_Flags',data=np.asarray(flags))
    h5.create_dataset('Longitude',data=np.asarray(Longitude))
    h5.create_dataset('Latitude',data=np.asarray(Latitude))
    h5.create_dataset('Height',data=np.asarray(Height))
    h5.create_dataset('Observation',data=np.asarray(Ob))
    h5.create_dataset('Background',data=np.asarray(Bkg))
    h5.create_dataset('Channels',data=np.asarray(Channels))
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
    parser.add_argument('--ascii', help="Output ASCII", action="store_true", required=False, dest='ascii')
    a = parser.parse_args()

    Longitude,\
    Latitude,\
    Land_Fraction,\
    Min_Level,\
    Max_Level,\
    BT_Obser,\
    BT_Model,\
    Chan_Height,\
    Cluster_Fraction,\
    BT_in_Cluster,\
    BT_Overall_Std,\
    BT_Model_Imager,\
    I__Sensor_ID,\
    chans         = read_cads_input(a.input)
    all_cloud_flags = np.zeros(BT_Obser.shape)
    all_cloud_flags = cads_wrap.wrap_cads_detect_cloud(np.asarray(chans),\
                                                Min_Level,\
                                                Max_Level,\
                                                BT_Obser,\
                                                BT_Model,\
                                                Chan_Height,\
                                                I__Sensor_ID) # 27 is CrIS
    if(a.ascii):
        print("Writing ASCII {}.txt".format(a.output))
        output_ascii(a.output+'.txt', all_cloud_flags,Longitude,Latitude)
    print("Writing H5 {}.h5".format(a.output))
    output_h5(a.output+'.h5',all_cloud_flags,Longitude,Latitude,Chan_Height, BT_Obser, BT_Model, I__Sensor_ID, np.asarray(chans))
    print("Quick test of H5 read in and print fields/dimensions.")
    dd = read_h5(a.output+'.h5')
    for ddd in list(dd.keys()):
        print(ddd,dd[ddd])
