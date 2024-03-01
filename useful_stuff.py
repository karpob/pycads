import numpy as np

odb_dir='/perm/ecmv1501/data/odb/'
ascii_dir='/perm/ecmv1501/data/txt/'

# List of sensor IDs ("groups" in ODB)
sensors={'iasi':16,
         'cris':27,
         'airs':11,
         'hiras':97,
         'giirs':98,
         'irs':57,    # placeholder
         'iasing':59} # placeholder

# Function to write arrays to file (10 values per line)
def print_rows(f,in_arr,format_string,cols):
    j=0
    for j in np.arange(len(in_arr)):
        f.write(format_string.format(in_arr[j]))
        if (j+1)%cols==0:
            f.write('\n')
    if (j+1)%cols!=0:
        f.write('\n')


# List of ODB columns to extract
odb_list_master=['vertco_reference_1@body','fg_depar@body','obsvalue@body',
                 'biascorr_fg@body','lat@hdr','lon@hdr','seqno@hdr','lsm@modsurf',
                 'boundary_lev@radiance','trop_lev@radiance','rank_cld@radiance_body','datum_event1@body','solar_zenith@sat']

db_list_ref=['vertco_reference_1@body','lat@hdr','lon@hdr',
              'seqno@hdr','datum_event1@body']


# List of collocated imager columns to extract
odb_list_imager=['avhrr_frac_cl1@collocated_imager_information',
                 'avhrr_frac_cl2@collocated_imager_information',
                 'avhrr_frac_cl3@collocated_imager_information',
                 'avhrr_frac_cl4@collocated_imager_information',
                 'avhrr_frac_cl5@collocated_imager_information',
                 'avhrr_frac_cl6@collocated_imager_information',
                 'avhrr_frac_cl7@collocated_imager_information',
                 'avhrr_m_ir1_cl1@collocated_imager_information',
                 'avhrr_m_ir1_cl2@collocated_imager_information',
                 'avhrr_m_ir1_cl3@collocated_imager_information',
                 'avhrr_m_ir1_cl4@collocated_imager_information',
                 'avhrr_m_ir1_cl5@collocated_imager_information',
                 'avhrr_m_ir1_cl6@collocated_imager_information',
                 'avhrr_m_ir1_cl7@collocated_imager_information',
                 'avhrr_m_ir2_cl1@collocated_imager_information',
                 'avhrr_m_ir2_cl2@collocated_imager_information',
                 'avhrr_m_ir2_cl3@collocated_imager_information',
                 'avhrr_m_ir2_cl4@collocated_imager_information',
                 'avhrr_m_ir2_cl5@collocated_imager_information',
                 'avhrr_m_ir2_cl6@collocated_imager_information',
                 'avhrr_m_ir2_cl7@collocated_imager_information',
                 'avhrr_stddev_ir@collocated_imager_information',
                 'avhrr_stddev_ir2@collocated_imager_information',
                 'avhrr_fg_ir1@collocated_imager_information',
                 'avhrr_fg_ir2@collocated_imager_information']

# Function to populate dictionary with the columns present for all sensors
def init_all(df):
    all_main = {}
    for k in list(df.keys()):
        print(k)
        if('seqno' in k):
            all_main['seqno'] = np.array(df['seqno@hdr']).astype(int)
        elif('vertco' in k):
            all_main['ch'] = np.array(df['vertco_reference_1@body']).astype(int)
        elif('biascor' in k):
            all_main['bc'] = np.array(df['biascorr_fg@body']).astype(float)
        elif('obsvalue' in k):
            all_main['obsvalue'] = np.array(df['obsvalue@body']).astype(float)
        elif('fg_depar' in k):
            all_main['fg_depar'] = np.array(df['fg_depar@body']).astype(float)
        elif('lat' in k):
            all_main['lat'] = np.array(df['lat@hdr']).astype(float)
        elif('lon' in k):
            all_main['lon'] = np.array(df['lon@hdr']).astype(float)
        elif('lsm' in k):
            all_main['lsm'] = np.array(df['lsm@modsurf']).astype(float)
        elif('boundary' in k):
            all_main['bl_level'] = np.array(df['boundary_lev@radiance']).astype(int)
        elif('trop_lev' in k):
            all_main['trop_level'] = np.array(df['trop_lev@radiance']).astype(int)
        elif('rank_cld' in k):
            all_main['rank_cld']= np.array(df['rank_cld@radiance_body']).astype(float)
        elif('datum_event1' in k):
            all_main['datum_event1']= np.array(df['datum_event1@body']).astype(float)
        elif('solar_zenith' in k):
            all_main['solar_zenith']= np.array(df['solar_zenith@sat']).astype(float)

        del df[k]
    return all_main

# Function to populate dictionary with imager information
def init_im(df):
    all_im = {'im_frac1': np.array(df['avhrr_frac_cl1@collocated_imager_information']).astype(float),
              'im_frac2': np.array(df['avhrr_frac_cl2@collocated_imager_information']).astype(float),
              'im_frac3': np.array(df['avhrr_frac_cl3@collocated_imager_information']).astype(float),
              'im_frac4': np.array(df['avhrr_frac_cl4@collocated_imager_information']).astype(float),
              'im_frac5': np.array(df['avhrr_frac_cl5@collocated_imager_information']).astype(float),
              'im_frac6': np.array(df['avhrr_frac_cl6@collocated_imager_information']).astype(float),
              'im_frac7': np.array(df['avhrr_frac_cl7@collocated_imager_information']).astype(float),
              'im_m1_c1': np.array(df['avhrr_m_ir1_cl1@collocated_imager_information']).astype(float),
              'im_m1_c2': np.array(df['avhrr_m_ir1_cl2@collocated_imager_information']).astype(float),
              'im_m1_c3': np.array(df['avhrr_m_ir1_cl3@collocated_imager_information']).astype(float),
              'im_m1_c4': np.array(df['avhrr_m_ir1_cl4@collocated_imager_information']).astype(float),
              'im_m1_c5': np.array(df['avhrr_m_ir1_cl5@collocated_imager_information']).astype(float),
              'im_m1_c6': np.array(df['avhrr_m_ir1_cl6@collocated_imager_information']).astype(float),
              'im_m1_c7': np.array(df['avhrr_m_ir1_cl7@collocated_imager_information']).astype(float),
              'im_m2_c1': np.array(df['avhrr_m_ir2_cl1@collocated_imager_information']).astype(float),
              'im_m2_c2': np.array(df['avhrr_m_ir2_cl2@collocated_imager_information']).astype(float),
              'im_m2_c3': np.array(df['avhrr_m_ir2_cl3@collocated_imager_information']).astype(float),
              'im_m2_c4': np.array(df['avhrr_m_ir2_cl4@collocated_imager_information']).astype(float),
              'im_m2_c5': np.array(df['avhrr_m_ir2_cl5@collocated_imager_information']).astype(float),
              'im_m2_c6': np.array(df['avhrr_m_ir2_cl6@collocated_imager_information']).astype(float),
              'im_m2_c7': np.array(df['avhrr_m_ir2_cl7@collocated_imager_information']).astype(float),
              'im_stdev1': np.array(df['avhrr_stddev_ir@collocated_imager_information']).astype(float),
              'im_stdev2': np.array(df['avhrr_stddev_ir2@collocated_imager_information']).astype(float),
              'im_fg1': np.array(df['avhrr_fg_ir1@collocated_imager_information']).astype(float),
              'im_fg2': np.array(df['avhrr_fg_ir2@collocated_imager_information']).astype(float)}
    return all_im

def init_ref(df):
    all_ref = {'seqno': np.array(df['seqno@hdr']).astype(int),
               'ch': np.array(df['vertco_reference_1@body']).astype(int),
               'lat': np.array(df['lat@hdr']).astype(float),
               'lon': np.array(df['lon@hdr']).astype(float),
               'datum_event1': np.array(df['datum_event1@body']).astype(int)}
    return all_ref


