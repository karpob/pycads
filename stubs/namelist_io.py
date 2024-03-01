import numpy as np

def getParmLwMwOpen():
    d = {}
    d['N__Bands'] = {}
    d['N__Window_Bounds1'] = {}
    d['N__Window_Bounds2'] = {}
    d['R__Window_Grad_Threshold'] = {}
    d['N__GradChkInterval'] ={}
    d['N__Window_Width'] = {}
    d['R__BT_Threshold'] = {}
    d['R__Grad_Threshold'] = {}
    d['N__BandToUse'] = {}
    d['L__Do_Quick_Exit'] = ".TRUE."
    d['L__Do_CrossBand'] = ".TRUE."
    d['L__Do_Imager_Cloud_Detection'] = ".FALSE."
    
    d['N__Bands'][1] =   [  1,    5,    9,   13,   17,   18,   19,   20,   21,   22,\
                     23,   24,   25,   26,   27,   28,   29,   30,   31,   32,\
                     33,   34,   35,   36,   37,   38,   39,   40,   41,   42,\
                     43,   44,   45,   46,   47,   48,   49,   50,   51,   52,\
                     53,   54,   55,   56,   57,   58,   59,   60,   61,   62,\
                     63,   64,   65,   66,   67,   68,   69,   70,   71,   72,\
                     73,   74,   75,   76,   77,   78,   79,   80,   81,   82,\
                     83,   84,   85,   86,   87,   88,   91,   92,   93,   94,\
                     95,   96,   97,   99,  101,  105,  107,  109,  111,  113,\
                    115,  116,  117,  118,  119,  120,  121,  122,  123,  124,\
                    125,  133,  135,  137,  139,  141,  144,  147,  161,  173,\
                    177,  181,  185,  195,  210,  221,  225,  229,  249,  257,\
                    269,  273,  293,  301,  317,  333,  349,  369,  409,  433,\
                    457,  481,  501,  549,  701,  705,  709]

    d['N__Bands'][2] = [   3,    6,    7,    8,   10,   12,   14,   15,   16,   89,\
                    90,  102,  103,  104,  106,  108,  110,  114,  126,  127,\
                   129,  132,  134,  138,  140,  143,  145,  146,  148,  149,\
                   150,  151,  153,  155,  156,  157,  158,  159,  162,  163,\
                   164,  165,  166,  169,  170,  171,  172,  175,  180,  189,\
                   200,  201,  205,  206,  214,  217,  218,  226,  228,  230,\
                   231,  233,  236,  237,  240,  241,  245,  248,  252,  264,\
                   265,  281,  285,  297,  324,  327,  361,  378,  389,  392,\
                   400,  473,  493,  500,  503,  511,  527,  528,  529,  530,\
                   531,  534,  538,  542,  544,  545,  547,  550,  553,  555,\
                   590,  594,  598,  602,  606,  610,  614,  618,  622,  626,\
                   645,  649,  653,  657,  661,  665,  685,  702,  703,  704,\
                   706,  707,  713]

    d['N__Bands'][3] =  [ 717,  725,  728,  729,  730,  731,  732,  733,  734,  735,\
                    736,  741,  749,  757,  765,  773,  781,  789,  794,  797,\
                    805,  806,  815,  822,  829,  839,  845,  853,  861,  868,\
                    869,  872,  877,  885,  887,  893,  898,  900,  909,  912,\
                    915,  917,  921,  929,  933,  941,  949,  957,  963,  965,\
                    973,  975,  978,  981,  989,  991,  993,  996, 1005, 1014,\
                   1025, 1029, 1037, 1042, 1053, 1061, 1073, 1077, 1085, 1093,\
                   1101, 1109, 1117, 1125, 1133, 1141]
    d['N__Bands'][4] = [1149, 1157, 1164, 1165, 1173, 1181, 1189, 1197, 1205, 1213, 1221, 1251]

    d['N__Bands'][5] = [1189, 1197, 1205, 1213, 1221, 1251]
    d['N__Band_Size'] = []
    for nn in list(d['N__Bands'].keys()):
        d['N__Band_Size'].append(len(d['N__Bands'][nn]))

    d['N__Window_Bounds1'][1] = 229
    d['N__Window_Bounds2'][1] = 549 
    d['R__Window_Grad_Threshold'][1] = 0.4

#N__Band_Size(1:5)= 137, 76, 12, 6, 6
    d['N__GradChkInterval'][1] = 5
    d['N__GradChkInterval'][2] = 5
    d['N__GradChkInterval'][3] = 5
    d['N__GradChkInterval'][4] = 5

    d['N__Window_Width'][1] = 3
    d['N__Window_Width'][2] = 3
    d['N__Window_Width'][3] = 3
    d['N__Window_Width'][4] = 3

    d['R__BT_Threshold'][1] = 200.3
    d['R__BT_Threshold'][2] = 0.5
    d['R__BT_Threshold'][3] = 0.5
    d['R__BT_Threshold'][4] = 0.5

    d['R__Grad_Threshold'][1] = 0.02
    d['R__Grad_Threshold'][2] = 0.02
    d['R__Grad_Threshold'][3] = 0.02
    d['R__Grad_Threshold'][4] = 0.02


    d['N__BandToUse'][1] = 1
    d['N__BandToUse'][2] = 1
    d['N__BandToUse'][3] = 1
    d['N__BandToUse'][4] = 1
    d['N__Num_Bands'] = 1 #len(d['N__Bands'].keys())
    #N__Num_Imager_Chans = 0
    #N__Num_Imager_Clusters = 0
    #N__Imager_Chans(:) = 0

    #R__Stddev_Threshold(:) = 0.0
    #R__Coverage_Threshold = 0.0
    #R__FG_Departure_Threshold = 0.0
    """
    L__Do_Imager_Cloud_Detection = .TRUE.

    N__Num_Imager_Chans = 2
    N__Num_Imager_Clusters = 7

    N__Imager_Chans(:) = 0
    N__Imager_Chans(1:N__Num_Imager_Chans) = (/ 2, 3 /)

    R__Stddev_Threshold(:) = 0.0
    R__Stddev_Threshold(1:N__Num_Imager_Chans) = (/ 0.75, 0.80 /)

    R__Coverage_Threshold = 0.03
    R__FG_Departure_Threshold = 1.0
    """
    return d

def getParmAllSwMwOpen():
    d = {}
    d['N__Bands'] = {}
    d['N__Window_Bounds1'] = {}
    d['N__Window_Bounds2'] = {}
    d['R__Window_Grad_Threshold'] = {}
    d['N__GradChkInterval'] ={}
    d['N__Window_Width'] = {}
    d['R__BT_Threshold'] = {}
    d['R__Grad_Threshold'] = {}
    d['N__BandToUse'] = {}
    d['L__Do_Quick_Exit'] = ".TRUE."
    d['L__Do_CrossBand'] = ".TRUE."
    d['L__Do_Imager_Cloud_Detection'] = ".FALSE."

    d['N__Bands'][1] = [1650, 1673, 1678, 1691, 1695, 1703, 1705, 1711, 1721, 1739,\
                        1742, 1744, 1757, 1769, 1782, 1785, 1789, 1793, 1798, 1960,\
                        1961, 1962, 1963, 1965, 1966, 1967, 1968, 1969, 1970,\
                        1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980,\
                        1981, 1982, 1983, 1984, 1985, 1986, 1987, 2119, 2140, 2143,\
                         2147, 2153, 2158, 2161, 2168, 2171, 2175, 2182]

    d['N__Bands'][2] =  [ 717,  725,  728,  729,  730,  731,  732,  733,  734,  735,\
                    736,  741,  749,  757,  765,  773,  781,  789,  794,  797,\
                    805,  806,  815,  822,  829,  839,  845,  853,  861,  868,\
                    869,  872,  877,  885,  887,  893,  898,  900,  909,  912,\
                    915,  917,  921,  929,  933,  941,  949,  957,  963,  965,\
                    973,  975,  978,  981,  989,  991,  993,  996, 1005, 1014,\
                   1025, 1029, 1037, 1042, 1053, 1061, 1073, 1077, 1085, 1093,\
                   1101, 1109, 1117, 1125, 1133, 1141]
    d['N__Bands'][3] = [1149, 1157, 1164, 1165, 1173, 1181, 1189, 1197, 1205, 1213, 1221, 1251]

    d['N__Bands'][4] = [1189, 1197, 1205, 1213, 1221, 1251]
    d['N__Band_Size'] = []
    for nn in list(d['N__Bands'].keys()):
        d['N__Band_Size'].append(len(d['N__Bands'][nn]))
    d['N__Window_Bounds1'][1] = 2119
  
    d['N__Window_Bounds2'][1] = 2182 
    d['R__Window_Grad_Threshold'][1] = 0.4

#N__Band_Size(1:5)= 137, 76, 12, 6, 6
    d['N__GradChkInterval'][1] = 5
    d['N__GradChkInterval'][2] = 5
    d['N__GradChkInterval'][3] = 5
    d['N__GradChkInterval'][4] = 5

    d['N__Window_Width'][1] = 3
    d['N__Window_Width'][2] = 3
    d['N__Window_Width'][3] = 3
    d['N__Window_Width'][4] = 3

    d['R__BT_Threshold'][1] =200.5
    d['R__BT_Threshold'][2] = 0.5
    d['R__BT_Threshold'][3] = 0.5
    d['R__BT_Threshold'][4] = 0.5

    d['R__Grad_Threshold'][1] = 0.02
    d['R__Grad_Threshold'][2] = 0.02
    d['R__Grad_Threshold'][3] = 0.02
    d['R__Grad_Threshold'][4] = 0.02


    d['N__BandToUse'][1] = 1
    d['N__BandToUse'][2] = 1
    d['N__BandToUse'][3] = 1
    d['N__BandToUse'][4] = 1
    d['N__Num_Bands'] = len(d['N__Bands'].keys())
    #N__Num_Imager_Chans = 0
    #N__Num_Imager_Clusters = 0
    #N__Imager_Chans(:) = 0

    #R__Stddev_Threshold(:) = 0.0
    #R__Coverage_Threshold = 0.0
    #R__FG_Departure_Threshold = 0.0
    """
    L__Do_Imager_Cloud_Detection = .TRUE.

    N__Num_Imager_Chans = 2
    N__Num_Imager_Clusters = 7

    N__Imager_Chans(:) = 0
    N__Imager_Chans(1:N__Num_Imager_Chans) = (/ 2, 3 /)

    R__Stddev_Threshold(:) = 0.0
    R__Stddev_Threshold(1:N__Num_Imager_Chans) = (/ 0.75, 0.80 /)

    R__Coverage_Threshold = 0.03
    R__FG_Departure_Threshold = 1.0
    """
    return d




def getParmAllSwMw():
    d = {}
    d['N__Bands'] = {}
    d['N__Window_Bounds1'] = {}
    d['N__Window_Bounds2'] = {}
    d['R__Window_Grad_Threshold'] = {}
    d['N__GradChkInterval'] ={}
    d['N__Window_Width'] = {}
    d['R__BT_Threshold'] = {}
    d['R__Grad_Threshold'] = {}
    d['N__BandToUse'] = {}
    d['L__Do_Quick_Exit'] = ".TRUE."
    d['L__Do_CrossBand'] = ".TRUE."
    d['L__Do_Imager_Cloud_Detection'] = ".FALSE."
    """
    d['N__Bands'][1] = [1650, 1673, 1678, 1691, 1695, 1703, 1705, 1711, 1721, 1739,\
                        1742, 1744, 1757, 1769, 1782, 1785, 1789, 1793, 1798, 1960,\
                        1961, 1962, 1963, 1965, 1966, 1967, 1968, 1969, 1970,\
                        1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980,\
                        1981, 1982, 1983, 1984, 1985, 1986, 1987, 2119, 2140, 2143,\
                         2147, 2153, 2158, 2161, 2168, 2171, 2175, 2182]
    """
    d['N__Bands'][1] = [1650, 1673, 1678, 1691, 1695, 1703, 1705, 1711, 1721, 1739,\
                        1742, 1744, 1757, 1769, 1782, 1785, 1789, 1793, 1798, 1800,\
                        1801, 1802, 1804, 1805, 1806, 1807, 1808, 1809, 1810, 1817,\
                        1961, 1962, 1963, 1965, 1966, 1967, 1968, 1969, 1970, 1971,\
                        1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980, 1981,\
                        1982, 1983, 1984, 1985, 1986, 1987, 2119, 2120, 2121, 2122,\
                        2127, 2140, 2143, 2147, 2153, 2158, 2161, 2167, 2168, 2170,\
                        2171, 2175]


    d['N__Bands'][2] =  [ 717,  725,  728,  729,  730,  731,  732,  733,  734,  735,\
                    736,  741,  749,  757,  765,  773,  781,  789,  794,  797,\
                    805,  806,  815,  822,  829,  839,  845,  853,  861,  868,\
                    869,  872,  877,  885,  887,  893,  898,  900,  909,  912,\
                    915,  917,  921,  929,  933,  941,  949,  957,  963,  965,\
                    973,  975,  978,  981,  989,  991,  993,  996, 1005, 1014,\
                   1025, 1029, 1037, 1042, 1053, 1061, 1073, 1077, 1085, 1093,\
                   1101, 1109, 1117, 1125, 1133, 1141]
    d['N__Bands'][3] = [1149, 1157, 1164, 1165, 1173, 1181, 1189, 1197, 1205, 1213, 1221, 1251]

    d['N__Bands'][4] = [1189, 1197, 1205, 1213, 1221, 1251]
    d['N__Band_Size'] = []
    for nn in list(d['N__Bands'].keys()):
        d['N__Band_Size'].append(len(d['N__Bands'][nn]))
    d['N__Window_Bounds1'][1] = 2119
    d['N__Window_Bounds2'][1] = 2175 
    d['R__Window_Grad_Threshold'][1] = 0.4

#N__Band_Size(1:5)= 137, 76, 12, 6, 6
    d['N__GradChkInterval'][1] = 5
    d['N__GradChkInterval'][2] = 5
    d['N__GradChkInterval'][3] = 5
    d['N__GradChkInterval'][4] = 5

    d['N__Window_Width'][1] = 3
    d['N__Window_Width'][2] = 3
    d['N__Window_Width'][3] = 3
    d['N__Window_Width'][4] = 3

    d['R__BT_Threshold'][1] = 0.5
    d['R__BT_Threshold'][2] = 0.5
    d['R__BT_Threshold'][3] = 0.5
    d['R__BT_Threshold'][4] = 0.5

    d['R__Grad_Threshold'][1] = 0.02
    d['R__Grad_Threshold'][2] = 0.02
    d['R__Grad_Threshold'][3] = 0.02
    d['R__Grad_Threshold'][4] = 0.02


    d['N__BandToUse'][1] = 1
    d['N__BandToUse'][2] = 1
    d['N__BandToUse'][3] = 1
    d['N__BandToUse'][4] = 1
    d['N__Num_Bands'] = len(d['N__Bands'].keys())
    #N__Num_Imager_Chans = 0
    #N__Num_Imager_Clusters = 0
    #N__Imager_Chans(:) = 0

    #R__Stddev_Threshold(:) = 0.0
    #R__Coverage_Threshold = 0.0
    #R__FG_Departure_Threshold = 0.0
    """
    L__Do_Imager_Cloud_Detection = .TRUE.

    N__Num_Imager_Chans = 2
    N__Num_Imager_Clusters = 7

    N__Imager_Chans(:) = 0
    N__Imager_Chans(1:N__Num_Imager_Chans) = (/ 2, 3 /)

    R__Stddev_Threshold(:) = 0.0
    R__Stddev_Threshold(1:N__Num_Imager_Chans) = (/ 0.75, 0.80 /)

    R__Coverage_Threshold = 0.03
    R__FG_Departure_Threshold = 1.0
    """
    return d



def getParmSwMw3band():
    d = {}
    d['N__Bands'] = {}
    d['N__Window_Bounds1'] = {}
    d['N__Window_Bounds2'] = {}
    d['R__Window_Grad_Threshold'] = {}
    d['N__GradChkInterval'] ={}
    d['N__Window_Width'] = {}
    d['R__BT_Threshold'] = {}
    d['R__Grad_Threshold'] = {}
    d['N__BandToUse'] = {}
    d['L__Do_Quick_Exit'] = ".TRUE."
    d['L__Do_CrossBand'] = ".TRUE."
    d['L__Do_Imager_Cloud_Detection'] = ".FALSE."

    d['N__Bands'][1] = [ 1939, 1940, 1941, 1942, 1943, 1944, 1945, 1946, 1947, 1948,\
                         1949, 1950, 1951, 1952, 1953, 1954, 1955, 1956, 1957, 1958,\
                         1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966, 1967, 1968,\
                         1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978,\
                         1979, 1980, 1981, 1982, 1983, 1984, 1985, 1986, 1987, 2119,\
                         2140, 2143, 2147, 2153, 2158, 2161, 2168, 2171, 2175, 2182]


    d['N__Bands'][2] =  [ 717,  725,  728,  729,  730,  731,  732,  733,  734,  735,\
                    736,  741,  749,  757,  765,  773,  781,  789,  794,  797,\
                    805,  806,  815,  822,  829,  839,  845,  853,  861,  868,\
                    869,  872,  877,  885,  887,  893,  898,  900,  909,  912,\
                    915,  917,  921,  929,  933,  941,  949,  957,  963,  965,\
                    973,  975,  978,  981,  989,  991,  993,  996, 1005, 1014,\
                   1025, 1029, 1037, 1042, 1053, 1061, 1073, 1077, 1085, 1093,\
                   1101, 1109, 1117, 1125, 1133, 1141]
    d['N__Bands'][3] = [1149, 1157, 1164, 1165, 1173, 1181, 1189, 1197, 1205, 1213, 1221, 1251]


    d['N__Bands'][4] =[1945, 1946, 1947, 1948, 1949, 1950, 1951, 1952]
    d['N__Bands'][5] =[1939, 1940, 1941, 1942, 1943, 1944]
 
   
    d['N__Band_Size'] = []
    for nn in list(d['N__Bands'].keys()):
        d['N__Band_Size'].append(len(d['N__Bands'][nn]))
    d['N__Window_Bounds1'][1] = 2119
  
    d['N__Window_Bounds2'][1] = 2182 
    d['R__Window_Grad_Threshold'][1] = 1.3859235346353243

#N__Band_Size(1:5)= 137, 76, 12, 6, 6
    d['N__GradChkInterval'][1] = 5
    d['N__GradChkInterval'][2] = 5
    d['N__GradChkInterval'][3] = 5
    d['N__GradChkInterval'][4] = 5
    d['N__GradChkInterval'][5] = 5

    d['N__Window_Width'][1] = 3
    d['N__Window_Width'][2] = 3
    d['N__Window_Width'][3] = 3
    d['N__Window_Width'][4] = 3
    d['N__Window_Width'][5] = 1

    d['R__BT_Threshold'][1] = 0.011044710320051257
    d['R__BT_Threshold'][2] = 0.5
    d['R__BT_Threshold'][3] = 0.5
    d['R__BT_Threshold'][4] = 1.0
    d['R__BT_Threshold'][5] = 3.0

    d['R__Grad_Threshold'][1] = 0.02
    d['R__Grad_Threshold'][2] = 0.02
    d['R__Grad_Threshold'][3] = 0.02
    d['R__Grad_Threshold'][4] = 0.02
    d['R__Grad_Threshold'][5] = 0.02


    d['N__BandToUse'][1] = 1
    d['N__BandToUse'][2] = 1
    d['N__BandToUse'][3] = 1
    d['N__BandToUse'][4] = 4
    d['N__BandToUse'][5] = 5
    d['N__Num_Bands'] = len(d['N__Bands'].keys())
    #N__Num_Imager_Chans = 0
    #N__Num_Imager_Clusters = 0
    #N__Imager_Chans(:) = 0

    #R__Stddev_Threshold(:) = 0.0
    #R__Coverage_Threshold = 0.0
    #R__FG_Departure_Threshold = 0.0
    """
    L__Do_Imager_Cloud_Detection = .TRUE.

    N__Num_Imager_Chans = 2
    N__Num_Imager_Clusters = 7

    N__Imager_Chans(:) = 0
    N__Imager_Chans(1:N__Num_Imager_Chans) = (/ 2, 3 /)

    R__Stddev_Threshold(:) = 0.0
    R__Stddev_Threshold(1:N__Num_Imager_Chans) = (/ 0.75, 0.80 /)

    R__Coverage_Threshold = 0.03
    R__FG_Departure_Threshold = 1.0
    """
    return d



def getParmSwMw():
    d = {}
    d['N__Bands'] = {}
    d['N__Window_Bounds1'] = {}
    d['N__Window_Bounds2'] = {}
    d['R__Window_Grad_Threshold'] = {}
    d['N__GradChkInterval'] ={}
    d['N__Window_Width'] = {}
    d['R__BT_Threshold'] = {}
    d['R__Grad_Threshold'] = {}
    d['N__BandToUse'] = {}
    d['L__Do_Quick_Exit'] = ".TRUE."
    d['L__Do_CrossBand'] = ".TRUE."
    d['L__Do_Imager_Cloud_Detection'] = ".FALSE."

    d['N__Bands'][1] =  [1939, 1940, 1941, 1942, 1943, 1944, 1945, 1946, 1947, 1948, 1949, 1950, 1951, 1952,\
                         1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966,\
                         1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980,\
                         1981, 1982, 1983, 1984, 1985, 1986, 1987, 2119, 2140, 2143, 2147, 2153, 2158, 2161,\
                         2168, 2171, 2175, 2182]

    d['N__Bands'][2] =  [ 717,  725,  728,  729,  730,  731,  732,  733,  734,  735,\
                    736,  741,  749,  757,  765,  773,  781,  789,  794,  797,\
                    805,  806,  815,  822,  829,  839,  845,  853,  861,  868,\
                    869,  872,  877,  885,  887,  893,  898,  900,  909,  912,\
                    915,  917,  921,  929,  933,  941,  949,  957,  963,  965,\
                    973,  975,  978,  981,  989,  991,  993,  996, 1005, 1014,\
                   1025, 1029, 1037, 1042, 1053, 1061, 1073, 1077, 1085, 1093,\
                   1101, 1109, 1117, 1125, 1133, 1141]
    d['N__Bands'][3] = [1149, 1157, 1164, 1165, 1173, 1181, 1189, 1197, 1205, 1213, 1221, 1251]

    d['N__Bands'][4] = [1189, 1197, 1205, 1213, 1221, 1251]
    d['N__Band_Size'] = []
    for nn in list(d['N__Bands'].keys()):
        d['N__Band_Size'].append(len(d['N__Bands'][nn]))
    d['N__Window_Bounds1'][1] = 2119
  
    d['N__Window_Bounds2'][1] = 2182 
    d['R__Window_Grad_Threshold'][1] = 0.4

#N__Band_Size(1:5)= 137, 76, 12, 6, 6
    d['N__GradChkInterval'][1] = 5
    d['N__GradChkInterval'][2] = 5
    d['N__GradChkInterval'][3] = 5
    d['N__GradChkInterva'][4] = 5

    d['N__Window_Width'][1] = 3
    d['N__Window_Width'][2] = 3
    d['N__Window_Width'][3] = 3
    d['N__Window_Width'][4] = 3

    d['R__BT_Threshold'][1] = 0.5
    d['R__BT_Threshold'][2] = 0.5
    d['R__BT_Threshold'][3] = 0.5
    d['R__BT_Threshold'][4] = 0.5

    d['R__Grad_Threshold'][1] = 0.02
    d['R__Grad_Threshold'][2] = 0.02
    d['R__Grad_Threshold'][3] = 0.02
    d['R__Grad_Threshold'][4] = 0.02


    d['N__BandToUse'][1] = 1
    d['N__BandToUse'][2] = 1
    d['N__BandToUse'][3] = 1
    d['N__BandToUse'][4] = 1
    d['N__Num_Bands'] = len(d['N__Bands'].keys())
    #N__Num_Imager_Chans = 0
    #N__Num_Imager_Clusters = 0
    #N__Imager_Chans(:) = 0

    #R__Stddev_Threshold(:) = 0.0
    #R__Coverage_Threshold = 0.0
    #R__FG_Departure_Threshold = 0.0
    """
    L__Do_Imager_Cloud_Detection = .TRUE.

    N__Num_Imager_Chans = 2
    N__Num_Imager_Clusters = 7

    N__Imager_Chans(:) = 0
    N__Imager_Chans(1:N__Num_Imager_Chans) = (/ 2, 3 /)

    R__Stddev_Threshold(:) = 0.0
    R__Stddev_Threshold(1:N__Num_Imager_Chans) = (/ 0.75, 0.80 /)

    R__Coverage_Threshold = 0.03
    R__FG_Departure_Threshold = 1.0
    """
    return d

def getParmSw():
    d = {}
    d['N__Bands'] = {}
    d['N__Window_Bounds1'] = {}
    d['N__Window_Bounds2'] = {}
    d['R__Window_Grad_Threshold'] = {}
    d['N__GradChkInterval'] ={}
    d['N__Window_Width'] = {}
    d['R__BT_Threshold'] = {}
    d['R__Grad_Threshold'] = {}
    d['N__BandToUse'] = {}
    d['L__Do_Quick_Exit'] = ".TRUE."
    d['L__Do_CrossBand'] = ".TRUE."
    d['L__Do_Imager_Cloud_Detection'] = ".FALSE."

    d['N__Bands'][1] =  [1939, 1940, 1941, 1942, 1943, 1944, 1945, 1946, 1947, 1948, 1949, 1950, 1951, 1952,\
                         1953, 1954, 1955, 1956, 1957, 1958, 1959, 1960, 1961, 1962, 1963, 1964, 1965, 1966,\
                         1967, 1968, 1969, 1970, 1971, 1972, 1973, 1974, 1975, 1976, 1977, 1978, 1979, 1980,\
                         1981, 1982, 1983, 1984, 1985, 1986, 1987, 2119, 2140, 2143, 2147, 2153, 2158, 2161,\
                         2168, 2171, 2175, 2182]

    d['N__Band_Size'] = []
    for nn in list(d['N__Bands'].keys()):
        d['N__Band_Size'].append(len(d['N__Bands'][nn]))
    d['N__Window_Bounds1'][1] = 2119
    d['N__Window_Bounds2'][1] = 2182 
    d['R__Window_Grad_Threshold'][1] = 0.4

#N__Band_Size(1:5)= 137, 76, 12, 6, 6
    d['N__GradChkInterval'][1] = 5

    d['N__Window_Width'][1] = 3

    d['R__BT_Threshold'][1] = 0.5

    d['R__Grad_Threshold'][1] = 0.02


    d['N__BandToUse'][1] = 1
    d['N__Num_Bands'] = len(d['N__Bands'].keys())
    #N__Num_Imager_Chans = 0
    #N__Num_Imager_Clusters = 0
    #N__Imager_Chans(:) = 0

    #R__Stddev_Threshold(:) = 0.0
    #R__Coverage_Threshold = 0.0
    #R__FG_Departure_Threshold = 0.0
    """
    L__Do_Imager_Cloud_Detection = .TRUE.

    N__Num_Imager_Chans = 2
    N__Num_Imager_Clusters = 7

    N__Imager_Chans(:) = 0
    N__Imager_Chans(1:N__Num_Imager_Chans) = (/ 2, 3 /)

    R__Stddev_Threshold(:) = 0.0
    R__Stddev_Threshold(1:N__Num_Imager_Chans) = (/ 0.75, 0.80 /)

    R__Coverage_Threshold = 0.03
    R__FG_Departure_Threshold = 1.0
    """
    return d


def getParmLwMw():
    d = {}
    d['N__Bands'] = {}
    d['N__Window_Bounds1'] = {}
    d['N__Window_Bounds2'] = {}
    d['R__Window_Grad_Threshold'] = {}
    d['N__GradChkInterval'] ={}
    d['N__Window_Width'] = {}
    d['R__BT_Threshold'] = {}
    d['R__Grad_Threshold'] = {}
    d['N__BandToUse'] = {}
    d['L__Do_Quick_Exit'] = ".TRUE."
    d['L__Do_CrossBand'] = ".TRUE."
    d['L__Do_Imager_Cloud_Detection'] = ".FALSE."
    
    d['N__Bands'][1] =   [  1,    5,    9,   13,   17,   18,   19,   20,   21,   22,\
                     23,   24,   25,   26,   27,   28,   29,   30,   31,   32,\
                     33,   34,   35,   36,   37,   38,   39,   40,   41,   42,\
                     43,   44,   45,   46,   47,   48,   49,   50,   51,   52,\
                     53,   54,   55,   56,   57,   58,   59,   60,   61,   62,\
                     63,   64,   65,   66,   67,   68,   69,   70,   71,   72,\
                     73,   74,   75,   76,   77,   78,   79,   80,   81,   82,\
                     83,   84,   85,   86,   87,   88,   91,   92,   93,   94,\
                     95,   96,   97,   99,  101,  105,  107,  109,  111,  113,\
                    115,  116,  117,  118,  119,  120,  121,  122,  123,  124,\
                    125,  133,  135,  137,  139,  141,  144,  147,  161,  173,\
                    177,  181,  185,  195,  210,  221,  225,  229,  249,  257,\
                    269,  273,  293,  301,  317,  333,  349,  369,  409,  433,\
                    457,  481,  501,  549,  701,  705,  709]

    d['N__Bands'][2] = [   3,    6,    7,    8,   10,   12,   14,   15,   16,   89,\
                    90,  102,  103,  104,  106,  108,  110,  114,  126,  127,\
                   129,  132,  134,  138,  140,  143,  145,  146,  148,  149,\
                   150,  151,  153,  155,  156,  157,  158,  159,  162,  163,\
                   164,  165,  166,  169,  170,  171,  172,  175,  180,  189,\
                   200,  201,  205,  206,  214,  217,  218,  226,  228,  230,\
                   231,  233,  236,  237,  240,  241,  245,  248,  252,  264,\
                   265,  281,  285,  297,  324,  327,  361,  378,  389,  392,\
                   400,  473,  493,  500,  503,  511,  527,  528,  529,  530,\
                   531,  534,  538,  542,  544,  545,  547,  550,  553,  555,\
                   590,  594,  598,  602,  606,  610,  614,  618,  622,  626,\
                   645,  649,  653,  657,  661,  665,  685,  702,  703,  704,\
                   706,  707,  713]

    d['N__Bands'][3] =  [ 717,  725,  728,  729,  730,  731,  732,  733,  734,  735,\
                    736,  741,  749,  757,  765,  773,  781,  789,  794,  797,\
                    805,  806,  815,  822,  829,  839,  845,  853,  861,  868,\
                    869,  872,  877,  885,  887,  893,  898,  900,  909,  912,\
                    915,  917,  921,  929,  933,  941,  949,  957,  963,  965,\
                    973,  975,  978,  981,  989,  991,  993,  996, 1005, 1014,\
                   1025, 1029, 1037, 1042, 1053, 1061, 1073, 1077, 1085, 1093,\
                   1101, 1109, 1117, 1125, 1133, 1141]
    d['N__Bands'][4] = [1149, 1157, 1164, 1165, 1173, 1181, 1189, 1197, 1205, 1213, 1221, 1251]

    d['N__Bands'][5] = [1189, 1197, 1205, 1213, 1221, 1251]
    d['N__Band_Size'] = []
    for nn in list(d['N__Bands'].keys()):
        d['N__Band_Size'].append(len(d['N__Bands'][nn]))

    d['N__Window_Bounds1'][1] = 229
    d['N__Window_Bounds2'][1] = 549 
    d['R__Window_Grad_Threshold'][1] = 0.4

#N__Band_Size(1:5)= 137, 76, 12, 6, 6
    d['N__GradChkInterval'][1] = 5
    d['N__GradChkInterval'][2] = 5
    d['N__GradChkInterval'][3] = 5
    d['N__GradChkInterval'][4] = 5

    d['N__Window_Width'][1] = 6
    d['N__Window_Width'][2] = 6
    d['N__Window_Width'][3] = 8
    d['N__Window_Width'][4] = 3

    d['R__BT_Threshold'][1] = 0.3
    d['R__BT_Threshold'][2] = 0.5
    d['R__BT_Threshold'][3] = 0.5
    d['R__BT_Threshold'][4] = 0.5

    d['R__Grad_Threshold'][1] = 0.02
    d['R__Grad_Threshold'][2] = 0.02
    d['R__Grad_Threshold'][3] = 0.02
    d['R__Grad_Threshold'][4] = 0.02


    d['N__BandToUse'][1] = 1
    d['N__BandToUse'][2] = 1
    d['N__BandToUse'][3] = 1
    d['N__BandToUse'][4] = 1
    d['N__Num_Bands'] = len(d['N__Bands'].keys())
    #N__Num_Imager_Chans = 0
    #N__Num_Imager_Clusters = 0
    #N__Imager_Chans(:) = 0

    #R__Stddev_Threshold(:) = 0.0
    #R__Coverage_Threshold = 0.0
    #R__FG_Departure_Threshold = 0.0
    """
    L__Do_Imager_Cloud_Detection = .TRUE.

    N__Num_Imager_Chans = 2
    N__Num_Imager_Clusters = 7

    N__Imager_Chans(:) = 0
    N__Imager_Chans(1:N__Num_Imager_Chans) = (/ 2, 3 /)

    R__Stddev_Threshold(:) = 0.0
    R__Stddev_Threshold(1:N__Num_Imager_Chans) = (/ 0.75, 0.80 /)

    R__Coverage_Threshold = 0.03
    R__FG_Departure_Threshold = 1.0
    """
    return d
def writeNamelist(fname,d):
    fout = open(fname,'w')
    fout.write('&Cloud_Detect_Coeffs\n')

    fmt = ''
    for iii in range(10):
        fmt+=" {},"
    fmt+="\n"
    
    for k in list(d.keys()):
        if (type(d[k]) is dict):
            for kk in list(d[k].keys()):
                if(type(d[k][kk]) is list):
                    extra = False
                    cnt = 0
                   
                    nlines = len(d[k][kk])//10
                    if(len(d[k][kk])%10 or nlines==0):
                        extra = True
                        fmtlast = ''
                        extra_list = []
                        cnt_extra = 0
                        for ii in range(len(d[k][kk])%10):
                            fmtlast += " {},"
                            extra_list.append(d[k][kk][nlines*10+cnt_extra])
                            cnt_extra+=1
                    for iii in range(nlines):
                        if(iii==0):
                            fout.write("{}(1:{},{})=\n".format(k,len(d[k][kk]),kk)+fmt.format(d[k][kk][cnt],d[k][kk][cnt+1],d[k][kk][cnt+2],d[k][kk][cnt+3],\
                                   d[k][kk][cnt+4],d[k][kk][cnt+5], d[k][kk][cnt+6],d[k][kk][cnt+7],d[k][kk][cnt+8],d[k][kk][cnt+9]))
                        else:
                            fout.write(fmt.format(d[k][kk][cnt],d[k][kk][cnt+1],d[k][kk][cnt+2],d[k][kk][cnt+3],\
                                   d[k][kk][cnt+4],d[k][kk][cnt+5], d[k][kk][cnt+6],d[k][kk][cnt+7],d[k][kk][cnt+8],d[k][kk][cnt+9]))
                        cnt+=10
                    if(extra):
                        fmtlast+="\n"
                        if(nlines==0):
                            strout = "{}(1:{},{})=".format(k,len(d[k][kk]),kk)+fmtlast.format(*extra_list)
                            nospace = strout.replace(' ','')
                            fout.write(nospace)
                        else:
                            fout.write(fmtlast.format(*extra_list)) 
                elif('Bounds1' in k):
                    fout.write('{}({},1)= {}\n'.format(k.replace('Bounds1','Bounds'),kk,d[k][kk]) )
        
                elif('Bounds2' in k):
                    fout.write('{}({},2)= {}\n'.format(k.replace('Bounds2','Bounds'),kk,d[k][kk]) )
                else:
                    fout.write('{}({})= {}\n'.format(k,kk,d[k][kk]))
        elif(type(d[k]) is list):
            fmtlst = ''
            for ii in range(len(d[k])):
                fmtlst+= " {},"
            fmtlst+="\n"
            fout.write('{}(1:{})='.format(k,len(d[k]))+fmtlst.format(*d[k]) )
        else:
            fout.write('{} = {}\n'.format(k,d[k]) )
    fout.write('/\n')
    fout.close()
    fin= open(fname,'r')
    lines = fin.readlines()
    fin.close()
    starts = []
    for l in lines:
        starts.append(l[0])
    fout = open(fname,'w')
    for i,l in enumerate(lines[0:len(lines)-1]):
        if(starts[i+1] !=' '):
            fout.write(l.replace(",\n","\n"))
        else:
            fout.write(l)
    fout.write("/\n")
    fout.close()

if __name__ == '__main__':
    d = getParmLwMw()
    writeNamelist('LwMw.nl',d)

    d = getParmSwMw()
    writeNamelist('SwMw.nl',d)

    d = getParmSw()
    writeNamelist('Sw.nl',d)

    d = getParmSw()
    writeNamelist('Sw_init.nl',d)


    d = getParmAllSwMw()
    writeNamelist('Sw_outside_R.nl',d)

#    d = getParmSw()
#    writeNamelist('Sw.nl',d)


#    d = getParmMw()
#    writeNamelist('Mw.nl',d)


