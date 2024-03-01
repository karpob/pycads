
load gnu environment 
Installing it:

1. Install dependency for ascii io

 pip install fortranformat --target /perm/ecmv1501/local2/

2. edit CMakelist.txt to point to CADS directory (top level/above src):

 set(CADS_DIR "/perm/ecmv1501/CADS/CADS/")

3. Make and install  
 mkdir build
 cd build
 cmake ../
 make
 cmake --install . --prefix /perm/ecmv1501/local2/

4.Create sourceMe file that you'd source when you want it or contents/add these to your .bashrc if you want them automatically :
export PYTHONPATH="${PYTHONPATH}:/perm/ecmv1501/local2/:/perm/ecmv1501/local2/pycads/"
export LD_LIBRARY_PATH="${LD_LIBRARY_PATH}:/perm/ecmv1501/local2/pycads/"
export PATH="${PATH}:/perm/ecmv1501/local2/pycads/bin/"


Running It:
  
1. source sourceMe
2. change to a directory with a CRIS_CLDDET.NL
3. pick either:
    go_cloud_odb.py --> reads in ofb file and outputs cads ascii and h5 output with clouds flags.
    go_cloud_ascii.py --> reads in CADS ascii input file and outputs cads ascii wiht h5
4. run either script (they should be in your path after sourcing sourceMe)


usage: go_cloud_ascii.py [-h] --input INPUT --output OUTPUT

Run cads using traditional CADS ascii input file. Will output stardard cads output ascii, and h5 with output prefix

options:
  -h, --help       show this help message and exit
  --input INPUT    path to first input file
  --output OUTPUT  output prefix (will append h5 and txt)


usage: go_cloud_odb.py [-h] --input INPUT --output OUTPUT [--sensor SENSOR]

Run cads using traditional CADS ascii input file. Will output stardard cads output ascii, and h5 with output prefix

options:
  -h, --help       show this help message and exit
  --input INPUT    path to first input file
  --output OUTPUT  output prefix (will append h5 and txt)
  --sensor SENSOR  sensor (cris, iasi, etc..)

