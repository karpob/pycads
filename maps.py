import numpy as np
import matplotlib
import matplotlib.pyplot as plt
import matplotlib.gridspec as gridspec
import matplotlib.colors
import cartopy.crs as ccrs
import matplotlib.gridspec as gridspec
import cartopy.feature as cfeature
try: 
    from fast_histogram import histogram1d
    fastPresent = True
except: fastPresent = False
def getAutoBw(x):
    iqr = np.subtract(*np.percentile(x, [75, 25]))
    fd_bw = 2.0 * iqr * x.size ** (-1.0 / 3.0)
    sturges_bw = x.ptp() / (np.log2(x.size) + 1.0)
    if fd_bw:
        return min(fd_bw, sturges_bw)
    else:
        # limited variance, so we return a len dependent bw estimator
        return sturges_bw
def getCountAuto(x):
    start, end = x.min(), x.max()
    bw = getAutoBw(x)
    if(abs(bw)<1e-8):
        return 1
    else:
        return int( np.ceil((end-start)/bw))

def plotMap(lat, lon, data='red', title=None, graphicName=None, plotRange = None, units='', colorScheme = 'viridis', projectionSelected = ccrs.PlateCarree(),showFig=True,figureSize = (16,9),graphicDPI=100):
    fig, ax1 = plt.subplots(nrows=1, figsize=figureSize, subplot_kw={'projection': projectionSelected}) 
    ax1.coastlines(resolution='110m', color='black') # plot coastlines on map
    ax1.set_global() # show the whole earth
    ax1.gridlines() # put lat/lon markers

    # if a plot range isn't specified, make it the max and min of the data.
    if(plotRange): pass 
    elif(type(data) != type('color')): plotRange = ( data.min(), data.max() )

    # come up with normalization for data.
    if(type(data) != type('color')):
        norm = matplotlib.colors.Normalize(vmin=plotRange[0], vmax=plotRange[1]) #Nomalize color range to match set data range
    # plot scatter plot with data according to colormap and normalization specified
    
    if(type(data) != type('color')):
        ax1.scatter(lon, lat, c=data, cmap = plt.get_cmap(colorScheme), norm=norm, s=10, marker="o", edgecolors='none', transform=ccrs.PlateCarree())
    else:
        ax1.scatter(lon,lat,c=data, s=10, marker = "o",transform=ccrs.PlateCarree())
    if(type(data) != type('color')):
        statString = "Mean={:10.4f} Standard Deviation={:10.4f} Min={:10.4f} Max={:10.4f}  ".format( data.mean(), data.std(ddof=1), data.min(), data.max() )
        plt.title(statString, fontsize=12)
    if(type(title) != type(None)):
        fig.suptitle(title, fontsize=18)
    if(type(data) != type('color')): 
        bar = plt.cm.ScalarMappable(cmap = plt.get_cmap(colorScheme), norm=norm)
        bar._A = []
        cbar = plt.colorbar(bar, ax=ax1,orientation='horizontal')
        cbar.set_label(units)
    #plt.subplots_adjust(left=0.05, right=0.95, top=0.95, bottom=0.05)
    if(type(graphicName) != type(None)):
        fig.savefig(graphicName,dpi=graphicDPI)
    if(showFig): plt.show()
    plt.close(fig)

def plotMapHist(lat, lon, data, title=None, graphicName=None, plotRange = None, units='', colorScheme = 'viridis', projectionSelected = ccrs.PlateCarree(), showFig=True, figureSize = (16,9),graphicDPI=100):
    # setup figure 16, 9 size
    fig = plt.figure( figsize = figureSize )
    mapHeight = 85
    histHeight = 30
    barHeight = 10 
    gs = gridspec.GridSpec(3, 1, height_ratios=[mapHeight, histHeight,barHeight])
    ax1 = plt.subplot(gs[0], projection=projectionSelected)
    ax2 = plt.subplot(gs[1])
    ax3 = plt.subplot(gs[2])
    ax1.coastlines(resolution='110m', color='black') # plot coastlines on map
    ax1.set_global() # show the whole earth
    ax1.gridlines() # put lat/lon markers

    # if a plot range isn't specified, make it the max and min of the data.
    if(plotRange): pass 
    else: plotRange = ( data.min(), data.max() )

    # come up with normalization for data.
    norm = matplotlib.colors.Normalize(vmin=plotRange[0], vmax=plotRange[1]) #Nomalize color range to match set data range
    # plot scatter plot with data according to colormap and normalization specified
    ax1.scatter(lon, lat, c=data, cmap = plt.get_cmap(colorScheme), norm=norm, s=2, marker="o", edgecolors='none', transform=ccrs.PlateCarree())
    statString = "Mean={:10.4f} Standard Deviation={:10.4f} Min={:10.4f} Max={:10.4f}  ".format( data.mean(), data.std(ddof=1), data.min(), data.max() )
    ax1.set_title(statString, fontsize=12)
    if(type(title) != type(None)):
        fig.suptitle(title, fontsize=18) 
    colorMapForColorbar = plt.cm.ScalarMappable(cmap = plt.get_cmap(colorScheme), norm=norm)
    colorMapForColorbar._A = []
    # do colorbar first to get the xlims so we can force them with the histogram    
    cbar = matplotlib.colorbar.ColorbarBase(ax3, cmap = plt.get_cmap(colorScheme), norm=norm, orientation='horizontal')
    cbar.set_label(units)
    barlims = plotRange[0],plotRange[1]

    # Get bin count using 'auto' algorithm stolen from numpy private functions 
    binCount = getCountAuto(data)
    # if you get something weird, make it 10 at least.
    if(binCount < 10 and data.min() < data.max() ): 
        print("plotMapHist: bin count of {} too small, making it 10".format(binCount) )
        binCount = 10 
    # Compute histogram (using fast of the interwebs if installed)
    if(fastPresent and binCount > 1 ):
        binEdges = np.linspace(data.min(),data.max(),binCount+1)
        hist = histogram1d(data,binCount,(data.min(),data.max()))
    else:
        print("Using slower numpy histogram.")
        hist, binEdges = np.histogram(data, bins=binCount)
    # get midpoint value for histograms
    binMids = 0.5 * (binEdges[1:] + binEdges[:-1])
    colorMapForHist = matplotlib.cm.get_cmap(colorScheme)(norm(binMids))
    ax2.bar(binMids[:], hist, width=np.diff(binEdges) , color = colorMapForHist)
    ax2.set_ylabel('Count')
    ax2.set_xlim(barlims)
    ax2.set_yscale('log')
    ax2.spines['right'].set_visible(False)
    ax2.spines['top'].set_visible(False)
    ax2.spines['bottom'].set_visible(False)
    ax2.set_title('Total Count: {:d}'.format(data.flatten().shape[0])) 
    ax3Pos = ax3.get_position()
    ax3Pos.y0 = ax3Pos.y0+0.04
    ax3Pos.y1 = ax3Pos.y1+0.04
    ax3.set_position(ax3Pos)
    ax2.get_xaxis().set_visible(False)  
    if( type(graphicName) != type(None)): 
        fig.savefig(graphicName,dpi=graphicDPI)
    if(showFig):
        plt.show()
    plt.close(fig)
