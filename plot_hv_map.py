import os
import numpy as np
import struct
import math as m
import matplotlib.pyplot as plt
from mpl_toolkits.axes_grid1 import make_axes_locatable

def readImageFile(fileName, pname):
	
	i=0
	imlist = []
	
	numElements = os.path.getsize(fileName)/4
	nx = int(m.sqrt(numElements))
	print("***** " + pname + " component *****")
	print("Number of elements: ",numElements)
	print("Number of pixels on single axis: ", nx)
	
	with open(fileName,"rb") as f:
		byte = 1
		while byte:
			i = i + 1
			if i > nx*nx:
				break
			byte = f.read(4)
			imlist.append(struct.unpack('<f',byte)[0])
			
	image=np.array(imlist).reshape(nx,nx)
	
	return image, nx

def printImageValues(image, nx):
	
	nxhalf = int(nx/2)
	print('Test values:')
	print(image[nxhalf-2:nxhalf+2,nxhalf-2:nxhalf+2])
	print(image[nxhalf-1,nxhalf-1])
	print(image[nx-1,0],image[nx-1,nx-1])
	print(image[0,0],image[0,nx-1])
	print()


def plotImageXsecs(image, nx, component):
	
	nxhalf = int(nx/2)
	im_max = 1
	if len(image[image != 0]) > 0:
		im_p = image[image>0]
		im_n = image[image<=0]

	fig, ax = plt.subplots(figsize=(9,9))
	ax.imshow(image, cmap='bwr', vmin = -max(im_p), vmax = max(im_p))
	ax.invert_yaxis()
	ax.set_xlim([0, nx])
	ax.set_ylim([0, nx])
	ax.set_aspect(1.)
	
	# create new axes on the right and on the top of the current axes
	divider = make_axes_locatable(ax)
	# below height and pad are in inches
	ax_crossx = divider.append_axes("bottom", 1.2, pad=0.1, sharex=ax)
	ax_crossy = divider.append_axes("right", 1.2, pad=0.1, sharey=ax)

	# make some labels invisible
	ax.xaxis.set_tick_params(labelbottom=False, bottom=False)
	ax.yaxis.set_tick_params(labelleft=False, left=False)
	
	ax_crossy.xaxis.set_tick_params(labeltop=False, top=False)
	ax_crossy.xaxis.set_tick_params(labelbottom=True, bottom=True, labelrotation=90)
	ax_crossy.yaxis.set_tick_params(labelright=True, right=True)
	ax_crossy.yaxis.set_tick_params(labelleft=False, left=False)

	# Add axis labels
	# x X-section 
	ax_crossx.set_xlabel("x-pixel")
	ax_crossx.set_ylabel(component + " velocity")
	# y X-section 
	ax_crossy.yaxis.set_label_position("right")
	ax_crossy.set_ylabel("y-pixel")
	ax_crossy.set_xlabel(component + " velocity")

	
	ax_crossx.plot(range(1,nx+1),image[nxhalf,:])
	ax_crossx.set_xlim([0, nx])

	ax_crossy.plot(image[:,nxhalf],range(1,nx+1))
	ax_crossy.set_ylim([0, nx])
	
def CheckImageDims(unx,vnx):
	
	if unx == vnx:
		nx = unx
	else:
		print("Image sizes are different, exiting.")
		quit()

	return nx

def getImageCent(image, nx, cN):
	
	nxhalf = int(nx/2)
	cNhalf = int(cN/2)
	cent = image[nxhalf-cNhalf:nxhalf+cNhalf,nxhalf-cNhalf:nxhalf+cNhalf]

	return cent

def plotQuiver(uimage, vimage, nx, cN):
	
	ucent = getImageCent(uimage, nx, cN)
	vcent = getImageCent(vimage, nx, cN)

	X = nx/2 + np.arange(0, cN) - cN/2
	Y = nx/2 + np.arange(0, cN) - cN/2
	U, V = np.meshgrid(X, Y)
	
	fig, ax = plt.subplots(figsize=(9,9))
	q = ax.quiver(X, Y, ucent, vcent, width=0.001)
#	ax.quiverkey(q, X=0.3, Y=1.1, U=10,
#		label='Quiver key, length = 10', labelpos='E')
	ax.set_xlim([(nx-cN)/2, (nx+cN)/2])
	ax.set_ylim([(nx-cN)/2, (nx+cN)/2])
	ax.set_xlabel("x-pixel")
	ax.set_ylabel("y-pixel")

def readAndPlot(component, filename):

    image, nx = readImageFile(filename, component)
    printImageValues(image,nx)
    plotImageXsecs(image,nx, component)
	
    return image, nx
    
def main():

    uimage, unx = readAndPlot("Toroidal", "./output/uvel_9000.data")
    vimage, vnx = readAndPlot("Poloidal", "./output/vvel_9000.data")

    nx = CheckImageDims(unx,vnx)

    # Make quiver plot of central cNxcN region
    cN = 64
    plotQuiver(uimage, vimage, nx, cN)

    plt.show()
    quit()


if __name__ == '__main__':
    main()
