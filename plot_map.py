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
	ax.set_title(component + " velocity map")
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

def main():

	component = "Toroidal"
	uimage, unx = readImageFile("./output/uvel_1000.data", component)
	printImageValues(uimage,unx)
	plotImageXsecs(uimage,unx, component)

	component = "Poloidal"
	vimage, vnx = readImageFile("./output/vvel_1000.data", component)
	printImageValues(vimage,vnx)
	plotImageXsecs(vimage,vnx, component)

	component = "Radial"
	wimage, wnx = readImageFile("./output/wvel_1000.data", component)
	printImageValues(wimage,wnx)
	plotImageXsecs(wimage,wnx, component)

	plt.show()
		
	quit()


if __name__ == '__main__':
    main()
