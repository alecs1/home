#from subprocess import call, Popen, PIPE
#from multiprocessing import Pool
import os

def main() :
	dir = 'D:/tmp/tga-in'
	fList = os.listdir(dir)

	for i in range(100):
		crtFName = fList[i%len(fList)]
		print str(i) + ' ' + crtFName
		newFName = str(i) + '-' + crtFName
		command = 'cp ' + dir + '/' + crtFName + ' ' + dir + '/' + newFName
		print 'Command: ' + command
		os.system(command)
	return 0		

if __name__ == "__main__":
    main()

