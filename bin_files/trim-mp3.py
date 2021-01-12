#!/usr/bin/python


import sys, getopt
from os import system

def main(argv):
    inputfile = ''
    datafile = ''
    # ================================================================
    # get arguments
    try:
        opts, args = getopt.getopt(argv,'hi:f:',['ifile=','format='])
    except getopt.GetoptError:
        print 'try -h for help'
        sys.exit('non consistent arguments')
    # ================================================================

    # ================================================================
    # check if mandatory options are present
    cond_i = False
    cond_d = False 
    for opt, arg in opts:
        if opt == '-h':
            print 'test.py -i <input mp3 file without extension>'
            print 'there should be a list file with timestamps with same name as the mp3 file but without extension'
            sys.exit()
        elif opt == '-i' or opt == '--ifile':
            cond_i = True
    if not cond_i:
        print 'try -h for help'
        sys.exit('input file is mandatory')
    # ================================================================

    # ================================================================
    # default format 
    dataformat = 'mm:ss title'
    # ================================================================

    # ================================================================
    # Get options and arguments
    for opt, arg in opts:
        if opt in ('-i', '--ifile'):
            inputfile = arg + '.mp3'
            datafile = arg
        elif opt in ('-f', '--format'):
            dataformat = arg 
    print 'Input file is: ', inputfile
    print 'Output file is: ', datafile
    # ================================================================

    # ================================================================
    # read data file
    df = open(datafile)
    lines = df.read().splitlines()
    numbers = []
    titles = []
    initimes = []
    endtimes = []
    for i, line in enumerate(lines):
        linedata = line.split(';')
        print linedata
        if i > 0:
            endtimes.append(linedata[1].strip())
        if i < len(lines) - 1:
            numbers.append(linedata[0].strip())
            initimes.append(linedata[1].strip())
            titles.append(linedata[2].strip().replace(' ', '-').lower())
    #
    # print data
    for number, title, initime, endtime in zip(numbers, titles, initimes, endtimes):
        print number, title, initime, endtime
    # ================================================================

    # ================================================================
    # call system to trim mp3 file into songs
    for i, title in enumerate(titles):
        if i+1 < 10:
            n = '0' + str(i+1)
        else:
            n = str(i+1)
        filename = n + '_' + title + '.mp3'
        # time to make the files
        command = 'trim_ffmpeg ' + inputfile + ' ' + initimes[i] + ' ' + endtimes[i] + ' ' + filename
        print '# ================================================================'
        print command
        print '# ================================================================'
        system(command)
    # ================================================================

if __name__ == '__main__':
    main(sys.argv[1:])