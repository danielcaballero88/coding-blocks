import os 

def main(workdir):
    # check for existence of file
    filepath = os.path.join(workdir, "testfile.txt")
    exists = os.path.exists(filepath)
    print(f"os.path.exists({filepath}) = ", exists)

    # check for existence of directory
    dirpath = os.path.join(workdir, "testdir")
    exists = os.path.exists(dirpath)
    print(f"os.path.exists({dirpath}) = ", exists)

    # check for existence of link 
    softlinkpath = os.path.join(workdir, "softlink_testfile.txt")
    exists = os.path.exists(softlinkpath)
    print(f"os.path.exists({softlinkpath}) = ", exists)


    # check if there is a file named testfile.txt
    filepath = os.path.join(workdir, "testfile.txt")
    isfile = os.path.isfile(filepath)
    print(f"os.path.isfile({filepath}) = ", isfile)

    # check if there is a file named testdir
    dirpath = os.path.join(workdir, "testdir")
    isfile = os.path.isfile(dirpath)
    print(f"os.path.isfile({dirpath}) = ", isfile)

    # check if there is a directory named testdir 
    dirpath = os.path.join(workdir, "testdir")
    isfile = os.path.isdir(dirpath)
    print(f"os.path.isdir({dirpath}) = ", isfile)

    # check if there is a file named softlink_testfile.txt
    softlinkpath = os.path.join(workdir, "softlink_testfile.txt")
    isfile = os.path.isfile(softlinkpath)
    print(f"os.path.isfile({softlinkpath}) = ", isfile)

    # check if there is a link named softlink_softlink_testfile.txt
    softlinkpath = os.path.join(workdir, "softlink_testfile.txt")
    islink = os.path.islink(softlinkpath)
    print(f"os.path.islink({softlinkpath}) = ", islink)

    # check if there is a link named testfile.txt
    softlinkpath = os.path.join(workdir, "testfile.txt")
    islink = os.path.islink(softlinkpath)
    print(f"os.path.islink({softlinkpath}) = ", islink)


if __name__ == "__main__":
    filedir = os.path.dirname(__file__)
    main(filedir)