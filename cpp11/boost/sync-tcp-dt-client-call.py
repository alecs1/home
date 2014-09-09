from subprocess import call, Popen, PIPE
from multiprocessing import Pool

def finish_callback():
    print "done"

def finish_callback2():
    print "done " + str(i)

def main() :
    pool = Pool(processes=10)
    for i in range(20):
        #pool.apply_async(call, ["./sync-tcp-dt-client.exe", "10.58.10.224"], callback=finish_callback)
        print i

        branch = 1
        if branch == 0:
            pool.apply_async(Popen,
                             [["./sync-tcp-dt-client.exe", "10.58.10.224"], PIPE],
                             callback=finish_callback)
        elif branch == 1:
            pool.apply_async(Popen,
                             [["./echo-client-tcp-blocking.exe", "10.58.10.224", "777"], PIPE],
                             [finish_callback2, [i]])
        pass

    return 0

if __name__ == "__main__":
    main()


