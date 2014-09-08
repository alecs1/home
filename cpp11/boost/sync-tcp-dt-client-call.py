from subprocess import call, Popen, PIPE
from multiprocessing import Pool

def finish_callback():
    print "done"

def main() :
    pool = Pool(processes=10)
    for i in range(20):
        #pool.apply_async(call, ["./sync-tcp-dt-client.exe", "10.58.10.224"], callback=finish_callback)
        pool.apply_async(Popen,
                         [["./sync-tcp-dt-client.exe", "10.58.10.224"], PIPE],
                         callback=finish_callback)
        pass

    return 0

if __name__ == "__main__":
    main()


