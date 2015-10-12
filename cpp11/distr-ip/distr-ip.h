#pragma once


struct SubTaskShared {
    ~SubTaskShared() {
        delete inFile;
        delete outFile;
        delete inHeader;
        delete outHeader;
    }
    //global mutex protected
    bool initialised;
    boost::iostreams::mapped_file_source* inFile;
    boost::iostreams::mapped_file_sink* outFile;
    TGA_HEADER* inHeader;
    TGA_HEADER* outHeader;
    //end global mutex protected

};

struct SubTaskDef {
    SubTaskDef(std::atomic<uint32_t>& aRemainingTasksCount, SubTaskShared* aShared,
               uint32_t aX, uint32_t aY, uint32_t aW, uint32_t aH):
        remainingTasksCount(aRemainingTasksCount),
        shared(aShared),
        x(aX), y(aY), w(aW), h(aH)
    {
    }

    //TODO - all these are shared - correctly handle deletion with mutex protection
    std::atomic<uint32_t>& remainingTasksCount;
    SubTaskShared* shared; //this could also use a shared_ptr


    uint32_t x, y, w, h;
};

struct TaskDef {
    TaskDef(std::string dPath, std::string fName, std::string oPath, OpType operation, uint32_t aId):
        dir(dPath),
        fileName(fName),
        outDir(oPath),
        op(operation),
        id(aId)
    {
        subTask = NULL;
    }
    ~TaskDef() {
        if (subTask != NULL)
            delete subTask;
    }

    std::string filePath() {
        return dir + "/" + fileName;
    }
    std::string outFilePath() {
        return outDir + "/" + fileName;
    }
    std::string dir;
    std::string fileName;
    std::string outDir;
    OpType op;
    uint32_t id;
    SubTaskDef* subTask; //should this be NULL the task is not split
};


struct WorkBatchDef;
void pushBackTasks(WorkBatchDef* work);
//TODO - has a non-trivial destructor which may allocate!
//idea: the destructor pushes back all elements from "work" that were not deleted, but it looks like a hidden kind of crap that happens behind your back.
struct WorkBatchDef {
public:
    WorkBatchDef(boost::asio::io_service &io_s, std::vector<TaskDef*> aWork, boost::lockfree::queue<TaskDef*>& aFailedQueue) :
        io_service(io_s),
        work(aWork),
        failedQueue(aFailedQueue)
    {
    }

    ~WorkBatchDef() {
        if (work.size() > 0) {
            std::cout << __func__ << " - FIXME, pushing back tasks was left to the destructor\n";
            pushBackTasks(this);
        }
    }
public:
    boost::asio::io_service &io_service;
    std::vector<TaskDef*> work;
    boost::lockfree::queue<TaskDef*> &failedQueue;
};

//socket and other stuff reused during connection to a client
struct ConnDef {
public:
    ConnDef(boost::asio::io_service& ioService, std::atomic<uint32_t>& aRemainingTasks, std::mutex& aGlobalMutex):
        sock(ioService),
        remainingTasks(aRemainingTasks),
        globalMutex(aGlobalMutex)
    {
        sBuf = 4 * SQUARE_SIDE_PIXELS * SQUARE_SIDE_PIXELS + S_HEADER_CLIENTWORKDEF; //able to hold SQUARE_SIDE_PIXELS**2 32 bit pixels + its header
        buf = new char[sBuf];
        validationBuf = new char[sBuf];
    }

    ~ConnDef() {
        delete[] buf;
        delete[] validationBuf;
    }

public:
    boost::asio::ip::tcp::socket sock;
    std::atomic<uint32_t>& remainingTasks;
    char* buf;
    char* validationBuf;
    uint64_t sBuf;

    uint64_t lastOpExpectedBytes;
    uint64_t fileBufPos;
    uint64_t sBackFile;

    std::fstream inS;
    std::fstream outS;
    std::mutex& globalMutex;
};


void sendNextChunk(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work,
                   const boost::system::error_code& err, size_t bytes);
void readNextChunk(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work,
    const boost::system::error_code& err, size_t bytes);
void sendNextHeader(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work);
void readNextHeader(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work,
                    const boost::system::error_code& err, size_t bytes);

void processSingleReply(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work,
                       const boost::system::error_code& err, size_t bytes);
void sendSingleRequest(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work);
