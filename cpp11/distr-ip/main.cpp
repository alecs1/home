#include <iostream>
#include <sstream>
#include <vector>
#include <memory>
#include <thread>
#include <atomic>
#include <chrono>

#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include <boost/thread/thread.hpp>
#include <boost/lockfree/queue.hpp>


#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include <stdio.h>

#include "global_defines.h"
#include "definitions.h"


auto hourAndThread = [] () {
    auto now = std::chrono::system_clock::now();
    //auto tt = std::chrono::system_clock::to_time_t(now);
    //auto lt = 
    //char fTime[100];
    //std::strftime(fTime, 100, "%H:%M:%S", &tt);
    std::stringstream ss;
    ss << std::this_thread::get_id();
    return ss.str();
};

auto printFuncInfo = [] (const char* func) {
    std::stringstream ss;
    ss << hourAndThread();
    ss << " " << func;
    std::cout << ss.str() << "\n";
};


struct TaskDef {
	TaskDef(std::string dPath, std::string fName, std::string oPath, OpType operation, uint64_t aId):
        dir(dPath),
        fileName(fName),
        outDir(oPath),
        op(operation),
        id(aId)
    {}
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
    uint64_t id;
    bool done; //done when the output file is confirmed to be written
};


struct WorkBatchDef;
void pushBackTasks(WorkBatchDef* work);

//TODO - non-trivial destructor which may allocate!
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
        }
        pushBackTasks(this);
    }
public:
    boost::asio::io_service &io_service;
    std::vector<TaskDef*> work;
    boost::lockfree::queue<TaskDef*> &failedQueue;
};

void pushBackTasks(WorkBatchDef* work) {
    if (work->work.size() > 0)
        std::cout << __func__ << " - will push to fail queue " << work->work.size() << " elements\n";

    while (work->work.size() > 0) {
        work->failedQueue.push(work->work.back());
        work->work.pop_back();
    }
}

/**
enum class ProtoState {
    SendHeader,
    Send
};
/**/

//socket and other stuff reused during connection to a client
struct ConnDef {
public:
    ConnDef(boost::asio::io_service& io_service):
        sock(io_service)
    {
        sBuf = 10000;
        buf = new char[sBuf];
    }

    ~ConnDef() {
         delete[] buf;
    }

public:
    boost::asio::ip::tcp::socket sock;
    char* buf;
    uint64_t sBuf;

    uint64_t lastOpExpectedBytes;
    uint64_t fileBufPos;
    uint64_t sBackFile;

    std::fstream inS;
    std::fstream outS;
};

struct MainLoopState {
    std::atomic<int> acceptSlots;
};

void readNextHeader(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work,
                    const boost::system::error_code& err, size_t bytes);

void readWork2(boost::lockfree::queue<TaskDef*>& workQueue) {
    std::string outDir("D:\\tmp\\tga-out");
    boost::filesystem::path path("D:\\tmp\\tga-in");

    uint64_t id = 0;

    try {
        if (boost::filesystem::exists(path)) {
            for (auto iter = boost::filesystem::directory_iterator(path);
                iter != boost::filesystem::directory_iterator();
                iter++)
            {
                //will fail at least with: directories name "*.tga*", files named "*.tga<*>", fs races
                std::string fName = iter->path().filename().string();
                std::string dName = path.string();
                if ((fName.rfind(".tga") != std::string::npos)
                    || (fName.rfind(".TGA") != std::string::npos))
                {
                    workQueue.push(new TaskDef(dName, fName, outDir, OpType::BW, id));
                    id += 1;
                    std::cout << __func__ << fName << "\n";
                }
            }
        }
    }
    catch (std::exception& ex) {
        std::cout << ex.what() << "\n";
    }
}

void sendNextChunk(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work,
                   const boost::system::error_code& err, size_t bytes)
{
    if (err != boost::system::errc::success) {
        std::cout << __func__ << " - error: " << err.message() << "\n\n\n";
        //pushBackTasks(work.get());
        return; //safe to exit, connection and all memory will be deallocated via the shared pointers destruction
    }

    //check if there's any case when write_async has no error but sends fewer byts
    if (bytes != conn->lastOpExpectedBytes) {
        std::cout << __func__ << " - last socket write: " << bytes << " bytes, expected: " <<
            conn->lastOpExpectedBytes << "\n\n\n";
        //pushBackTasks(work.get());
        return;
    }

    if (conn->fileBufPos == 0) {
        conn->inS.open(work->work.back()->filePath(), std::ifstream::in | std::ios::binary);
        if (conn->inS.rdstate() == std::ios::goodbit) {
            std::cout << __func__ << " - std::fstream::open failed: " <<
                work->work.back()->filePath() << "\n";
            return;
        }
    }

    if (conn->inS.eof() == false) {
        conn->inS.read(conn->buf, conn->sBuf);
        bytes = conn->inS.gcount();

        if (bytes <= 0) {
            std::cout << __func__ << " - std::ifstream::read() error\n";
            //pushBackTasks(work.get());
            return;
        }
        conn->lastOpExpectedBytes = bytes;
        conn->fileBufPos += bytes;

        boost::asio::async_write(conn->sock,
                                 boost::asio::buffer(conn->buf, bytes),
                                 boost::bind(&sendNextChunk, conn, work,
                                             boost::asio::placeholders::error,
                                             boost::asio::placeholders::bytes_transferred));
    }
    else {
        conn->inS.close();
        //conn->lastOpExpectedBytes = S_HEADER_SERVERREQDEF; //superfluous
        //we're done sending the file, start receiving
        boost::asio::async_read(conn->sock,
                                boost::asio::buffer(conn->buf, S_HEADER_SERVERREQDEF),
                                boost::bind(&readNextHeader, conn, work,
                                            boost::asio::placeholders::error,
                                            boost::asio::placeholders::bytes_transferred));
    }
}

//sendNextHeader -> sendNextChunk -> readNextChunk >>
void sendNextHeader(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work) {
    TaskDef* taskDef = work->work.back();
    ClientWorkDef def;

    def.reqId = taskDef->id;
    def.op = taskDef->op;
    def.transmit = TransmitType::FullFile;
    def.compression = CompressionType::None;
    def.w = 0;
    def.h = 0;
    def.dataSize = boost::filesystem::file_size(taskDef->filePath());
    std::cout << "sending file: " << taskDef->filePath() << " of size " << def.dataSize << "\n";

    //keep buffer alive, maybe even reuse it!
    conn->fileBufPos = 0;
    def.serialise(conn->buf);
    boost::asio::async_write(conn->sock, boost::asio::buffer(conn->buf, S_HEADER_CLIENTWORKDEF),
        boost::bind(&sendNextChunk, conn, work,
                    boost::asio::placeholders::error,
                    boost::asio::placeholders::bytes_transferred));
}


void readNextChunk(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work,
    const boost::system::error_code& err, size_t bytes);
void readNextHeader(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work,
                    const boost::system::error_code& err, size_t bytes)
{
    if (err != boost::system::errc::success) {
        std::cout << __func__ << " - error: " << err.message() << "\n\n\n";
        return;
    }

    if (bytes != S_HEADER_SERVERREQDEF) {
        std::cout << __func__ << " - last socket write: " << bytes << " bytes, expected: " <<
            S_HEADER_SERVERREQDEF << "\n\n\n";
        return;
    }

    ServerReqDef reply(conn->buf);
    if (!reply.valid) {
        return;
    }

    conn->outS.open(work->work.back()->outFilePath(),
                    std::ios::binary | std::ios::trunc | std::ios::out);
    if (conn->outS.rdstate() == std::ios::goodbit) {
        std::cout << __func__ << " - std::fstream::open failed: " <<
                  work->work.back()->outFilePath() << "\n";
        return;
    }

    conn->fileBufPos = 0;
    conn->sBackFile = reply.dataSize;
    conn->lastOpExpectedBytes = conn->sBackFile;
    if (conn->lastOpExpectedBytes > conn->sBuf)
        conn->lastOpExpectedBytes = conn->sBuf;

    boost::asio::async_read(conn->sock, boost::asio::buffer(conn->buf, conn->lastOpExpectedBytes),
        boost::bind(&readNextChunk, conn, work,
        boost::asio::placeholders::error,
        boost::asio::placeholders::bytes_transferred));
}

void readNextChunk(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work,
    const boost::system::error_code& err, size_t bytes)
{
    if (err != boost::system::errc::success) {
        std::cout << __func__ << " - error: " << err.message() << "\n\n\n";
        return;
    }

    if (bytes != conn->lastOpExpectedBytes) {
        std::cout << __func__ << " - last socket write: " << bytes << " bytes, expected: " <<
            conn->lastOpExpectedBytes << "\n\n\n";
        return;
    }

    conn->fileBufPos += bytes;

    if (conn->fileBufPos == conn->sBackFile) {
        conn->outS.close();
        delete work->work.back();
        work->work.pop_back();
        sendNextHeader(conn, work);
    }
    else {
        conn->lastOpExpectedBytes = conn->sBackFile - conn->fileBufPos;
        if (conn->lastOpExpectedBytes > conn->sBuf)
            conn->lastOpExpectedBytes = conn->sBuf;

        boost::asio::async_read(conn->sock, boost::asio::buffer(conn->buf, conn->lastOpExpectedBytes),
            boost::bind(&readNextChunk, conn, work,
            boost::asio::placeholders::error,
            boost::asio::placeholders::bytes_transferred));
    }
}

void initClientCom(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work) {
    //actually nothing special to do for now, start sending the header
    sendNextHeader(conn, work);
}

//does not yeld until exit, holds the state of the current communication
void workerLoop2(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work) {
    //std::cout << __func__ << "\n";

    boost::system::error_code err;
    uint32_t loopCount = 0;
    try {
        while (work->work.size() > 0)
        {
            TaskDef* taskDef = work->work.back();
            ClientWorkDef def;

            def.reqId = loopCount;
            def.op = taskDef->op;
            def.transmit = TransmitType::FullFile;
            def.compression = CompressionType::None;
            def.w = 0;
            def.h = 0;
            def.dataSize = boost::filesystem::file_size(taskDef->filePath());
            std::cout << "sending file: " << taskDef->filePath() << " of size " << def.dataSize << "\n";


            std::array<char, S_HEADER_CLIENTWORKDEF> headerBuf;
            def.serialise(headerBuf.data());
            boost::asio::write(conn->sock, boost::asio::buffer(headerBuf, headerBuf.size()), err);

            const int bufSize = 10000;
            std::array<char, bufSize> buf;
            std::ifstream fileSIn(taskDef->filePath(), std::ifstream::in | std::ios::binary);

            uint64_t totalWrote = 0;
            uint64_t wrote = 0;
            uint64_t bytes = 0;
            while (1) {
                if (fileSIn.eof() == false) {
                    fileSIn.read(buf.data(), buf.size());
                    bytes = fileSIn.gcount();
                    if (bytes <= 0) {
                        std::cout << __func__ << " - std::ifstream::read() error\n";
                    }

                    wrote = boost::asio::write(conn->sock, boost::asio::buffer(buf, bytes), err);
                    totalWrote += wrote;
                    if (err) {
                        std::cout << __func__ << " - boost::asio::write() error\n";
                    }
                    if (wrote != bytes) {
                        std::cout << __func__ << " - wrote=" << wrote << ", expected=" << bytes << "\n";
                    }
                }
                else
                    break;
            }
            if (totalWrote != def.dataSize) {
                std::cout << "Error, totalWrote=" << totalWrote << ", expected=" << def.dataSize << "\n";
            }

            std::cout << __func__ << " - sent " << taskDef->filePath() << ", " << totalWrote << " bytes\n";


            //we're now expecting the data back
            std::array<char, S_HEADER_SERVERREQDEF> replyHeaderBuf;
            size_t len = boost::asio::read(conn->sock,
                boost::asio::buffer(replyHeaderBuf, replyHeaderBuf.size()));

            if (len != S_HEADER_SERVERREQDEF) {
                std::cout << "Error reading header\n";
            }
            ServerReqDef reply(replyHeaderBuf.data());
            std::shared_ptr<char> pImgData = std::shared_ptr<char>(new char[reply.dataSize]);
            uint64_t readBytes = 0;
            while (readBytes < reply.dataSize) {
                readBytes += conn->sock.read_some(boost::asio::buffer(pImgData.get() + readBytes, reply.dataSize - readBytes));
                std::cout << "Read " << readBytes << ", remaining " << reply.dataSize - readBytes << "\n";
            }

            //write down the file

            std::fstream outStream;
            outStream.open(taskDef->outFilePath(), std::ios::binary | std::ios::trunc | std::ios::out);
            if (outStream.rdstate() == std::ios::goodbit) {
                outStream.write(pImgData.get(), reply.dataSize);
            }
            outStream.close();
            delete taskDef;
            work->work.pop_back();

            //at this point we're done with the TaskDef, delete forever

            std::cout << "Done reading reply, will start over\n";
        }

        //now close the connection
        ClientWorkDef def;
        def.op = OpType::Stop;
        std::array<char, S_HEADER_CLIENTWORKDEF> headerBuf;
        def.serialise(headerBuf.data());
        boost::asio::write(conn->sock, boost::asio::buffer(headerBuf, headerBuf.size()), err);
    }
    catch (std::exception ex) {
        std::cout << __func__ << " - exception:" << ex.what() << "\n";
        std::cout << __func__ << " - will push to fail queue " << work->work.size() << " elements\n";
    }

    //whatever task was not finished will be put back to the fail queue
    while (work->work.size() > 0) {
        work->failedQueue.push(work->work.back());
        work->work.pop_back();
    }

    std::cout << __func__ << " - done\n\n\n";
}

void acceptConn(std::shared_ptr<ConnDef> conn,
                std::shared_ptr<WorkBatchDef> work,
                MainLoopState& mlState,
                const boost::system::error_code& err)
{
    printFuncInfo(__func__);
    mlState.acceptSlots--;
    std::cout << "accept slots: " << mlState.acceptSlots << "\n";
    if (!err) {
        //check stuff, then start
        work->io_service.post(boost::bind(&workerLoop2, conn, work));
    }
    
}


void mainLoop() {
    printFuncInfo(__func__);

    //read work definition
    boost::lockfree::queue<TaskDef*> allWork2(10000);
    readWork2(allWork2);

    boost::asio::io_service io_service;
    boost::asio::io_service::work work(io_service);
    boost::thread_group pool;
    int threadCount = 20;
    for(int i = 0; i < threadCount; i++) {
        pool.create_thread(boost::bind(&boost::asio::io_service::run, &io_service));
    }

    boost::asio::ip::tcp::acceptor acceptor(io_service,
                                            boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), PORT));

    MainLoopState mlState;
    mlState.acceptSlots = 0;
    bool stop = false;
    int loopCount = 0;

    uint64_t rangeStart = 0;
    uint64_t rangeEnd = 0;
    while (!stop) {
        //compute work batch
        if (mlState.acceptSlots < 1) {

            std::vector<TaskDef*> taskList;
            for (int i = 0; i < 5; i++) {
                TaskDef* newTask;
                if (allWork2.pop(newTask))
                    taskList.push_back(newTask);
            }
            std::shared_ptr<WorkBatchDef> newWork(new WorkBatchDef(io_service, taskList, allWork2));


            std::shared_ptr<ConnDef> newConn(new ConnDef(io_service));
            acceptor.async_accept(newConn->sock,
                boost::bind(&acceptConn,
                newConn,
                newWork,
                boost::ref(mlState),
                boost::asio::placeholders::error));

            mlState.acceptSlots++;
            std::cout << "accept slots: " << mlState.acceptSlots << "\n";
        }
        loopCount += 1;
        //printf("%s - %d\n", __func__, loopCount);
        //sleep to make debugging easier
        std::this_thread::sleep_for(std::chrono::seconds(1));
    }

    pool.join_all();


    if (!allWork2.empty()) {
        std::cout << __func__ << " - error work queue is not empty\n";
    }


    printFuncInfo(__func__);
}

int main()
{
    printFuncInfo(__func__);
    mainLoop();
    return 0;
}
