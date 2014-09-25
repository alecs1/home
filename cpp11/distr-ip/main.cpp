#include <iostream>
#include <sstream>
#include <fstream>
#include <vector>
#include <memory>
#include <thread>
#include <atomic>
#include <chrono>
#include <mutex>

#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include <boost/thread/thread.hpp>
#include <boost/lockfree/queue.hpp>
#include <boost/iostreams/device/mapped_file.hpp>


#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include <stdio.h>

#include "global_defines.h"
#include "definitions.h"
#include "TGA.h"
#include "distr-ip.h"

#define S_TASK_BATCH 5


auto hourAndThread = [] () {
    //auto now = std::chrono::system_clock::now();
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


void initSubTaskSharedData(SubTaskDef& subTask, std::mutex& mutex, const std::string& inFPath, const std::string& outFPath)
{
    std::cout << __func__ << " - " << subTask.shared->initialised << ", " << inFPath << ", " << outFPath << "\n";

    SubTaskShared* shared = subTask.shared;

    if (shared->initialised)
        return;

    //TODO - learn about this unique_lock, lock guard
    std::lock_guard<std::mutex> lockHandle(mutex);
    
    //mutex.lock();
    //try {
        if (!(shared->initialised)) {
            shared->inFile = new boost::iostreams::mapped_file_source(inFPath);

            std::fstream file;
            file.open(outFPath, std::ios::binary | std::ios::trunc | std::ios::out);
            file.close();
            uint64_t fSize = boost::filesystem::file_size(boost::filesystem::path(inFPath));
            std::cout << __func__ << " - new fSize=" << fSize << "\n";
            boost::iostreams::mapped_file_params sinkParams;
            sinkParams.path = outFPath;
            sinkParams.new_file_size = fSize;
            shared->outFile = new boost::iostreams::mapped_file_sink(sinkParams);

            shared->inHeader = new TGA_HEADER;
            shared->outHeader = new TGA_HEADER;
            GetTGAHeader(shared->inFile, shared->inHeader);
            *(shared->outHeader) = *(shared->inHeader);

            memcpy(shared->outFile->data(), shared->outHeader, sizeof(*shared->outHeader));
            //the tricky bit here: we check if the initialisation is done outside of a lock, thus we have to make sure that setting initialised is under no circumstances optimised to happen before everything is done
            if (shared->inFile && shared->outFile && shared->inHeader && shared->outHeader) {
                shared->initialised = true;
            }
            else {
                std::cout << __func__ << " - failed to initialise structures for file: " << inFPath << "\n";
            }
        }
//    }
//    catch (std::exception ex) {
//        std::cout << __func__ << " - some exception: " << ex.what() << "\n, we could release mutex, but let's just crash, can't recover\n";
//        abort();
//    }

    //mutex.unlock();
}

void pushBackTasks(WorkBatchDef* workBatch) {
    if (workBatch->work.size() > 0)
        std::cout << __func__ << " - will push to fail queue " << workBatch->work.size() << " elements\n";

    while (workBatch->work.size() > 0) {
        //this is the global queue
        workBatch->failedQueue.push(workBatch->work.back());
        workBatch->work.pop_back();
    }
}


int splitWork(std::string inDir,
              std::string fName,
              std::string outDir,
              boost::lockfree::queue<TaskDef*>& workQueue,
              uint32_t& id)
{
    std::cout << __func__ << " - inPath=" << inDir << ", fName=" << fName << ", outDir=" << outDir << "\n";

    //TODO - check those inversion bits
    TGA_HEADER inHeader;
    std::atomic<uint32_t>* remaining = new std::atomic<uint32_t>;

    SubTaskShared* shared = new SubTaskShared;
    shared->initialised = false;
    shared->inFile = NULL;
    shared->outFile = NULL;
    shared->inHeader = NULL;
    shared->outHeader = NULL;

    //bool* initialised = new bool;
    //*initialised = false;

    *remaining = 0;
    boost::iostreams::mapped_file_source inFile(inDir + "/" + fName);
    GetTGAHeader(&inFile, &inHeader);

    std::cout << __func__ << " - " << inDir << "/" << fName << ": " << inHeader.width << ", " << inHeader.height << "\n";

    int taskCount = 0;

    for(int y = 0; y < inHeader.height; y += 100) {
        for(int x = 0; x < inHeader.width; x += 100) {
            int h = 100;
            if (inHeader.height - y < 100)
                h = inHeader.height - y;
            int w = 100;
            if (inHeader.width - x < 100)
                w = inHeader.width - x;
            (*remaining) += 1;
            SubTaskDef* subTask = new SubTaskDef(*remaining, shared, x, y, w, h);
            TaskDef* task = new TaskDef(inDir, fName, outDir, OpType::BW, id);
            task->subTask = subTask;
            workQueue.push(task);
            id += 1;
            taskCount += 1;
            std::cout << __func__ << " - " << inDir << "/" << fName << ": " << x << ", " << y << ", " << w << ", " << h << "\n";
        }
    }
    return taskCount;
}

uint32_t readWork(boost::lockfree::queue<TaskDef*>& workQueue) {
#ifdef WIN32
    std::string outDir("D:\\tmp\\tga-out");
    boost::filesystem::path path("D:\\tmp\\tga-in");
#else
    std::string outDir("/home/alex/tmp/tga/out");
    boost::filesystem::path path("/home/alex/tmp/tga/in");
#endif

    uint32_t id = 0;

    try {
        if (boost::filesystem::exists(path)) {
            for (auto iter = boost::filesystem::directory_iterator(path);
                iter != boost::filesystem::directory_iterator();
                iter++)
            {
                //aici: read size of the file and decide its splitting
                //will fail at least with: directories name "*.tga*", files named "*.tga<*>", fs races
                std::string fName = iter->path().filename().string();
                std::string dName = path.string();
                if ((fName.rfind(".tga") != std::string::npos)
                    || (fName.rfind(".TGA") != std::string::npos))
                {
                    if (boost::filesystem::file_size(iter->path()) > S_MIN_FILE_SIZE_FOR_SPLITTING) {
                        splitWork(dName, fName, outDir, workQueue, id);
                    }
                    else {
                        workQueue.push(new TaskDef(dName, fName, outDir, OpType::BW, id));
                        id += 1;
                    }
                    std::cout << __func__ << fName << "\n";
                }
            }
        }
    }
    catch (std::exception& ex) {
        std::cout << ex.what() << "\n";
        abort();
    }
    return id;
}



int manageNextTask(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch) {
    if (workBatch->work.size() == 0) {
        std::cout << "Finished a batch of jobs, will take another one\n\n";
        for (int i = 0; i < S_TASK_BATCH; i++) {
            TaskDef *taskDef;
            if (workBatch->failedQueue.pop(taskDef)) {
                workBatch->work.push_back(taskDef);
            }
        }
    }

    if (workBatch->work.size() > 0) {
        if (workBatch->work.back()->subTask == NULL) {
            sendNextHeader(conn, workBatch);
        }
        else {
            sendSingleRequest(conn, workBatch);
        }
    }
    else {
        std::cout << __func__ << " - finishing comm with the current client, there were not tasks to take.\n\n\n";
    }
    return workBatch->work.size();
}

void readSingleReply(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch,
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
    conn->lastOpExpectedBytes = conn->lastOpExpectedBytes - S_HEADER_CLIENTWORKDEF + S_HEADER_SERVERREQDEF; //size of data + reply header
    boost::asio::async_read(conn->sock, boost::asio::buffer(conn->buf, conn->lastOpExpectedBytes),
                            boost::bind(&processSingleReply, conn, workBatch,
                                        boost::asio::placeholders::error,
                                        boost::asio::placeholders::bytes_transferred));
}

void sendSingleRequest(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch)
{
    TaskDef* task = workBatch->work.back();
    SubTaskDef* subTask = task->subTask;
    SubTaskShared* shared = subTask->shared;

    if ( shared->initialised == false) {
        initSubTaskSharedData(*subTask, conn->globalMutex,
                              task->filePath(), task->outFilePath());
    }

    ClientWorkDef def;
    def.reqId = task->id;
    def.op = task->op;
    def.compression = CompressionType::None;
    def.transmit = TransmitType::Bloc100x100;
    def.x = subTask->x;
    def.y = subTask->y;
    def.w = subTask->w;
    def.h = subTask->h;
    def.bpp = subTask->shared->inHeader->bits;
    def.dataSize = def.h * def.w * (def.bpp/8);
    def.serialise(conn->buf);
    uint64_t bytes = getRectFromFile(shared->inFile, shared->inHeader,
                                     subTask->x, subTask->y, subTask->w, subTask->h, conn->buf + S_HEADER_CLIENTWORKDEF);
    std::cout << __func__ << " - " << def.x << ", " << def.y << ", " << def.w << ", " << def.h << ", " << def.bpp << ", " << def.dataSize << ", " << bytes << "\n";
    conn->lastOpExpectedBytes = bytes + S_HEADER_CLIENTWORKDEF;
    ToBWBlock(conn->validationBuf + S_HEADER_SERVERREQDEF, def.bpp, def.w, def.h);
    boost::asio::async_write(conn->sock, boost::asio::buffer(conn->buf, bytes+S_HEADER_CLIENTWORKDEF),
        boost::bind(&readSingleReply, conn, workBatch,
                    boost::asio::placeholders::error,
                    boost::asio::placeholders::bytes_transferred));
}

void processSingleReply(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch,
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


    TaskDef* task = workBatch->work.back();
    SubTaskDef* subTask = task->subTask;
    SubTaskShared* shared = subTask->shared;

    ServerReqDef reply(conn->buf);
    if (!reply.valid) {
        abort();
        return;
    }
    if (reply.reqId != task->id) {
        std::cout << __func__ << " reply did not match request\n";
        abort();
        return;
    }

    for(unsigned int i = S_HEADER_SERVERREQDEF; i < conn->lastOpExpectedBytes; i++) {
        if (conn->buf[i] != conn->validationBuf[i]) {
            //std::cout << __func__ << " - invalid reply, byte: " << i << "\n";
            //abort();
        }
    }


    bytes = writeRectToFile(shared->outFile, shared->outHeader, subTask->x, subTask->y, subTask->w, subTask->h, conn->buf + S_HEADER_SERVERREQDEF);
    if (bytes != (shared->outHeader->bits/8) * subTask->w * subTask->h) {
        std::cout << __func__ << " - wrote to file: " << bytes << ", expected: " << (shared->outHeader->bits/8) * subTask->w * subTask->h << "\n";
    }

    //ordering important to ensure we don't double delete (ABA problem)
    if ( (--subTask->remainingTasksCount) == 0) {
        delete subTask->shared;
    }

    delete workBatch->work.back();
    workBatch->work.pop_back();
    conn->remainingTasks -= 1;

    manageNextTask(conn, workBatch);
}

//sendNextHeader -> sendNextChunk -> readNextChunk >>
void sendNextHeader(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch)
{
    TaskDef* task = workBatch->work.back();
    ClientWorkDef def;

    def.reqId = task->id;
    def.op = task->op;
    def.compression = CompressionType::None;
    def.transmit = TransmitType::FullFile;
    def.x = 0;
    def.y = 0;
    def.w = 0;
    def.h = 0;
    def.dataSize = boost::filesystem::file_size(task->filePath());
    std::cout << "sending file: " << task->filePath() << " of size " << def.dataSize << "\n";


    //keep buffer alive, maybe even reuse it!
    conn->fileBufPos = 0;
    conn->lastOpExpectedBytes = S_HEADER_CLIENTWORKDEF;
    def.serialise(conn->buf);
    boost::asio::async_write(conn->sock, boost::asio::buffer(conn->buf, S_HEADER_CLIENTWORKDEF),
        boost::bind(&sendNextChunk, conn, workBatch,
                    boost::asio::placeholders::error,
                    boost::asio::placeholders::bytes_transferred));
}


void readNextHeader(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch,
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
    TaskDef* task = workBatch->work.back();
    if (reply.reqId != task->id) {
        std::cout << __func__ << " reply did not match request\n";
        abort();
        return;
    }

    conn->outS.open(workBatch->work.back()->outFilePath(),
                    std::ios::binary | std::ios::trunc | std::ios::out);
    if (conn->outS.rdstate() != std::ios::goodbit) {
        std::cout << __func__ << " - std::fstream::open failed: " <<
            workBatch->work.back()->outFilePath() << ", " << conn->outS.rdstate() << "\n";
        return;
    }

    conn->fileBufPos = 0;
    conn->sBackFile = reply.dataSize;
    conn->lastOpExpectedBytes = conn->sBackFile;
    if (conn->lastOpExpectedBytes > conn->sBuf)
        conn->lastOpExpectedBytes = conn->sBuf;

    boost::asio::async_read(conn->sock, boost::asio::buffer(conn->buf, conn->lastOpExpectedBytes),
        boost::bind(&readNextChunk, conn, workBatch,
        boost::asio::placeholders::error,
        boost::asio::placeholders::bytes_transferred));
}

void readNextChunk(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch,
    const boost::system::error_code& err, size_t bytes)
{
    std::cout << __func__ << " - " << conn->outS.tellg() << "\n";
    if (err != boost::system::errc::success) {
        std::cout << __func__ << " - error: " << err.message() << "\n\n\n";
        return;
    }

    if (bytes != conn->lastOpExpectedBytes) {
        std::cout << __func__ << " - last socket write: " << bytes << " bytes, expected: " <<
            conn->lastOpExpectedBytes << "\n\n\n";
        return;
    }

    for(unsigned int i = 0; i < bytes; i++) {
        if (conn->buf[i] != conn->validationBuf[i]) {
            //std::cout << __func__ << " - invalid reply, byte: " << i << ", len = " << bytes << ", already wrote: " << conn->outS.tellg() << "\n";
            //abort();
        }
    }

    conn->outS.write(conn->buf, bytes);
    if (conn->outS.rdstate() != std::ios::goodbit) {
        std::cout << __func__ << "std::fstream::write faled:" << conn->inS.rdstate() << "\n\n\n";
        return;
    }

    conn->fileBufPos += bytes;

    if (conn->fileBufPos == conn->sBackFile) {
        conn->outS.close();
        delete workBatch->work.back();
        workBatch->work.pop_back();
        conn->remainingTasks -= 1;
        if (workBatch->work.size() == 0) {
            std::cout << "Finished a batch of jobs, will take another one\n\n";
            for (int i = 0; i < S_TASK_BATCH; i++) {
                TaskDef *taskDef;
                if (workBatch->failedQueue.pop(taskDef)) {
                    workBatch->work.push_back(taskDef);
                }
            }
        }
        if (workBatch->work.size() > 0) {
            if (workBatch->work.back()->subTask == NULL) {
                sendNextHeader(conn, workBatch);
            }
            else {
                sendSingleRequest(conn, workBatch);
            }
        }
        else {
            std::cout << __func__ << " - finishing comm with the current client\n\n\n";
        }
    }
    else {
        conn->lastOpExpectedBytes = conn->sBackFile - conn->fileBufPos;
        if (conn->lastOpExpectedBytes > conn->sBuf)
            conn->lastOpExpectedBytes = conn->sBuf;

        boost::asio::async_read(conn->sock, boost::asio::buffer(conn->buf, conn->lastOpExpectedBytes),
            boost::bind(&readNextChunk, conn, workBatch,
            boost::asio::placeholders::error,
            boost::asio::placeholders::bytes_transferred));
    }
}

void sendNextChunk(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch,
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
        conn->inS.open(workBatch->work.back()->filePath(), std::ifstream::in | std::ios::binary);
        if (conn->inS.rdstate() != std::ios::goodbit) {
            std::cout << __func__ << " - std::fstream::open failed: " <<
                workBatch->work.back()->filePath() << ", " << conn->inS.rdstate() << "\n";
            return;
        }
    }

    if (conn->inS.eof() == false) {
        conn->inS.read(conn->buf, conn->sBuf);
        bytes = conn->inS.gcount();

        //debug
        memcpy(conn->validationBuf, conn->buf, bytes);

        if (bytes <= 0) {
            std::cout << __func__ << " - std::ifstream::read() error\n";
            //pushBackTasks(work.get());
            return;
        }
        conn->lastOpExpectedBytes = bytes;
        conn->fileBufPos += bytes;

        boost::asio::async_write(conn->sock,
                                 boost::asio::buffer(conn->buf, bytes),
                                 boost::bind(&sendNextChunk, conn, workBatch,
                                             boost::asio::placeholders::error,
                                             boost::asio::placeholders::bytes_transferred));
    }
    else {
        conn->inS.close();
        //conn->lastOpExpectedBytes = S_HEADER_SERVERREQDEF; //superfluous
        //we're done sending the file, start receiving
        boost::asio::async_read(conn->sock,
                                boost::asio::buffer(conn->buf, S_HEADER_SERVERREQDEF),
                                boost::bind(&readNextHeader, conn, workBatch,
                                            boost::asio::placeholders::error,
                                            boost::asio::placeholders::bytes_transferred));
    }
}

void acceptConn(std::shared_ptr<ConnDef> conn,
                std::shared_ptr<WorkBatchDef> workBatch,
                boost::asio::io_service& ioService,
                boost::asio::ip::tcp::acceptor& acceptor,
                const boost::system::error_code& err)
{
    printFuncInfo(__func__);

    //take references to needed variable before posting the new thread
    boost::lockfree::queue<TaskDef*> &workQueue = workBatch->failedQueue;
    std::mutex& globalMutex = conn->globalMutex;
    std::atomic<uint32_t>& remainingTasks = conn->remainingTasks;

    if ( (!err) && (workBatch->work.size() > 0) ) {
        if (workBatch->work.back()->subTask == NULL) {
            workBatch->io_service.post(boost::bind(&sendNextHeader, conn, workBatch));
        }
        else {
            workBatch->io_service.post(boost::bind(&sendSingleRequest, conn, workBatch));
        }
    }
    else {
        //std::cout << __func__ << "dropping connection just accepted, err:" << err.message()
        //    << ", queue length: " << work->work.size() << "\n";
    }


    std::vector<TaskDef*> taskList;
    for (int i = 0; i < S_TASK_BATCH; i++) {
        TaskDef* newTask;
        if (workQueue.pop(newTask))
            taskList.push_back(newTask);
    }
    std::shared_ptr<WorkBatchDef> newWork(new WorkBatchDef(ioService, taskList, workQueue));
    std::shared_ptr<ConnDef> newConn(new ConnDef(ioService, remainingTasks, globalMutex));
    acceptor.async_accept(newConn->sock,
        boost::bind(&acceptConn,
        newConn,
        newWork,
        boost::ref(ioService),
        boost::ref(acceptor),
        boost::asio::placeholders::error));
}


void mainLoop() {
    printFuncInfo(__func__);

    //read work definition
    boost::lockfree::queue<TaskDef*> allWork(10000);
    std::atomic<uint32_t> taskCount;
    taskCount = readWork(allWork);
    std::cout << __func__ << " - number of tasks: " << taskCount << "\n";

    boost::asio::io_service io_service;
    boost::asio::io_service::work work(io_service);
    boost::thread_group pool;
    int threadCount = 4;
    for(int i = 0; i < threadCount; i++) {
        pool.create_thread(boost::bind(&boost::asio::io_service::run, &io_service));
    }

    boost::asio::ip::tcp::acceptor acceptor(io_service,
                                            boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), PORT));


    std::vector<TaskDef*> taskList;
    for (int i = 0; i < S_TASK_BATCH; i++) {
        TaskDef* newTask;
        if (allWork.pop(newTask))
            taskList.push_back(newTask);
    }
    std::shared_ptr<WorkBatchDef> newWork(new WorkBatchDef(io_service, taskList, allWork));

    std::mutex globalMutex;
    std::shared_ptr<ConnDef> newConn(new ConnDef(io_service, taskCount, globalMutex));
    acceptor.async_accept(newConn->sock,
        boost::bind(&acceptConn,
        newConn,
        newWork,
        boost::ref(io_service),
        boost::ref(acceptor),
        boost::asio::placeholders::error));

    //check for termination condition here instead of inside acceptConn
    while (true) {
        std::this_thread::sleep_for(std::chrono::seconds(5));
        if (taskCount == 0) {
            io_service.stop();
            break;
        }
        std::cout << __func__ << " - remaining tasks: " << taskCount << "\n";
    }

    pool.join_all();


    if (!allWork.empty()) {
        std::cout << __func__ << " - error work queue is not empty\n";
    }


    printFuncInfo(__func__);

    //pwrite();
}

int main()
{
    printFuncInfo(__func__);
    mainLoop();
    return 0;
}
