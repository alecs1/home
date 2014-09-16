#include <iostream>
#include <sstream>
#include <vector>
#include <memory>
#include <thread>
#include <atomic>
#include <chrono>

#include <boost/asio.hpp>
//#include <boost/asio/spawn.hpp>
#include <boost/bind.hpp>
#include <boost/thread/thread.hpp>

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
	TaskDef(std::string dPath, std::string fName, std::string oPath, OpType operation):
        dir(dPath),
        fileName(fName),
        outDir(oPath),
        op(operation)
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
    bool done; //done when the output file is confirmed to be written
};

struct WorkBatchDef {
public:
    WorkBatchDef(boost::asio::io_service &io_s, int aRangeStart, int aRangeEnd, std::vector<TaskDef> &aWork):
        io_service(io_s),
        rangeStart(aRangeStart),
        rangeEnd(aRangeEnd),
        work(aWork)
    {
    }
public:
    boost::asio::io_service &io_service;
    int rangeStart;
    int rangeEnd;
    std::vector<TaskDef> &work;
};

struct ConnDef {
public:
    ConnDef(boost::asio::io_service& io_service):
        sock(io_service)
    { }

public:
    //std::shared_ptr<boost::asio::ip::tcp::socket> sock;
    boost::asio::ip::tcp::socket sock;
};

struct MainLoopState {
    std::atomic<int> acceptSlots;
};


void readWork(std::vector<TaskDef> &allWork) {
    std::string outDir("D:\\tmp\\tga-out");
    boost::filesystem::path path("D:\\tmp\\tga-in");

    try {
        if (boost::filesystem::exists(path)) {
            for (auto iter = boost::filesystem::directory_iterator(path);
                 iter != boost::filesystem::directory_iterator();
                 iter++) 
			{
				//prone do fail at least with: directories name "*.tga*", files named "*.tga<*>", fs races
				std::string fName = iter->path().filename().string();
                std::string dName = path.string();
				if ((fName.rfind(".tga") != std::string::npos)
					|| (fName.rfind(".TGA") != std::string::npos)) {
                    allWork.push_back(TaskDef(dName, fName, outDir, OpType::BW));
                    std::cout << allWork.back().filePath() << "\n";
				}
            }
        }
    }
	catch (std::exception& ex) {
		std::cout << ex.what() << "\n";
	}

}

//does not yeld until exit, holds the state of the current communication
void workerLoop(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work) {
    //std::cout << __func__ << "\n";
    
    boost::system::error_code err;
    
    int crtWork = work->rangeStart;
    while (true) {

        if (crtWork > work->rangeEnd) {
            ClientWorkDef def;
            def.op = OpType::Stop;
            std::array<char, S_HEADER_CLIENTWORKDEF> headerBuf;
            def.serialise(headerBuf.data());
            boost::asio::write(conn->sock, boost::asio::buffer(headerBuf, headerBuf.size()), err);
            break;
        }

        ClientWorkDef def;
        TaskDef& taskDef = work->work.at(crtWork);
        def.reqId = crtWork;
        def.op = taskDef.op;
        def.transmit = TransmitType::FullFile;
        def.compression = CompressionType::None;
        def.w = 0;
        def.h = 0;
        def.dataSize = boost::filesystem::file_size(taskDef.filePath());
        std::cout << "sending file: " << taskDef.filePath() << " of size " << def.dataSize << "\n";


        std::array<char, S_HEADER_CLIENTWORKDEF> headerBuf;
        def.serialise(headerBuf.data());
        boost::asio::write(conn->sock, boost::asio::buffer(headerBuf, headerBuf.size()), err);

        const int bufSize = 10000;
        std::array<char, bufSize> buf;
        std::ifstream fileSIn(taskDef.filePath(), std::ifstream::in | std::ios::binary);

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

        std::cout << __func__ << " - sent " << taskDef.filePath() << ", " << totalWrote << " bytes\n";


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
        outStream.open(taskDef.outFilePath(), std::ios::binary | std::ios::trunc | std::ios::out);
        if (outStream.rdstate() == std::ios::goodbit) {
            outStream.write(pImgData.get(), reply.dataSize);
        }
        outStream.close();

        std::cout << "Done reading reply, will start over\n";
        crtWork += 1;
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
        work->io_service.post(boost::bind(&workerLoop, conn, work));
    }
    
}


void mainLoop() {
    printFuncInfo(__func__);

    //read work definition
    std::vector<TaskDef> allWork;
    readWork(allWork);

    boost::asio::io_service io_service;
    boost::asio::io_service::work work(io_service);
    boost::thread_group pool;
    int threadCount = 20;
    for(int i = 0; i < threadCount; i++) {
        pool.create_thread(boost::bind(&boost::asio::io_service::run, &io_service));
    }

    //init acceptor and spawn one worker per new connection

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

            if (rangeStart >= allWork.size()) {
                std::cout << __func__ << "All work has been asigned\n";
                break;
            }

            rangeEnd = rangeStart + 5;
            std::shared_ptr<WorkBatchDef> newWork(new WorkBatchDef(io_service, rangeStart, rangeEnd, allWork));
            rangeStart = rangeEnd + 1;


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


    printFuncInfo(__func__);
}

int main()
{
    printFuncInfo(__func__);
    mainLoop();
    return 0;
}
