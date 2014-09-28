#include <stdio.h>
#include <iostream>
#include <fstream>
#include <thread>

#include <boost/asio.hpp>

//could not get this thing to link on Debian
//#ifdef WIN32
#include <boost/interprocess/sync/named_mutex.hpp>
//#endif

#include "global_defines.h"
#include "definitions.h"
#include "TGA.h"

#ifdef WIN32
#define SRV "10.58.10.224"
#else
#define SRV "127.0.0.1"
#endif

bool startNewInstance() {
#ifdef WIN32
    /*
    BOOL WINAPI CreateProcess(
        _In_opt_     LPCTSTR lpApplicationName,
        _Inout_opt_  LPTSTR lpCommandLine,
        _In_opt_     LPSECURITY_ATTRIBUTES lpProcessAttributes,
        _In_opt_     LPSECURITY_ATTRIBUTES lpThreadAttributes,
        _In_         BOOL bInheritHandles,
        _In_         DWORD dwCreationFlags,
        _In_opt_     LPVOID lpEnvironment,
        _In_opt_     LPCTSTR lpCurrentDirectory,
        _In_         LPSTARTUPINFO lpStartupInfo,
        _Out_        LPPROCESS_INFORMATION lpProcessInformation
        );
    */
    STARTUPINFO startupInfo;
    PROCESS_INFORMATION procInfo;
    memset(&startupInfo, 0, sizeof(startupInfo));
    startupInfo.cb = sizeof(startupInfo);
    memset(&procInfo, 0, sizeof(procInfo));
    BOOL created = CreateProcess(
        NULL, //lpApplicationName
        "worker-client.exe", //lpCommandLine
        NULL, //lpProcessAttributes
        NULL, //lpThreadAttributes
        FALSE, //bInheritHandles
        CREATE_DEFAULT_ERROR_MODE | CREATE_NO_WINDOW | DETACHED_PROCESS, //dwCreationFlags
        NULL, //lpEnvironment
        NULL, //lpCurrentDirectory
        &startupInfo, //lpStartupInfo
        &procInfo); //lpProcessInformation
    std::cout << "new instance is started: " << created << "\n";

    if (!created) {
        auto createError = GetLastError();
        std::cout << "CreateProcessW error: " << createError << "\n";
    }
    return created;
#else
    std::cout << __func__ << " - not implemented on Unix systems\n";
    return false;
#endif

}

//increase: true-new instance is starting, false-instance is finishing
int manageInstances(bool increase) {
#ifdef WIN32
    std::cout << __func__ << " - start\n";

    int instCount = 1;
    boost::interprocess::permissions p;
    p.set_unrestricted();

    //these mutex resources remain aquired if unlock is not run.
    boost::interprocess::named_mutex instMutex(boost::interprocess::open_or_create,
        "worker-client2",
        p);

    instMutex.lock();
    {
        std::fstream fInstCount;

        fInstCount.open("D:\\instances-count.txt", std::ios::in | std::ios::binary);
        if (fInstCount.rdstate() == std::ios::goodbit) {
            std::cout << "D:\\instances-count.txt already exists :) \n";
            fInstCount.seekg(0);
            fInstCount >> instCount;
            instCount += 1;
            fInstCount.close();
        }
        else {
            std::cout << "D:\\instances-count.txt to be created \n";
        }

        fInstCount.open("D:\\instances-count.txt", std::ios::out | std::ios::binary | std::ios::trunc);
        if (!increase) {
            instCount -= 1;
        }
        fInstCount << instCount;
        fInstCount.close();


        std::ifstream fInstMax("D:\\instances-max.txt", std::ifstream::in | std::ios::binary);
        int instMax;
        fInstMax.seekg(0);
        fInstMax >> instMax;
        fInstMax.close();
        std::cout << __func__ << " - no. of instances to be running in parallel:" << instMax << "\n";
        std::cout << __func__ << " - no. of instances to start:" << instMax - instCount << "\n";

        //newly created processes will wait for this function to finish
        for ( ; instCount <= instMax; instCount++) {
            if (startNewInstance()) {
                std::cout << "started instance: " << instCount << "\n";
            }
            else {
                std::cout << "failed to start instance: " << instCount << "\n";
            }
        }
    }
    instMutex.unlock();
    std::cout << __func__ << " - end\n";
    return instCount;
#else
    std::cout << __func__ << " - not implemented on Unix systems\n";
    return -1;
#endif
}


int main(int argc, char* argv[]) {

    //manageInstances(true);

    int loopCount = 0;
    bool stop = false;

    try {
        boost::asio::io_service ioServ;
        boost::asio::ip::tcp::endpoint endPoint;
        endPoint.address(boost::asio::ip::address::from_string(SRV));
        endPoint.port(PORT);
        boost::asio::ip::tcp::socket sock(ioServ);
        sock.connect(endPoint);

        while (!stop) {

            boost::system::error_code err;
            std::array<char, S_HEADER_CLIENTWORKDEF> headerBuf;

            uint64_t readBytes = 0;
            readBytes = boost::asio::read(sock, boost::asio::buffer(headerBuf, headerBuf.size()));

            ClientWorkDef def(headerBuf.data());

            std::shared_ptr<char> pImgData(new char[def.dataSize], [](char*p) { delete[] p; });
            readBytes = 0;
            readBytes = boost::asio::read(sock, boost::asio::buffer(pImgData.get(), def.dataSize));
            if (readBytes != def.dataSize) {
                std::cout << __func__ << " - aborting, readBytes=" << readBytes << ", expected=" << def.dataSize << "\n";
                abort();
            }
            std::cout << __func__ << " - readBytes=" << readBytes << ", expected=" << def.dataSize << "\n";

            if (def.op == OpType::Stop) {
                stop = true;
                std::cout << __func__ << " - exiting on stop command\n";
                break;
            }

            if (def.transmit == TransmitType::SquareBlock) {
                std::cout << "TransmitType::Bloc100x100: " << (uint8_t)def.transmit << "\n";
                ToBWBlock((unsigned char*)pImgData.get(), def.bpp, def.w, def.h);
            }
            else if (def.transmit == TransmitType::FullFile) {
                std::cout << "TransmitType::FullFile: " << (uint8_t)def.transmit << "\n";
            }


            ServerReqDef reply;
            reply.compression = def.compression;
            reply.dataSize = def.dataSize;
            reply.reqId = def.reqId;
            std::array<char, S_HEADER_SERVERREQDEF> replyHeaderBuf;
            reply.serialise(replyHeaderBuf.data());
            boost::asio::write(sock, boost::asio::buffer(replyHeaderBuf, replyHeaderBuf.size()), err);


            uint64_t newSize = def.dataSize;
            uint64_t totalWrote = 0;
            uint64_t wrote = 0;
            uint64_t toSend = 0;
            while (totalWrote < newSize) {
                toSend = 10000;
                if (newSize - totalWrote < toSend)
                    toSend = newSize - totalWrote;
                wrote = boost::asio::write(sock, boost::asio::buffer(pImgData.get() + totalWrote, toSend), err);
                totalWrote += wrote;
                if (err) {
                    std::cout << __func__ << " - aborting - boost::asio::write() error\n";
                    abort();
                }
                if (wrote != toSend) {
                    std::cout << __func__ << " - aborting, wrote=" << wrote << ", expected=" << toSend << "\n";
                    abort();
                }
            }

            //std::this_thread::sleep_for(std::chrono::seconds(10));
            loopCount += 1;
            printf("%s - %d\n", __func__, loopCount);
        }
    }
    catch (std::exception ex) {
        std::cout << __func__ << " - exception: " << ex.what() << "\n";
        std::cout << __func__ << " - program exiting with error\n";
    }

    //start new instance before exiting
    std::cout << "starting new instances\n";
    startNewInstance();
    //manageInstances(false);

    return 0;
}
