#include <stdio.h>
#include <iostream>
#include <thread>

#include <boost/asio.hpp>

#include "global_defines.h"
#include "definitions.h"
#include "TGA.h"

#define SRV "10.58.10.224"


int main(int argc, char* argv[]) {

    boost::asio::io_service ioServ;


    boost::asio::ip::tcp::endpoint endPoint;
    endPoint.address(boost::asio::ip::address::from_string(SRV));
    endPoint.port(PORT);
    boost::asio::ip::tcp::socket sock(ioServ);
    sock.connect(endPoint);

    int loopCount = 0;
    bool stop = false;

    try {
        while (!stop) {

            boost::system::error_code err;
            std::array<char, S_HEADER_CLIENTWORKDEF> headerBuf;

            size_t len = 0;
            while (len < S_HEADER_CLIENTWORKDEF) {
                len += sock.read_some(boost::asio::buffer(headerBuf, headerBuf.size()));
                std::cout << "Read " << len << ", remaining " << S_HEADER_CLIENTWORKDEF - len << "\n";
            }

            ClientWorkDef def(headerBuf.data());
            uint64_t readBytes = 0;
            std::shared_ptr<char> pImgData(new char[def.dataSize], [](char*p) { delete[] p; });
            readBytes = 0;

            readBytes = boost::asio::read(sock, boost::asio::buffer(pImgData.get(), def.dataSize));
            if (readBytes != def.dataSize) {
                std::cout << __func__ << " - readBytes=" << readBytes << ", expected=" << def.dataSize << "\n";
            }
            std::cout << __func__ << " - readBytes=" << readBytes << ", expected=" << def.dataSize << "\n";

            if (def.op == OpType::Stop) {
                stop = true;
                std::cout << __func__ << " - exiting on stop command\n";
                break;
            }

            /*
            int w, h, bpp;
            char* pImg = LoadTGAFromMem(pImgData.get(), def.dataSize, &w, &h, &bpp);
            TGADef imgDef(w, h, bpp, pImg);
            std::cout << "w=" << w << ", " << "h=" << h << ", bpp=" << bpp << "\n";

            std::shared_ptr<TGADef> retImg(ToBW(imgDef));
            */

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
                wrote = boost::asio::write(sock, boost::asio::buffer(pImgData.get() + wrote, toSend), err);
                totalWrote += wrote;
                if (err) {
                    std::cout << "boost::asio::write() error\n";
                }
                if (wrote != toSend) {
                    std::cout << "wrote=" << wrote << ", expected=" << toSend << "\n";
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
    std::cout << "starting new instance\n";
#ifdef WIN32
    //system("worker-client.exe");
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
        ".\\worker-client.exe", //lpCommandLine
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

    //system("start worker-client.exe");
#else
    //
#endif


    return 0;
}
