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
    while (!stop) {
        /*
        std::array<char, 10000> buf;
        boost::system::error_code err;

        size_t len = sock.read_some(boost::asio::buffer(buf), err);

        if (err == boost::asio::error::eof) {
            printf("%s-connection closed (cleanly?)\n", __func__);
            break;
        }
        else if (err){
            throw boost::system::system_error(err);
        }

        std::cout.write(buf.data(), len);

        boost::asio::write(sock, boost::asio::buffer("hello!-this is client!"),
                       boost::asio::transfer_all(), err);

        */
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
        /*
        while (readBytes < def.dataSize) {
            readBytes += sock.read_some(boost::asio::buffer(pImgData.get() + readBytes, def.dataSize - readBytes));
            std::cout << "Read " << readBytes << ", remaining " << def.dataSize - readBytes << "\n";
        }
        */
        readBytes = boost::asio::read(sock, boost::asio::buffer(pImgData.get(), def.dataSize));
        if (readBytes != def.dataSize) {
            std::cout << __func__ << " - readBytes=" << readBytes << ", expected=" << def.dataSize << "\n";
        }
        std::cout << __func__ << " - readBytes=" << readBytes << ", expected=" << def.dataSize << "\n";

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
            wrote = boost::asio::write(sock, boost::asio::buffer(pImgData.get()+wrote, toSend), err);
            totalWrote += wrote;
            if (err) {
                std::cout << "boost::asio::write() error\n";
            }
            if (wrote != toSend) {
                std::cout << "wrote=" << wrote << ", expected=" << toSend << "\n";
            }
        }
        
        //delete[] pImgData;
        //std::this_thread::sleep_for(std::chrono::seconds(10));
        loopCount += 1;
        printf("%s - %d\n", __func__, loopCount);
    }
    return 0;
}
