#include <stdio.h>
#include <iostream>

#include <boost/asio.hpp>

#include "global_defines.h"

#define SRV "10.58.10.224"


int main(int argc, char* argv[]) {
    /*
      some other guy's code:
      boost::asio::io_service io_service;
      boost::asio::ip::tcp::resolver::query query("localhost", "41005");
      boost::asio::ip::tcp::resolver resolver(io_service);
      boost::asio::ip::tcp::resolver::iterator destination = resolver.resolve(query);
      boost::asio::ip::tcp::resolver::iterator end ;
      boost::asio::ip::tcp::endpoint endpoint;
    */

    boost::asio::io_service ioServ;


    boost::asio::ip::tcp::endpoint endPoint;
    endPoint.address(boost::asio::ip::address::from_string(SRV));
    endPoint.port(PORT);
    boost::asio::ip::tcp::socket sock(ioServ);
    sock.connect(endPoint);

    int loopCount = 0;
    bool stop = false();
    while (!stop) {
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

        loopCount+=1;
        printf("%s - %d\n", __func__, loopCount);
    }
    return 0;
}
