#include <iostream>
#include <stdio.h>

#include <boost/asio.hpp>

int main(int argc, char* argv[]) {
    try {
        if (argc != 2) {
            printf("Usage: client <host>\n");
            return 1;
        }
        
        boost::asio::io_service io_service;
        boost::asio::ip::tcp::resolver resolver(io_service);
        boost::asio::ip::tcp::resolver::query query(argv[1], "daytime"); //
        boost::asio::ip::tcp::resolver::iterator endpointIterator = resolver.resolve(query);
        boost::asio::ip::tcp::socket socket(io_service);
        boost::asio::connect(socket, endpointIterator);

        for(;;) {
            std::array<char, 128> buf; //wrapper over int[]
            boost::system::error_code err;

            size_t len = socket.read_some(boost::asio::buffer(buf), err);

            if (err == boost::asio::error::eof)
                break;
            else if (err)
                throw boost::system::system_error(err);

            std::cout.write(buf.data(), len);
        }
    }
    catch (std::exception& e) {
        std::cerr << e.what() << "\n";
    }

    return 0;
}
