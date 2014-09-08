//boost standard examples
#include <iostream>
#include <stdio.h>

#include <boost/asio.hpp>
#include <boost/date_time/local_time/local_time.hpp>

auto strDate = []() {
        boost::local_time::time_zone_ptr tzUTC( new boost::local_time::posix_time_zone("UTC"));
        boost::local_time::local_date_time ldt = boost::local_time::local_microsec_clock::local_time(tzUTC);
        std::stringstream ss;
        ss << ldt;
        return ss.str();
};


int main(int argc, char* argv[]) {
    auto dateId = strDate();
    printf("client: %s - started\n", dateId.c_str());

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

            if (err == boost::asio::error::eof) {
                printf("client: %s - connection finished\n", dateId.c_str());
                break;
            }
            else if (err){
                throw boost::system::system_error(err);
            }

            std::cout.write(buf.data(), len);
        }
    }
    catch (std::exception& e) {
        std::cerr << e.what() << "\n";
    }

    printf("client: %s - done\n", dateId.c_str());
    return 0;
}
