//boost standard examples
#include <iostream>
#include <stdio.h>
#include <boost/asio.hpp>
#include <boost/date_time/local_time/local_time.hpp>


#include <thread>
#include <chrono>

std::string make_daytime_string() {
    //pretend that this takes a lot of time, so we can't really serve requests efficiently
    std::this_thread::sleep_for(std::chrono::seconds(5));
    time_t now = time(0);
    return ctime(&now);
}

auto strDate = []() {
        boost::local_time::time_zone_ptr tzUTC( new boost::local_time::posix_time_zone("UTC"));
        boost::local_time::local_date_time ldt = boost::local_time::local_microsec_clock::local_time(tzUTC);
        std::stringstream ss;
        ss << ldt;
        return ss.str();
};

int main(int argc, char* argv[]) {
    try {
        boost::asio::io_service io_service;
        boost::asio::ip::tcp::acceptor acceptor(io_service, 
                                                boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), 13));
        
        for(;;) {
            boost::asio::ip::tcp::socket socket(io_service);

            printf("accept() - waiting for client - %s\n", strDate().c_str());
            acceptor.accept(socket); //blocks indefinitely, until som client connects
            printf("accept() - got client - %s\n", strDate().c_str());

            std::string message = make_daytime_string();
            boost::system::error_code ignoredError;
            boost::asio::write(socket, boost::asio::buffer(message), ignoredError);
        }
        
    }

    catch (std::exception& e) {
        std::cerr << e.what() << "\n";
    }

    return 0;
}
