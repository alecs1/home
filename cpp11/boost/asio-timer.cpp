//boost standard examples

#include <iostream>
#include <sstream>
#include <stdio.h>

#include <thread>
#include <chrono>

#include <boost/asio.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/local_time/local_time.hpp>


auto strDate = []() {
        boost::local_time::time_zone_ptr tzUTC( new boost::local_time::posix_time_zone("UTC"));
        boost::local_time::local_date_time ldt = boost::local_time::local_microsec_clock::local_time(tzUTC);
        std::stringstream ss;
        ss << ldt;
        return ss.str();
};

//TODO - use bind to assign parameters
auto fPrint = [] (std::string const & str) { printf("%s\n", str.c_str()); };
auto fPrint2 = [] () { fPrint(std::string("fPrint2")); };
auto fPrint3 = [] (const boost::system::error_code& /*e*/) { fPrint(std::string("fPrint3")); };
void fPrint4(const boost::system::error_code& /*e*/) {
    fPrint(std::string("fPrint4 - ") + strDate());
}


int main() {
    boost::asio::io_service io;

    //blocking wait
    boost::asio::deadline_timer t(io, boost::posix_time::seconds(3));
    std::cout << "t.wait 3 sec - " << strDate() << "\n";
    t.wait();
    std::cout << "after wait - " << strDate() << "\n\n\n";

    //async_wait
    t.expires_from_now(boost::posix_time::seconds(8));
    std::cout << "t.async_wait 8 sec; io.run() - " << strDate() << "\n";
    t.async_wait(&fPrint4);
    std::cout << "after t.async_wait(), we can still do other stuff, like loop for 5 seconds - " << strDate() << "\n";
    for(int i = 0; i < 5; i++) {
        std::cout << "    sleep 1 s - " << strDate() << "\n";
        std::this_thread::sleep_for(std::chrono::seconds(1));
    }

    std::cout << "calling io.run() - " << strDate() << "\n";
    io.reset();
    io.run();
    std::cout << "after io.run() - " << strDate() << "\n\n\n";


    std::cout << "calling io.run() with no queue - " << strDate() << "\n";
    io.run();
    std::cout << "after io.run() - " << strDate() << "\n\n\n";
    

    t.expires_from_now(boost::posix_time::seconds(8));
    std::cout << "t.async_wait 8 sec; io.run() - " << strDate() << "\n";
    t.async_wait(&fPrint4);
    std::cout << "after t.async_wait(), will not call io.run() - " << strDate() << "\n\n\n";


    std::cout << "main()-done - " << strDate() << "\n";
    return 0;
}
