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
void fPrint4(const boost::system::error_code& /*e*/) {
    fPrint(std::string("fPrint4 - ") + strDate());
}
//TODO - this still cannot be used as param to async_wait()
auto fPrint5 = [] (const boost::system::error_code& /*e*/) { fPrint(std::string("fPrint4 - ") + strDate()); };

void fPrint6(const boost::system::error_code& /*e*/,
             boost::asio_deadline_timer* t, int* count)
{
    if (*count < 5)
        return;

    fPrint(std::string("fPrint4 - ") + strDate());
    (*count) += 1;

    t->expires_at(t->expires_at() + boost::posix_time::seconds(1));
    t->async_wait(boost::bind(fPrint6, boost::asio::placeholders::error, t, count));
}

int main() {
    boost::asio::io_service io;
    boost::asio::deadline_timer t(io, boost::posix_time::seconds(1));
    int count = 0;

    t.async_wait(boost::bind(fPrint6, boost::asio::placeholders::error, &t, &count));

    t.async_wait(&fPrint4);




    std::cout << "main()-done - " << strDate() << "\n";
    return 0;
}
