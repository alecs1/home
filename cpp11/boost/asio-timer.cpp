//#include <iostream>
#include <stdio.h>
#include <boost/asio.hpp>

#include <boost/date_time/posix_time/posix_time.hpp>

int main() {
    boost::asio::io_service io;

    boost::asio::deadline_timer t(io, boost::posix_time::seconds(5));
    printf("before wait\n");
    t.wait();
    printf("after wait\n");
    return 0;
}
