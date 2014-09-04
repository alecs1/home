//boost standard examples
#include <iostream>
#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include <boost/date_time/posix_time/posix_time.hpp>
#include <boost/date_time/local_time/local_time.hpp>

auto strDate = []() {
        boost::local_time::time_zone_ptr tzUTC( new boost::local_time::posix_time_zone("UTC"));
        boost::local_time::local_date_time ldt = boost::local_time::local_microsec_clock::local_time(tzUTC);
        std::stringstream ss;
        ss << ldt;
        return ss.str();
};

class MyClass {
private:
    boost::asio::deadline_timer m_timer;
    int m_count;

public:
    MyClass(boost::asio::io_service& io) :
        m_timer(io, boost::posix_time::seconds(1)),
        m_count(0)
    {
        m_timer.async_wait(boost::bind(&MyClass::print, this));
        std::cout << "MyClass() - " << strDate() << "\n";
    }

    ~MyClass() {
        std::cout << "~MyClass() - " << strDate() << "\n";
    }

    void print() {
        if (m_count >= 5)
            return;

        std::cout << __func__ << ", m_count=" << m_count << " - " << strDate() << "\n";
    }
    
    /*
    apparently no lambda function here :)
    auto print2 = [] (std::string anotherStr) {
        std::cout << __func__ << " " << anotherStr << " " << m_count << " - "  << strDate() << "\n";
    };
    */
};

int main() {
    boost::asio::io_service io;
    MyClass obj(io);
//obj.print2("xxxyyy");
    io.run();
    return 0;
}
