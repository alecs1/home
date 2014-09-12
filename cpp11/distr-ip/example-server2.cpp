//boost standard examples
#include <iostream>
#include <stdio.h>
#include <boost/bind.hpp>
#include <boost/asio.hpp>
#include <boost/date_time/local_time/local_time.hpp>
#include <boost/enable_shared_from_this.hpp>
#include <boost/thread/thread.hpp>

#include <thread>
#include <chrono>

std::string makeDaytimeString() {
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

class TCPConn :
    public boost::enable_shared_from_this<TCPConn>
{
public:
    ~TCPConn() {
        std::cout << strDate()
                  << " ~TCPConn, this=" << this << ", t=" << std::this_thread::get_id() << "\n\n\n";
    }

    typedef boost::shared_ptr<TCPConn> pointer;

    static pointer create(boost::asio::io_service& io_service) {
        return pointer(new TCPConn(io_service));
    }

    boost::asio::ip::tcp::socket& socket() {
        return m_socket;
    }

    void start() {
        std::cout << strDate()
                  << " TCPConn::start() - start, this=" << this << ", t=" << std::this_thread::get_id() << "\n";
        m_message = makeDaytimeString();
        boost::asio::async_write(m_socket,
                                 boost::asio::buffer(m_message),
                                 boost::bind(&TCPConn::handleWrite,
                                             shared_from_this(),
                                             boost::asio::placeholders::error,
                                             boost::asio::placeholders::bytes_transferred));
        std::cout << strDate()
                  << " TCPConn::start() - done, this=" << this << ", t=" << std::this_thread::get_id() << "\n";
    }

private:
    TCPConn(boost::asio::io_service& io_service):
        m_socket(io_service)
    {
        std::cout << strDate()
                  << " TCPConn, this=" << this << ", t=" << std::this_thread::get_id() << "\n";
    }

    //this does nothing?
    void handleWrite(const boost::system::error_code& /*error*/,
                     size_t /*bytes_transferred*/)
    {
    }

private:
    boost::asio::ip::tcp::socket m_socket;
    std::string m_message;
};



class TCPServer {
public:
    TCPServer(boost::asio::io_service& io_service) :
        m_acceptor(io_service, boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), 13))
    {
        //std::cout << "TCPServer(), t=" << std::this_thread::get_id() << "\n";
        startAccept();
    }

    ~TCPServer() {
        std::cout << " ~TCPServer(), t=\n" << std::this_thread::get_id() << "\n";
    }
private:
    void startAccept() {
        std::cout << strDate()
                 << " TCPServer::startAccept() - start, t=" <<  std::this_thread::get_id() << "\n";
        TCPConn::pointer newConn = TCPConn::create(m_acceptor.get_io_service());
        m_acceptor.async_accept(newConn->socket(),
                                boost::bind(&TCPServer::handleAccept, this, newConn,
                                            boost::asio::placeholders::error));
        std::cout << strDate()
                  << " TCPServer::startAccept() - done, t=" <<  std::this_thread::get_id() << "\n";
    }
    void handleAccept(TCPConn::pointer newConn,
                      const boost::system::error_code& error)
    {
        std::cout << strDate()
                  << " TCPServer::handleAccept(), t=" <<  std::this_thread::get_id() << "\n";
        if (!error) {
            newConn->start();
        }
        startAccept();
    }

private:
    boost::asio::ip::tcp::acceptor m_acceptor;
};



int main(int argc, char* argv[]) {
    try {
        std::cout << strDate()
                  << " Main, t=" << std::this_thread::get_id() << "\n";
        boost::asio::io_service io;
        TCPServer server(io);
        boost::thread t(boost::bind(&boost::asio::io_service::run, &io));
        boost::thread t2(boost::bind(&boost::asio::io_service::run, &io));
        boost::thread t3(boost::bind(&boost::asio::io_service::run, &io));
        //std::cout << "extra thread=" << t.id << "\n";
        io.run();
        t.join();
    }
    catch (std::exception& e) {
        std::cerr << e.what() << "\n";
    }

    return 0;
}
