#include <iostream>
#include <vector>
#include <memory>

#include <boost/asio.hpp>
//#include <boost/asio/spawn.hpp>
#include <boost/bind.hpp>
#include <boost/thread/thread.hpp>

int PORT = 8123;

auto hourThreadFunc = [] () {
    return std::string("do implement\n");
};


//no kernel since that requires serialisation
enum class OpDef {
    BW,
    Sharpen,
    Smoothen
};

struct TaskDef {
    std::string filePath;
    OpDef op;
    bool done; //done when the output file is confirmed to be written
};

struct WorkBatchDef {
public:
    WorkBatchDef(boost::asio::io_service &io_s, int aRangeStart, int aRangeEnd, std::vector<TaskDef> &aWork):
        io_service(io_s),
        rangeStart(aRangeStart),
        rangeEnd(aRangeEnd),
        work(aWork)
    {
    }
public:
    boost::asio::io_service &io_service;
    int rangeStart;
    int rangeEnd;
    std::vector<TaskDef> &work;
};

struct ConnDef {
public:
    ConnDef(boost::asio::io_service& io_service):
        sock(io_service)
    { }

public:
    //std::shared_ptr<boost::asio::ip::tcp::socket> sock;
    boost::asio::ip::tcp::socket sock;
};


//never yelds, holds the statefull connection details
void workerLoop(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> work) {
    std::cout << hourThreadFunc() << "\n";

}

void acceptConn(std::shared_ptr<ConnDef> conn,
                std::shared_ptr<WorkBatchDef> work,
                const boost::system::error_code& err)
{
    if (!err) {
        //check stuff, then start
        work->io_service.post(boost::bind(&workerLoop, conn, work));
    }
    
}


void mainLoop() {
    //read work definition
    std::vector<TaskDef> allWork;

    boost::asio::io_service io_service;
    boost::asio::io_service::work work(io_service);
    boost::thread_group pool;
    int threadCount = 20;
    for(int i = 0; i < threadCount; i++) {
        pool.create_thread(boost::bind(&boost::asio::io_service::run, &io_service));
    }

    //init acceptor and spawn one worker per new connection

    boost::asio::ip::tcp::acceptor acceptor(io_service,
                                            boost::asio::ip::tcp::endpoint(boost::asio::ip::tcp::v4(), PORT));

    bool stop = false;

    while (!stop) {
        //compute work batch
        std::shared_ptr<WorkBatchDef> newWork(new WorkBatchDef(io_service, 0, 0, allWork));

        std::shared_ptr<ConnDef> newConn(new ConnDef(io_service));
        acceptor.async_accept(newConn->sock,
                              boost::bind(&acceptConn, newConn, newWork, boost::asio::placeholders::error));
    }

        pool.join_all();
}

int main()
{
    std::cout << "Hello World!" << "\n";
    mainLoop();
    return 0;
}
