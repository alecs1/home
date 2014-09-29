#include <mutex>
#include <fstream>

#include <boost/asio.hpp>
#include <boost/bind.hpp>
#include <boost/thread/thread.hpp>
#include <boost/lockfree/queue.hpp>
#include <boost/iostreams/device/mapped_file.hpp>
#define BOOST_FILESYSTEM_NO_DEPRECATED
#include <boost/filesystem.hpp>

#include "global_defines.h"
#include "TGA.h"
#include "definitions.h"
#include "distr-ip.h"

//sendNextHeader -> sendNextChunk -> readNextChunk >>
void sendNextHeader(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch)
{
    TaskDef* task = workBatch->work.back();
    ClientWorkDef def;

    def.reqId = task->id;
    def.op = task->op;
    def.compression = CompressionType::None;
    def.transmit = TransmitType::FullFile;
    def.x = 0;
    def.y = 0;
    def.w = 0;
    def.h = 0;
    def.dataSize = boost::filesystem::file_size(task->filePath());
    std::cout << "sending file: " << task->filePath() << " of size " << def.dataSize << "\n";


    //keep buffer alive, maybe even reuse it!
    conn->fileBufPos = 0;
    conn->lastOpExpectedBytes = S_HEADER_CLIENTWORKDEF;
    def.serialise(conn->buf);
    boost::asio::async_write(conn->sock, boost::asio::buffer(conn->buf, S_HEADER_CLIENTWORKDEF),
        boost::bind(&sendNextChunk, conn, workBatch,
                    boost::asio::placeholders::error,
                    boost::asio::placeholders::bytes_transferred));
}


void readNextHeader(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch,
                    const boost::system::error_code& err, size_t bytes)
{
    if (err != boost::system::errc::success) {
        std::cout << __func__ << " - error: " << err.message() << "\n\n\n";
        return;
    }

    if (bytes != S_HEADER_SERVERREQDEF) {
        std::cout << __func__ << " - last socket write: " << bytes << " bytes, expected: " <<
            S_HEADER_SERVERREQDEF << "\n\n\n";
        return;
    }

    ServerReqDef reply(conn->buf);
    if (!reply.valid) {
        return;
    }
    TaskDef* task = workBatch->work.back();
    if (reply.reqId != task->id) {
        std::cout << __func__ << " reply did not match request\n";
        abort();
        return;
    }

    conn->outS.open(workBatch->work.back()->outFilePath(),
                    std::ios::binary | std::ios::trunc | std::ios::out);
    if (conn->outS.rdstate() != std::ios::goodbit) {
        std::cout << __func__ << " - std::fstream::open failed: " <<
            workBatch->work.back()->outFilePath() << ", " << conn->outS.rdstate() << "\n";
        return;
    }

    conn->fileBufPos = 0;
    conn->sBackFile = reply.dataSize;
    conn->lastOpExpectedBytes = conn->sBackFile;
    if (conn->lastOpExpectedBytes > conn->sBuf)
        conn->lastOpExpectedBytes = conn->sBuf;

    boost::asio::async_read(conn->sock, boost::asio::buffer(conn->buf, conn->lastOpExpectedBytes),
        boost::bind(&readNextChunk, conn, workBatch,
        boost::asio::placeholders::error,
        boost::asio::placeholders::bytes_transferred));
}

void readNextChunk(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch,
    const boost::system::error_code& err, size_t bytes)
{
    std::cout << __func__ << " - " << conn->outS.tellg() << "\n";
    if (err != boost::system::errc::success) {
        std::cout << __func__ << " - error: " << err.message() << "\n\n\n";
        return;
    }

    if (bytes != conn->lastOpExpectedBytes) {
        std::cout << __func__ << " - last socket write: " << bytes << " bytes, expected: " <<
            conn->lastOpExpectedBytes << "\n\n\n";
        return;
    }

    for(unsigned int i = 0; i < bytes; i++) {
        if (conn->buf[i] != conn->validationBuf[i]) {
            //std::cout << __func__ << " - invalid reply, byte: " << i << ", len = " << bytes << ", already wrote: " << conn->outS.tellg() << "\n";
            //abort();
        }
    }

    conn->outS.write(conn->buf, bytes);
    if (conn->outS.rdstate() != std::ios::goodbit) {
        std::cout << __func__ << "std::fstream::write faled:" << conn->inS.rdstate() << "\n\n\n";
        return;
    }

    conn->fileBufPos += bytes;

    if (conn->fileBufPos == conn->sBackFile) {
        conn->outS.close();
        delete workBatch->work.back();
        workBatch->work.pop_back();
        conn->remainingTasks -= 1;
        if (workBatch->work.size() == 0) {
            std::cout << "Finished a batch of jobs, will take another one\n\n";
            for (int i = 0; i < S_TASK_BATCH; i++) {
                TaskDef *taskDef;
                if (workBatch->failedQueue.pop(taskDef)) {
                    workBatch->work.push_back(taskDef);
                }
            }
        }
        if (workBatch->work.size() > 0) {
            if (workBatch->work.back()->subTask == NULL) {
                sendNextHeader(conn, workBatch);
            }
            else {
                sendSingleRequest(conn, workBatch);
            }
        }
        else {
            std::cout << __func__ << " - finishing comm with the current client\n\n\n";
        }
    }
    else {
        conn->lastOpExpectedBytes = conn->sBackFile - conn->fileBufPos;
        if (conn->lastOpExpectedBytes > conn->sBuf)
            conn->lastOpExpectedBytes = conn->sBuf;

        boost::asio::async_read(conn->sock, boost::asio::buffer(conn->buf, conn->lastOpExpectedBytes),
            boost::bind(&readNextChunk, conn, workBatch,
            boost::asio::placeholders::error,
            boost::asio::placeholders::bytes_transferred));
    }
}

void sendNextChunk(std::shared_ptr<ConnDef> conn, std::shared_ptr<WorkBatchDef> workBatch,
                   const boost::system::error_code& err, size_t bytes)
{
    if (err != boost::system::errc::success) {
        std::cout << __func__ << " - error: " << err.message() << "\n\n\n";
        //pushBackTasks(work.get());
        return; //safe to exit, connection and all memory will be deallocated via the shared pointers destruction
    }

    //check if there's any case when write_async has no error but sends fewer byts
    if (bytes != conn->lastOpExpectedBytes) {
        std::cout << __func__ << " - last socket write: " << bytes << " bytes, expected: " <<
            conn->lastOpExpectedBytes << "\n\n\n";
        //pushBackTasks(work.get());
        return;
    }

        if (conn->fileBufPos == 0) {
        conn->inS.open(workBatch->work.back()->filePath(), std::ifstream::in | std::ios::binary);
        if (conn->inS.rdstate() != std::ios::goodbit) {
            std::cout << __func__ << " - std::fstream::open failed: " <<
                workBatch->work.back()->filePath() << ", " << conn->inS.rdstate() << "\n";
            return;
        }
    }

    if (conn->inS.eof() == false) {
        conn->inS.read(conn->buf, conn->sBuf);
        bytes = conn->inS.gcount();

        //debug
        memcpy(conn->validationBuf, conn->buf, bytes);

        if (bytes <= 0) {
            std::cout << __func__ << " - std::ifstream::read() error\n";
            //pushBackTasks(work.get());
            return;
        }
        conn->lastOpExpectedBytes = bytes;
        conn->fileBufPos += bytes;

        boost::asio::async_write(conn->sock,
                                 boost::asio::buffer(conn->buf, bytes),
                                 boost::bind(&sendNextChunk, conn, workBatch,
                                             boost::asio::placeholders::error,
                                             boost::asio::placeholders::bytes_transferred));
    }
    else {
        conn->inS.close();
        //conn->lastOpExpectedBytes = S_HEADER_SERVERREQDEF; //superfluous
        //we're done sending the file, start receiving
        boost::asio::async_read(conn->sock,
                                boost::asio::buffer(conn->buf, S_HEADER_SERVERREQDEF),
                                boost::bind(&readNextHeader, conn, workBatch,
                                            boost::asio::placeholders::error,
                                            boost::asio::placeholders::bytes_transferred));
    }
}
