//no kernel application since that requires serialisation
enum class OpType : uint8_t {
    Stop = 0,
    BW = 1,
    Sharpen = 2,
    Smoothen = 3
};

enum class TransmitType : uint8_t  {
    FullFile = 0,
    FullImageData = 1,
    PartialImageData = 2
};

enum class CompressionType : uint8_t {
    None = 0,
    Zlib = 1
};

//what we're sending to the client
#define S_SIG_BYTES 14 //some easily identifiable bytes at the start (signature)
#define S_OP sizeof(uint8_t)
#define S_TRANSMIT sizeof(uint8_t)
#define S_COMPRESSION sizeof(uint8_t)
#define S_DIMENSION sizeof(uint32_t)
#define S_DATASIZE sizeof(uint64_t)
#define S_REQ_ID sizeof(uint64_t) //some unique id of this request, used when replying
#define S_HEADER_CLIENTWORKDEF (S_SIG_BYTES + S_OP + S_TRANSMIT + S_COMPRESSION + S_DIMENSION + S_DIMENSION + S_DATASIZE + S_REQ_ID)


//TODO - try to use a bit less direct memory access
struct ClientWorkDef {
    ClientWorkDef() {
        op = OpType::Stop;
        reqId = 0xAAA;
    }

    ClientWorkDef(/*uint8_t* */ const char* header) {
        char sigBytes[S_SIG_BYTES];
        uint32_t pos = 0;
        memcpy(sigBytes, header + pos, S_SIG_BYTES); pos += S_SIG_BYTES;
        memcpy(&op, header+pos, S_OP); pos += S_OP;
        memcpy(&transmit, header+pos, S_TRANSMIT); pos += S_TRANSMIT;
        memcpy(&compression, header+pos, S_COMPRESSION); pos += S_COMPRESSION;
        memcpy(&w, header+pos, S_DIMENSION); pos += S_DIMENSION;
        memcpy(&h, header+pos, S_DIMENSION); pos += S_DIMENSION;
        memcpy(&dataSize, header+pos, S_DATASIZE); pos += S_DATASIZE;
        memcpy(&reqId, header + pos, S_REQ_ID); pos += S_REQ_ID;

        if (pos != S_HEADER_CLIENTWORKDEF)
            std::cout << __func__ << " - error deserialising" << pos << ", " << S_HEADER_CLIENTWORKDEF << "\n";

        if (strcmp("CLIENTWORKDEF", sigBytes) != 0)
            std::cout << __func__ << "- error deserialising, got wrong header signature:" << sigBytes << "\n";

        std::cout << "ClientWorkDef: " << sigBytes << ", " << (uint8_t)op << ", " << (uint8_t)transmit << ", "
            << (uint8_t)compression << ", " << w << ", " << h << ", " << dataSize << ", " << reqId << "\n";
    }

    int serialise(/*uint8_t* */ char* header) {
        uint32_t pos = 0;
        char sigBytes[] = "CLIENTWORKDEF\0";
        memcpy(header + pos, sigBytes, S_SIG_BYTES); pos += S_SIG_BYTES;
        memcpy(header+pos, &op, S_OP); pos += S_OP;
        memcpy(header+pos, &transmit, S_TRANSMIT); pos += S_TRANSMIT;
        memcpy(header+pos, &compression, S_COMPRESSION); pos += S_COMPRESSION;
        memcpy(header+pos, &w, S_DIMENSION); pos += S_DIMENSION;
        memcpy(header+pos, &h, S_DIMENSION); pos += S_DIMENSION;
        memcpy(header+pos, &dataSize, S_DATASIZE); pos += S_DATASIZE;
        memcpy(header + pos, &reqId, S_REQ_ID); pos += S_REQ_ID;
        if (pos != S_HEADER_CLIENTWORKDEF)
            std::cout << "Error serialising" << pos << ", " << S_HEADER_CLIENTWORKDEF << "\n";
        std::cout << "ClientWorkDef-serialise: ";
        std::cout.write(header, S_HEADER_CLIENTWORKDEF);
        std::cout << "\n";
        return 0;
    }

    OpType op;
    TransmitType transmit;
    CompressionType compression;
    uint32_t w, h; //0 and irelevant if TransmitType is FullFile
    uint64_t dataSize;
    uint64_t reqId;
};


#define S_HEADER_SERVERREQDEF (S_SIG_BYTES + S_COMPRESSION + S_DATASIZE + S_REQ_ID)
struct ServerReqDef {
    CompressionType compression;
    uint64_t dataSize;
    uint64_t reqId;
    bool valid;

    ServerReqDef() {
        reqId = 0xBBB;
        valid = false;
    }
    
    ServerReqDef(const char* header) {
        char sigBytes[S_SIG_BYTES];
        uint32_t pos = 0;
        valid = true;
        memcpy(sigBytes, header + pos, S_SIG_BYTES); pos += S_SIG_BYTES;
        memcpy(&compression, header + pos, S_SIG_BYTES); pos += S_COMPRESSION;
        memcpy(&dataSize, header + pos, S_DATASIZE); pos += S_DATASIZE;
        memcpy(&reqId, header + pos, S_REQ_ID); pos += S_REQ_ID;

        if (pos != S_HEADER_SERVERREQDEF) {
            valid = false;
            std::cout << __func__ << " - error deserialising" << pos << ", " << S_HEADER_SERVERREQDEF << "\n";
        }

        if (strcmp("SERVERREQDEF", sigBytes) != 0) {
            valid = false;
            std::cout << __func__ << " - error deserialising, got wrong header signature:" << sigBytes << "\n";
        }

        std::cout << "ServerReqDef: " << sigBytes << ", " << (uint8_t)compression << ", " <<
            dataSize << ", " << reqId << "\n";
    }

    int serialise(char* header) {
        uint32_t pos = 0;
        char sigBytes[] = "SERVERREQDEF\0";
        memcpy(header + pos, sigBytes, S_SIG_BYTES); pos += S_SIG_BYTES;
        memcpy(header + pos, &compression, S_COMPRESSION); pos += S_COMPRESSION;
        memcpy(header + pos, &dataSize, S_DATASIZE); pos += S_DATASIZE;
        memcpy(header + pos, &reqId, S_REQ_ID); pos += S_REQ_ID;

        if (pos != S_HEADER_SERVERREQDEF)
            std::cout << __func__ << "- error serialising" << pos << ", " << S_HEADER_SERVERREQDEF << "\n";

        std::cout << "\n";
        return 0;
    }

};