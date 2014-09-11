//no kernel application since that requires serialisation
enum class OpType : uint8_t {
    BW = 0,
    Sharpen = 1,
    Smoothen = 2
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
#define S_OP sizeof(uint8_t)
#define S_TRANSMIT sizeof(uint8_t)
#define S_COMPRESSION sizeof(uint8_t)
#define S_DIMENSION sizeof(uint32_t)
#define S_DATASIZE sizeof(uint32_t)
#define S_HEADER_CLIENTWORKDEF (S_OP + S_TRANSMIT + S_COMPRESSION + S_DIMENSION + S_DATASIZE)


//TODO - try to use a bit less direct memory access
struct ClientWorkDef {
    ClientWorkDef(uint8_t* header) {
        uint32_t pos = 0;
        memcpy(&op, header+pos, S_OP); pos += S_OP;
        memcpy(&transmit, header+pos, S_TRANSMIT); pos += S_TRANSMIT;
        memcpy(&compression, header+pos, S_COMPRESSION); pos += S_COMPRESSION;
        memcpy(&w, header+pos, S_DIMENSION); pos += S_DIMENSION;
        memcpy(&h, header+pos, S_DIMENSION); pos += S_DIMENSION;
        memcpy(&dataSize, header+pos, S_DIMENSION); pos += S_DATASIZE;
    }

    int serialise(uint8_t* header) {
        uint32_t pos = 0;
        memcpy(header+pos, &op, S_OP); pos += S_OP;
        memcpy(header+pos, &transmit, S_TRANSMIT); pos += S_TRANSMIT;
        memcpy(header+pos, &compression, S_COMPRESSION); pos += S_COMPRESSION;
        memcpy(header+pos, &w, S_DIMENSION); pos += S_DIMENSION;
        memcpy(header+pos, &h, S_DIMENSION); pos += S_DIMENSION;
        memcpy(header+pos, &dataSize, S_DATASIZE); pos += S_DATASIZE;
    }

    OpType op;
    TransmitType transmit;
    CompressionType compression;
    uint32_t w, h; //0 and irelevant if TransmitType is FullFile
    uint32_t dataSize; //4 Gib max file size
};
