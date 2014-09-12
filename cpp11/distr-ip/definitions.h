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
#define S_FIRST_BYTES 14 //some identifiable bytes at the start
#define S_OP sizeof(uint8_t)
#define S_TRANSMIT sizeof(uint8_t)
#define S_COMPRESSION sizeof(uint8_t)
#define S_DIMENSION sizeof(uint32_t)
#define S_DATASIZE sizeof(uint64_t)
#define S_HEADER_CLIENTWORKDEF (S_FIRST_BYTES + S_OP + S_TRANSMIT + S_COMPRESSION + S_DIMENSION + S_DIMENSION + S_DATASIZE)


//TODO - try to use a bit less direct memory access
struct ClientWorkDef {
    ClientWorkDef() {
    }

    ClientWorkDef(/*uint8_t* */ char* header) {
        char firstBytes[S_FIRST_BYTES];
        uint32_t pos = 0;
        memcpy(firstBytes, header + pos, S_FIRST_BYTES); pos += S_FIRST_BYTES;
        memcpy(&op, header+pos, S_OP); pos += S_OP;
        memcpy(&transmit, header+pos, S_TRANSMIT); pos += S_TRANSMIT;
        memcpy(&compression, header+pos, S_COMPRESSION); pos += S_COMPRESSION;
        memcpy(&w, header+pos, S_DIMENSION); pos += S_DIMENSION;
        memcpy(&h, header+pos, S_DIMENSION); pos += S_DIMENSION;
        memcpy(&dataSize, header+pos, S_DATASIZE); pos += S_DATASIZE;

        if (pos != S_HEADER_CLIENTWORKDEF)
            std::cout << "Error deserialising" << pos << ", " << S_HEADER_CLIENTWORKDEF << "\n";

        if (strcmp("CLIENTWORKDEF", firstBytes) != 0)
            std::cout << "Error deserialising, got wrong header signature:" << firstBytes << "\n";

        std::cout << "ClientWorkDef " << firstBytes << ", " << (uint8_t)op << ", " << (uint8_t)transmit << ", "
            << (uint8_t)compression << ", " << w << ", " << h << ", " << dataSize << "\n";
    }

    int serialise(/*uint8_t* */ char* header) {
        uint32_t pos = 0;
        char firstBytes[] = "CLIENTWORKDEF\0";
        memcpy(header + pos, firstBytes, S_FIRST_BYTES); pos += S_FIRST_BYTES;
        memcpy(header+pos, &op, S_OP); pos += S_OP;
        memcpy(header+pos, &transmit, S_TRANSMIT); pos += S_TRANSMIT;
        memcpy(header+pos, &compression, S_COMPRESSION); pos += S_COMPRESSION;
        memcpy(header+pos, &w, S_DIMENSION); pos += S_DIMENSION;
        memcpy(header+pos, &h, S_DIMENSION); pos += S_DIMENSION;
        memcpy(header+pos, &dataSize, S_DATASIZE); pos += S_DATASIZE;
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
    uint64_t dataSize; //4 Gib max file size
};
