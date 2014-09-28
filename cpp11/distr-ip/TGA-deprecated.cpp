#include <string.h>
#include <stdlib.h>

#include "TGA.h"
#include "global_defines.h"

#ifdef _WIN32
    // if windows use standard C I/O
    #include <stdio.h>
#elif defined __ANDROID__
    #include "module.lala"
#endif

const int IT_COMPRESSED = 10;
const int IT_UNCOMPRESSED = 2;


uint64_t getRectFromMem(unsigned char* inFile, TGA_HEADER* header, uint32_t xR, uint32_t yR, uint32_t wR, uint32_t hR, unsigned char* dest)
{
    int w = header->width;
    int h = header->height;
    int rowSize = w * header->bits / 8;
    bool bInverted = ((header->descriptor & (1 << 5)) != 0);
    const unsigned char* origDest = dest;

    //skip header
    uint32_t startOffset = sizeof(*header) + header->identsize;
    const unsigned char* src = inFile + startOffset;

    for (unsigned int i = yR; i < yR+hR; i++)
    {
        uint32_t row = bInverted ? (h-i-1) : i;
        const unsigned char * srcRow = src + row * rowSize + xR * (header->bits/8);
        if (header->bits == 24)
        {
            for (unsigned int j = xR; j < xR + wR; j++)
            {
                dest[0] = srcRow[2];
                dest[1] = srcRow[1];
                dest[2] = srcRow[0];
                srcRow += 3;
                dest += 3;
            }
        }
        else
        {
            for (unsigned int j = xR; j < xR + wR; j++)
            {
                dest[0] = srcRow[2];
                dest[1] = srcRow[1];
                dest[2] = srcRow[0];
                dest[3] = srcRow[3];
                srcRow += 4;
                dest += 4;
            }
        }
    }
    return dest - origDest;
}

uint32_t writeRectToMem(unsigned char* outFile, TGA_HEADER* header, uint32_t xR, uint32_t yR, uint32_t wR, uint32_t hR, unsigned char* src)
{
    int w = header->width;
    int h = header->height;
    int rowSize = w * header->bits / 8;
    bool bInverted = ((header->descriptor & (1 << 5)) != 0);
    uint32_t wrote = 0;

    //skip header
    uint32_t startOffset = sizeof(*header) + header->identsize;
    unsigned char* dest = outFile + startOffset;

    for (unsigned int i = yR; i < yR+hR; i++)
    {
        uint32_t row = bInverted ? (h-i-1) : i;
        unsigned char * destRow = dest + row * rowSize + xR * (header->bits/8);
        if (header->bits == 24)
        {
            for (unsigned int j = xR; j < xR + wR; j++)
            {
                destRow[2] = src[0];
                destRow[1] = src[1];
                destRow[0] = src[2];
                src += 3;
                destRow += 3;
            }
        }
        else
        {
            for (unsigned int j = xR; j < xR + wR; j++)
            {
                destRow[2] = src[0];
                destRow[1] = src[1];
                destRow[0] = src[2];
                destRow[3] = src[3];
                src += 4;
                destRow += 4;
            }
        }
        wrote += wR * (header->bits/8);
    }
    return wrote;
}

char * LoadTGAFromMem(const char * data, uint64_t size, int * width, int * height, int * bpp)
{
    uint64_t pos = 0;
    TGA_HEADER header;

    memcpy(&header, data + pos, sizeof(header)); pos += sizeof(header);

    int fileLen = size;
    pos = sizeof(header) + header.identsize;

    if (header.imagetype != IT_COMPRESSED && header.imagetype != IT_UNCOMPRESSED)
    {
        return NULL;
    }

    if (header.bits != 24 && header.bits != 32)
    {
        return NULL;
    }

    int bufferSize = fileLen - sizeof(header) - header.identsize;
    char * pBuffer = new char[bufferSize];

    memcpy(pBuffer, data + pos, bufferSize);

    *width = header.width;
    *height = header.height;
    *bpp = header.bits;
    char * pOutBuffer = new char[header.width * header.height * header.bits / 8];

    switch (header.imagetype)
    {
    case IT_UNCOMPRESSED:
        LoadUncompressedImage(pOutBuffer, pBuffer, &header);
        break;
    case IT_COMPRESSED:
        LoadCompressedImage(pOutBuffer, pBuffer, &header);
        break;
    }

    delete[] pBuffer;

    return pOutBuffer;
}

//TODO - not implemented
EXPORT_DLL
char * ToBWFullFile(const char * fData, uint64_t size, uint64_t * newSize) {
    abort();
    return NULL;
/*
    uint64_t pos = 0;

    TGA_HEADER header;
    memcpy(&header, fData + pos, sizeof(header)); pos += sizeof(header);


    int fileLen = size;

    pos = sizeof(header) + header.identsize;

    if (header.imagetype != IT_COMPRESSED && header.imagetype != IT_UNCOMPRESSED)
    {
        return NULL;
    }

    if (header.bits != 24 && header.bits != 32)
    {
        return NULL;
    }

    //int bufferSize = fileLen - sizeof(header) - header.identsize;
    char * pBuffer = fData + pos;
    //new char[bufferSize];
    //memcpy(pBuffer, fData + pos, bufferSize);

    //*width = header.width;
    //*height = header.height;
    //*bpp = header.bits;
    char * pOutBuffer = new char[header.width * header.height * header.bits / 8];

    switch (header.imagetype)
    {
    case IT_UNCOMPRESSED:
        LoadUncompressedImage(pOutBuffer, pBuffer, &header);
        break;
    case IT_COMPRESSED:
        LoadCompressedImage(pOutBuffer, pBuffer, &header);
        break;
    }

    delete[] pBuffer;

    return pOutBuffer;
*/

}
