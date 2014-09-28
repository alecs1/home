#include <cstring>
#include <iostream>

#include <boost/iostreams/device/mapped_file.hpp>

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

void LoadCompressedImage( char* pDest, char * pSrc, TGA_HEADER * pHeader )
{
    int w = pHeader->width;
    int h = pHeader->height;
    int rowSize = w * pHeader->bits / 8;
    bool bInverted = ( (pHeader->descriptor & (1 << 5)) != 0 );
    char * pDestPtr = bInverted ? pDest + (h + 1) * rowSize : pDest;
    int countPixels = 0;
    int nPixels = w * h;

    while( nPixels > countPixels )
    {
        unsigned char chunk = *pSrc ++;
        if ( chunk < 128 )
        {
            int chunkSize = chunk + 1;
            for ( int i = 0; i < chunkSize; i ++ )
            {
                if ( bInverted && (countPixels % w) == 0 )
                    pDestPtr -= 2 * rowSize;
                *pDestPtr ++ = pSrc[2];
                *pDestPtr ++ = pSrc[1];
                *pDestPtr ++ = pSrc[0];
                pSrc += 3;
                if ( pHeader->bits != 24 )
                    *pDestPtr ++ = *pSrc ++;
                countPixels ++;
            }
        }
        else
        {
            int chunkSize = chunk - 127;
            for ( int i = 0; i < chunkSize; i ++ )
            {
                if ( bInverted && (countPixels % w) == 0 )
                    pDestPtr -= 2 * rowSize;
                *pDestPtr ++ = pSrc[2];
                *pDestPtr ++ = pSrc[1];
                *pDestPtr ++ = pSrc[0];
                if ( pHeader->bits != 24 )
                    *pDestPtr ++ = pSrc[3];
                countPixels ++;
            }
            pSrc += (pHeader->bits >> 3);
        }
    }
}

void LoadUncompressedImage( char* pDest, char * pSrc, TGA_HEADER * pHeader )
{
    int w = pHeader->width;
    int h = pHeader->height;
    int rowSize = w * pHeader->bits / 8;
    bool bInverted = ( (pHeader->descriptor & (1 << 5)) != 0 );
    for ( int i = 0; i < h; i ++ )
    {
        char * pSrcRow = pSrc + 
            ( bInverted ? ( h - i - 1 ) * rowSize : i * rowSize );
        if ( pHeader->bits == 24 )
        {
            for ( int j = 0; j < w; j ++ )
            {
                *pDest ++ = pSrcRow[2];
                *pDest ++ = pSrcRow[1];
                *pDest ++ = pSrcRow[0];
                pSrcRow += 3;
            }
        }
        else
        {
            for ( int j = 0; j < w; j ++ )
            {
                *pDest ++ = pSrcRow[2];
                *pDest ++ = pSrcRow[1];
                *pDest ++ = pSrcRow[0];
                *pDest ++ = pSrcRow[3];
                pSrcRow += 4;
            }
        }
    }
}

uint64_t getRectFromFile(boost::iostreams::mapped_file_source* inFile, TGA_HEADER* header, uint32_t xR, uint32_t yR, uint32_t wR, uint32_t hR, char* dest)
{
    int w = header->width;
    int h = header->height;
    int rowSize = w * header->bits / 8;
    bool bInverted = ((header->descriptor & (1 << 5)) != 0);
    const char* origDest = dest;

    //skip header
    uint32_t startOffset = sizeof(*header) + header->identsize;
    const char* src = inFile->data();
    src += startOffset;

    for (unsigned int i = yR; i < yR+hR; i++)
    {
        uint32_t row = bInverted ? (h-i-1) : i;
        const char * srcRow = src + row * rowSize + xR * (header->bits/8);
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

uint32_t writeRectToFile(boost::iostreams::mapped_file_sink* outFile, TGA_HEADER* header, uint32_t xR, uint32_t yR, uint32_t wR, uint32_t hR, char* src)
{
    int w = header->width;
    int h = header->height;
    int rowSize = w * header->bits / 8;
    bool bInverted = ((header->descriptor & (1 << 5)) != 0);
    uint32_t wrote = 0;

    //skip header
    uint32_t startOffset = sizeof(*header) + header->identsize;
    char* dest = outFile->data();
    dest += startOffset;

    for (unsigned int i = yR; i < yR+hR; i++)
    {
        uint32_t row = bInverted ? (h-i-1) : i;
        char * destRow = dest + row * rowSize + xR * (header->bits/8);
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

int GetTGAHeader(boost::iostreams::mapped_file_source* inFile, TGA_HEADER* outHeader) {
    memcpy(outHeader, inFile->data(), sizeof(*outHeader));
    return sizeof(*outHeader);
}

char * LoadTGA( const char * szFileName, int * width, int * height, int * bpp )
{
    
    FILE * f = fopen(szFileName, "rb");
	
	if (f == NULL)
        return NULL;

    TGA_HEADER header;
    fread( &header, sizeof(header), 1, f );

    fseek( f, 0, SEEK_END );
    int fileLen = ftell( f );
    fseek( f, sizeof( header ) + header.identsize, SEEK_SET );

    if ( header.imagetype != IT_COMPRESSED && header.imagetype != IT_UNCOMPRESSED )
    {
        fclose( f );
        return NULL;
    }

    if ( header.bits != 24 && header.bits != 32 )
    {
        fclose( f );
        return NULL;
    }

    int bufferSize = fileLen - sizeof( header ) - header.identsize;
    char * pBuffer = new char[bufferSize];
    fread( pBuffer, 1, bufferSize, f );
    fclose( f );

    *width = header.width;
    *height = header.height;
    *bpp = header.bits;
    char * pOutBuffer = new char[ header.width * header.height * header.bits / 8 ];

    switch( header.imagetype )
    {
    case IT_UNCOMPRESSED:
        LoadUncompressedImage( pOutBuffer, pBuffer, &header );
        break;
    case IT_COMPRESSED:
        LoadCompressedImage( pOutBuffer, pBuffer, &header );
        break;
    }

    delete[] pBuffer;

    return pOutBuffer;
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
    //fread(pBuffer, 1, bufferSize, f);
    //fclose(f);
    memcpy(pBuffer, data + pos, bufferSize);

    *width = header.width;
    *height = header.height;
    *bpp = header.bits;
    char * pOutBuffer = new char[header.width * header.height * header.bits / 8];

    switch (header.imagetype)
    {
    case IT_UNCOMPRESSED:
        //LoadUncompressedImage(pOutBuffer, pBuffer, &header);
        LoadUncompressedImage(pOutBuffer, pBuffer, &header);
        break;
    case IT_COMPRESSED:
        LoadCompressedImage(pOutBuffer, pBuffer, &header);
        break;
    }

    delete[] pBuffer;

    return pOutBuffer;
}

void ToBWBlock(unsigned char*data, uint8_t bpp, uint32_t w, uint32_t h) {
    unsigned char* crtPix = data;
    unsigned int val;
    for (unsigned int i = 0; i < h; i++) {
        for (unsigned int j = 0; j < w; j++) {
            val = (crtPix[0] + crtPix[1] + crtPix[2]) / 3;
            if ( (val < 10) || (val > 250)) {
                //std::cout << __func__ << " -  " << i << ", " << i << ": " << crtPix[0] << "-" << crtPix[1]  << "-" << crtPix[2] << "->" << val << "\n";
                printf("%s - %d, %d: %d-%d-%d->%d\n", __func__, i, j, crtPix[0], crtPix[1], crtPix[2], val);
            }
            crtPix[0] = crtPix[1] = crtPix[2] = val;
            crtPix += (bpp / 8);
        }
    }
}

//char* TGATOBW()

EXPORT_DLL
char * ToBWFullFile(const char * fData, uint64_t size, uint64_t * newSize) {

    //FILE * f = fopen(szFileName, "rb");

    //if (f == NULL)
    //    return NULL;
    uint64_t pos = 0;

    TGA_HEADER header;
    //fread(&header, sizeof(header), 1, f);
    memcpy(&header, fData + pos, sizeof(header)); pos += sizeof(header);

    //fseek(f, 0, SEEK_END);
    int fileLen = size;
    //fseek(f, sizeof(header) + header.identsize, SEEK_SET);
    pos = sizeof(header) + header.identsize;

    if (header.imagetype != IT_COMPRESSED && header.imagetype != IT_UNCOMPRESSED)
    {
        //fclose(f);
        return NULL;
    }

    if (header.bits != 24 && header.bits != 32)
    {
        //fclose(f);
        return NULL;
    }

    int bufferSize = fileLen - sizeof(header) - header.identsize;
    char * pBuffer = new char[bufferSize];
    //fread(pBuffer, 1, bufferSize, f);
    //fclose(f);
    memcpy(pBuffer, fData + pos, bufferSize);

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

    //delete[] pBuffer;

    return pOutBuffer;


}
