#include <cstring>
#include <iostream>

//#include "stdafx.h"
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

//void LoadUncompressedImage(char* pDest, char * pSrc, TGA_HEADER * pHeader)
uint32_t getRectFromFile(boost::iostreams::mapped_file_source* inFile, TGA_HEADER* header, uint32_t xR, uint32_t yR, uint32_t wR, uint32_t hR, char* dest)
{
    int w = header->width;
    int h = header->height;
    int rowSize = w * header->bits / 8;
    bool bInverted = ((header->descriptor & (1 << 5)) != 0);

    //skip header

    for (int i = yR; i < yR+hR; i++)
    {
        char * pSrcRow = pSrc +
            (bInverted ? (h - i - 1) * rowSize : i * rowSize);
        if (header->bits == 24)
        {
            for (int j = 0; j < w; j++)
            {
                *pDest++ = pSrcRow[2];
                *pDest++ = pSrcRow[1];
                *pDest++ = pSrcRow[0];
                pSrcRow += 3;
            }
        }
        else
        {
            for (int j = 0; j < w; j++)
            {
                *pDest++ = pSrcRow[2];
                *pDest++ = pSrcRow[1];
                *pDest++ = pSrcRow[0];
                *pDest++ = pSrcRow[3];
                pSrcRow += 4;
            }
        }
    }
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

    //FILE * f = fopen(szFileName, "rb");

    //if (f == NULL)
    //    return NULL;
    uint64_t pos = 0;

    TGA_HEADER header;
    //fread(&header, sizeof(header), 1, f);
    memcpy(&header, data + pos, sizeof(header)); pos += sizeof(header);

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

/*
TGADef* ToBW(TGADef& img) {
    std::cout << __func__ << " - implement\n";

    return new TGADef(0, 0, 0, NULL);
}
*/

EXPORT_DLL
char * ToBW(const char * fData, uint64_t size, uint64_t * newSize) {

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