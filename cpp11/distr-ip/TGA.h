#pragma once

#include <stdint.h>
#include <stdio.h>

#ifdef WIN32
#define EXPORT_DLL __declspec(dllexport)
#else
#define EXPORT_DLL
#endif

#ifdef ANDROID
#include <android/log.h>
#endif

//from android/log.h, which I can't be bothered to find:
/**
typedef enum android_LogPriority {
    ANDROID_LOG_UNKNOWN = 0,
    ANDROID_LOG_DEFAULT, // only for SetMinPriority()
    ANDROID_LOG_VERBOSE,
    ANDROID_LOG_DEBUG,
    ANDROID_LOG_INFO,
    ANDROID_LOG_WARN,
    ANDROID_LOG_ERROR,
    ANDROID_LOG_FATAL,
    ANDROID_LOG_SILENT, // only for SetMinPriority(); must be last
} android_LogPriority;
int __android_log_print(int prio, const char *tag, const char *fmt, ...);
*/

//A printf like function that works on the Android debug path too.
//Since in the current project we don't have log.h and other stuff, keep it simple and do this:
//$ adb shell stop
//$ adb shell setprop log.redirect-stdio true
//$ adb shell start
int logi(const char* format, ...);

#ifdef ANDROID
#define LOGI(...) ((void)__android_log_print(ANDROID_LOG_WARN, "DistrIpWorkerClient", __VA_ARGS__))
#else
#define LOGI(...) logi(__VA_ARGS__)
#endif


#pragma pack(push,x1)					// Byte alignment (8-bit)
#pragma pack(1)

typedef struct
{
    unsigned char  identsize;			// size of ID field that follows 18 byte header (0 usually)
    unsigned char  colourmaptype;		// type of colour map 0=none, 1=has palette
    unsigned char  imagetype;			// type of image 2=rgb uncompressed, 10 - rgb rle compressed

    short colourmapstart;				// first colour map entry in palette
    short colourmaplength;				// number of colours in palette
    unsigned char  colourmapbits;		// number of bits per palette entry 15,16,24,32

    short xstart;						// image x origin
    short ystart;						// image y origin
    short width;						// image width in pixels
    short height;						// image height in pixels
    unsigned char  bits;				// image bits per pixel 24,32
    unsigned char  descriptor;			// image descriptor bits (vh flip bits)

    // pixel data follows header

} TGA_HEADER;

#pragma pack(pop,x1)

namespace boost {
    namespace iostreams {
        class mapped_file_source;
        class mapped_file_sink;
    }
}

EXPORT_DLL
char * LoadTGA(const char * szFileName, int * width, int * height, int * bpp);

EXPORT_DLL
void ToBWBlock(unsigned char*data, uint8_t bpp, uint32_t w, uint32_t h);

EXPORT_DLL
uint64_t getRectFromFile(boost::iostreams::mapped_file_source* inFile, TGA_HEADER* header, uint32_t x, uint32_t y, uint32_t w, uint32_t h, char* dest);

EXPORT_DLL
uint32_t writeRectToFile(boost::iostreams::mapped_file_sink* outFile, TGA_HEADER* header, uint32_t x, uint32_t y, uint32_t w, uint32_t h, char* src);

EXPORT_DLL
int GetTGAHeader(boost::iostreams::mapped_file_source* inFile, TGA_HEADER* outHeader);

//internal to the lib:
void LoadCompressedImage( char* pDest, char * pSrc, TGA_HEADER * pHeader );
void LoadUncompressedImage( char* pDest, char * pSrc, TGA_HEADER * pHeader );

//Deprecated or not fully implemented functions.
EXPORT_DLL
char * LoadTGAFromMem(const char * data, uint64_t size, int * width, int * height, int * bpp);

//processes file directly
EXPORT_DLL
char * ToBWFullFile(const char * fData, uint64_t size, uint64_t * newSize);

EXPORT_DLL
uint64_t getRectFromMem(unsigned char* inFile, TGA_HEADER* header, uint32_t xR, uint32_t yR, uint32_t wR, uint32_t hR, unsigned char* dest);

EXPORT_DLL
uint32_t writeRectToMem(unsigned char* outFile, TGA_HEADER* header, uint32_t xR, uint32_t yR, uint32_t wR, uint32_t hR, unsigned char* src);
