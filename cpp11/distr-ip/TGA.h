#pragma once

#include <stdint.h>

#ifdef WIN32
#define EXPORT_DLL __declspec(dllexport)
#else
#define EXPORT_DLL
#endif

EXPORT_DLL
char * LoadTGA(const char * szFileName, int * width, int * height, int * bpp);

EXPORT_DLL
char * LoadTGAFromMem(const char * data, uint64_t size, int * width, int * height, int * bpp);
