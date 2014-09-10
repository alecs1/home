#pragma once

#ifdef WIN32
#define EXPORT_DLL __declspec(dllexport)
#else
#define EXPORT_DLL
#endif

EXPORT_DLL
char * LoadTGA(const char * szFileName, int * width, int * height, int * bpp);
