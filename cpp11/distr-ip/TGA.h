#pragma once

#include <stdint.h>
#include <stdio.h>

#ifdef WIN32
#define EXPORT_DLL __declspec(dllexport)
#else
#define EXPORT_DLL
#endif

EXPORT_DLL
char * LoadTGA(const char * szFileName, int * width, int * height, int * bpp);

EXPORT_DLL
char * LoadTGAFromMem(const char * data, uint64_t size, int * width, int * height, int * bpp);

//processes file directly
EXPORT_DLL
char * ToBW(const char * fData, uint64_t size, uint64_t * newSize);


/*
struct TGADef {
    int w;
    int h;
    int bpp;
    char* data;
    //aData is assumed to be allocated with new, TGADef will own the pointer after construction.
    TGADef(int aW, int aH, int aBpp, char* aData):
        w(aW),
        h(aH),
        data(aData)
    { }
    ~TGADef() {
        printf("~TGADef - free %p\n", data);
        delete[] data;
    }
};

EXPORT_DLL
TGADef* ToBW(TGADef& img);
*/
