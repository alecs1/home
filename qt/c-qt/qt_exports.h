#ifndef QT_EXPORTS_H
#define QT_EXPORTS_H

#ifdef __cplusplus
#define EXTERN_C extern "C"
#else
#define EXTERN_C
#endif

typedef enum QTType {
    EC_QPushButton,
    EC_QApplication,
} QTType;

/*EXTERN_C*/ typedef struct QTContainer {
    void* qtObj;
    QTType type;
} QTContainer;

EXTERN_C QTContainer* C_QPushButton(char* text, QTContainer* parent);

EXTERN_C void C_QWidget_Show(QTContainer* widget);

EXTERN_C QTContainer* C_QApplication(int* argc, char* argv[]);
EXTERN_C int C_QApplication_Exec(QTContainer* app);

//this is really tricky now, since it already depends on the preprocessor; skip for the moment
EXTERN_C int c_connect(QTContainer* src, void* signal, QTContainer* dest, void* slot);

#endif

