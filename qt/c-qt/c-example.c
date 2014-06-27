#include "qt_exports.h"

#include <stdio.h>

int main(int argc, char* argv[]) {
    int retVal = -1;


    QTContainer* app = C_QApplication(&argc, argv);

    QTContainer* button = C_QPushButton("Close", NULL);
    C_QWidget_Show(button);

    retVal = C_QApplication_Exec(app);

    printf("Returning with retVal=%d\n", retVal);
    return retVal;
}
