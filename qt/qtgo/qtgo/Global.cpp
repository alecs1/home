#include <stdint.h>
#include "Global.h"

#include <QMainWindow>

PlatformType platformType() {
#if defined(Q_OS_ANDROID)
   return PlatformType::Android;
#else
   return PlatformType::LinuxDesktop;
#endif
}

