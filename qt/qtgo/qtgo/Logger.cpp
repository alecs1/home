#include "Logger.h"

void Logger::Log(const QString &msg, const LogLevel lev) {
    printf("%s - %s\n", "level here", msg.toUtf8().constData());
}
