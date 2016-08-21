#pragma once

#include <QString>

enum LogLevel:uint8_t {
    LOG_DEBUG,
    LOG_INFO,
    LOG_ERROR
};

struct Logger {
    static void Log(const QString& msg, const LogLevel lev=LogLevel::LOG_INFO);
};
