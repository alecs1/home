#pragma once

#include <QString>
#include <QPlainTextEdit>

class QFile;

enum LogLevel:uint8_t {
    DBG,
    INFO,
    ERR,
    COUNT
};


/**
 * @brief Simple non-threadsafe logger with all stuff static
 */
struct Logger {
    static const int stdBufferSize = 1000000;
    static char stdoutBuffer[stdBufferSize];
    static char stderrBuffer[stdBufferSize];
    static QFile* file;
    static QString fileName;

    static QPlainTextEdit* viewer;

    static void initLogging();
    static void finish();
    static void setViewer(QPlainTextEdit* edit);

    static void log(const QString& msg, const LogLevel lev=LogLevel::INFO);
    static void logQDebug(const QtMsgType type, const QMessageLogContext& context, const QString& message);
};
