#include "Logger.h"

#include <QFile>
#include <QDateTime>
#include <QDir>

QString levelStrings[LogLevel::COUNT];

QString Logger::fileName;
QFile* Logger::file = nullptr;
char Logger::stdBuffer[stdBufferSize];

void Logger::initLogging() {
    levelStrings[LogLevel::INFO] = "I";
    levelStrings[LogLevel::DEBUG] = "D";
    levelStrings[LogLevel::ERROR] = "E";
    levelStrings[LogLevel::COUNT] = "LOGGER_ERROR";

    if (!QDir("logs").exists())    {
        QDir().mkdir("logs");
    }

    QDateTime now = QDateTime::currentDateTime();
    fileName = "logs/log_" + now.toString("yyyy-MM-dd_hh-mm-ss-zzz") + ".txt";
    file = new QFile(fileName);
    file->open(QIODevice::WriteOnly);

    memset(stdBuffer, 0, sizeof(stdBuffer));
    setvbuf(stdout, stdBuffer, _IOFBF, sizeof(stdBuffer));
    setvbuf(stderr, stdBuffer, _IOFBF, sizeof(stdBuffer));
}

/**
 * Destructor, of sorts
 */
void Logger::finish() {
    if (file != nullptr) {
        file->write(stdBuffer);
        file->close();
    }
    fflush(stdout);
    fflush(stderr);
}

/**
 * Write the message to file, but flush everything from standard buffers first.
 */
void Logger::log(const QString &msg, const LogLevel lev) {
    file->write(stdBuffer);
    fflush(stdout);
    fflush(stderr);
    //printf("%s - %s\n", "level here", msg.toUtf8().constData());

    QDateTime now = QDateTime::currentDateTime();
    QString formatted = now.toString("yyyy-MM-dd hh:mm:ss.zzz") + " " + levelStrings[lev] + " " + msg + "\n";
    file->write(formatted.toUtf8());
}

void Logger::qDebugMessage(const QtMsgType type, const QMessageLogContext& context, const QString& message)
{
    QString levelStr;
    switch (type) {
    case QtDebugMsg:
        levelStr = "Debug";
        break;
    case QtInfoMsg:
        levelStr = "Info";
        break;
    case QtWarningMsg:
        levelStr = "Warning";
        break;
    case QtCriticalMsg:
        levelStr = "Critical";
        break;
    case QtFatalMsg:
        levelStr = "Fatal";
        break;
    }

    QString contextInfo = QString("%1: %2 %3").arg(context.file).arg(context.line).arg(context.function);

    log(levelStr + " " + contextInfo + ". " + message, LogLevel::DEBUG);
}

