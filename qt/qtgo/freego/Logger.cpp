#include "Logger.h"

#include <QFile>
#include <QDateTime>
#include <QDir>
#include <QPlainTextEdit>

QString levelStrings[Logger::COUNT+1];

QString Logger::fileName;
QFile* Logger::file = nullptr;
QPlainTextEdit* Logger::viewer = nullptr;
char Logger::stdoutBuffer[stdBufferSize];
char Logger::stderrBuffer[stdBufferSize];

void Logger::initLogging() {
    levelStrings[Logger::INFO] = "I";
    levelStrings[Logger::DBG] = "D";
    levelStrings[Logger::ERR] = "E";
    levelStrings[Logger::COUNT] = "LOGGER_ERROR";

    if (!QDir("logs").exists())    {
        QDir().mkdir("logs");
    }

    QDateTime now = QDateTime::currentDateTime();
    fileName = "logs/log_" + now.toString("yyyy-MM-dd_hh-mm-ss-zzz") + ".txt";
    file = new QFile(fileName);
    file->open(QIODevice::WriteOnly);

    fflush(stdout);
    fflush(stderr);
    memset(stdoutBuffer, 0, sizeof(stdoutBuffer));
    setvbuf(stdout, stdoutBuffer, _IOFBF, sizeof(stdoutBuffer));
    setvbuf(stderr, stderrBuffer, _IOFBF, sizeof(stderrBuffer));

    qInstallMessageHandler(logQDebug);
}

/**
 * Destructor, of sorts
 */
void Logger::finish() {
    log("Logger closing down.");
    if (file != nullptr) {
        file->write(stdoutBuffer);
        file->close();
    }
    fflush(stdout);
    fflush(stderr);
}

void Logger::setViewer(QPlainTextEdit* edit) {
    viewer = edit;
}

/**
 * Write the message to file, but flush everything from standard buffers first.
 */
void Logger::log(const QString &msg, const LogLevel lev) {
    file->write(stdoutBuffer);
    file->write(stderrBuffer);

    QDateTime now = QDateTime::currentDateTime();
    QString formatted = now.toString("yyyy-MM-dd hh:mm:ss.zzz") + " " + levelStrings[lev] + " " + msg;
    file->write(formatted.toUtf8());
    file->write("\n");

    if (viewer) {
        viewer->appendPlainText(formatted);
    }

    puts(formatted.toUtf8().constData());
    fflush(stdout);
    fflush(stderr);

    //stdBuffer[0] = 0;
    //QString dbg = QString("buffer after flush:%1%2%3%4%5\n").arg(stdBuffer[0]).arg(stdBuffer[1]).arg(stdBuffer[2]).arg(stdBuffer[3]).arg(stdBuffer[4]);
    //file->write(dbg.toUtf8().constData());
    //file->write("\n");
    //TODO: without this memset we're writing stuff from stdBuffer more than once. Part of the problem is related to the way FILE* buffer work.
    //However, doing "stdBuffer[0] = 0;" does not solve the problem entirely. Why is it not enough?!
    //memset(stdoutBuffer, 0, sizeof(stdoutBuffer));
}

void Logger::logQDebug(const QtMsgType type, const QMessageLogContext& context, const QString& message)
{
    QString levelStr;
    switch (type) {
    case QtDebugMsg:
        levelStr = "Debug";
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
    case QtInfoMsg:
        levelStr = "Info";
        break;
    }

    QString contextStr = QString("%1: %2 %3").arg(context.file).arg(context.line).arg(context.function);

    log(levelStr + " " + contextStr + ". " + message, Logger::DBG);
}

