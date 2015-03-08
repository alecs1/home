import QtQuick 2.0
import QtQuick.Controls 1.2

Item {
    anchors {fill: parent}
    id: window

    TabView {
        anchors {fill: parent}

        Tab {
            title: "About FreeGo"

        }
        Tab {
            title: "Components"
        }
        Tab {
            title: "License"
        }
    }
}
