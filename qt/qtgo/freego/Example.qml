import QtQuick 2.0


Rectangle {
    width: 400
    height: 400

    color: "#000000"

    Rectangle {
        height: 300
        anchors.left: parent.left; anchors.right: parent.right
        anchors.leftMargin: 30; anchors.rightMargin: 30
        anchors.bottom: parent.bottom
        anchors.bottomMargin: 30
        color: "#dd0000"

        Text {
            anchors.centerIn: parent
            font.pixelSize: 30
            text: "Quit"
            color: "#e0e0e0"
        }

        MouseArea {
            anchors.fill: parent
            onClicked: Qt.quit();
        }
    }
}
