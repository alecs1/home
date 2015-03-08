import QtQuick 2.0
import QtQuick.Controls 1.2

Item {

    //anchors {fill: parent}
    id: window

    TabView {
        id: tabView
        anchors {fill: parent}

        Tab {
            id: tabAbout
            title: "About FreeGo"

            Rectangle {
                id: shitItem
                anchors {fill: parent}
                Text {
                    anchors.top: parent.top
                    anchors.horizontalCenter: parent.horizontalCenter
                    horizontalAlignment: Text.AlignHCenter
                    id: textFreeGo
                    text: "FreeGo"
                    font.pixelSize: tabAbout.width/10
                    SequentialAnimation on font.letterSpacing {
                        loops: Animation.Infinite
                        NumberAnimation {
                            from: 0; to: textFreeGo.font.pixelSize*2; easing.type: Easing.InQuad; duration: 5000;
                            onRunningChanged: {
                                console.log('window.width=', window.width, ', tabView.width=', tabView.width, ', tabAbout.width=', tabAbout.width,
                                            ', shitItem.width=', shitItem.width, ', textFreeGo.width=', textFreeGo.width)
                            }
                        }

                        PauseAnimation { duration: 200 }
                        NumberAnimation { from: textFreeGo.font.pixelSize*2; to: 0; easing.type: Easing.OutQuad; duration: 5000 }
                        onRunningChanged: {
                            console.log('window.width=', window.width, ', tabView.width=', tabView.width, ', tabAbout.width=', tabAbout.width,
                                        ', shitItem.width=', shitItem.width, ', textFreeGo.width=', textFreeGo.width)
                        }
                        PauseAnimation { duration: 200 }
                    }
                }


                Text {
                    anchors.top: textFreeGo.bottom
                    anchors.horizontalCenter: parent.horizontalCenter
                    horizontalAlignment: Text.AlignHCenter
                    id: textDescription
                    text: "Go for Android and desktop."
                    font.pixelSize: tabAbout.width/20
                }
                function printThisShit() {
                    console.log('window=', window, ', tabView=', tabView, ', tabAbout=', tabAbout, ', shitItem=', shitItem, ', textFreeGo=', textFreeGo )
                    console.log('tabView.parent=', tabView.parent, ', textFreeGo.parent=', textFreeGo.parent)
                    console.log('window.width=', window.width, ', tabView.width=', tabView.width, ', tabAbout.width=', tabAbout.width,
                                ', shitItem.width=', shitItem.width, ', textFreeGo.width=', textFreeGo.width)
                }
                Component.onCompleted: {
                    printThisShit()
                    console.log("căcat")
                    console.debug("căcat 2")
                }
            }

        }

        Tab {
            title: "Components"
        }
        Tab {
            title: "License"
        }

    }

}
