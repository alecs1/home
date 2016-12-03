import QtQuick 2.0
import QtQuick.Controls 1.2

Item {

    id: window

    TabView {
        id: tabView
        anchors {fill: parent}

        Tab {
            id: tabAbout
            title: 'About FreeGo'

            Item {
                id: shitItem
                anchors {fill: parent}

                Image {
                    id: imageLogo
                    source: "qrc:/resources/ko-rule-185-orig.png"
                    anchors.top: parent.top
                    anchors.horizontalCenter: parent.horizontalCenter
                    fillMode: Image.PreserveAspectFit
                    width: textFreeGo.width/3
                }


                //Why the fuck do you change your center on animations?
                //if I don't createm and Item inside Tab these Text components will overlap
                //if I do create Item, then textFreeGo will look like changing the center, making a jerky and headache producing animation
                Text {
                    anchors.top: imageLogo.bottom
                    anchors.horizontalCenter: parent.horizontalCenter
                    horizontalAlignment: Text.AlignHCenter
                    id: textFreeGo
                    text: "FreeGo"
                    font.pixelSize: tabAbout.width/7
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
            id: tabComponents
            title: 'Components'
            Text {
                wrapMode: Text.Wrap
                textFormat: Text.RichText
                font.pixelSize: (parent.width + parent.height)/50
                text: '<b>GNU Go</b><br>The open source Go playing software, provides all the gameplay.<br>
                       <br><b>Qt</b><br> Qt widgets and Quick provide the visual interface on Linux, Windows and Android.<br>
                       <br><b>CMake</b><br> Make it very easy to build FreeGo everywhere.'
            }
        }
        Tab {
            id: tabLicense
            title: 'License'
            Text {
                wrapMode: Text.Wrap
                textFormat: Text.RichText
                font.pixelSize: (parent.width + parent.height)/50
                text: 'The license is GNU GPL v3.
                       <br>Souce code at: <a href=\"https://github.com/alecs1/home/tree/master/qt/qtgo/\">https://github.com/alecs1/home/tree/master/qt/qtgo/</a>'
                onLinkActivated: {
                    Qt.openUrlExternally(link)
                }
            }
        }

    }

}
