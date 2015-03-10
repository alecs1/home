import QtQuick 2.0


Item {
    Image {
        id: playerColour
        source: "qrc:/resources/cursorWhite.svg"
        anchors { fill:parent }
        fillMode: Image.PreserveAspectFit

        SequentialAnimation {
            id: sequentialAnimation
            onRunningChanged: {
                console.log('Started animating sequential: ', sequentialAnimation)
            }
            running: true
            loops: Animation.Infinite
            //some test stuff
            NumberAnimation {
                id: numberAnimation
                target: playerColour
                properties: 'y'
                duration: 1000
                easing {
                    type: Easing.OutBack
                    overshoot: 50
                }
                onRunningChanged: {
                    console.log('Started animating number: ', numberAnimation)
                }
            }
            RotationAnimation {
                id: rotationAnimation
                target: playerColour
                duration: 5000
                direction: RotationAnimation.Clockwise
                from: 0
                to: 360
                onRunningChanged: {
                    console.log('Started animating rotation: ', rotationAnimation)
                }
            }
        }
    }
}

