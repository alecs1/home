# copy needed directory structures: qtgo/qtgo, qtgo/gnugo,

function(FAndroidSpecificSetup source)
    file(COPY ${SOURCE}/qtgo/ DESTINATION FreeGo)
    file(COPY ${SOURCE}/gnugo DESTINATION gnugo_include FILES_MATCHING PATTERN "*.h")
    file(COPY ${ANDROID_TEMPLATE}/ DESTINATION FreeGo/${BUILD_APK_TARGET_DIR})
    #file(COPY ${SOURCE}/qtgo/res DESTINATION FreeGo/${BUILD_APK_TARGET_DIR})
    file(COPY ${SOURCE}/qtgo/AndroidManifest.xml DESTINATION FreeGo/${BUILD_APK_TARGET_DIR})
    message("FAndroidSpecificSetup ran")
endfunction(FAndroidSpecificSetup)

FAndroidSpecificSetup(${SOURCE} ${ANDROID_TEMPLATE} ${BUILD_APK_TARGET_DIR})
