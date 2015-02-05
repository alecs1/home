# copy needed directory structures

function(FAndroidSpecificSetup source)
    file(COPY ${SOURCE}/qtgo DESTINATION .)
    file(COPY ${SOURCE}/gnugo DESTINATION gnugo_include FILES_MATCHING PATTERN "*.h")
    file(COPY ${ANDROID_TEMPLATE}/ DESTINATION qtgo/${BUILD_APK_TARGET_DIR})
    file(COPY ${SOURCE}/qtgo/AndroidManifest.xml DESTINATION qtgo/${BUILD_APK_TARGET_DIR})
    message("FAndroidSpecificSetup ran")
endfunction(FAndroidSpecificSetup)

FAndroidSpecificSetup(${SOURCE} ${ANDROID_TEMPLATE} ${BUILD_APK_TARGET_DIR})
