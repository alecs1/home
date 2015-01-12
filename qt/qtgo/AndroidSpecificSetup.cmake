function(FAndroidSpecificSetup source)
    file(COPY ${source}/qtgo DESTINATION .)
    file(COPY ${source}/gnugo DESTINATION gnugo_include FILES_MATCHING PATTERN "*.h")
    message("FAndroidSpecificSetup ran")
endfunction(FAndroidSpecificSetup)

FAndroidSpecificSetup(${SOURCE})
