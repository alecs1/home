LOCAL_PATH := $(call my-dir)

include $(CLEAR_VARS)

LOCAL_MODULE := distr-ip
LOCAL_SRC_FILES := ../main.cpp ../TGA.cpp ../entire-file-api.cpp
LOCAL_CPPFLAGS := -v -g -std=c++11 -Wall -pthread -fexceptions -DANDROID
LOCAL_LDLIBS := -L$(LOCAL_PATH)/../boost-android/stage/lib/android/arm/

#this is probably very very wrong; since these are static libraries, they have to appear in a certain order, etc etc etc
LOCAL_LDLIBS += -llog -lboost_system-gcc-mt-sd-1_55 -lboost_thread_pthread-gcc-mt-sd-1_55 -lboost_filesystem-gcc-mt-sd-1_55 -lboost_iostreams-gcc-mt-sd-1_55
LOCAL_LDLIBS += -lboost_atomic-gcc-mt-sd-1_55

#LOCAL_STATIC_LIBRARIES := libboost_system-gcc-mt-sd-1_55

LOCAL_WHOLE_STATIC_LIBRARIES := libgnustl_static #looks like another linker abomination

LOCAL_C_INCLUDES += $(LOCAL_PATH)/../boost-android/

include $(BUILD_EXECUTABLE)    # <-- Use this to build an executable.