#SET (CMAKE_CXX_FLAGS "${CMAKE_CXX_FLAGS} -std=c++11")
#SET (CMAKE_EXE_LINKER_FLAGS "${CMAKE_EXE_LINKER_FLAGS} -lboost_system")

SET (CMAKE_VERBOSE_MAKEFILE ON)


add_executable(asio-timer asio-timer.cpp)
target_link_libraries(asio-timer boost_system)

add_executable(asio-bind asio-bind.cpp)
target_link_libraries(asio-bind boost_system)

add_executable(asio-bind-this asio-bind-this.cpp)
target_link_libraries(asio-bind-this boost_system)

add_executable(bind bind.cpp)
target_link_libraries(bind boost_system)

add_executable(thread thread.cpp)
target_link_libraries(thread boost_system boost_thread)

add_executable(sync-tcp-dt-client sync-tcp-dt-client.cpp)
target_link_libraries(sync-tcp-dt-client boost_system)

add_executable(sync-tcp-dt-server sync-tcp-dt-server.cpp)
target_link_libraries(sync-tcp-dt-server boost_system)
