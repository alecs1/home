# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.0

#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:

# Remove some rules from gmake that .SUFFIXES does not remove.
SUFFIXES =

.SUFFIXES: .hpux_make_needs_suffix_list

# Suppress display of executed commands.
$(VERBOSE).SILENT:

# A target that is always out of date.
cmake_force:
.PHONY : cmake_force

#=============================================================================
# Set environment variables for the build.

# The shell in which to execute make rules.
SHELL = /bin/sh

# The CMake executable.
CMAKE_COMMAND = /usr/bin/cmake

# The command to remove a file.
RM = /usr/bin/cmake -E remove -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /home/alex/github/home/qt/qtgo/gnugo

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /home/alex/github/home/qt/qtgo/gnugo

# Include any dependencies generated for this target.
include utils/CMakeFiles/utils.dir/depend.make

# Include the progress variables for this target.
include utils/CMakeFiles/utils.dir/progress.make

# Include the compile flags for this target's objects.
include utils/CMakeFiles/utils.dir/flags.make

utils/CMakeFiles/utils.dir/getopt.o: utils/CMakeFiles/utils.dir/flags.make
utils/CMakeFiles/utils.dir/getopt.o: utils/getopt.c
	$(CMAKE_COMMAND) -E cmake_progress_report /home/alex/github/home/qt/qtgo/gnugo/CMakeFiles $(CMAKE_PROGRESS_1)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building C object utils/CMakeFiles/utils.dir/getopt.o"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -o CMakeFiles/utils.dir/getopt.o   -c /home/alex/github/home/qt/qtgo/gnugo/utils/getopt.c

utils/CMakeFiles/utils.dir/getopt.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/utils.dir/getopt.i"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -E /home/alex/github/home/qt/qtgo/gnugo/utils/getopt.c > CMakeFiles/utils.dir/getopt.i

utils/CMakeFiles/utils.dir/getopt.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/utils.dir/getopt.s"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -S /home/alex/github/home/qt/qtgo/gnugo/utils/getopt.c -o CMakeFiles/utils.dir/getopt.s

utils/CMakeFiles/utils.dir/getopt.o.requires:
.PHONY : utils/CMakeFiles/utils.dir/getopt.o.requires

utils/CMakeFiles/utils.dir/getopt.o.provides: utils/CMakeFiles/utils.dir/getopt.o.requires
	$(MAKE) -f utils/CMakeFiles/utils.dir/build.make utils/CMakeFiles/utils.dir/getopt.o.provides.build
.PHONY : utils/CMakeFiles/utils.dir/getopt.o.provides

utils/CMakeFiles/utils.dir/getopt.o.provides.build: utils/CMakeFiles/utils.dir/getopt.o

utils/CMakeFiles/utils.dir/getopt1.o: utils/CMakeFiles/utils.dir/flags.make
utils/CMakeFiles/utils.dir/getopt1.o: utils/getopt1.c
	$(CMAKE_COMMAND) -E cmake_progress_report /home/alex/github/home/qt/qtgo/gnugo/CMakeFiles $(CMAKE_PROGRESS_2)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building C object utils/CMakeFiles/utils.dir/getopt1.o"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -o CMakeFiles/utils.dir/getopt1.o   -c /home/alex/github/home/qt/qtgo/gnugo/utils/getopt1.c

utils/CMakeFiles/utils.dir/getopt1.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/utils.dir/getopt1.i"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -E /home/alex/github/home/qt/qtgo/gnugo/utils/getopt1.c > CMakeFiles/utils.dir/getopt1.i

utils/CMakeFiles/utils.dir/getopt1.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/utils.dir/getopt1.s"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -S /home/alex/github/home/qt/qtgo/gnugo/utils/getopt1.c -o CMakeFiles/utils.dir/getopt1.s

utils/CMakeFiles/utils.dir/getopt1.o.requires:
.PHONY : utils/CMakeFiles/utils.dir/getopt1.o.requires

utils/CMakeFiles/utils.dir/getopt1.o.provides: utils/CMakeFiles/utils.dir/getopt1.o.requires
	$(MAKE) -f utils/CMakeFiles/utils.dir/build.make utils/CMakeFiles/utils.dir/getopt1.o.provides.build
.PHONY : utils/CMakeFiles/utils.dir/getopt1.o.provides

utils/CMakeFiles/utils.dir/getopt1.o.provides.build: utils/CMakeFiles/utils.dir/getopt1.o

utils/CMakeFiles/utils.dir/random.o: utils/CMakeFiles/utils.dir/flags.make
utils/CMakeFiles/utils.dir/random.o: utils/random.c
	$(CMAKE_COMMAND) -E cmake_progress_report /home/alex/github/home/qt/qtgo/gnugo/CMakeFiles $(CMAKE_PROGRESS_3)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building C object utils/CMakeFiles/utils.dir/random.o"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -o CMakeFiles/utils.dir/random.o   -c /home/alex/github/home/qt/qtgo/gnugo/utils/random.c

utils/CMakeFiles/utils.dir/random.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/utils.dir/random.i"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -E /home/alex/github/home/qt/qtgo/gnugo/utils/random.c > CMakeFiles/utils.dir/random.i

utils/CMakeFiles/utils.dir/random.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/utils.dir/random.s"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -S /home/alex/github/home/qt/qtgo/gnugo/utils/random.c -o CMakeFiles/utils.dir/random.s

utils/CMakeFiles/utils.dir/random.o.requires:
.PHONY : utils/CMakeFiles/utils.dir/random.o.requires

utils/CMakeFiles/utils.dir/random.o.provides: utils/CMakeFiles/utils.dir/random.o.requires
	$(MAKE) -f utils/CMakeFiles/utils.dir/build.make utils/CMakeFiles/utils.dir/random.o.provides.build
.PHONY : utils/CMakeFiles/utils.dir/random.o.provides

utils/CMakeFiles/utils.dir/random.o.provides.build: utils/CMakeFiles/utils.dir/random.o

utils/CMakeFiles/utils.dir/gg_utils.o: utils/CMakeFiles/utils.dir/flags.make
utils/CMakeFiles/utils.dir/gg_utils.o: utils/gg_utils.c
	$(CMAKE_COMMAND) -E cmake_progress_report /home/alex/github/home/qt/qtgo/gnugo/CMakeFiles $(CMAKE_PROGRESS_4)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building C object utils/CMakeFiles/utils.dir/gg_utils.o"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -o CMakeFiles/utils.dir/gg_utils.o   -c /home/alex/github/home/qt/qtgo/gnugo/utils/gg_utils.c

utils/CMakeFiles/utils.dir/gg_utils.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/utils.dir/gg_utils.i"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -E /home/alex/github/home/qt/qtgo/gnugo/utils/gg_utils.c > CMakeFiles/utils.dir/gg_utils.i

utils/CMakeFiles/utils.dir/gg_utils.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/utils.dir/gg_utils.s"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -S /home/alex/github/home/qt/qtgo/gnugo/utils/gg_utils.c -o CMakeFiles/utils.dir/gg_utils.s

utils/CMakeFiles/utils.dir/gg_utils.o.requires:
.PHONY : utils/CMakeFiles/utils.dir/gg_utils.o.requires

utils/CMakeFiles/utils.dir/gg_utils.o.provides: utils/CMakeFiles/utils.dir/gg_utils.o.requires
	$(MAKE) -f utils/CMakeFiles/utils.dir/build.make utils/CMakeFiles/utils.dir/gg_utils.o.provides.build
.PHONY : utils/CMakeFiles/utils.dir/gg_utils.o.provides

utils/CMakeFiles/utils.dir/gg_utils.o.provides.build: utils/CMakeFiles/utils.dir/gg_utils.o

utils/CMakeFiles/utils.dir/winsocket.o: utils/CMakeFiles/utils.dir/flags.make
utils/CMakeFiles/utils.dir/winsocket.o: utils/winsocket.c
	$(CMAKE_COMMAND) -E cmake_progress_report /home/alex/github/home/qt/qtgo/gnugo/CMakeFiles $(CMAKE_PROGRESS_5)
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Building C object utils/CMakeFiles/utils.dir/winsocket.o"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -o CMakeFiles/utils.dir/winsocket.o   -c /home/alex/github/home/qt/qtgo/gnugo/utils/winsocket.c

utils/CMakeFiles/utils.dir/winsocket.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing C source to CMakeFiles/utils.dir/winsocket.i"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -E /home/alex/github/home/qt/qtgo/gnugo/utils/winsocket.c > CMakeFiles/utils.dir/winsocket.i

utils/CMakeFiles/utils.dir/winsocket.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling C source to assembly CMakeFiles/utils.dir/winsocket.s"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && /usr/bin/cc  $(C_DEFINES) $(C_FLAGS) -S /home/alex/github/home/qt/qtgo/gnugo/utils/winsocket.c -o CMakeFiles/utils.dir/winsocket.s

utils/CMakeFiles/utils.dir/winsocket.o.requires:
.PHONY : utils/CMakeFiles/utils.dir/winsocket.o.requires

utils/CMakeFiles/utils.dir/winsocket.o.provides: utils/CMakeFiles/utils.dir/winsocket.o.requires
	$(MAKE) -f utils/CMakeFiles/utils.dir/build.make utils/CMakeFiles/utils.dir/winsocket.o.provides.build
.PHONY : utils/CMakeFiles/utils.dir/winsocket.o.provides

utils/CMakeFiles/utils.dir/winsocket.o.provides.build: utils/CMakeFiles/utils.dir/winsocket.o

# Object files for target utils
utils_OBJECTS = \
"CMakeFiles/utils.dir/getopt.o" \
"CMakeFiles/utils.dir/getopt1.o" \
"CMakeFiles/utils.dir/random.o" \
"CMakeFiles/utils.dir/gg_utils.o" \
"CMakeFiles/utils.dir/winsocket.o"

# External object files for target utils
utils_EXTERNAL_OBJECTS =

utils/libutils.a: utils/CMakeFiles/utils.dir/getopt.o
utils/libutils.a: utils/CMakeFiles/utils.dir/getopt1.o
utils/libutils.a: utils/CMakeFiles/utils.dir/random.o
utils/libutils.a: utils/CMakeFiles/utils.dir/gg_utils.o
utils/libutils.a: utils/CMakeFiles/utils.dir/winsocket.o
utils/libutils.a: utils/CMakeFiles/utils.dir/build.make
utils/libutils.a: utils/CMakeFiles/utils.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --red --bold "Linking C static library libutils.a"
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && $(CMAKE_COMMAND) -P CMakeFiles/utils.dir/cmake_clean_target.cmake
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && $(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/utils.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
utils/CMakeFiles/utils.dir/build: utils/libutils.a
.PHONY : utils/CMakeFiles/utils.dir/build

utils/CMakeFiles/utils.dir/requires: utils/CMakeFiles/utils.dir/getopt.o.requires
utils/CMakeFiles/utils.dir/requires: utils/CMakeFiles/utils.dir/getopt1.o.requires
utils/CMakeFiles/utils.dir/requires: utils/CMakeFiles/utils.dir/random.o.requires
utils/CMakeFiles/utils.dir/requires: utils/CMakeFiles/utils.dir/gg_utils.o.requires
utils/CMakeFiles/utils.dir/requires: utils/CMakeFiles/utils.dir/winsocket.o.requires
.PHONY : utils/CMakeFiles/utils.dir/requires

utils/CMakeFiles/utils.dir/clean:
	cd /home/alex/github/home/qt/qtgo/gnugo/utils && $(CMAKE_COMMAND) -P CMakeFiles/utils.dir/cmake_clean.cmake
.PHONY : utils/CMakeFiles/utils.dir/clean

utils/CMakeFiles/utils.dir/depend:
	cd /home/alex/github/home/qt/qtgo/gnugo && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /home/alex/github/home/qt/qtgo/gnugo /home/alex/github/home/qt/qtgo/gnugo/utils /home/alex/github/home/qt/qtgo/gnugo /home/alex/github/home/qt/qtgo/gnugo/utils /home/alex/github/home/qt/qtgo/gnugo/utils/CMakeFiles/utils.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : utils/CMakeFiles/utils.dir/depend

