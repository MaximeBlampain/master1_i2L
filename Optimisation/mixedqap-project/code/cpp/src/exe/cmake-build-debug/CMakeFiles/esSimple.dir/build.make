# CMAKE generated file: DO NOT EDIT!
# Generated by "Unix Makefiles" Generator, CMake Version 3.17

# Delete rule output on recipe failure.
.DELETE_ON_ERROR:


#=============================================================================
# Special targets provided by cmake.

# Disable implicit rules so canonical targets will work.
.SUFFIXES:


# Disable VCS-based implicit rules.
% : %,v


# Disable VCS-based implicit rules.
% : RCS/%


# Disable VCS-based implicit rules.
% : RCS/%,v


# Disable VCS-based implicit rules.
% : SCCS/s.%


# Disable VCS-based implicit rules.
% : s.%


.SUFFIXES: .hpux_make_needs_suffix_list


# Command-line flag to silence nested $(MAKE).
$(VERBOSE)MAKESILENT = -s

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
CMAKE_COMMAND = /Applications/CLion.app/Contents/bin/cmake/mac/bin/cmake

# The command to remove a file.
RM = /Applications/CLion.app/Contents/bin/cmake/mac/bin/cmake -E rm -f

# Escaping for special characters.
EQUALS = =

# The top-level source directory on which CMake was run.
CMAKE_SOURCE_DIR = /Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe

# The top-level build directory on which CMake was run.
CMAKE_BINARY_DIR = /Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe/cmake-build-debug

# Include any dependencies generated for this target.
include CMakeFiles/esSimple.dir/depend.make

# Include the progress variables for this target.
include CMakeFiles/esSimple.dir/progress.make

# Include the compile flags for this target's objects.
include CMakeFiles/esSimple.dir/flags.make

CMakeFiles/esSimple.dir/esSimple.cpp.o: CMakeFiles/esSimple.dir/flags.make
CMakeFiles/esSimple.dir/esSimple.cpp.o: ../esSimple.cpp
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --progress-dir=/Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_1) "Building CXX object CMakeFiles/esSimple.dir/esSimple.cpp.o"
	/Library/Developer/CommandLineTools/usr/bin/c++  $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -o CMakeFiles/esSimple.dir/esSimple.cpp.o -c /Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe/esSimple.cpp

CMakeFiles/esSimple.dir/esSimple.cpp.i: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Preprocessing CXX source to CMakeFiles/esSimple.dir/esSimple.cpp.i"
	/Library/Developer/CommandLineTools/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -E /Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe/esSimple.cpp > CMakeFiles/esSimple.dir/esSimple.cpp.i

CMakeFiles/esSimple.dir/esSimple.cpp.s: cmake_force
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green "Compiling CXX source to assembly CMakeFiles/esSimple.dir/esSimple.cpp.s"
	/Library/Developer/CommandLineTools/usr/bin/c++ $(CXX_DEFINES) $(CXX_INCLUDES) $(CXX_FLAGS) -S /Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe/esSimple.cpp -o CMakeFiles/esSimple.dir/esSimple.cpp.s

# Object files for target esSimple
esSimple_OBJECTS = \
"CMakeFiles/esSimple.dir/esSimple.cpp.o"

# External object files for target esSimple
esSimple_EXTERNAL_OBJECTS =

esSimple: CMakeFiles/esSimple.dir/esSimple.cpp.o
esSimple: CMakeFiles/esSimple.dir/build.make
esSimple: CMakeFiles/esSimple.dir/link.txt
	@$(CMAKE_COMMAND) -E cmake_echo_color --switch=$(COLOR) --green --bold --progress-dir=/Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe/cmake-build-debug/CMakeFiles --progress-num=$(CMAKE_PROGRESS_2) "Linking CXX executable esSimple"
	$(CMAKE_COMMAND) -E cmake_link_script CMakeFiles/esSimple.dir/link.txt --verbose=$(VERBOSE)

# Rule to build all files generated by this target.
CMakeFiles/esSimple.dir/build: esSimple

.PHONY : CMakeFiles/esSimple.dir/build

CMakeFiles/esSimple.dir/clean:
	$(CMAKE_COMMAND) -P CMakeFiles/esSimple.dir/cmake_clean.cmake
.PHONY : CMakeFiles/esSimple.dir/clean

CMakeFiles/esSimple.dir/depend:
	cd /Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe/cmake-build-debug && $(CMAKE_COMMAND) -E cmake_depends "Unix Makefiles" /Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe /Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe /Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe/cmake-build-debug /Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe/cmake-build-debug /Users/nfourny/Documents/applications/mixedqap-project/code/cpp/src/exe/cmake-build-debug/CMakeFiles/esSimple.dir/DependInfo.cmake --color=$(COLOR)
.PHONY : CMakeFiles/esSimple.dir/depend

