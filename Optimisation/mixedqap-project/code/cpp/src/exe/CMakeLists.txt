cmake_minimum_required(VERSION 3.0)

PROJECT(mixedQAP)

SET(CMAKE_CXX_FLAGS "-O3 -std=c++11 -Wall")

INCLUDE_DIRECTORIES(../evaluation)
INCLUDE_DIRECTORIES(../algo)
INCLUDE_DIRECTORIES(../operator)

ADD_EXECUTABLE(t-solution t-solution.cpp)
ADD_EXECUTABLE(t-eval t-eval.cpp)
ADD_EXECUTABLE(randomSearch randomSearch.cpp)
ADD_EXECUTABLE(hillClimber hillClimbing.cpp)
ADD_EXECUTABLE(esSimple esSimple.cpp)
ADD_EXECUTABLE(iteratedLocalSearch iteratedLocalSearch.cpp)
