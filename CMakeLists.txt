# -*- mode: CMake; coding: utf-8; -*-
CMAKE_MINIMUM_REQUIRED(VERSION 2.8.4)
PROJECT(sagittarius-dbd-db2)

SET(SAGITTARIUS_CONFIG  "sagittarius-config")

EXECUTE_PROCESS(
  COMMAND ${SAGITTARIUS_CONFIG} --pkglibdir
  OUTPUT_VARIABLE SAGITTARIUS_SITE_LIB_DIR
  OUTPUT_STRIP_TRAILING_WHITESPACE)

ADD_SUBDIRECTORY(src work/src)

# uninstall target
CONFIGURE_FILE(
  "${CMAKE_CURRENT_SOURCE_DIR}/cmake_uninstall.cmake.in"
  "${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake"
  IMMEDIATE @ONLY)

ADD_CUSTOM_TARGET(uninstall
    COMMAND ${CMAKE_COMMAND} -P ${CMAKE_CURRENT_BINARY_DIR}/cmake_uninstall.cmake)
