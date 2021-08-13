cmake_minimum_required(VERSION 3.16)
project(BlockchainObserver C)

set(CMAKE_C_STANDARD 11)

add_executable(BlockchainObserver main.c blockchainSock.c blockchainSock.h observer.c observer.h json.h blockchainTypes.c blockchainTypes.h jsonOut.c jsonOut.h json.c parson.h parson.c ABEYJson.c ABEYJson.h)


target_link_libraries(BlockchainObserver ws2_32.dll)

target_link_libraries(BlockchainObserver libpthread.a)

