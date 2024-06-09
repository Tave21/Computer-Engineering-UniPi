#g++ -o serverApp Server/MainServer.cpp Server/Server.cpp Util/UtilFunctions.cpp Util/CryptoFunctions.cpp Packets/StartPacket.cpp Packets/GenericPacket.cpp Packets/AuthenticationPacket.cpp Filemanager/FileManager.cpp DTO/User.cpp DTO/Message.cpp -lssl -lcrypto
#g++ -o clientApp Client/MainClient.cpp Client/Client.cpp Util/UtilFunctions.cpp Util/CryptoFunctions.cpp Packets/StartPacket.cpp Packets/AuthenticationPacket.cpp Packets/GenericPacket.cpp -lssl -lcrypto

CXX = g++
CXXFLAGS = -std=c++17
LDLIBS = -lssl -lcrypto -Wno-pointer-arith

.PHONY: all clean

all: Client Server

Client: 
	$(CXX) $(CXXFLAGS) -o clientApp Client/MainClient.cpp Client/Client.cpp Util/UtilFunctions.cpp Util/CryptoFunctions.cpp Packets/StartPacket.cpp Packets/AuthenticationPacket.cpp Packets/GenericPacket.cpp $(LDLIBS)

Server: 
	$(CXX) $(CXXFLAGS) -o serverApp Server/MainServer.cpp Server/Server.cpp Util/UtilFunctions.cpp Util/CryptoFunctions.cpp Packets/StartPacket.cpp Packets/GenericPacket.cpp Packets/AuthenticationPacket.cpp Filemanager/FileManager.cpp DTO/User.cpp DTO/Message.cpp $(LDLIBS)

clean: 
	rm -f clientApp serverApp
