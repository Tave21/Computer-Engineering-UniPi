#ifndef SERVER_HPP
#define SERVER_HPP

#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include <arpa/inet.h>

#include <openssl/rand.h>
#include <openssl/aes.h>

#include <iostream>
#include "../Filemanager/FileManager.hpp"


class Server{
    public:

        Server(int port, int backlog);
        ~Server();
        int getPort();
        int getBacklog();
        sockaddr_in getAddress();
        void handle_connections();
        void handle_client_disconnection(int);
        void handle_command(string* , int );
        
    private:
        fd_set read_fds;
        fd_set master;
        int fdmax;
        int port;
        int sock_fd;
        int backlog;
        unsigned char* receive_buffer;
        uint32_t receive_msg_len;
        sockaddr_in address;

        void handle_socket();
        void setup();
        void send_message(int currentSocket, const void *msg, const uint32_t len);
        int receive_message(int i);
        void send_int(int currentSocket, const uint32_t msg);
        int receive_int(int i);
            
};

#endif