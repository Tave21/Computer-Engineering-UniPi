#ifndef CLIENT_HPP
#define CLIENT_HPP

#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include <arpa/inet.h>
#include "errno.h"
#include <termios.h>

#include "../Util/utilHeader.hpp"

class Client{
    private: 
        string nickname;
        unsigned char* recv_buffer;
        
        int sock_fd;
        sockaddr_in server_addr;

        bool logged; //status of login


    public:
        bool authentication();
        bool authentication(string nickname); 
        string getNickname(); 

        //status
        bool getLogged();
        void setLogged(bool);

        //operations
        void viewMessages();
        void addMessage();
        void logIn();
        void Registration();
        void logOut();

        //socket operations
        void send_to_server(void* buffer, uint32_t len);
        void recv_from_server(uint32_t &len);
        void send_int_to_server(uint32_t msg);
        int recv_int_from_server(uint32_t &rcv_msg);
        void exitFunction();
        Client(string, int);
        ~Client();
};

#endif