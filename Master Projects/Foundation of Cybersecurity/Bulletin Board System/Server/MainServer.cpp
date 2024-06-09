#include "Server.hpp"
using namespace std;


int main(int argc, char* argv[]){
    int port;
    int backlog;

    OpenSSL_add_all_algorithms();

    if(argc != 3) {
	    cerr << "Correct usage: ./serv PORT BACKLOG" << endl;
        return -1;
    }
    
    try {
        cout<<"SERVER STARTED"<<endl;
        
	    port      = atoi(argv[1]);
        backlog   = atoi(argv[2]);

	    Server server(port, backlog);
        while(1){
            server.handle_connections();
        }
    } 
    catch(invalid_argument& e) {
	    cerr << e.what() << endl;
	    return -1;
    }
    catch(runtime_error& e) {
	    cerr << e.what() << endl;
	    return -1;
    }
    catch(exception& e) {
        cerr << e.what() << endl;
        return -1;
    }   

    return 0;
}