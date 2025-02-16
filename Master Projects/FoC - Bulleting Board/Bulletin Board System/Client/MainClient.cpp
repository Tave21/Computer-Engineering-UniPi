#include "Client.hpp"

#include <openssl/err.h>
#include <openssl/ssl.h>
#include <openssl/rand.h>
#include <openssl/pem.h>
#include <openssl/evp.h>


void startingMenu(Client &);
void loggedMenu(Client &);

int main(int argc, char* argv[]) {

    OpenSSL_add_all_algorithms();
    
    if(argc != 2){
        cerr<<"Correct usage: ./client SERVER_PORT"<<endl;
        return -1;
    }

    string server_ip = "127.0.0.1";
    int server_port = atoi(argv[1]); //port taken as argument
    Client client(server_ip, server_port); //connection to server

    try{
        while(true){
            if(client.getLogged() == true){
                loggedMenu(client);
            }else{
                startingMenu(client);
            }
        }
            
    }catch(const runtime_error& ex) {
        cerr << ex.what() << endl;
        return -1;
    }
    
    return 0;
}

void startingMenu(Client &client){
    int choice = 0;
    string c;
    bool control = false;

   while (!control){
        cout << "BULLETIN BOARD SYSTEM" << endl<<endl;
        cout << "1. Log In" << endl;
        cout << "2. Registration" << endl;
        cout << "3. Exit"<< endl;
        cout << "Select an option "<<endl<<endl;
        cin >> c;

        try {
            choice = stoi(c);
        } catch (invalid_argument const& e) {
            cerr << "Wrong input value, try again"<<endl<<endl;
            break;
        }        

        switch (choice) {
            case 1:
                client.logIn();
                control = true;
                break;
            case 2:
                client.Registration(); 
                control = true;
                break;
            case 3:
                cout<<"Closing the program..."<<endl;
                client.exitFunction();
                exit(0);

            default:
                cout << "Invalid option, try again" <<endl<<endl;
                break;
        }
    }  
}

void loggedMenu(Client &client){
    string c;
    bool control = false;
    int choice = 0;

     while (!control){

        cout << "Operations:" << endl;
        cout << "1. View list of available messages" << endl;
        cout << "2. Add a message" << endl;
        cout << "3. Log Out" << endl;
        cout << "Select an option "<<endl<<endl;
        cin >> c;

        try {
            choice = stoi(c);
        } catch (invalid_argument const& e) {
            cerr << "Wrong input value, try again"<<endl<<endl;
            break;
        }

        switch (choice) {
            case 1:
                client.viewMessages(); 
                break;
            case 2:
                client.addMessage(); 
                break;
            case 3:
                client.logOut();
                control = true;
                client.setLogged(false);
                break;
            default:
                cout << "Invalid option, try again" <<endl<<endl;
                break;
        }
    }
}