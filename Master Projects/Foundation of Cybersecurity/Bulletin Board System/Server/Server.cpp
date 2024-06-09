#include "Server.hpp"

EVP_PKEY* privK = nullptr;
vector<uint8_t> symm_key_no_hashed;
vector<uint8_t> hmac_key_no_hashed;

size_t symm_key_no_hash_size;
size_t hmac_key_no_hash_size;
unsigned int symm_key_size;
unsigned int hmac_key_size;

//generate random intialization vector (IV)
vector<uint8_t> iv_file(AES_BLOCK_SIZE);
vector<uint8_t> key_file(KEY_SIZE);


/*
void reset_files() {

    string nome_file1 = "Users/giova/transactions.txt";
    string nome_file2 = "Users/giova/transactions_enc.txt";
    string nome_file3 = "Users/fede/transactions.txt";
    string nome_file4 = "Users/fede/transactions_enc.txt";

    // Apri e svuota il contenuto dei file
    ofstream file1(nome_file1);
    ofstream file2(nome_file2);
    ofstream file3(nome_file3);
    ofstream file4(nome_file4);

    if (file1.is_open() && file2.is_open() && file3.is_open() && file4.is_open()) {
        // Chiudi i file dopo averli svuotati
        file1.close();
        file2.close();
        file3.close();
        file4.close();
    } else {
        cerr << "Unable to reset files" << endl;
    }
}
*/

bool userExist(User user){
    if(user.getNickname() == ""){
        return false; //user not found, nickname not registered
    }
    return true;
}

//string getCurrentTimestamp();

Server::Server(int port, int backlog){
    receive_buffer = new unsigned char[MAX_MSG_SIZE];

    if(port < 49152 && port > 65535 )
        throw invalid_argument("Invalid port");

    if(backlog < 0)
        throw invalid_argument("Invalid backlog");
    this->port = port;
    this->backlog = backlog;

    RAND_bytes(iv_file.data(), iv_file.size());
    RAND_bytes(key_file.data(), KEY_SIZE);

    handle_socket();
    setup();
}

Server::~Server(){
    if (sock_fd != -1) 
        close(sock_fd);

    delete[] receive_buffer;
}

int Server::getPort(){
    return this->port;
}

int Server::getBacklog(){
    return this->backlog;
}

sockaddr_in Server::getAddress(){
    return this->address;
}

void Server::handle_socket(){
    // create socket
    sock_fd = socket(AF_INET, SOCK_STREAM, 0);

    if (sock_fd == -1) 
    {
        throw runtime_error("Failed in creating socket");
    }

    // bind socket
    memset(&address, 0, sizeof(address));
    address.sin_family = AF_INET;
    address.sin_addr.s_addr = INADDR_ANY;
    address.sin_port = htons(port);

    if (bind(sock_fd, (struct sockaddr*) &address, sizeof(address)) == -1) 
    {
        throw runtime_error("Failed in binding socket");
    }

    // listen 
    if (listen(sock_fd, backlog) == -1) 
    {
        throw runtime_error("Error in listen");
    }
}


void Server::handle_connections(){
    //reset_files();

    struct sockaddr_in cl_addr;   
    int len = sizeof(cl_addr);
    int new_sd = accept(sock_fd, (struct sockaddr*) &cl_addr, (socklen_t*) &len);

    while(1){

        receive_message(new_sd);

        if(strncmp((char*)receive_buffer, "Auth", strlen("Auth")) == 0){
            string buf[] = {"0", "Auth"};
            handle_command(buf, new_sd);
        }
        else if(strncmp((char*)receive_buffer, "FirstAuth", strlen("FirstAuth")) == 0){
            string buf[] = {"FirstAuth"};
            handle_command(buf, new_sd);
        }
        else if(strncmp((char*)receive_buffer, "Registration", strlen("Registration")) == 0){
            string buf[] = {"Registration"};
            handle_command(buf, new_sd);
        }
        else if(strncmp((char*)receive_buffer, "LOGOUT", strlen("LOGOUT")) == 0){
            cout<<"LOGOUT"<<endl;
        }
        else if(strncmp((char*)receive_buffer, "EXIT", strlen("EXIT")) == 0){
            cout<<"EXIT"<<endl;
            handle_client_disconnection(new_sd);
            break;
        }
        else if(strcmp((char*)receive_buffer, "") == 0){ //disconnection 
            cout<<"CLIENT DISCONNECTED"<<endl;
            handle_client_disconnection(new_sd);
            break;
        }
        else if(strncmp((char*)receive_buffer, "ERROR", strlen("ERROR")) == 0){ // client error
            cout<<"CLIENT DISCONNECTED"<<endl;
            handle_client_disconnection(new_sd);
            break;
        }
        else{
            GenericPacket gp; 
            ssize_t len = ntohl(receive_msg_len);
            vector<uint8_t> tmp(receive_buffer, receive_buffer+len);

            gp.deserialize(tmp);
            vector<uint8_t> plaintext;
            cbc_decrypt(gp.getCiphertext(), plaintext, gp.getIv(), symm_key_no_hashed);

            // HMAC
            vector<unsigned char> hmac = gp.getHMAC();
            if(!verifyHMAC(gp.getCiphertext().data(), gp.getCipherLen(), hmac, hmac_key_no_hashed)){
                cout << "Error in message verification" << endl;
            } else{
                string* deserializedMessage = deserializeMessage(plaintext);
                handle_command(deserializedMessage, new_sd);
            }
        }
    }
}


void Server::setup(){
    FD_ZERO(&master);
    FD_ZERO(&read_fds);

    FD_SET(sock_fd, &master); 
    FD_SET(0, &master);
    fdmax = sock_fd;
    //reset_files();
}

void Server::handle_client_disconnection(int sock){
    close(sock);
    FD_CLR(sock, &master);
}

void Server::handle_command(string* fields, int comm_sock){
    string cmd;
    int counter;
    if(fields[0] == "Registration"){
        cmd = "Registration";
    } else if(fields[0] == "FirstAuth"){
        cmd = "FirstAuth";
    } else {
        counter = stoi(fields[0]);
        cmd = fields[1];
    }

    if(cmd == "FirstAuth"){
        vector<uint8_t> symm_key;
        vector<uint8_t> hmac_key;

        receive_message(comm_sock);
        if(strcmp((char*)receive_buffer, "")==0){
            return;
        }

        StartPacket st;

        vector<uint8_t> tmp(receive_buffer, receive_buffer+strlen((char*)receive_buffer));
        st.deserialize(tmp, true); //deserialize the Startpacket recevied from the client, true because it's the registration

        EVP_PKEY* symmetric_param = generate_params(); //generate symmetric DH key
        EVP_PKEY* hmac_param = generate_params(); //generate HMAC DH key

                 //  b (private key)   Ya (received from client)
        generate_secrets(symmetric_param, st.getSymmEVP(), symm_key_no_hashed, symm_key_no_hash_size);
        //in symm_key_no_hashed there is the shared secret Kab for symmetric encryption
        generate_secrets(hmac_param, st.getHmacEVP(), hmac_key_no_hashed, hmac_key_no_hash_size);
        //in hmac_key_no_hashed there is the shared secret Kab' for HMAC

        //compute the hash of the shared secret for both symmetric and hmac, and store them in symm_key and hmac_key
        generateSHA(symm_key_no_hashed.data(), symm_key_no_hash_size, symm_key, symm_key_size);
        generateSHA(hmac_key_no_hashed.data(), hmac_key_no_hash_size, hmac_key, hmac_key_size);


        //Derive private key

        privK = read_server_private_key();

        vector<uint8_t> iv = generate_iv(); //generate initialization vector

        unsigned int len_sign;

        AuthenticationPacket ap(iv, symmetric_param, hmac_param); //create the Authentication packet

        vector<uint8_t> hashed = ap.serializeSign(symm_key, hmac_key); //serialize sign ( = symm_key + hmac_key)

        vector<uint8_t> sign = sign_message(hashed, privK); //sign hashed

        vector<uint8_t> sign_cipher;
        vector<uint8_t> padded_data(sign);
        //add padding
        addPKCS7Padding(padded_data, EVP_CIPHER_block_size(EVP_aes_256_cbc()));

        //encrypt the sign
        cbc_encrypt(padded_data, sign_cipher, iv , symm_key_no_hashed);

        //sending message to client
        ap.setSign_len(sign_cipher.size());
        ap.setSign(sign_cipher); // Sign = E_cbc (S(symm + hmac) + padding)
        ap.setIv(iv);

        vector<uint8_t> s;
        

        vector<uint8_t> msg = ap.serialize(); //serialize the Authentication packet just created
        ssize_t len = ap.getLen();
        send_message(comm_sock, msg.data(), len); //send the Authentication packet to the client

/*
        vector<uint8_t> plaintext;
        //receive Authentication packet from the client
        receive_message(comm_sock);
        if(strcmp((char*)receive_buffer, "")==0){
            return;
        }

        AuthenticationPacket at = AuthenticationPacket();
        int lm = ntohl(receive_msg_len);
        vector<uint8_t> r_vec(receive_buffer, receive_buffer + lm);

        at.deserialize(r_vec);

        vector<uint8_t> sign_enc = at.getSign();

        //decrypt
        cbc_decrypt(sign_enc, plaintext, at.getIv(), symm_key_no_hashed);
        removePKCS7Padding(plaintext);
        at.setSign(plaintext);
     
        vector<uint8_t> clear_text = at.serializeSign(symm_key, hmac_key);
        
        EVP_PKEY* client_pubk = read_public_key(st.getNickname()); //check if the public key is correct
        if(!verify_signature(clear_text, plaintext, client_pubk)){
            cerr<<"Wrong Signature";

            vector<uint8_t> serializedMessage = createResponseMessage(string("NO"));
            vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

            int len = serializedPacket.size();
            send_message(comm_sock, serializedPacket.data(), len); //send "NO" to the client because the signature is wrong
            return;
        }
        //if the signature is ok send "OK" to the client
        vector<uint8_t> serializedMessage = createResponseMessage(string("OK"));
        vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

        len = serializedPacket.size();
        send_message(comm_sock, serializedPacket.data(), len);
*/
        EVP_PKEY_free(symmetric_param);
        EVP_PKEY_free(hmac_param);

        return;

    } 


    if(cmd == "Auth"){ //if the client sent this he wants to authenticate

        vector<uint8_t> symm_key;
        vector<uint8_t> hmac_key;

        receive_message(comm_sock);
        if(strcmp((char*)receive_buffer, "")==0){
            return;
        }

        StartPacket st;

        vector<uint8_t> tmp(receive_buffer, receive_buffer+strlen((char*)receive_buffer));
        st.deserialize(tmp,false); //deserialize the Startpacket recevied from the client, false because it's not the registration but login phase

        FileManager fm(st.getNickname()); //check if nickname is registered
        if(!userExist(fm.getUser())){
            send_message(comm_sock, "NO", strlen("NO")); //it's not registered
            return;
        }

        EVP_PKEY* symmetric_param = generate_params(); //generate symmetric DH key
        EVP_PKEY* hmac_param = generate_params(); //generate HMAC DH key

                 //  b (private key)   Ya (received from client)
        generate_secrets(symmetric_param, st.getSymmEVP(), symm_key_no_hashed, symm_key_no_hash_size);
        //in symm_key_no_hashed there is the shared secret Kab for symmetric encryption
        generate_secrets(hmac_param, st.getHmacEVP(), hmac_key_no_hashed, hmac_key_no_hash_size);
        //in hmac_key_no_hashed there is the shared secret Kab' for HMAC

        //compute the hash of the shared secret for both symmetric and hmac, and store them in symm_key and hmac_key
        generateSHA(symm_key_no_hashed.data(), symm_key_no_hash_size, symm_key, symm_key_size);
        generateSHA(hmac_key_no_hashed.data(), hmac_key_no_hash_size, hmac_key, hmac_key_size);


        //Derive private key

        privK = read_server_private_key();

        vector<uint8_t> iv = generate_iv(); //generate initialization vector

        unsigned int len_sign;

        AuthenticationPacket ap(iv, symmetric_param, hmac_param); //create the Authentication packet

        vector<uint8_t> hashed = ap.serializeSign(symm_key, hmac_key); //serialize sign ( = symm_key + hmac_key)

        vector<uint8_t> sign = sign_message(hashed, privK); //sign hashed

        vector<uint8_t> sign_cipher;
        vector<uint8_t> padded_data(sign);
        //add padding
        addPKCS7Padding(padded_data, EVP_CIPHER_block_size(EVP_aes_256_cbc()));

        //encrypt the sign
        cbc_encrypt(padded_data, sign_cipher, iv , symm_key_no_hashed);

        //sending login message to client
        ap.setSign_len(sign_cipher.size());
        ap.setSign(sign_cipher); // Sign = E_cbc ( S(symm + hmac) + padding)
        ap.setIv(iv);

        vector<uint8_t> s;
        

        vector<uint8_t> msg = ap.serialize(); //serialize the Authentication packet just created
        ssize_t len = ap.getLen();
        send_message(comm_sock, msg.data(), len); //send the Authentication packet to the client

        /*
        vector<uint8_t> plaintext;

        //receive Authentication packet from the client
        receive_message(comm_sock);
        if(strcmp((char*)receive_buffer, "")==0){
            return;
        }

        
        AuthenticationPacket at = AuthenticationPacket();
        int lm = ntohl(receive_msg_len);
        vector<uint8_t> r_vec(receive_buffer, receive_buffer + lm);

        at.deserialize(r_vec);

        vector<uint8_t> sign_enc = at.getSign();

        //decrypt
        cbc_decrypt(sign_enc, plaintext, at.getIv(), symm_key_no_hashed);
        removePKCS7Padding(plaintext);
        at.setSign(plaintext);
     
        vector<uint8_t> clear_text = at.serializeSign(symm_key, hmac_key);
        
        EVP_PKEY* client_pubk = read_public_key(st.getNickname()); //check if the public key is correct
        if(!verify_signature(clear_text, plaintext, client_pubk)){
            cerr<<"Wrong Signature";

            vector<uint8_t> serializedMessage = createResponseMessage(string("NO"));
            vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

            int len = serializedPacket.size();
            send_message(comm_sock, serializedPacket.data(), len); //send "NO" to the client because the signature is wrong
            return;
        }
        //if the signature is ok send "OK" to the client
        vector<uint8_t> serializedMessage = createResponseMessage(string("OK"));
        vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

        len = serializedPacket.size();
        send_message(comm_sock, serializedPacket.data(), len);
        */

        EVP_PKEY_free(symmetric_param);
        EVP_PKEY_free(hmac_param);

        return;
    }

    if((cmd == "Registration")){
        string nickname = fields[1];
        string password = fields[2];
        string salt = generateSalt(16);

        FileManager fm(nickname); //check if the user is already registered
         
        //if the user is already registered send "NO" to the client
        if(userExist(fm.getUser())){ //return user with nickname == "" if it doesn't exist
            vector<uint8_t> serializedMessage = createResponseMessage(string("NO"));
            vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

            int len = serializedPacket.size();
            send_message(comm_sock, serializedPacket.data(), len);
            return;
        }
        
        User user = User(nickname, password, salt, 0);
        fm.insertUser(user); //create user file and password file

        vector<uint8_t> serializedMessage = createResponseMessage(string("OK"));
        vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

        int len = serializedPacket.size();
        send_message(comm_sock, serializedPacket.data(), len);

        cout << "Registration ended" << endl;

        return;
    }

    if(cmd == "Login"){

        string nickname = fields[2];
        string password = fields[3];

        FileManager fm(nickname);

        fm.resetCounter(nickname); //reset the counter of the user

        User user = fm.getUser(); //get all user's information from the file
        int c = user.getCounter(); //get the counter of the user

        if(counter != c){ //counter check
            cerr<<"Wrong counter, old message detected"<<endl;

            vector<uint8_t> serializedMessage = createResponseMessage(string("NO"));
            vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

            int len = serializedPacket.size();
            send_message(comm_sock, serializedPacket.data(), len);
            return;
        }

        fm.updateCounter(nickname); //update the user's counter
        string salt = user.getSalt();
        string pwd_salted = salt + password; //salt + password inserted

        vector<uint8_t> input_pwd(pwd_salted.begin(), pwd_salted.end());

        string pwd_str = user.getPassword(); //correct password
        vector<uint8_t> pwd(pwd_str.begin(), pwd_str.end());
        unsigned char* hash = pwd.data();

        if(verifySHA(input_pwd.data(), input_pwd.size(), hash)){ //check if the password inserted is correct
            vector<uint8_t> serializedMessage = createResponseMessage(string("OK"));
            vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

            int len = serializedPacket.size();
            send_message(comm_sock, serializedPacket.data(), len);
            cout << "LOGIN" << endl;
        }
        else{
            cout << "LOGIN FAILED" << endl;
            vector<uint8_t> serializedMessage = createResponseMessage(string("NO"));
            vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

            int len = serializedPacket.size();
            send_message(comm_sock, serializedPacket.data(), len);
        }

        return;
    }


    if(cmd == "Message"){

        string nickname = fields[2];
        string title = fields[3];
        string body= fields[4];
        //used to retrieve counter
        FileManager fm(nickname);
        User user = fm.getUser();

        if(counter != user.getCounter()){ //counter check
            cerr<<"Wrong counter, old message detected"<<endl;
            vector<uint8_t> serializedMessage = createResponseMessage(string("NO"));
            vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

            int len = serializedPacket.size();
            send_message(comm_sock, serializedPacket.data(), len);
            return;
        }

        fm.updateCounter(nickname);

        fm.insertMessage(nickname, title, body);
        
        cout << "Message inserted" << endl;


        vector<uint8_t> serializedMessage = createResponseMessage(("OK"));
        vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

        int len = serializedPacket.size();
        send_message(comm_sock, serializedPacket.data(), len);

        return;
    }

    if(cmd == "List of Messages"){


        int T = stoi(fields[2]); //number of messages user wants to retrieve
        string nickname= fields[3];
        

        int num = 0; //number of retrieved messages

        FileManager fm(nickname);
        User user = fm.getUser();

        if(counter != user.getCounter()){ //counter check
            cerr<<"Wrong counter, old message detected"<<endl;
            vector<uint8_t> serializedMessage = createResponseMessage(string("NO"));
            vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

            int len = serializedPacket.size();
            send_message(comm_sock, serializedPacket.data(), len);
            return;
        }
        //everything is ok
        vector<uint8_t> serializedMessage = createResponseMessage("OK");
        vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

        int len = serializedPacket.size();
        send_message(comm_sock, serializedPacket.data(), len);


        fm.updateCounter(nickname); //update the counter of the user

        Message* list = fm.getMessages(T, num);

        // parse the result in one message
        string list_of_messages= "";
        for(int i = 0; i < num; i++)
            list_of_messages = list_of_messages + list[i].convertString() + "\n";
        
        serializedMessage = createResponseMessage(list_of_messages, ';');
        serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);
        len = serializedPacket.size();
        send_message(comm_sock, serializedPacket.data(), len);

        cout << "List of messages sent" << endl;

        return;
    }

    delete[] fields;

}

void Server::send_message(int currentSocket, const void *msg, uint32_t len)
{
    ssize_t ret;
    len++;
    uint32_t actual_len = htonl(len);
    // Send message length
    ret = send(currentSocket, &actual_len, sizeof(actual_len), 0);
    // If -1 error it means that no bytes were sent
    if (ret <= 0)
    {
        cerr << "ERR: Message length not sent" << endl
             << endl;
        return;
    }
    // Send message
    ret = send(currentSocket, msg, len, 0);
    // If -1 error it means that no bytes were sent
    if (ret <= 0)
    {
        cerr << "ERR: Message not sent" << endl
             << endl;
        return;
    }
}

int Server::receive_message(int i)
{
    memset(receive_buffer, 0, MAX_MSG_SIZE);

    int ret = recv(i, (void*)&receive_msg_len, sizeof(uint32_t), 0);
    if(ret < 0){
        cerr<<"Error in receiving the message"<<endl;;
        return -1;
    }

    int len = ntohl(receive_msg_len);

    //disconnection of the client
    if(ret == 0){
        handle_client_disconnection(i);
        return -1;
    }
    

    ret = recv(i, receive_buffer, len, 0);
    if(ret < 0){
        perror("Receive error");
        return -1;
    }

    //disconnection of the client
    if(ret == 0){
        handle_client_disconnection(i);
        return -1;
    }

    return 0;
}

void Server::send_int(int currentSocket, const uint32_t msg)
{
    ssize_t ret;
    // Send message
    uint32_t msg_to_net = htonl(msg);
    ret = send(currentSocket, (void*) &msg_to_net, sizeof(uint32_t), 0);
    // If -1 error it means that no bytes were sent
    if (ret <= 0)
    {
        cerr << "ERR: Message not sent" << endl
             << endl;
        return;
    }
}

int Server::receive_int(int i)
{
    uint32_t receive_msg;

    int ret = recv(i, (void*)&receive_msg, sizeof(uint32_t), 0);
    if(ret < 0){
        cerr<<"Error in receiving the message"<<endl;;
        return -1;
    }

    int msg = ntohl(receive_msg);


    //disconnection of the client
    if(ret == 0){
        handle_client_disconnection(i);
        return -1;
    }
    

    return msg;
}

