#include "Client.hpp"

EVP_PKEY* PrivK = nullptr;
vector<uint8_t> symm_key_no_hashed;
vector<uint8_t> hmac_key_no_hashed;
vector<uint8_t> symm_key;
vector<uint8_t> hmac_key;

size_t symm_key_no_hash_size;
size_t hmac_key_no_hash_size;
unsigned int symm_key_size;
unsigned int hmac_key_size;

EVP_PKEY* symmetric_param;
EVP_PKEY* hmac_param;

int counter = 0;

//Client constructor that creates the socket and connects to the server
Client::Client(string server_ip, int server_port){

    logged = false;
    recv_buffer = new unsigned char[MAX_MSG_SIZE];

    //socket creation
    sock_fd = socket(AF_INET, SOCK_STREAM, 0);

    if(sock_fd == -1)
        throw runtime_error("Failed to create socket!");

    server_addr.sin_family = AF_INET;
    server_addr.sin_port = htons(server_port);

    if(inet_pton(AF_INET, (const char*) server_ip.c_str(), &(server_addr.sin_addr)) <= 0)
        throw runtime_error("Invalid server IP address!");

    if(connect(sock_fd, (struct sockaddr *)&server_addr, sizeof(server_addr)) == -1)
        throw runtime_error("Failed to connect to the server!");

    cout << "Connected to the server." << endl;
}

Client::~Client(){
    close(sock_fd);
    delete[] recv_buffer;
}

bool Client::getLogged(){ //returns the status of the login
    return this->logged;
}


void Client::Registration() {
    string nick;
    uint32_t len;

    cout << "Choose a nickname: ";
    cin >> nick;

    cout << "Choose a password: "; 
    char *pwd = getpass(""); //get password without showing it
    string password(pwd);

    cout<<endl;

    if(!authentication()){
        cout<<"Authentication failed"<<endl<<endl;
        return;
    }


    //send information to the server
    vector<uint8_t> serializedMessage = serializeRegistrationMessage(nick, password); 
    vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

    len = serializedPacket.size();
    send_to_server(serializedPacket.data(), len);

    //receive response from the server about the registration
    recv_from_server(len);

    vector<string> msg = receiveResponseMessage(recv_buffer, len, symm_key_no_hashed, hmac_key_no_hashed);

    if(msg[0] == "OK"){
        cout<< "Correctly registrated"<<endl<<endl;
    }else{
        cout<< "Registration Error: Nickname already in use"<<endl<<endl;
    }

}


bool Client::authentication(){ // establishing keys and hmac and checking if I am talking with server

    uint32_t len;

    /*
    if(privK == 0){
        cout<<endl<<"Wrong PEM Password Inserted"<<endl; //PEM password
        return false;
    }
    */

    send_to_server((void*)"FirstAuth", strlen("FirstAuth"));

    //Send Startpacket to Server
    symmetric_param = generate_params(); //generate symmetric DH key
    hmac_param = generate_params(); //generate hmac DH key

    //create StartPacket
    StartPacket st = StartPacket("", symmetric_param, hmac_param); //"" means that we are in the registration phase
    
    vector<uint8_t> buffer = st.serialize(); //serialize StartPacket
    len = st.getLen(); //get length in byte of the serialized StartPacket

    send_to_server(buffer.data(), len); //.data() returns a pointer to the first element in the array

    uint32_t l; //lenght of the received message
    recv_from_server(l); //check if there is error
    
    if(strcmp((char*)recv_buffer, "")==0){
        cout<<"Wrong message received"<<endl;
        return false;
    }

    AuthenticationPacket at = AuthenticationPacket(); //empty AuthenticationPacket
    vector<uint8_t> tmp(recv_buffer, recv_buffer + l);
    at.deserialize(tmp); //deserialize the received message, filling the AuthenticationPacket fields

                 //  a (private key)   Yb (received from server)
    generate_secrets(symmetric_param, at.getSymmetricParam(), symm_key_no_hashed, symm_key_no_hash_size);
    //in symm_key_no_hashed there is the shared secret Kab for symmetric encryption

    generate_secrets(hmac_param, at.getHmacParam(), hmac_key_no_hashed, hmac_key_no_hash_size);
    //in hmac_key_no_hashed there is the shared secret Kab' for hmac


    //compute the hash of the shared secret for both symmetric and hmac, and store them in symm_key and hmac_key
    generateSHA(symm_key_no_hashed.data(), symm_key_no_hash_size, symm_key, symm_key_size);
    generateSHA(hmac_key_no_hashed.data(), hmac_key_no_hash_size, hmac_key, hmac_key_size);

    
    vector<uint8_t> str = at.getSign(); //get the sign (symm+hmac) from the AuthenticationPacket created

    vector<uint8_t> signed_text;

    //decrypt the sign with the shared secret symm_key_no_hashed and IV 
    cbc_decrypt(str, signed_text, at.getIv(), symm_key_no_hashed);
    removePKCS7Padding(signed_text); //remove padding from the decrypted sign

    at.setSign(signed_text); //set the decrypted text as signature of the AuthenticationPacket

    vector<uint8_t> clear_text = at.serializeSign(symm_key, hmac_key); //serialize the signature of the AuthenticationPacket 

    EVP_PKEY* server_pubk = read_server_public_key(); //read the public key of the server

    //verify the signature of signed_text with the public key of the server 
    if(!verify_signature(clear_text, signed_text, server_pubk)){
        cerr<<"Wrong server Signature"<<endl;
        return false;
    }
    
/*
    unsigned int symm_sign_len;
    unsigned int hmac_sign_len;
    
    //generate a new IV
    vector<uint8_t> iv = generate_iv();
    //create a new AuthenticationPacket with the new IV, the shared secret and hmac
    AuthenticationPacket auth = AuthenticationPacket(iv, symmetric_param, hmac_param);
*/
/*
    //calculate the hash of the shared secret and hmac
    generateSHA(symm_key_no_hashed.data(), symm_key_no_hash_size, symm_key, symm_key_size);
    generateSHA(hmac_key_no_hashed.data(), hmac_key_no_hash_size, hmac_key, hmac_key_size);
*/
/*
    vector<uint8_t> hashed = auth.serializeSign(symm_key, hmac_key);//serialize sign( = symm+hmac)

    vector<uint8_t> signed_msg = sign_message(hashed, privK); //sign the hashed with private key
    vector<uint8_t> enc_buff;

    vector<uint8_t> padded_data(signed_msg); 
    //add padding to the signed message
    addPKCS7Padding(padded_data, EVP_CIPHER_block_size(EVP_aes_256_cbc()));
    //encrypt the signed message with the shared secret and IV
    cbc_encrypt(padded_data, enc_buff, iv, symm_key_no_hashed);

    auth.setSign(enc_buff);
    auth.setSign_len(enc_buff.size());
    auth.setIv(iv);

    vector<uint8_t> b =  auth.serialize();
    int buffer_len = auth.getLen();
    //send the new AuthenticationPacket to the server
    send_to_server(b.data(), buffer_len);
    
    recv_from_server(l);

    vector<string> msg = receiveResponseMessage(recv_buffer, l, symm_key_no_hashed, hmac_key_no_hashed);
    if(msg[0] == "OK")
        return true;
    else
        return false;
*/
    return true;


}

bool Client::authentication(string nickname){

    uint32_t len;
    /*
    if(privK == 0){
        cout<<endl<<"Wrong nickname(his private key doesn't exist) or PEM Password Inserted"<<endl; //PEM password
        return false;
    }
    
    */
    
    send_to_server((void*)"Auth", strlen("Auth"));

    //Send Startpacket to Server
    symmetric_param = generate_params(); //generate symmetric DH key
    hmac_param = generate_params(); //generate hmac DH key

    //create StartPacket
    StartPacket st = StartPacket(nickname, symmetric_param, hmac_param);
    
    vector<uint8_t> buffer = st.serialize(); //serialize StartPacket
    len = st.getLen(); //get length in byte of the serialized StartPacket

    send_to_server(buffer.data(), len); //.data() returns a pointer to the first element in the array

    uint32_t l; //lenght of the received message
    recv_from_server(l); //check if there is error
    
    if(strncmp((char*)recv_buffer, "NO", strlen("NO"))==0){
        cout<<"Wrong Nickname(it doesn't exist in server)"<<endl;
        return false;
    }   
    else if(strcmp((char*)recv_buffer, "")==0){
        cout<<"Wrong message received"<<endl;
        return false;
    }

    

    AuthenticationPacket at = AuthenticationPacket(); //empty AuthenticationPacket
    vector<uint8_t> tmp(recv_buffer, recv_buffer + l);
    at.deserialize(tmp); //deserialize the received message, filling the AuthenticationPacket fields

                 //  a (private key)   Yb (received from server)
    generate_secrets(symmetric_param, at.getSymmetricParam(), symm_key_no_hashed, symm_key_no_hash_size);
    //in symm_key_no_hashed there is the shared secret Kab

    generate_secrets(hmac_param, at.getHmacParam(), hmac_key_no_hashed, hmac_key_no_hash_size);
    //in hmac_key_no_hashed there is the shared secret Kab' for hmac


    //compute the hash of the shared secret for both symmetric and hmac, and store them in symm_key and hmac_key
    generateSHA(symm_key_no_hashed.data(), symm_key_no_hash_size, symm_key, symm_key_size);
    generateSHA(hmac_key_no_hashed.data(), hmac_key_no_hash_size, hmac_key, hmac_key_size);

    vector<uint8_t> str = at.getSign(); //get the sign (symm+hmac) from the AuthenticationPacket created

    vector<uint8_t> signed_text;

    //decrypt the sign with the shared secret symm_key_no_hashed and IV 
    cbc_decrypt(str, signed_text, at.getIv(), symm_key_no_hashed);
    removePKCS7Padding(signed_text); //remove padding from the decrypted sign

    at.setSign(signed_text); //set the decrypted text as signature of the AuthenticationPacket(why?)

    vector<uint8_t> clear_text = at.serializeSign(symm_key, hmac_key); //serialize the signature of the AuthenticationPacket 

    EVP_PKEY* server_pubk = read_server_public_key(); //read the public key of the server

    //verify the signature of signed_text with the public key of the server 
    if(!verify_signature(clear_text, signed_text, server_pubk)){
        cerr<<"Wrong Signature"<<endl;
        return false;
    }
    
/*

    unsigned int symm_sign_len;
    unsigned int hmac_sign_len;
    
    //generate a new IV
    vector<uint8_t> iv = generate_iv();
    //create a new AuthenticationPacket with the new IV, the shared secret and hmac
    AuthenticationPacket auth = AuthenticationPacket(iv, symmetric_param, hmac_param);
*/
  /* why is it used two times?
    //calculate the hash of the shared secret and hmac
    generateSHA(symm_key_no_hashed.data(), symm_key_no_hash_size, symm_key, symm_key_size);
    generateSHA(hmac_key_no_hashed.data(), hmac_key_no_hash_size, hmac_key, hmac_key_size);
  */ 
    

/*
    vector<uint8_t> hashed = auth.serializeSign(symm_key, hmac_key);//serialize sign( = symm+hmac)

    vector<uint8_t> signed_msg = sign_message(hashed, privK); //sign the hashed with private key
    vector<uint8_t> enc_buff;

    vector<uint8_t> padded_data(signed_msg); 
    //add padding to the signed message
    addPKCS7Padding(padded_data, EVP_CIPHER_block_size(EVP_aes_256_cbc()));
    //encrypt the signed message with the shared secret and IV
    cbc_encrypt(padded_data, enc_buff, iv, symm_key_no_hashed);

    auth.setSign(enc_buff);
    auth.setSign_len(enc_buff.size());
    auth.setIv(iv);

    vector<uint8_t> b =  auth.serialize();
    int buffer_len = auth.getLen();
    //send the new AuthenticationPacket to the server
    send_to_server(b.data(), buffer_len);
    
    recv_from_server(l);

    vector<string> msg = receiveResponseMessage(recv_buffer, l, symm_key_no_hashed, hmac_key_no_hashed);
    if(msg[0] == "OK")
        return true;
    else
        return false;
    */
    return true;

}

void Client::logIn(){
    string nick;
    uint32_t len;

    cout << "Insert nickname: ";
    cin >> nick;
    nickname = nick;

    cout << "Insert password: "; 
    char *pwd = getpass(""); //get password without showing it
    string password(pwd);

    cout<<endl;

    if(!authentication(nickname)){
        cout<<"Authentication failed"<<endl<<endl;
        return;
    }


    counter = 0;

    vector<uint8_t> serializedMessage = serializeLoginMessage(nickname, password, counter); //send message to server with "Login"
    vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

    len = serializedPacket.size();
    send_to_server(serializedPacket.data(), len);
    counter = counter+1; //used to prevent replay attacks

    recv_from_server(len);

    vector<string> msg = receiveResponseMessage(recv_buffer, len, symm_key_no_hashed, hmac_key_no_hashed);
    //verify credentials
    if(msg[0] == "OK"){
        cout<< "LOGGED IN"<<endl<<endl;
        this->logged = true;
    }else{
        cout<< "Wrong Password Inserted / counter error"<<endl<<endl;
        //nickname must be correct, otherwise the authentication would have failed
        //or counter error
    }

}

void Client::logOut(){
    this->logged = false;
    counter = 0;
    send_to_server((void*)"LOGOUT", strlen("LOGOUT"));
    cout<<"LOGGED OUT"<<endl<<endl;
}

void Client::exitFunction(){
    send_to_server((void*)"EXIT", strlen("EXIT"));
}

void Client::send_to_server(void* buffer, uint32_t len){
    ssize_t ret;

    len++; // ''
    uint32_t actual_len = htonl(len);
    // send message length
    ret = send(sock_fd, &actual_len, sizeof(actual_len), 0);
    // -1 error, if returns 0 no bytes are sent
    if (ret <= 0)
    {
        cerr << "Error: message length not sent" << endl;
        return;
    }
    // send message
    ret = send(sock_fd, (void*)buffer, len, 0);
    // -1 error, if returns 0 no bytes are sent
    if (ret <= 0)
    {
        cerr << "Error: message not sent" << endl;
        return;
    }
}

void Client::recv_from_server(uint32_t &len){
    ssize_t ret;
    uint32_t size;
    // Receive message length
    memset(recv_buffer, 0, MAX_MSG_SIZE);
    ret = recv(sock_fd, &size, sizeof(uint32_t), 0);

    if (ret == 0){
        cerr << "ERR: server disconnected" << endl
             << endl;
        return;
    }

    try{
        // Allocate receive buffer
        len = ntohl(size);

        if (!recv_buffer)
        {
            cerr << "ERR: recv_buffer malloc fail" << endl
                 << endl;
            throw 1;
        }
        // receive message
        ret = recv(sock_fd, (void*)recv_buffer, len, 0);
        if (ret == 0)
        {
            cerr << "ERR: Client disconnected" << endl
                 << endl;
            throw 2;
        }
    }
    catch (int error_code){

        free(recv_buffer);
        if (error_code == 2)
        {
            return;
        }
        else
        {
            return;
        }
    }
}


void Client::addMessage(){
    string title; //title of the message
    string author = nickname; //the author is the user that is logged in
    string body; //body of the message
    uint32_t len;

    cout << "Insert title: ";
    cin >> title;

    cout << "Insert body: ";
    cin >> body;
    cout<<endl;

    //check if the title and the body are too long
    if(title.size() > 100 || body.size() > 1000){
        cout<<"Title or/and body too long"<<endl;
        return;
    }

    //check if the title and the body are empty
    if(title.size() == 0 || body.size() == 0){
        cout<<"Title or/and body empty"<<endl;
        return;
    }


    vector<uint8_t> serializedMessage = serializeNewMessage(author, title, body, counter); //concatenate the fields of the message with | as delimiter
    vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);


    len = serializedPacket.size();
    send_to_server(serializedPacket.data(), len);
    counter = counter+1;

    recv_from_server(len);

    vector<string> msg = receiveResponseMessage(recv_buffer, len, symm_key_no_hashed, hmac_key_no_hashed);

    if(msg[0] == "OK"){
        cout<< "Message correctly inserted"<<endl<<endl;
    }else{
        cout<< "Insertion Error: counter problem"<<endl<<endl;
    }


}


void Client::viewMessages(){
    string T;
    int T_converted;
    uint32_t len;

    // send T
    cout << "Insert the number of recent messages you want to see: ";
    cin >> T;

    try {
        T_converted = stoi(T);
    } catch (const invalid_argument &e) {
        cerr << "Wrong Input value"<< endl<<endl;
        return;
    }

    vector<uint8_t> serializedMessage = serializeListOfMessages(T_converted, counter, nickname);
    vector<uint8_t> serializedPacket = createSerializedPacket(serializedMessage, serializedMessage.size(), symm_key_no_hashed, hmac_key_no_hashed);

    len = serializedPacket.size();

    send_to_server(serializedPacket.data(), len);
    counter = counter+1;

    recv_from_server(len);

    vector<string> msg = receiveResponseMessage(recv_buffer, len, symm_key_no_hashed, hmac_key_no_hashed);

    if(msg[0] == "OK"){
        cout<< "List of the last " << T_converted << " messages:" <<endl<<endl;
    }else{
        cout<< "Insertion Error: counter problem"<<endl<<endl;
    }

    recv_from_server(len);
    msg = receiveResponseMessage(recv_buffer, len, symm_key_no_hashed, hmac_key_no_hashed, false);
    msg = splitString(msg[0], ';');

    cout<<endl;
    cout << msg[0] << endl;
    cout << endl;
}

string Client::getNickname(){
    return nickname;
}

void Client::setLogged(bool b){
    this->logged = b;
}

void Client::send_int_to_server(uint32_t msg){
    ssize_t ret;
    // send message length
    msg = htonl(msg);
    ret = send(sock_fd, &msg, sizeof(uint32_t), 0);
    // -1 error, if returns 0 no bytes are sent
    if (ret <= 0)
    {
        cerr << "Error: message length not sent" << endl;
        return;
    }
}
        
int Client::recv_int_from_server(uint32_t &rcv_msg){
    ssize_t ret;
    // Receive message length
    ret = recv(sock_fd, &rcv_msg, sizeof(uint32_t), 0);

    if (ret == 0){
        cerr << "ERR: server disconnected" << endl
             << endl;
        return -1;
    }

    rcv_msg = ntohl(rcv_msg);
    return ret;
}
