#include "utilHeader.hpp"
#include "cryptoHeader.hpp"

#include <vector>

//login packet:                "counter|Login|nickname|password|"
vector<uint8_t> serializeLoginMessage(string nickname, string password, int counter) {
    string serializedMessage = to_string(counter) + "|Login|" + nickname+ "|" + password+'|';
    vector<uint8_t> vec(serializedMessage.begin(), serializedMessage.end());
    return vec;
}

//registration packet:                "Registration|nickname|password|"
vector<uint8_t> serializeRegistrationMessage(string nickname, string password) {
    string serializedMessage = "Registration|" + nickname+ "|" + password+'|';
    vector<uint8_t> vec(serializedMessage.begin(), serializedMessage.end());
    return vec;
}

//new message packet:                "counter|Message|nickname|title|body|"
vector<uint8_t> serializeNewMessage(string nickname, string title, string body, int counter){
    string serializedMessage = to_string(counter) + "|Message|" + nickname + "|" + title + "|"+ body + '|';
    vector<uint8_t> vec(serializedMessage.begin(), serializedMessage.end());
    return vec;
}

//ListOfTransfers packet:                 "counter|List of Messages|T"
vector<uint8_t> serializeListOfMessages(int T, int counter, string nickname){
    string serializedMessage = to_string(counter) + "|List of Messages|" + to_string(T) + '|' + nickname + '|';
    vector<uint8_t> vec(serializedMessage.begin(), serializedMessage.end());
    return vec;
}

string* deserializeMessage(vector<uint8_t> serializedMessage){
    istringstream stream(string((char*)serializedMessage.data()));
    string* deserialized = new string[10];
    string token;
    int i = 0;
    // | is used as delimiter because in the body of the message there could be commas
    while (getline(stream, token, '|')) {
        deserialized[i] = token; //vector of strings
        i++;
    }

    return deserialized; //important to delete the pointer after using it
}


vector<uint8_t> createSerializedPacket(vector<uint8_t> serializedMessage, size_t mess_len, vector<uint8_t> symm_key_no_hashed, vector<uint8_t> hmac_key_no_hashed){
    vector<uint8_t> ciphertext;
    vector<uint8_t> digest;
    uint32_t digestlen;
    vector<uint8_t> iv = generate_iv(); //generate iv for the message
    vector<uint8_t> ser_mes(serializedMessage.begin(), serializedMessage.begin()+mess_len);

    vector<uint8_t> padded_data(ser_mes);
    //encrypt the serialized message
    cbc_encrypt(padded_data, ciphertext, iv, symm_key_no_hashed);
    //generate HMAC for the message
    generateHMAC(ciphertext.data(), ciphertext.size(), digest, digestlen, hmac_key_no_hashed);
    
    GenericPacket gp(iv, ciphertext.size(), ciphertext, digest);
    vector<uint8_t> serializedPacket = gp.serialize();
    return serializedPacket;
}

int convert_to_int(unsigned char* buffer){
    int value = 0;
    for(int i = 0; i < sizeof(buffer); i++){
        value = (value << 8) | buffer[i];        
    }

    return value;
}

bool compare_to(unsigned char* buf, const char* cmd){
    for (size_t i = 0; i < sizeof(cmd); i++) {
        if (buf[i] != cmd[i]) {
            return false; 
        }
    }
    return true;
}

vector<uint8_t> createResponseMessage(string msg, char delimiter){
    string message = msg + delimiter; //add the delimiter to the end of the message
    vector<uint8_t> vec(message.begin(), message.end());
    return vec;
}

vector<string> receiveResponseMessage(unsigned char* buff, ssize_t len, vector<uint8_t> key, vector<uint8_t> hmac_key, bool split){
    GenericPacket gp; 
    vector<uint8_t> tmp(buff, buff+len);

    gp.deserialize(tmp);
    vector<uint8_t> plaintext;
    // Decrypt
    cbc_decrypt(gp.getCiphertext(), plaintext, gp.getIv(), key);

    // HMAC
    vector<unsigned char> hmac = gp.getHMAC();
    if(!verifyHMAC(gp.getCiphertext().data(), gp.getCipherLen(), hmac, hmac_key)){
        cout << "Error in verifying the message" << endl;
        return {"",};
    }

    vector<string> msg;
    if(split)
        msg = splitString(string((char*)plaintext.data()), '|');
    else
        msg.push_back(string((char*)plaintext.data()));

    return msg;
}

vector<string> splitString(const string &input, char delimiter) {
    vector<string> tokens;
    stringstream ss(input);
    string token;

    while (getline(ss, token, delimiter)) {
        tokens.push_back(token);
    }

    return tokens;
}

string deserializeResponseMessage(string msg, char delimiter) {
    istringstream iss(msg);
    string temp;

    if (getline(iss, temp, delimiter)) {
        return temp;
    }
    return "";
}