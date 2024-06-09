#include "AuthenticationPacket.hpp"

AuthenticationPacket::AuthenticationPacket(){
    this->symmetric_param;
    this->hmac_param;
}

//set also sign

AuthenticationPacket::AuthenticationPacket(vector<uint8_t> iv, EVP_PKEY* symmetric_param, EVP_PKEY* hmac_param, vector<uint8_t> sign){

    this->iv = iv;
    this->symmetric_param = symmetric_param;
    this->hmac_param = hmac_param;
    this->sign = sign;

    this->iv_len = iv.size();
    this->sign_len = sign.size();
}

//it doesn't set sign
AuthenticationPacket::AuthenticationPacket(vector<uint8_t> iv, EVP_PKEY* symmetric_param, EVP_PKEY* hmac_param){
    this->iv = iv;
    this->symmetric_param = symmetric_param;
    this->hmac_param = hmac_param;

    this->iv_len = iv.size();
}

vector<uint8_t> AuthenticationPacket::getIv(){
    return iv;
}

EVP_PKEY* AuthenticationPacket::getSymmetricParam(){
    return symmetric_param;
}

EVP_PKEY* AuthenticationPacket::getHmacParam(){
    return hmac_param;
}

vector<uint8_t> AuthenticationPacket::getSign(){
    return sign;
}

void AuthenticationPacket::setSign(vector<uint8_t> sign){
    this->sign = sign;
}

void AuthenticationPacket::setIv(vector<uint8_t> iv){
    this->iv = iv;
}

uint32_t AuthenticationPacket::getIv_len(){
    return this->iv_len;
}
void AuthenticationPacket::setIv_len(uint32_t len){
    this->iv_len = len;
}
uint32_t AuthenticationPacket::getSymm_len(){
    return this->symm_len;  
}
void AuthenticationPacket::setSymm_len(uint32_t len){
    this->symm_len = len;
}
uint32_t AuthenticationPacket::getHmac_len(){
    return hmac_len;
}
void AuthenticationPacket::setHmac_len(uint32_t len){
    this->hmac_len = len;
}
uint32_t AuthenticationPacket::getSign_len(){
    return sign_len;  
}
void AuthenticationPacket::setSign_len(uint32_t len){
    this->sign_len = len;
}

uint32_t AuthenticationPacket::getSymm_hash_len(){
    return symm_hash_len;
}
void AuthenticationPacket::setSymm_hash_len(uint32_t len){
    this->symm_hash_len = len;
}
uint32_t AuthenticationPacket::getHmac_hash_len(){
    return hmac_hash_len;
}
void AuthenticationPacket::setHmac_hash_len(uint32_t len){
    this->hmac_hash_len = len;
}

void AuthenticationPacket::deserializeSign(vector<uint8_t>& symm_sign, vector<uint8_t>& hmac_sign){

    // Estrai symm_hash_len caratteri in symm_sign
    symm_sign.assign(sign.begin(), sign.begin() + symm_hash_len);

    // Estrai gli hmac_hash_len successivi caratteri in hmac_sign
    hmac_sign.assign(sign.begin() + symm_hash_len, sign.end());
}



vector<uint8_t> AuthenticationPacket::serializeSign(vector<uint8_t> symm, vector<uint8_t> hmac) {

    symm_hash_len = symm.size();
    hmac_hash_len = hmac.size();
    sign_len = symm_hash_len + hmac_hash_len;

    // CHECK
    vector<uint8_t> mess_sign;
    
    // Copy symm and hmac into mess_sign
    mess_sign.insert(mess_sign.end(), symm.begin(), symm.end());
    mess_sign.insert(mess_sign.end(), hmac.begin(), hmac.end());


    return mess_sign;
}

//serializes the authentication packet to be sent over the network
vector<uint8_t> AuthenticationPacket::serialize() { 
    //serialize parameters
    vector<uint8_t> key_buffer_symmetric = serializeKey(symmetric_param);
    vector<uint8_t> key_buffer_hmac = serializeKey(hmac_param);

    //compute lengths
    symm_len = key_buffer_symmetric.size();
    hmac_len = key_buffer_hmac.size();

    //compute total length of serialized data
    size_t total_len = MAX_NUM_CIPHER * 6 + iv_len + symm_len + hmac_len + sign_len;
    vector<uint8_t> serialized_data(total_len, 0); // Inizializza con zeri

    size_t offset = 0;

    //inserting the parameters lenghts in the first 6 * MAX_NUM_CIPHER bytes of serialized_data vector
    string s = to_string(iv_len);
    addZeros(s, MAX_NUM_CIPHER);
    vector<uint8_t> iv_len_data(s.begin(), s.end());
    serialized_data.insert(serialized_data.begin() + offset, iv_len_data.begin(), iv_len_data.end());
    offset += MAX_NUM_CIPHER;

    s = to_string(symm_len);
    addZeros(s, MAX_NUM_CIPHER);
    vector<uint8_t> symm_len_data(s.begin(), s.end());
    serialized_data.insert(serialized_data.begin() + offset, symm_len_data.begin(), symm_len_data.end());
    offset += MAX_NUM_CIPHER;

    s = to_string(hmac_len);
    addZeros(s, MAX_NUM_CIPHER);
    vector<uint8_t> hmac_len_data(s.begin(), s.end());
    serialized_data.insert(serialized_data.begin() + offset, hmac_len_data.begin(), hmac_len_data.end());
    offset += MAX_NUM_CIPHER;

    s = to_string(sign_len);
    addZeros(s, MAX_NUM_CIPHER);
    vector<uint8_t> sign_len_data(s.begin(), s.end());
    serialized_data.insert(serialized_data.begin() + offset, sign_len_data.begin(), sign_len_data.end());
    offset += MAX_NUM_CIPHER;

    s = to_string(symm_hash_len);
    addZeros(s, MAX_NUM_CIPHER);
    vector<uint8_t> symm_hash_len_data(s.begin(), s.end());
    serialized_data.insert(serialized_data.begin() + offset, symm_hash_len_data.begin(), symm_hash_len_data.end());
    offset += MAX_NUM_CIPHER;

    s = to_string(hmac_hash_len);
    addZeros(s, MAX_NUM_CIPHER);
    vector<uint8_t> hmac_hash_len_data(s.begin(), s.end());
    serialized_data.insert(serialized_data.begin() + offset, hmac_hash_len_data.begin(), hmac_hash_len_data.end());
    offset += MAX_NUM_CIPHER;

    offset = 60;

    //inserting the parameter values in the serialized_data vector
    serialized_data.insert(serialized_data.begin() + offset, iv.begin(), iv.end());
    offset += iv_len;

    serialized_data.insert(serialized_data.begin() + offset, key_buffer_symmetric.begin(), key_buffer_symmetric.end());
    offset += symm_len;

    serialized_data.insert(serialized_data.begin() + offset, key_buffer_hmac.begin(), key_buffer_hmac.end());
    offset += hmac_len;

    serialized_data.insert(serialized_data.begin() + offset, sign.begin(), sign.end());

    return serialized_data;
}

//deserializes the authentication packet received from the network
void AuthenticationPacket::deserialize(const vector<uint8_t> serialized_data) {
    size_t offset = 0;
    string tmp;

    //lenght information extraction
    vector<uint8_t> iv_len_data(serialized_data.begin() + offset, serialized_data.begin() + offset + MAX_NUM_CIPHER);
    iv_len = stoi(string(iv_len_data.begin(), iv_len_data.end()));
    offset += MAX_NUM_CIPHER;

    vector<uint8_t> symm_len_data(serialized_data.begin() + offset, serialized_data.begin() + offset + MAX_NUM_CIPHER);
    symm_len = stoi(string(symm_len_data.begin(), symm_len_data.end()));
    offset += MAX_NUM_CIPHER;

    vector<uint8_t> hmac_len_data(serialized_data.begin() + offset, serialized_data.begin() + offset + MAX_NUM_CIPHER);
    hmac_len = stoi(string(hmac_len_data.begin(), hmac_len_data.end()));
    offset += MAX_NUM_CIPHER;

    vector<uint8_t> sign_len_data(serialized_data.begin() + offset, serialized_data.begin() + offset + MAX_NUM_CIPHER);
    sign_len = stoi(string(sign_len_data.begin(), sign_len_data.end()));
    offset += MAX_NUM_CIPHER;

    vector<uint8_t> symm_hash_len_data(serialized_data.begin() + offset, serialized_data.begin() + offset + MAX_NUM_CIPHER);
    symm_hash_len = stoi(string(symm_hash_len_data.begin(), symm_hash_len_data.end()));
    offset += MAX_NUM_CIPHER;

    vector<uint8_t> hmac_hash_len_data(serialized_data.begin() + offset, serialized_data.begin() + offset + MAX_NUM_CIPHER);
    hmac_hash_len = stoi(string(hmac_hash_len_data.begin(), hmac_hash_len_data.end()));
    offset += MAX_NUM_CIPHER;

    offset = 60;
    //data extraction
    iv.assign(serialized_data.begin() + offset, serialized_data.begin() + offset + iv_len);
    offset += iv_len;

    vector<uint8_t> symm_buffer(serialized_data.begin() + offset, serialized_data.begin() + offset + symm_len);
    offset += symm_len;

    vector<uint8_t> hmac_buffer(serialized_data.begin() + offset, serialized_data.begin() + offset + hmac_len);
    offset += hmac_len;

    sign.assign(serialized_data.begin() + offset, serialized_data.begin() + offset + sign_len);

    symmetric_param = deserializeKey(symm_buffer.data(), symm_len);
    hmac_param = deserializeKey(hmac_buffer.data(), hmac_len);

}


ssize_t AuthenticationPacket::getLen(){
    return 6*MAX_NUM_CIPHER + this->iv_len + this->symm_len + this->hmac_len + this->sign_len;
}