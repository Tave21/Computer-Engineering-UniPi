#include "GenericPacket.hpp"

unsigned char gen_buff[MAX_NUM_CIPHER];

GenericPacket::GenericPacket(){
}
GenericPacket::GenericPacket(vector<uint8_t> iv, uint32_t cipher_len, vector<uint8_t> ciphertext, vector<uint8_t> HMAC){
    this->iv = iv;
    this->iv_len = iv.size();
    this->cipher_len = cipher_len;
    this->ciphertext = ciphertext;
    this->ciphertext.resize(cipher_len);
    this->HMAC_len = HMAC.size();
    this->HMAC = HMAC;
}

vector<uint8_t> GenericPacket::getIv(){
    return iv;
}

uint32_t GenericPacket::getCipherLen(){
    return cipher_len;
}

uint32_t GenericPacket::getIvLen(){
    return iv_len;
}

uint32_t GenericPacket::getHMACLen(){
    return HMAC_len;
}

uint32_t GenericPacket::getLen(){
    return 3*MAX_NUM_CIPHER + iv_len + cipher_len + HMAC_len;
}

vector<uint8_t> GenericPacket::getCiphertext(){
    return ciphertext;
}

vector<uint8_t> GenericPacket::getHMAC(){
    return HMAC;
}


vector<uint8_t> GenericPacket::serialize() {

    size_t total_len = 4 * MAX_NUM_CIPHER + iv_len + cipher_len + HMAC_len;
    // CHECK
    vector<uint8_t> serialized_data;

    size_t offset = 0;

    string s = "";

    string tmp = to_string(iv_len);
    addZeros(tmp, MAX_NUM_CIPHER);
    s += tmp;

    tmp = to_string(cipher_len);
    addZeros(tmp, MAX_NUM_CIPHER);
    s += tmp;

    tmp = to_string(HMAC_len);
    addZeros(tmp, MAX_NUM_CIPHER);
    s += tmp;

    serialized_data.insert(serialized_data.end(), s.begin(), s.end());

    string iv_str = string(iv.begin(), iv.end());
    serialized_data.insert(serialized_data.end(), iv_str.begin(), iv_str.end());
    string cip_str = string(ciphertext.begin(), ciphertext.end());
    serialized_data.insert(serialized_data.end(), cip_str.begin(), cip_str.end());
    string h_str = string(HMAC.begin(), HMAC.end());
    serialized_data.insert(serialized_data.end(), h_str.begin(), h_str.end());

    return serialized_data;
}

void GenericPacket::deserialize(vector<uint8_t> serialized_data) {
    size_t offset = 0;

    iv_len = stoi(string(serialized_data.begin() + offset, serialized_data.begin() + offset + MAX_NUM_CIPHER));
    offset += MAX_NUM_CIPHER;

    // assign cipher_len from serialized_data
    cipher_len = stoi(string(serialized_data.begin() + offset, serialized_data.begin() + offset + MAX_NUM_CIPHER));
    offset += MAX_NUM_CIPHER;

    // assign HMAC_len from serialized_data
    HMAC_len = stoi(string(serialized_data.begin() + offset, serialized_data.begin() + offset + MAX_NUM_CIPHER));
    offset += MAX_NUM_CIPHER;

    iv.assign(serialized_data.begin() + offset, serialized_data.begin() + offset + iv_len);
    offset += iv_len;

    ciphertext.assign(serialized_data.begin() + offset, serialized_data.begin() + offset + cipher_len);
    offset += cipher_len;

    HMAC.assign(serialized_data.begin() + offset, serialized_data.begin() + offset + HMAC_len);
}
