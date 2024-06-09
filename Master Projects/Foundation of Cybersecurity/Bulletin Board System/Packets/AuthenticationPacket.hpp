#ifndef AUTHENTICATIONPACKET_HPP
#define AUTHENTICATIONPACKET_HPP

#include "../Util/cryptoHeader.hpp"
#include <sstream>

using namespace std;

class AuthenticationPacket {
    // Clear

    uint32_t iv_len;
    uint32_t symm_len;
    uint32_t hmac_len;
    uint32_t sign_len;

    //lenght of the symm and hmac part of the sign
    uint32_t symm_hash_len;
    uint32_t hmac_hash_len;

    vector<uint8_t> iv;
    EVP_PKEY* symmetric_param = nullptr;
    EVP_PKEY* hmac_param = nullptr;

    //Encrypted sign
    vector<uint8_t> sign;

    public:
        AuthenticationPacket();
        AuthenticationPacket(vector<uint8_t> iv, EVP_PKEY* symmetric_param, EVP_PKEY* hmac_param, vector<uint8_t> sign);
        AuthenticationPacket(vector<uint8_t> iv, EVP_PKEY* symmetric_param, EVP_PKEY* hmac_param);
        vector<uint8_t> getIv();
        EVP_PKEY* getSymmetricParam();
        EVP_PKEY* getHmacParam();
        vector<uint8_t> getSign();
        void setSign(vector<uint8_t>);
        void setIv(vector<uint8_t>);

        uint32_t getIv_len();
        void setIv_len(uint32_t);
        uint32_t getSymm_len();
        void setSymm_len(uint32_t);
        uint32_t getHmac_len();
        void setHmac_len(uint32_t);
        uint32_t getSign_len();
        void setSign_len(uint32_t);
        
        uint32_t getSymm_hash_len();
        void setSymm_hash_len(uint32_t);
        uint32_t getHmac_hash_len();
        void setHmac_hash_len(uint32_t);

        vector<uint8_t> serialize();
        void deserialize(const vector<uint8_t>);
        vector<uint8_t> serializeSign(vector<uint8_t>, vector<uint8_t>);
        void deserializeSign(vector<uint8_t>& symm_sign, vector<uint8_t>& hmac_sign); 
        ssize_t getLen();
};

#endif