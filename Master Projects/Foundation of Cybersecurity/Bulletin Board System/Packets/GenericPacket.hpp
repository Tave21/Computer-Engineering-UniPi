#ifndef GENERICPACKET_HPP
#define GENERICPACKET_HPP

#include "../Util/cryptoHeader.hpp"

using namespace std;

class GenericPacket{
    private:
        uint32_t iv_len;
        uint32_t cipher_len;
        uint32_t HMAC_len;
        
        vector<uint8_t> iv;
        vector<uint8_t> ciphertext; // command,payload
        vector<uint8_t> HMAC;

    public:
        GenericPacket();
        GenericPacket(vector<uint8_t>, uint32_t, vector<uint8_t>, vector<uint8_t>);
        vector<uint8_t> getIv();
        uint32_t getCipherLen();
        uint32_t getIvLen();
        uint32_t getHMACLen();
        uint32_t getLen();
        vector<uint8_t> getCiphertext();
        vector<uint8_t> getHMAC();

        vector<uint8_t> serialize();
        void deserialize(vector<uint8_t>);
};

#endif