#ifndef CRYPTOHEADER_HPP
#define CRYPTOHEADER_HPP

#include <openssl/err.h>
#include <openssl/ssl.h>
#include <openssl/rand.h>
#include <openssl/pem.h>
#include <openssl/evp.h>
#include <openssl/dh.h>
#include <openssl/aes.h>

#include <sys/socket.h>
#include <sys/types.h>
#include <unistd.h>
#include <arpa/inet.h>

#include <iostream>
#include <fstream>
#include <stdio.h>
#include <limits>
#include <sstream> 

#include <cstdint>
#include <cstring>
#include <limits.h>
#include <vector>
#include <string>
#include <math.h>
#include <dirent.h>
#include <string.h>

using namespace std;

#define KEY_SIZE 256
#define MAX_MSG_SIZE 5000
#define MAX_NUM_CIPHER 10
#define HMAC_DIGEST_SIZE 32
#define TRANSFER_CLEARTEXT_DIM 96
#define TRANSFER_CIPHERTEXT_DIM 96

//Cryptofunctions.cpp
EVP_PKEY* generate_params();
void generate_secrets(EVP_PKEY* private_key, EVP_PKEY* peer_ephemeral_key, vector<uint8_t>& shared_secret, size_t& shared_secret_size);
EVP_PKEY* read_server_private_key();
EVP_PKEY* read_server_public_key();
vector<unsigned char> sign_message(const vector<unsigned char>& buffer, EVP_PKEY* private_key);
bool verify_signature(const vector<unsigned char>& buffer, const vector<unsigned char>& signature, EVP_PKEY* public_key);
void cbc_encrypt(const vector<uint8_t>& input_buffer, vector<uint8_t>& output_buffer, vector<uint8_t>& iv, vector<uint8_t>& key);
void cbc_decrypt(const vector<uint8_t>& input_buffer, vector<uint8_t>& output_buffer, const vector<uint8_t>& iv, vector<uint8_t>& key);
bool verifySHA(const unsigned char* input_buffer, size_t input_buffer_size, const unsigned char* input_digest);
void generateSHA(const unsigned char* input_buffer, size_t input_buffer_size, vector<uint8_t>& digest, unsigned int& digest_size);
void generateHMAC(unsigned char* input_buffer, size_t input_buffer_size, vector<unsigned char>& digest, unsigned int& digest_size, vector<unsigned char> key);
bool verifyHMAC(unsigned char* input_buffer, size_t input_buffer_size, vector<unsigned char>& input_digest, vector<unsigned char> key);
vector<unsigned char> generate_iv();
int hash_hmac_key(unsigned char* &hmac_key, unsigned char* hmac_key_no_hashed);
vector<uint8_t> serializeKey(EVP_PKEY* key);
EVP_PKEY* deserializeKey(uint8_t* serialized_key, int serialized_key_size);
void addZeros(string& input, int width);
void addPKCS7Padding(vector<uint8_t>& data, size_t block_size);
void removePKCS7Padding(vector<uint8_t>& data);
string generateSalt(size_t length) ;
#endif