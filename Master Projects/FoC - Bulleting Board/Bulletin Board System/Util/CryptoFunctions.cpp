#include "cryptoHeader.hpp"
#include <iomanip>

#pragma GCC diagnostic ignored "-Wdeprecated-declarations"

EVP_PKEY *generate_params(){ //generates DH parameters

    //pointers to EVP structures
    EVP_PKEY *dh_params = nullptr;
    EVP_PKEY_CTX *dh_gen_ctx = nullptr;
    EVP_PKEY *dh_key = nullptr;

    int ret;

    // Allocate p and g
    dh_params = EVP_PKEY_new(); //generate an empty structure for containing p and g
                                //dh_params is a pointer to a EVP_PKEY structure

    if (!dh_params){
        cerr << "ERR: Couldn't generate new dh params!" << endl;
        return nullptr;
    }

    // Set default dh parameters for p & g
    DH *default_params = DH_get_2048_224();
                        //modulus length = 2048 bits
                        //exponent length = 224 bits

    ret = EVP_PKEY_set1_DH(dh_params, default_params);
            //this function sets the DH parameters (p and g) that belong to "default_params" 
            //in the EVP_KEY structure dh_params

    // Delete p & g
    DH_free(default_params);

    if (ret != 1){
        EVP_PKEY_free(dh_params);
        cerr << "ERR: Couldn't load default params!" << endl;
        return nullptr;
    }

    // a or b

    // Create the context for the key generation using the parameters specified in dh_params
    dh_gen_ctx = EVP_PKEY_CTX_new(dh_params, nullptr);

    if (!dh_gen_ctx){
        EVP_PKEY_free(dh_params);
        EVP_PKEY_CTX_free(dh_gen_ctx);
        cerr << "ERR: Couldn't load define dh context!" << endl;
        return nullptr;
    }

    //initialize the key generation context
    ret = EVP_PKEY_keygen_init(dh_gen_ctx);

    if (ret != 1){
        EVP_PKEY_free(dh_params);
        EVP_PKEY_CTX_free(dh_gen_ctx);
        cerr << "ERR: Couldn't dh keygen init!" << endl;
        return nullptr;
    }

    // Generate the DH key that is saved in dh_key, starting from the context dh_gen_ctx (that cointains the parameters in dh_params)
    ret = EVP_PKEY_keygen(dh_gen_ctx, &dh_key);

    //DH key: private key (a/b) + public key(g^a mod p or g^b mod p)

    if (ret != 1)
    {
        EVP_PKEY_free(dh_params);
        EVP_PKEY_CTX_free(dh_gen_ctx);
        cerr << "ERR: Couldn't dh keygen!" << endl;
        return nullptr;
    }

    EVP_PKEY_CTX_free(dh_gen_ctx);
    EVP_PKEY_free(dh_params);

    return dh_key; //return the DH key just generated
}

void generate_secrets(EVP_PKEY* private_key, EVP_PKEY* peer_ephemeral_key, vector<uint8_t>& shared_secret, size_t& shared_secret_size) {

    //create a context for the public key derivation
    EVP_PKEY_CTX* derive_ctx = EVP_PKEY_CTX_new(private_key, NULL);
    if (!derive_ctx){
        cerr << "Error: " << errno << endl;
        throw runtime_error("Failed to create derive context.");
    }
    //initialize the context
    if (EVP_PKEY_derive_init(derive_ctx) <= 0) {
        EVP_PKEY_CTX_free(derive_ctx);
        throw runtime_error("Failed to initialize derive context.");
    }

        //set the peer's public key (Yb) in the context
    if (EVP_PKEY_derive_set_peer(derive_ctx, peer_ephemeral_key) <= 0) {
        EVP_PKEY_CTX_free(derive_ctx);
        throw runtime_error("Failed to set peer ephemeral keys in the context.");
    }

    EVP_PKEY_derive(derive_ctx, NULL, &shared_secret_size); //get the size of the shared secret and save it in shared_secret_size
    shared_secret.resize(int(shared_secret_size)); //resize the shared_secret vector to the size of the shared secret

    //fill the shared_secret vector with the shared secret Kab = g^(ab) mod p
    if (EVP_PKEY_derive(derive_ctx, shared_secret.data(), &shared_secret_size) <= 0) {
        EVP_PKEY_CTX_free(derive_ctx);
        throw runtime_error("Failed to generate shared secret.");
    }
    
    EVP_PKEY_CTX_free(derive_ctx);
}

EVP_PKEY* read_server_private_key(){
    string privPath = "Server/keys/private_key.pem";

    FILE *file = fopen(privPath.c_str(), "r");
    if (!file) {
        return 0;
    } 

    EVP_PKEY *privk = EVP_PKEY_new();
    privk =  PEM_read_PrivateKey(file, NULL, NULL, (void *)"francesco");
    fclose(file);

    return privk;
}

EVP_PKEY* read_server_public_key(){
    string pubPath = "Server/keys/public_key.pem";

    FILE *file = fopen(pubPath.c_str(), "r");
    if (!file) {
        return 0;
    } 

    EVP_PKEY *pubk = EVP_PKEY_new();
    pubk =  PEM_read_PUBKEY(file, NULL, NULL, NULL);
    fclose(file);

    return pubk;
}



                                        //data to be signed                 //private key
vector<unsigned char> sign_message(const vector<unsigned char>& buffer, EVP_PKEY* private_key) {
    if (!private_key)
        throw runtime_error("Private key not loaded.");

    EVP_MD_CTX* ctx = EVP_MD_CTX_new(); //new signing context
    EVP_PKEY* privkey = private_key;

    //initialize the context with the private key
    if (EVP_DigestSignInit(ctx, nullptr, EVP_sha256(), nullptr, privkey) != 1) {
        EVP_MD_CTX_free(ctx);
        EVP_PKEY_free(privkey);
        throw runtime_error("Failed to initialize signing context.");
    }

    //update the context with the data to be signed
    if (EVP_DigestSignUpdate(ctx, buffer.data(), buffer.size()) != 1) {
        EVP_MD_CTX_free(ctx);
        EVP_PKEY_free(privkey);
        throw runtime_error("Failed to update signing context.");
    }

    size_t signatureLen;
    //determine the length of the signature
    if (EVP_DigestSignFinal(ctx, nullptr, &signatureLen) != 1) {
        EVP_MD_CTX_free(ctx);
        EVP_PKEY_free(privkey);
        throw runtime_error("Failed to determine signature length.");
    }

    vector<unsigned char> signature(signatureLen);
    //finalize the signature and save it in the buffer signature
    if (EVP_DigestSignFinal(ctx, signature.data(), &signatureLen) != 1) {
        EVP_MD_CTX_free(ctx);
        EVP_PKEY_free(privkey);
        throw runtime_error("Failed to sign the buffer.");
    }

    EVP_MD_CTX_free(ctx);
    EVP_PKEY_free(privkey);

    return signature;
}

                    //original signature                         //signature to verify                  //public key
bool verify_signature(const vector<unsigned char>& buffer, const vector<unsigned char>& signature, EVP_PKEY* public_key) {
    if (!public_key)
        throw runtime_error("Public key not loaded.");

    EVP_MD_CTX* ctx = EVP_MD_CTX_new(); //new verification context
    EVP_PKEY* pubkey = public_key;

    //initialize the context with the public key
    if (EVP_DigestVerifyInit(ctx, nullptr, EVP_sha256(), nullptr, pubkey) != 1) {
        EVP_MD_CTX_free(ctx);
        EVP_PKEY_free(pubkey);
        throw runtime_error("Failed to initialize verification context.");
    }

    //update the context with the data to be verified
    if (EVP_DigestVerifyUpdate(ctx, buffer.data(), buffer.size()) != 1) {
        EVP_MD_CTX_free(ctx);
        EVP_PKEY_free(pubkey);
        throw runtime_error("Failed to update verification context.");
    }
    //check the signature
    int result = EVP_DigestVerifyFinal(ctx, signature.data(), signature.size());
    EVP_MD_CTX_free(ctx);
    EVP_PKEY_free(pubkey);

    if (result == 1) {
        return true; // Signature verified successfully
    } else if (result == 0) {
        return false; // Signature verification failed
    } else {
        throw runtime_error("Error occurred during signature verification.");
    }
}

 //AES-256 CBC = Cipher Block Chaining: used when PT is not multiple of block size
void cbc_encrypt(const vector<uint8_t>& input_buffer, vector<uint8_t>& output_buffer, vector<uint8_t>& iv, vector<uint8_t>& key){
    vector <uint8_t> plaintext;
    vector<uint8_t> ciphertext;
    EVP_CIPHER_CTX* ctx = nullptr; // Initialize to nullptr
    uint32_t processed_bytes = 0;

    iv = generate_iv(); //generates a random IV 

    const long unsigned int block_size = EVP_CIPHER_block_size(EVP_aes_256_cbc());

    plaintext.resize(input_buffer.size());
    std::copy(input_buffer.begin(), input_buffer.end(), plaintext.begin());

    if (plaintext.size() > INT_MAX - block_size)
        throw runtime_error("Integer overflow");

    //initialize the context for encryption
    if (!(ctx = EVP_CIPHER_CTX_new()))
        throw runtime_error("Impossible to create EVP_CIPHER_CTX.");
    //initialize the encryption context with the key and the IV
    if (EVP_EncryptInit_ex(ctx, EVP_aes_256_cbc(), NULL, reinterpret_cast<const unsigned char*>(key.data()), reinterpret_cast<const unsigned char*>(iv.data())) != 1)
        throw runtime_error("Encrypting initialization failed.");

    ciphertext.resize(plaintext.size() + block_size);

    int update_len = 0;
    //encrypt the plaintext and save the number of bytes processed in update_len
    if (EVP_EncryptUpdate(ctx, reinterpret_cast<unsigned char*>(ciphertext.data()), &update_len, reinterpret_cast<const unsigned char*>(plaintext.data()), static_cast<int>(plaintext.size())) != 1)
        throw runtime_error("Encrypting update failed");

    processed_bytes += update_len;

    int final_len = 0;
    //finalize the encryption and save the number of bytes processed in final_len
    if (EVP_EncryptFinal_ex(ctx, reinterpret_cast<unsigned char*>(ciphertext.data() + processed_bytes), &final_len) != 1) {
        throw runtime_error("finalizing encryption failed");
    }

    processed_bytes += final_len;
    ciphertext.resize(processed_bytes); 

    EVP_CIPHER_CTX_free(ctx); // free the context

    output_buffer = ciphertext;
}

 //AES-256 CBC = Cipher Block Chaining: used when PT is not multiple of block size
void cbc_decrypt(const vector<uint8_t>& input_buffer, vector<uint8_t>& output_buffer, const vector<uint8_t>& iv, vector<uint8_t>& key){
    vector<uint8_t> ciphertext;
    vector<uint8_t> plaintext;
    EVP_CIPHER_CTX* ctx = nullptr; // initialize to nullptr 
    uint32_t processed_bytes = 0;

    ciphertext.resize(input_buffer.size());
    // Copy the input buffer to the ciphertext buffer
    copy(input_buffer.begin(), input_buffer.end(), ciphertext.begin());

    if (iv.size() != EVP_CIPHER_iv_length(EVP_aes_256_cbc()))
        throw runtime_error("invalid IV's lenght");

    plaintext.resize(ciphertext.size());


    if (key.empty() || ciphertext.empty())
        throw runtime_error("Empty Symmetric key / ciphertext");

    if (!(ctx = EVP_CIPHER_CTX_new())) //create decrpytion context
        throw runtime_error("Impossible to create EVP_CIPHER_CTX");

    //initialize the decryption context with key and IV
    if (EVP_DecryptInit_ex(ctx, EVP_aes_256_cbc(), NULL, reinterpret_cast<const unsigned char*>(key.data()), reinterpret_cast<const unsigned char*>(iv.data())) != 1)
        throw runtime_error("Decrypting initialization failed");

    int update_len = 0;
    //decrypt the ciphertext and save the number of bytes processed in update_len
    if (EVP_DecryptUpdate(ctx, reinterpret_cast<unsigned char*>(plaintext.data()), &update_len, reinterpret_cast<const unsigned char*>(ciphertext.data()), static_cast<int>(ciphertext.size())) != 1) {
        throw runtime_error("Decrypting update failed");
    }

    processed_bytes += update_len;

    int final_len = 0;
    //finalize the decryption and save the number of bytes processed in final_len
    //it is used for: padding removal and check, processing the final block correctly (?)
    if (EVP_DecryptFinal_ex(ctx, reinterpret_cast<unsigned char*>(plaintext.data() + processed_bytes), &final_len) != 1) {
        auto error_code = ERR_get_error();
        cout << error_code << endl;

        char error_string[1024];
        ERR_error_string(error_code, error_string);

        cout << error_string << endl;

        ERR_print_errors_fp(stderr);

        if (error_code == EVP_R_BAD_DECRYPT)
            throw runtime_error("Decryption failed: authentication error / modified ciphertext");
        else {
            throw runtime_error("Decryption failed: unknown error");
        }
    }

    processed_bytes += final_len;
    plaintext.resize(processed_bytes); // resize to the actual plaintext size

    EVP_CIPHER_CTX_free(ctx); // free the context

    output_buffer = plaintext; 
}




//Compute SHA512 of an input buffer and save it in digest
void generateSHA(const unsigned char* input_buffer, size_t input_buffer_size, vector<uint8_t>& digest, unsigned int& digest_size) 
{
    digest.resize(EVP_MD_size(EVP_sha512())); //512 / 8 = 64 bytes
    
    EVP_MD_CTX* ctx = EVP_MD_CTX_new(); //create context for digest 

    if (!ctx)
        throw runtime_error("Failed to create EVP_MD_CTX.");

    //initialize the digest context
    if (EVP_DigestInit(ctx, EVP_sha512()) != 1) 
    {
        EVP_MD_CTX_free(ctx);
        throw runtime_error("Failed to initialize digest.");
    }

    //update the digest context with the input buffer
    if (EVP_DigestUpdate(ctx, input_buffer, input_buffer_size) != 1) 
    {
        EVP_MD_CTX_free(ctx);
        throw runtime_error("Failed to update digest.");
    }

    //finalize the digest context and save the digest in the buffer digest
    if (EVP_DigestFinal(ctx, digest.data(), &digest_size) != 1) 
    {
        EVP_MD_CTX_free(ctx);
        throw runtime_error("Failed to finalize digest.");
    }

    EVP_MD_CTX_free(ctx);
}

//verify the SHA of the input buffer
bool verifySHA(const unsigned char* input_buffer, size_t input_buffer_size, const unsigned char* input_digest) 
{
    vector<uint8_t> generated_digest;
    unsigned int generated_digest_size = 0;
    
    try {
        generateSHA(input_buffer, input_buffer_size, generated_digest, generated_digest_size); //generate the digest of the input buffer
        std::stringstream ss;
        for(uint8_t byte : generated_digest) {
            ss << std::hex << std::setw(2) << std::setfill('0') << static_cast<int>(byte);
        }
        std::string hexStr = ss.str();
        //cout << hexStr << endl;
        //return CRYPTO_memcmp(input_digest, generated_digest.data(), EVP_MD_size(EVP_sha256())) == 0; //compare the generated digest with the input digest
        if(hexStr == string((char*)input_digest)) {
            return true;
        } else {
            return false;
        }
    } catch (...) { //any type of exception
        throw;
    }
}
//Compute HMAC of an input buffer and save it in digest
void generateHMAC(unsigned char* input_buffer, size_t input_buffer_size, vector<unsigned char>& digest, unsigned int& digest_size, vector<unsigned char> key) 
{    
    digest.resize(EVP_MD_size(EVP_sha256()));
    HMAC_CTX* ctx = HMAC_CTX_new();

    HMAC_Init_ex(ctx, key.data(), HMAC_DIGEST_SIZE, EVP_sha256(), nullptr);
    HMAC_Update(ctx, input_buffer, input_buffer_size);
    HMAC_Final(ctx, digest.data(), &digest_size);    

    HMAC_CTX_free(ctx);
}

//verify the HMAC of the input buffer
bool verifyHMAC(unsigned char* input_buffer, size_t input_buffer_size, vector<unsigned char>& input_digest, vector<unsigned char> key) 
{
    vector<unsigned char> generated_digest;
    unsigned int generated_digest_size = 0;

    generateHMAC(input_buffer, input_buffer_size, generated_digest, generated_digest_size, key);
    bool res = CRYPTO_memcmp(input_digest.data(), generated_digest.data(), EVP_MD_size(EVP_sha256())) == 0;

    return res;
}

vector<unsigned char> generate_iv() {
    int iv_len = EVP_CIPHER_iv_length(EVP_aes_256_cbc());
    std::vector<unsigned char> iv(iv_len);

    int ret = RAND_bytes(iv.data(), iv_len);
    if (ret != 1) {
        // Handle the error, possibly by throwing an exception or returning an empty vector.
        throw std::runtime_error("Failed to generate IV");
    }

    return iv;
}

string generateSalt(size_t length) {
    string salt;
    salt.resize(length);
    RAND_bytes(reinterpret_cast<unsigned char*>(&salt[0]), length);
    return salt;
}
// used to serialize the public/private key to sent it over the network
vector<uint8_t> serializeKey(EVP_PKEY* key) {

    BIO *bio = BIO_new(BIO_s_mem()); //type of I/O stream
    if (!bio)
        throw runtime_error("Failed to create BIO.");

    if (!PEM_write_bio_PUBKEY(bio, key)) { //write the key in PEM format in the BIO
        BIO_free(bio);
        throw runtime_error("Failed to write key in the BIO.");
    }

    int serialized_key_size = BIO_pending(bio); //get the size of the serialized key in bytes
    vector<uint8_t> serialized_key(serialized_key_size); //create a vector of bytes of the size of the serialized key

    if (serialized_key.empty()) {
        BIO_free(bio);
        throw runtime_error("Failed to allocate memory.");
    }

    if (BIO_read(bio, serialized_key.data(), serialized_key_size) != serialized_key_size) {
        BIO_free(bio);
        throw runtime_error("Failed to write the serialized key in the buffer.");
    }
    
    BIO_free(bio);
    return serialized_key;
}

// used to deserialize the public/private key that has been sent over the network
EVP_PKEY* deserializeKey(uint8_t* serialized_key, int serialized_key_size) 
{
    BIO *bio = BIO_new_mem_buf(serialized_key, serialized_key_size);
    if (!bio)
        throw runtime_error("Failed to create the BIO");

    EVP_PKEY* deserialized_key = nullptr;
    deserialized_key = PEM_read_bio_PUBKEY(bio, NULL, NULL, NULL);
    if (deserialized_key == nullptr) {        
        BIO_free(bio);
        throw runtime_error("Failed to read the deserialized key");
    }

    BIO_free(bio);
    return deserialized_key;
}

void addZeros(string& input, int targetLength){
    if (input.length() >= targetLength) {
        return;
    }

    int numZeros = targetLength - input.length();
    string result(numZeros, '0'); 
    input = result + input;
}

void addPKCS7Padding(vector<uint8_t>& data, size_t block_size) {
    //if the data lenght is a multiple of block size, an entire block of padding is added
    unsigned int padding_length = block_size - (data.size() % block_size); //fill the block with padding_length bytes
    for (size_t i = 0; i < padding_length; ++i) {
        data.push_back(static_cast<uint8_t>(padding_length)); //each byte contains the padding length int value
    }

}

void removePKCS7Padding(vector<uint8_t>& data) {
    if (data.empty()) {
        throw runtime_error("Empty data, impossible to remove padding");
    }

    unsigned int padding_length = data[data.size()-1]; //it takes the last byte of the data in which the padding length is stored
    if (padding_length >= data.size()) {
        throw runtime_error("Invalid padding length.");
    }

    data.resize(data.size() - padding_length); //resize the data vector to the original size
}


#pragma GCC diagnostic pop 