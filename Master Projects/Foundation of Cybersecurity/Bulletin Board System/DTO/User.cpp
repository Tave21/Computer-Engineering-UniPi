#include "DTO.hpp"

User::User(){
    Nickname= "";
    password = "";
    salt = "";
    counter = 0;
}
User::User(string nickname, string Password, string Salt, int Counter){
    Nickname = nickname;
    salt = Salt;
    counter = Counter;
    password = Password;
}
User::~User(){}

string User::getNickname(){
    return this->Nickname;
}
string User::getPassword(){
    return this->password;
}
string User::getSalt(){
    return this->salt;
}
void User::setSalt(string Salt){
    this->salt = Salt;
}
void User::setNickname(string nickname){
    this->Nickname = nickname;
}
void User::setPassword(string password){
    this->password = password;
}

int User::getCounter(){
    return this->counter;
}

void User::setCounter(int c){
    this->counter = c;
}