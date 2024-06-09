#include "DTO.hpp"


Message::Message(){
    id = 0;       
    title= "";
    author= "";
    body= "";
}
Message::~Message(){}

int Message::getId(){
    return this->id;
}
string Message::getAuthor(){
    return this->author;
}
string Message::getTitle(){
    return this->title;
}
string Message::getBody(){
    return this->body;
}
void Message::setId(int id){
    this->id = id;
}
void Message::setTitle(string title){
    this->title= title;
}
void Message::setBody(string body){
    this->body= body;
}
void Message::setAuthor(string author){
    this->author= author;
}

string Message::convertString(){
    string res = "";
    //res += to_string(getId())+":";
    res += getAuthor();
    res += " | ";
    res += getTitle();
    res += " | ";
    res += getBody();
    return res;   
}