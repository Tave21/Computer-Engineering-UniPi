#ifndef DTO_HPP
#define DTO_HPP

#include "../Util/utilHeader.hpp"

class Message{
    private:
        int id;
        string title;
        string body;
        string author;

    public:
        Message();
        Message(bool);
        ~Message();

        int getId();
        string getAuthor();
        string getTitle();
        string getBody();

        void setId(int);
        void setAuthor(string);
        void setTitle(string);
        void setBody(string);
        string convertString();

};

class User{
    private:
        string Nickname;
        string password;
        string salt;
        int counter;

    public:
        User();
        User(string, string, string, int );
        ~User();
        
        string getNickname();
        string getPassword();
        string getSalt();

        void setNickname(string);
        void setPassword(string);
        void setSalt(string);

        void setCounter(int c);
        int getCounter();
};

#endif