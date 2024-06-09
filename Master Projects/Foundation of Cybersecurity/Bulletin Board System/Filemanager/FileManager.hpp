#ifndef FILEMANAGER_HPP
#define FILEMANAGER_HPP

#include <filesystem>
#include <iomanip>
#include <sstream>

#include "../DTO/DTO.hpp"

class FileManager {
    private:
        string nickname;
        string UserPath;
        string userFilePath;
        string pwdFilePath;

    public:
        FileManager(string);
        ~FileManager();
        
        User getUser();
        void insertMessage(string, string, string);
        Message* getMessages(int t, int&);
        static string getTime();
        string getUserFilePath();
        string getPwdPath();
        void insertUser(User user);
        
        void updateCounter(string);
        void resetCounter(string);
};

#endif