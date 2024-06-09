#include "FileManager.hpp"
#include <unistd.h>

void fileCopy(string source, string dest){
    ifstream inFile(source);
    ofstream outFile(dest);

    if (!inFile.is_open() || !outFile.is_open()) {
        cerr << "Unable to open the file(s)" << endl;
        return;
    }

    string fileContents;
    string currentLine;
    while (getline(inFile, currentLine)) {
        fileContents += currentLine + "\n";
    }
    outFile << fileContents;

    inFile.close();
    outFile.close();
}

void insertAtTheStart(string MessagesPath, string line) {
    string temp = "Util/temp.txt";
    ifstream inFile(MessagesPath);
    ofstream tempFile(temp); // Temporary file

    if (!inFile.is_open() || !tempFile.is_open()) {
        cerr << "Unable to open the file(s)" << endl;
        return;
    }

    // Write the new line at the beginning of the temporary file
    tempFile << line << endl;

    // Copy the content of the original file after the new line in the temporary file
    // to make it a sort of "LIFO"
    string fileContents;
    string currentLine;
    while (getline(inFile, currentLine)) {
        fileContents += currentLine + "\n";
    }
    // Write the content of the original file after the new line in the temporary file
    tempFile << fileContents;

    inFile.close();
    tempFile.close();

    fileCopy(temp, MessagesPath);

    if (remove(temp.c_str()) != 0) {
        cerr << "Impossible to remove the file" << endl;
        return;
    }
    
}

FileManager::FileManager(string nickname){
    this->nickname = nickname;
    //set path to the user's directory
    this->UserPath = "Users/" + this->nickname+ "/";
    this->userFilePath = this->UserPath + this->nickname+ ".txt"; // ex: "Users/Paolo/Paolo.txt"
    this->pwdFilePath = this->UserPath + "password.txt"; //// ex: "Users/Paolo/password.txt"
}

FileManager::~FileManager(){}

string FileManager::getUserFilePath(){
    return this->userFilePath;
}

string FileManager::getPwdPath(){
    return this->pwdFilePath;
}

/*
string FileManager::getTime(){ //probably useless
    time_t now = time(nullptr);

    //convert current time to tm struct to get specific parts (year, month, day, hour, minutes, seconds) 
    tm* timeinfo = localtime(&now);

    //convert timeinfo to string
    stringstream ss;
    ss << put_time(timeinfo, "%Y-%m-%d %H:%M:%S");
    string timestamp = ss.str();

    return timestamp;
}
*/ 

User FileManager::getUser(){ //get user from file, if exists
    ifstream file(userFilePath);
    string line;
    string temp;
    string pwd;
    User user = User();
    int i=0; 

    if (!filesystem::exists(userFilePath)) {
        return user; //return an empty user but with nickname = "" 
    }

    if (!file.is_open()) {
        cerr << "Unable to open the file" << endl;
        return user;
    }

    if(getline(file, line)){ // format: "nickname|salt|counter|"
        istringstream ss(line); //allow to read the line as a stream
        char delimiter = '|';
        while(getline(ss, temp, delimiter)){ //used to read a field from the line
                                             //read characters from the stream until the delimiter is found

            if(i>2)
                break;
                
            switch(i){
                case 0:
                    user.setNickname(temp);
                    break;
                case 1:
                    user.setSalt(temp);
                    break;
                case 2:
                    user.setCounter(stoi(temp));
                    break;
            }

            i++;
        }
    }

    //close file
    file.close();

    ifstream file1(pwdFilePath); //create a new file stream to read the password
    char c;

    if (!file1.is_open()) {
        cerr << "Error!" << endl;
        return user;
    }
    /*
    while (file1.get(c)) { //c is the character read from the file
        pwd += c;
    }
    */
    std::ifstream file2(pwdFilePath, std::ios::in);
    std::getline(file2, pwd);

    //close file1
    file1.close();
    
    user.setPassword(pwd);// it is encrypted

    return user;
}

void FileManager::insertMessage(string nickname, string title, string body){

    ifstream file("Messages/BBS_messages.txt"); 
    int id_msg ;
    if (!filesystem::exists("Messages/BBS_messages.txt")) {
        return ;
    }

    if (!file.is_open()) {
        cerr << "Unable to open the file" << endl;
        return ;
    }

    string line;
    string temp;

    //read the last id of the message
    if(getline(file, line)) { //format: id|title|nickname|body|
        istringstream ss(line);
        char delimiter = '|';
        getline(ss, temp, delimiter);
        id_msg = stoi(temp);
        id_msg++;
    } else {
        id_msg = 0; //if the file is empty
    }

    line = to_string(id_msg) + '|' + title + '|' + nickname + '|' + body + '|'; //format: id|title|nickname|body|
                                               //nickname = author
    //inserting the message at the beginning of the file
    insertAtTheStart("Messages/BBS_messages.txt", line);
}
namespace fs = std::filesystem;

//insert user: create user file and password file
void FileManager::insertUser(User user){
    fs::path userDir = fs::path(userFilePath).parent_path();
    if (!fs::exists(userDir)) {
        fs::create_directories(userDir);
    }
    std::ofstream file(userFilePath,std::ios::out);

    //format: nickname|salt|counter|
    string line = user.getNickname() + '|' + user.getSalt() + '|' + to_string(user.getCounter()) + '|';

    file<<line;

    std::ofstream file1(pwdFilePath,std::ios::out);

    // save the hashed salt+password in the password file    
    string pwd_plus_salt = user.getSalt() + user.getPassword(); 
    vector<uint8_t> pwd(pwd_plus_salt.begin(), pwd_plus_salt.end());
    vector<uint8_t> hashed_pwd;
    unsigned int digest_size = 0;
    generateSHA(pwd.data(), pwd.size(), hashed_pwd, digest_size);

    std::stringstream ss;
    for(uint8_t byte : hashed_pwd) {
        ss << std::hex << std::setw(2) << std::setfill('0') << static_cast<int>(byte);
    }
    file1 << ss.str();

    //file1 << string(hashed_pwd.begin(), hashed_pwd.end());

    //close files
    file1.close();
    file.close();
    
}

Message* FileManager::getMessages(int t, int &i){

    ifstream file("Messages/BBS_messages.txt"); 
    Message *messArray = new Message[t];
    i=0;
    int j=0;

    if (!filesystem::exists("Messages/BBS_messages.txt")) {
        return messArray;
    }

    if (!file.is_open()) {
        cerr << "Unable to open the file" << endl;
        return messArray;
    }

    string line;
    string temp;

    while(getline(file, line)) { //format: id|title|nickname|body|
        Message mess = Message(); 
        line.resize(line.size()-1);
        if(strcmp(line.c_str(), "") == 0 || strcmp(line.c_str(), " ") == 0){
            break;
        }

        istringstream ss(line);
        char delimiter = '|';
        while(getline(ss, temp, delimiter)){

            if(j>3)
                break;

            switch(j){
                case 0:
                    try{
                        mess.setId(stoi(temp));
                    } catch(exception e){
                        i--;
                        break;
                    }
                    break;
                case 1:
                    mess.setTitle(temp);
                    break;
                case 2:
                    mess.setAuthor(temp);
                    break;
                case 3:
                    mess.setBody(temp);
                    break;
            }

            j++;
        }

        messArray[i] = mess;
        i++;
        j=0;

        if(i>=t){
            break;
        }
    }

    file.close();
    return messArray;
}



void FileManager::updateCounter(string nickname){
    FileManager fm = FileManager(nickname);
    User user = fm.getUser();

    ofstream file(fm.getUserFilePath());
    uint32_t c = user.getCounter() + 1;
    string line = user.getNickname() + '|' + user.getSalt() + '|' + to_string(c) + '|';
    file << line;
}

void FileManager::resetCounter(string nickname){
    FileManager fm = FileManager(nickname);
    User user = fm.getUser();

    ofstream file(fm.getUserFilePath());
    string line = user.getNickname() + '|' + user.getSalt() + '|' + to_string(0) + '|';
    file << line;
}