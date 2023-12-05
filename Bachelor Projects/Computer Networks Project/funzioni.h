#include <stdio.h>
#include <stdbool.h>
#include <stdlib.h>
#include <unistd.h>
#include <arpa/inet.h>
#include <string.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <time.h>
#include <ctype.h>

#define ORARIO_APERTURA 9
#define ORARIO_CHIUSURA 23

void vect_strings(char *message, const char *delimitatore, int *numero_pezzi, char **command)
{
    int x = 0;
    char *pezzo;

    char *mess = malloc(sizeof(char) * strlen(message));
    strcpy(mess, message);

    pezzo = strtok(mess, delimitatore);
    while (pezzo != NULL)
    {
        command[x] = malloc(sizeof(char *));
        strcpy(command[x], pezzo);
        pezzo = strtok(NULL, delimitatore);
        x++;
    }
    *numero_pezzi = x;
}

int conta_occorrenze(char *mess, const char car)
{
    const int nu = strlen(mess);
    int i = 0;
    if (nu == 0)
    {
        return 0;
    }
    
    int ocor = 0;
    for (i = 0; i < nu; i++)
    {
        if (mess[i] == car)
        {
            ocor++;
        }
    }
    //ritorno numero di occorrenze
    return ocor;
}


bool validatore_data( int minuto,int ora, int giorno, int mese, int anno_f)
{
    // fa un check che la data in input sia valida e che sia successiva alla data di oggi 

    time_t rawtime;
    struct tm *timeinfo;
    time(&rawtime);
    timeinfo = localtime(&rawtime);
    const int current_day = timeinfo->tm_mday;
    const int current_month = timeinfo->tm_mon + 1;
    const int current_year = timeinfo->tm_year + 1900;
    const int current_hour = timeinfo->tm_hour;
    const int current_minute = timeinfo->tm_min;

    const int anno = anno_f + 2000;

    if (ora < ORARIO_APERTURA || ora > ORARIO_CHIUSURA)
    {
        return 0;
    }

    if (minuto > 59 || minuto < 0)
    {
        return 0;
    }

    if (giorno <= 0 || mese <= 0)
    {
        return 0;
    }

    if (anno < current_year)
    {
        return 0;
    }

    if (anno > 3000)
    {
        return 0;
    }

    if (anno == current_year && current_month > mese)
    {
        return 0;
    }

    if (anno == current_year && current_month == mese && current_day > giorno)
    {
        return 0;
    }

    if (anno == current_year && current_month == mese && current_day == giorno && current_hour > ora)
    {
        return 0;
    }

    if (anno == current_year && current_month == mese && current_day == giorno && current_hour == ora && current_minute > minuto)
    {
        return 0;
    }

    if (mese == 2 && giorno >= 29) // Validità per febbraio
    {
        if (giorno > 29)
        {
            return 0;
        }

        if (giorno == 29 && ((anno % 400 != 0) && ((anno % 4 != 0) || (anno % 100 == 0))))
        {
            return 0;
        }
    }

    if (mese == 4 || mese == 6 || mese == 9 || mese == 11)
    {
        if (giorno > 30)
        {
            return 0;
        }
    }
    else
    {
        if (giorno > 31)
        {
            return 0;
        }
    }

    return 1;
}

bool validatore_data_str(char *comm,int minuto,int ora)
{
    if (strlen(comm) < 5)
    {
        return 0;
    }

    if (strlen(comm) > 8)
    {
        return 0;
    }

    int num_ = conta_occorrenze(comm, '-');

    if (num_ != 2)
    {
        return 0;
    }

    
    char **command = malloc(sizeof(char *) * (num_ + 1));
    int a = 0;

    vect_strings(comm, "-", &a, command);

    if (a != 3)
    {
        return 0;
    }

    a = validatore_data(ora, minuto, atoi((char *)command[0]), atoi((char *)command[1]), atoi((char *)command[2]));

    if (a == 0)
    {
        return 0;
    }

    return 1;
}

/*
codici per i piatti, per inserirli in qpiatto array che tiene il numero di piatti ordinati dai clienti, in base al codice e al tavolo
A1 --> 0
A2 --> 1
P1 --> 2
P2 --> 3
S1 --> 4
S2 --> 5
D1 --> 6
D2 --> 7
*/

int calcola_codice_piatto(char *piatto)
{
    int cod = 0;
    if (!strcmp(piatto, "A1"))
    {
        cod = 0;
    }
    else if (!strcmp(piatto, "A2"))
    {
        cod = 1;
    }
    else if (!strcmp(piatto, "P1"))
    {
        cod = 2;
    }
    else if (!strcmp(piatto, "P2"))
    {
        cod = 3;
    }
    else if (!strcmp(piatto, "S1"))
    {
        cod = 4;
    }
    else if (!strcmp(piatto, "S2"))
    {
        cod = 5;
    }
    else if (!strcmp(piatto, "D1"))
    {
        cod = 6;
    }
    else if (!strcmp(piatto, "D2"))
    {
        cod = 7;
    }
    else
    {
        cod = 10; // considerato codice non valido
    }
    return cod;
}

void calcola_id_piatto(int id, char *piatto)
{
    if (id == 0)
    {
        strcpy(piatto, "A1\0");
    }
    else if (id == 1)
    {
        strcpy(piatto, "A2\0");
    }
    else if (id == 2)
    {
        strcpy(piatto, "P1\0");
    }
    else if (id == 3)
    {
        strcpy(piatto, "P2\0");
    }
    else if (id == 4)
    {
        strcpy(piatto, "S1\0");
    }
    else if (id == 5)
    {
        strcpy(piatto, "S2\0");
    }
    else if (id == 6)
    {
        strcpy(piatto, "D1\0");
    }
    else if (id == 7)
    {
        strcpy(piatto, "D2\0");
    }
}

bool invio_intero(int sd, int intero)
{
    uint32_t inte = htonl(intero); // trasformo da host a network

    if (send(sd, (void *)&inte, sizeof(uint32_t), 0) != sizeof(uint32_t))
    {
        perror("errore in fase di send della lunghezza intero");
        return false;
    }
    return true;
}
void ricevo_intero(int sd, int *intero)
{
    uint32_t tem = 0;
    if (recv(sd, (void *)&tem, sizeof(uint32_t), 0) != sizeof(uint32_t))
    {
        perror("errore in fase di ricezione dell'intero");
    }
    tem = ntohl(tem);
    *intero = tem;
    return;
}

void ricevo_stringa(int sd, uint8_t *stringa)
{
    int lun;
    int *pointer = &lun;
    ricevo_intero(sd, pointer);

    if (recv(sd, (void *)stringa, lun, 0) != lun)
    {
        perror("errore di ricezione del campo");
    }
    stringa[lun] = '\0';
}

void invio_stringa(int sd, uint8_t *stringa)
{

    uint32_t lunghezza = strlen((char *)stringa) + 1;
    invio_intero(sd, lunghezza);

    if (send(sd, (void *)stringa, lunghezza, 0) != lunghezza)
    {
        perror("errore in fase di send");
    }
    return;
}

int calcola_sala(int tavolo) {

	if (tavolo<= 5)
	{
        return 1;
	}
	else if (tavolo> 5 && tavolo <= 10)
	{
        return 2;
	}
	else if (tavolo> 10 && tavolo<= 15)
	{
        return 3;
	}
	else
	{
        return 4;
	}
}