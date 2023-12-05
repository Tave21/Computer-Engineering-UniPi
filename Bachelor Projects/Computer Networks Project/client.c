#include "funzioni.h"

#define MAX_LUN_COGNOME 15
#define NUMERO_CLIENT 1
struct prenotazione
{
	uint8_t *comando;
	uint8_t *cognome;
	uint8_t *data;
	uint32_t ora;
	uint32_t numero_persone;
};

struct prenotazione p;

int com;
int x;

void conta(const char *buf)
{

	int num_parametri = sscanf(buf, "%s %s %d %s %d", p.comando, p.cognome, &p.numero_persone, p.data, &p.ora);
	if (num_parametri == 5)
		com = 1;
	else if (num_parametri == 2)
	{
		com = 2;
	}
	else if (num_parametri == 1)
		com = 3;
	else
		com = 4;
	return;
}

int main(int argc, char **argv)
{

	int ret, sd;					// sd socket di comunicazione
	//int porta = atoi(argv[1]);		// salvo il numero di porta indicato
	struct sockaddr_in server_addr; // struttura dati per l'indirizzo del server
	const char *freccia = "   --> ";
	const char *comando1 = "find";
	const char *text1 = "ricerca la disponibilità per una prenotazione";
	const char *comando2 = "book";
	const char *text2 = "invia una prenotazione";
	const char *comando3 = "esc ";
	const char *text3 = "termina il client";
	//dettagli sui tavoli
    char array[20][30];
    strcpy(array[0],"INGRESSO_PRINCIPALE\0");
    strcpy(array[1],"FINESTRA_NORD_NORD_OVEST\0");
    strcpy(array[2],"CENTRO_SALA\0");
    strcpy(array[3],"INGRESSO_SALA_2\0");
    strcpy(array[4],"INGRESSO_SALA_3\0");
    strcpy(array[5],"INGRESSO_SALA_2\0");
    strcpy(array[6],"INGRESSO_SALA_4\0");
    strcpy(array[7],"CENTRO_SALA\0");
    strcpy(array[8],"FINESTRA_SUD_OVEST\0");
    strcpy(array[9],"FINESTRA_SUD_SUD_OVEST\0");
    strcpy(array[10],"INGRESSO_SALA_4\0");
    strcpy(array[11],"FINESTRA_SUD_SUD_EST\0");
    strcpy(array[12],"CENTRO_SALA\0");
    strcpy(array[13],"FINESTRA_SUD_EST\0");
    strcpy(array[14],"FINESTRA_EST\0");
    strcpy(array[15],"FINESTRA_EST_NORD_EST\0");
    strcpy(array[16],"CENTRO_SALA\0");
    strcpy(array[17],"FINESTRA_NORD_NORD_EST\0");
    strcpy(array[18],"CAMINO\0");
    strcpy(array[19],"INGRESSO_SALA_3\0");
	// variabili per la richiesta di prenotazione
	p.cognome = (uint8_t *)malloc(20);
	p.data = (uint8_t *)malloc(8);
	p.comando = (uint8_t *)malloc(4);
	// array per tenere l'accoppiamento fra il numero di tavolo e la scelta dell'opzione con la book
	int arr[21];
	int indici[20]; // array che contiene gli indici delle scelte possibili per i tavoli da prenotare
	int h = 0;
	for (h = 0; h < 20; h++)
	{
		indici[h] = 0; // inizializzo tutti a zero
	}
	// variabili per la conferma/riepilogo della prenotazione
	struct conferma
	{
		uint8_t *codice;
		uint32_t numero_tavolo;
		uint32_t sala;
	};
	struct conferma t;
	t.codice = (uint8_t *)malloc(20);
	// tavoli disponibili
	int *tav_disponibili = (int *)malloc(20 * sizeof(int));

	// creo il socket, allocando le risorse
	sd = socket(AF_INET, SOCK_STREAM, 0);

	// creo l'indirizzo
	memset(&server_addr, 0, sizeof(server_addr)); // inizializzo la memoria a 0
	// setto indirizzo server
	server_addr.sin_family = AF_INET;										 // setto la famiglia AF_INET
	server_addr.sin_port = htons(4242);									 // setto la porta del mio socket
	inet_pton(AF_INET, "127.0.0.1", &server_addr.sin_addr);					 // setto l'ip del mio socket, loopback, da presentation to numeric
																			 // ora posso tentare di connetermi al server
	ret = connect(sd, (struct sockaddr *)&server_addr, sizeof(server_addr)); // connetto il mio socket al server, di cui ho definito il
																			 // descrittore: server_addr (con IP e porta)

	if (ret < 0)
	{
		perror("Errore in fase di connessione: \n");
		exit(1);
	}
	invio_intero(sd, NUMERO_CLIENT + 9);
	// stampo la lista dei comandi
	printf("Lista comandi:\n -%s %s %s\n -%s %s %s\n -%s %s %s\n", comando1, freccia, text1, comando2, freccia, text2, comando3, freccia, text3);
	bool find_prima_di_book = false;
	// ciclo finché non immette un comando corretto o non si preme esc, fino a quel momento continuo a ricevere comandi
	while (1)
	{
		bool buono = false;
		char buf[50];
		// prendo in ingresso un comando
		while (fgets(buf, 50, stdin))
		{ // per prendere diverso numero di parametri input da stdin
			conta(buf);
			break;
		}

		if (!strcmp((char *)p.comando, comando1))
		{ // se è la find invio: 1 (codice find per lo switch dei comandi nel server , vedi server)
			// faccio una serie di check
			char *dummy = malloc(sizeof(char) * 20);
			const int ora = (int)p.ora;

			// controllo che il numero di persone sia valido
			if ((int)p.numero_persone <= 0)
			{
				printf("Numero persone inserito non valido\n");
				continue;
			}

			// controllo che la data sia valida
			sprintf(dummy, "%s", p.data);

			if (validatore_data_str(dummy, ora, 30) == 0)
			{
				printf("Data e/o ora Inserita non Valida\n");
				continue;
			}

			// controllo che il cognome sia valido
			if (strlen((char *)p.cognome) > MAX_LUN_COGNOME || strlen((char *)p.cognome) == 0)
			{
				printf("Cognome non valido\n");
				continue;
			}
			if (!invio_intero(sd, 1))
			{
				perror("errore in fase di invio codice find");
			}

			find_prima_di_book = true;
		}
		else if(!strcmp((char*)p.comando,"esc")) { //se è la esc
			invio_intero(sd,4);
			close(sd);
			exit(0); // termino il client
		}
		else if(strcmp((char*)p.comando,"book")) //se non è manco la book
		{
			printf("Comando non valido, riprovare\n");
			continue;
		}
		else
		{										  // se è la book invio codice : 2
			int scelta = atoi((char *)p.cognome); // indice della scelta dell'utente
			if (scelta <= 0)
			{
				printf("Hai inserito una opzione non valida\n");
				continue;
			}
			if (!find_prima_di_book)
			{ // se ho messo la book senza fare prima una find
				printf("Devi prima fare una find per poter effettuare la book\n");
				continue;
			}
			int l = 0;

			for (l = 0; l < 20; l++)
			{
				// nessun problema se mette scelta = 0, potrebbe fare match con gli indici inizializzati a 0, ma ho gia fatto il controllo prima su scelta = 0
				// controllo se la scelta è tra quelle disponibili
				if (indici[l] == scelta)
				{
					// ho trovato la scelta tra gli indici buoni
					buono = true;
					break;
				}
			}
			if (!buono)
			{
				printf("Hai scelto una opzione non presente tra le disponibili\n");
				continue;
			}
			if (!invio_intero(sd, 2))
			{
				perror("errore in fase di invio codice book");
			}
		}
		//  verifico quale comando è stato immesso e se risulta valido
		if (com == 1)
		{
			// cerco la prenotazione, aspettando risposta dal server
			int lunghezza = 0;

			// cognome
			invio_stringa(sd, p.cognome);
			// numero persone
			invio_intero(sd, p.numero_persone);
			// data
			invio_stringa(sd, p.data);
			// ora
			invio_intero(sd, p.ora);

			// ricevo risposta sui tavoli disponibili
			int res;
			ricevo_intero(sd, &res); // ricevo conferma
			if (res == 0)
			{
				find_prima_di_book = false;
				printf("Nessun tavolo disponibile per la richiesta\n");
				continue;
			}

			if (recv(sd, (void *)&lunghezza, sizeof(lunghezza), 0) != sizeof(lunghezza))
			{
				perror("errore in fase di receive");
			}
			uint32_t l = ntohl(lunghezza);

			if (recv(sd, (void *)tav_disponibili, l, 0) != l)
			{
				perror("errore in fase di receive");
			}

			// li stampo
			printf("Lista tavoli disponibili:\n");
			int i;
			int cont = 0;

			for (i = 0; i < 20; i++)
			{
				if (tav_disponibili[i] != 0)
				{
					cont++;
					indici[cont - 1] = cont;			// mi salvo gli indici buoni
					arr[cont - 1] = tav_disponibili[i]; // array il cui indice sarà la scelta dell'utente, per arrivare all'id del tavolo scelto
					int sala = calcola_sala(tav_disponibili[i]);
					if(cont > 9){
						if(tav_disponibili[i] < 10){
							printf("%d) T%d  SALA%d %s\n", cont, tav_disponibili[i],sala,array[tav_disponibili[i]-1]);
						} else {
							printf("%d) T%d SALA%d %s\n", cont, tav_disponibili[i],sala,array[tav_disponibili[i]-1]);
						}
					} else {
						if(tav_disponibili[i] < 10){
							printf("%d)  T%d  SALA%d %s\n", cont, tav_disponibili[i],sala,array[tav_disponibili[i]-1]);
						} else {
							printf("%d)  T%d SALA%d %s\n", cont, tav_disponibili[i],sala,array[tav_disponibili[i]-1]);
						}
					}
					
				}
			}
		}
		else if (com == 2)
		{
			int in = atoi((char *)p.cognome); // indice della scelta dell'utente
			t.numero_tavolo = arr[in - 1];	  // mi salvo l'id del tavolo
			invio_intero(sd, (int)t.numero_tavolo);
			// reinvio le informazioni
			invio_stringa(sd, p.cognome);

			// numero persone
			invio_intero(sd, p.numero_persone);

			// data
			invio_stringa(sd, p.data);

			// ora
			invio_intero(sd, p.ora);
			// ricevo intero di controllo della disponibilità del  tavolo
			int check = 0;
			ricevo_intero(sd, &check);
			if (check == 1)
			{
				printf("Tavolo scelto non più disponibile, rieffettuare una ricerca\n");
				continue;
			}

			// messaggio di attesa risposta
			printf("Attendi il completamento della prenotazione\n");
			//  ricezione del numero di tavolo
			recv(sd, (void *)&t.sala, sizeof(uint32_t), 0);
			// ricezione del codice
			char buff[20];
			ricevo_stringa(sd,(uint8_t*)buff);
			
			// stampo conferma e riepilogo prenotazione
			strcpy((char *)t.codice, buff);
			printf("PRENOTAZIONE EFFETTUATA\n Codice prenotazione : %s\n Numero tavolo : %d\n Sala : %d\n", t.codice, t.numero_tavolo, t.sala);

			free(p.cognome);
			free(p.data);
			free(p.comando);
			free(t.codice);
			free(tav_disponibili);
			close(sd); 
			exit(0);
		}
		else if (com == 3)
		{
	        free(p.cognome);
	        free(p.data);
	        free(p.comando);
	        free(t.codice);
	        free(tav_disponibili);
			close(sd);
			exit(0); // termino il client
		}
		else
		{ // se non è un comando conosciuto stampo errore
			printf("Comando non valido, riprovare\n");
		}
	}
	free(p.cognome);
	free(p.data);
	free(p.comando);
	free(t.codice);
	free(tav_disponibili);
	close(sd);
	return 0;
}
