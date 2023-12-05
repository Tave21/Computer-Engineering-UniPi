
#include "funzioni.h"
struct fd_associato_codice_pren {
	int fd; //file descriptor
	int tavolo; //numero tavolo associato
};
struct fd_associato_codice_pren td_file_descriptor[20];		// salvo i fd dei table device
int kd_file_descriptor[10];		// quelli dei kitchen device
int client_file_descriptor[70]; // quelli dei client
// xx_f_d[0] --> xx numero 0 (che sia tavolo, cuoco o cliente)

int check_device(int sd)
{
	int i = 0;
	for (i = 0; i < 10; i++)
	{
		if (kd_file_descriptor[i] == sd)
		{
			return 2;
		}
	}

	for (i = 0; i < 20; i++)
	{
		if (td_file_descriptor[i].fd == sd)
		{
			return 3;
		}
	}
	for (i = 0; i < 70; i++)
	{
		if (client_file_descriptor[i] == sd)
		{
			return 1;
		}
	}
	return 0;
}

void conta(char *buf, char *comando, char *arg1)
{
	sscanf(buf, "%s %s", comando, arg1);
}

int main(int argc, char **argv)
{
	char *comando = (char *)malloc(sizeof(char) * 10);
	char *arg1 = (char *)malloc(sizeof(char) * 10);
	int o = 0;
	for (o = 0; o < 10; o++)
	{
		kd_file_descriptor[o] = 0; // inizializzo
	}
	for (o = 0; o < 20; o++)
	{
		td_file_descriptor[o].fd = 0; // inizializzo
		td_file_descriptor[o].tavolo = 0;
	}
	for (o = 0; o < 70; o++)
	{
		client_file_descriptor[o] = 0; // inizializzo
	}
	int contatore_comande_pendenti = 0;
	int i = 0;
	char arr[8][3];
	strcpy(arr[0], "A1\0");
	strcpy(arr[1], "A2\0");
	strcpy(arr[2], "P1\0");
	strcpy(arr[3], "P2\0");
	strcpy(arr[4], "S1\0");
	strcpy(arr[5], "S2\0");
	strcpy(arr[6], "D1\0");
	strcpy(arr[7], "D2\0");
	int indice = 0; // indice per l'array accettate
	// dichiaro strutture di trasferimento protocollo binary
	struct prenotazione
	{ // prenotazione da cercare
		uint8_t *cognome;
		uint8_t *data;
		int ora;
		int numero_persone;
	};
	// dichiaro un array di array di strutture "piatto" per salvarmi fino a 20 piatti per ogni tavolo
	struct piatto
	{
		int cod;
		int quantita;
	};
	struct piatto qpiatti[20][8]; // 20 tavoli, 8 piatti del menu
								  // qpiatto[i-1][cod] è la quantita del piatto di codice cod nel tavolo i ordinata fino ad ora
	int z;
	int h;
	for (z = 0; z < 20; z++) // inizializzo tutte le quantità a 0 per tutti i codici di ogni piatto su tutti i tavoli
	{
		for (h = 0; h < 8; h++)
		{
			qpiatti[z][h].cod = h;
			qpiatti[z][h].quantita = 0;
		}
	}

	struct prenotazione p;
	p.cognome = (uint8_t *)malloc(20);
	p.data = (uint8_t *)malloc(9);

	struct conferma
	{
		uint8_t *codice;
		int numero_tavolo;
		uint32_t sala;
	};
	struct conferma t;
	t.codice = (uint8_t *)malloc(20);

	// dichiaro array di struct per salvarmi i tavoli presenti in sala
	struct tavolo
	{
		int id;			  // id del tavolo, es: T1,T2..
		char num_persone; // numero di persone del tavolo, es: 8
	};
	struct tavolo tavoli[20];
	// inizializzo id e numero persone
	for (i = 0; i < 20; i++)
	{
		tavoli[i].id = i + 1;
	}

	struct comanda
	{
		uint32_t id_comanda;	 // id della comanda, es: 1 --> com1 , 2--> com2 ...
		uint32_t tavolo;		 // id del tavolo, es: 1 --> T1
		struct piatto piatti[8]; // fino a 8 piatti per comanda, 8 piatti nel menu
		uint8_t status;			 // 0 inizializzato, 1 in attesa, 2 in preparazione ,3 in servizio
		uint32_t valida;
	};

	struct comanda arrivate[400]; // pongo il limite sul numero di comande arrivate, caso peggiore 20 comande a tavolo, per 20 tavoli = 400
	int q = 0;
	// inizializzo tutte le comande
	for (i = 0; i < 400; i++)
	{
		arrivate[i].tavolo = 0;			// inizializzo tavolo
		arrivate[i].valida = 0;			// all'inizio nessuna è valida
		arrivate[i].id_comanda = i + 1; // setto già i codici comande
		arrivate[i].status = 0;			// inizializzo status
		for (q = 0; q < 8; q++)
		{
			arrivate[i].piatti[q].cod = calcola_codice_piatto(arr[q]); // inserisco tutti i codici numerici delle comande
			arrivate[i].piatti[q].quantita = 0;
		}
	}

	// setto numero persone per tavolo
	tavoli[0].num_persone = 10;
	tavoli[1].num_persone = 6;
	tavoli[2].num_persone = 2;
	tavoli[3].num_persone = 8;
	tavoli[4].num_persone = 6;
	tavoli[5].num_persone = 3;
	tavoli[6].num_persone = 8;
	tavoli[7].num_persone = 12;
	tavoli[8].num_persone = 6;
	tavoli[9].num_persone = 6;
	tavoli[10].num_persone = 8;
	tavoli[11].num_persone = 10;
	tavoli[12].num_persone = 4;
	tavoli[13].num_persone = 5;
	tavoli[14].num_persone = 11;
	tavoli[15].num_persone = 9;
	tavoli[16].num_persone = 8;
	tavoli[17].num_persone = 4;
	tavoli[18].num_persone = 9;
	tavoli[19].num_persone = 6;

	// dichiaro array di struct per salvarmi le prenotazioni registrate
	struct preno
	{
		time_t rawtime;
		uint8_t *codice;
		bool effettuata; // tiene conto della validità di ogni prenotazione, utile per non scorrere tutto l'array
		char *cognome;
		char *data;
		int ora;
		int numero_persone;
		int id; // id del tavolo, es: T1,T2..
	};
	struct preno booking[1000];

	// inizializzo a false tutte le prenotazioni, perché all'inizio non ce ne sono
	for (i = 0; i < 1000; i++)
	{
		booking[i].cognome = malloc(sizeof(uint8_t) * 20);
		booking[i].codice = malloc(sizeof(uint8_t) * 15);
		booking[i].data = malloc(sizeof(uint8_t) * 9);
		booking[i].effettuata = false;
	}

	// creo strutture per indirizzo server e client
	struct sockaddr_in cl_addr, sv_addr;
	//int porta = atoi(argv[1]);

	// setting dei descrittori per socket client da registrare
	fd_set master;	// set principale dove ci saranno quelli registrati
	fd_set lettura; // set copia per analizzare il bidone master con la select()

	// dichiaro gli interi di mio interesse
	int newfd, listener;
	socklen_t lunghezza_indirizzo;
	int fdmax; // numero massimo di descrittori

	// creo il socket di ascolto, dove riceverò le richieste di connessione per prenotarsi
	listener = socket(AF_INET, SOCK_STREAM, 0);

	// setto indirizzo del server tramite la struttura
	memset(&sv_addr, 0, sizeof(sv_addr)); // inizializzo la memoria a 0
	sv_addr.sin_family = AF_INET;
	sv_addr.sin_addr.s_addr = INADDR_ANY; // metto il server in ascolto su tutte le interfacce(IP)
	sv_addr.sin_port = htons(4242);	  // setto la porta

	// faccio la bind
	int ret = 0;
	ret = bind(listener, (struct sockaddr *)&sv_addr, sizeof(sv_addr)); // associo la struttura my_addr
																		//  al socket, ovvero ip e porta dove ricevere
	if (ret < 0)
	{
		perror("Errore in fase di bind: \n");
		exit(0);
	}

	// metto in ascolto per richieste di connessione
	ret = listen(listener, 24); // questo socket sarà usato per ricevere richieste di connessione
								//  è un socket di ascolto, non verrà eliminato. Coda con 24 posizioni:1 client+ 3 kd + 20 td

	// all'inizio i due set sono uguali, posso azzerarli anche se non serve
	FD_ZERO(&master);
	FD_ZERO(&lettura);

	// aggiungo il listener al set principale
	FD_SET(listener, &master);
	FD_SET(0, &master); // metto nei controllati lo standard input
	// tengo traccia del maggiore
	fdmax = listener;
	//indici per salvare i fd
	int num_kd = 0;
	int num_td = 0;
	int num_cli = 0;
	while (1)
	{														 // while per renderlo iterativo
		lettura = master;									 // inizializzo il bidone di copia con il solo listener
		ret = select(fdmax + 1, &lettura, NULL, NULL, NULL); // si blocca finché non ci sono descrittori pronti in lettura
		if (ret < 0)
		{
			perror("Errore select");
			exit(1); // errore brutto
		}

		for (i = 0; i <= fdmax; i++)
		{ // scorro il set di copia
			if (FD_ISSET(i, &lettura))
			{ // controllo se il bidone è presente nell'insieme di copia
				if (i == 0)
				{ // standard input in ascolto
					char buf[50];
					// prendo in ingresso un comando
					while (fgets(buf, 50, stdin))
					{ // per prendere diverso numero di parametri input da stdin
						conta(buf, comando, arg1);
						break;
					}
					if (!strcmp(comando, "stop"))
					{
						// ha immesso stop
						int k = 0;
						bool cec = false;
						for (k = 0; k < 400; k++)
						{
							if (arrivate[k].status == 1 || arrivate[k].status == 2)
							{
								printf("Non puoi arrestare il server, comande in attesa o in preparazione\n");
								cec = true;
								break;
							}
						}
						if (cec)
						{ // se non posso arrestare tutto
							continue;
						}
						for (k = 0; k < 10; k++)
						{
							if (kd_file_descriptor[k] != 0)
							{
								invio_intero(kd_file_descriptor[k], 1024); // 1024 codice disconnessione
								close(kd_file_descriptor[k]);
							}
						}
						for (k = 0; k < 20; k++)
						{
							if (td_file_descriptor[k].fd != 0)
							{
								invio_intero(td_file_descriptor[k].fd, 1024); // 1024 codice disconnessione
								close(td_file_descriptor[k].fd);
							}
						}
						close(listener);
						//libero tutto
						for (k = 0; k < 1000; k++)
						{
							free(booking[k].cognome);
							free(booking[k].codice);
							free(booking[k].data);
						}
						printf("* disconnessione *\n");
						free(p.data);
						free(t.codice);
						free(p.cognome);
						exit(0);
					}
					else if (!strcmp(comando, "stat"))
					{
						// ha immesso stat
						if (arg1[0] == 'T' && (strlen(arg1) < 4))
						{
							arg1++;
							int id_tavolo = atoi(arg1); // prendo solo il numero
							if (id_tavolo <= 0 || id_tavolo > 20)
							{
								printf("codice tavolo non corretto, riprova\n");
								continue;
							}
							// mostro comande relative al pasto in corso
							int x = 0;
							char *stat = (char *)malloc(sizeof(char) * 20);
							char *cod = (char *)malloc(sizeof(char) * 20);

							for (x = 0; x < 400; x++)
							{
								if (arrivate[x].tavolo == id_tavolo)
								{
									// printf("trovata\n");
									if (arrivate[x].status == 1)
									{
										strcpy(stat, "in attesa");
									}
									else if (arrivate[x].status == 2)
									{
										strcpy(stat, "in preparazione");
									}
									else
									{
										strcpy(stat, "in servizio");
									}
									printf("com%d <%s>\n", arrivate[x].id_comanda, stat);
									for (q = 0; q < 8; q++)
									{
										calcola_id_piatto(arrivate[x].piatti[q].cod, cod);
										if (arrivate[x].piatti[q].quantita)
										{
											printf("%s %d\n", cod, arrivate[x].piatti[q].quantita);
										}
									}
								}
							}
							free(stat);
							free(cod);
						}
						else if ((arg1[0] == 'p' || arg1[0] == 'a' || arg1[0] == 's') && strlen(arg1) < 2)
						{
							int sttus = 0;
							if (arg1[0] == 'p')
							{
								sttus = 2;
							}
							else if (arg1[0] == 'a')
							{
								sttus = 1;
							}
							else
							{
								sttus = 3;
							}
							int x = 0;
							char *cod = (char *)malloc(sizeof(char) * 20);
							for (x = 0; x < 400; x++)
							{
								if (arrivate[x].status == sttus)
								{
									printf("com%d T%d\n", arrivate[x].id_comanda, arrivate[x].tavolo);
									for (q = 0; q < 8; q++)
									{
										calcola_id_piatto(arrivate[x].piatti[q].cod, cod);
										if (arrivate[x].piatti[q].quantita)
										{
											printf("%s %d\n", cod, arrivate[x].piatti[q].quantita);
										}
									}
								}
							}
						}
						else
						{
							printf("codice tavolo/status non corretto, riprova\n");
							continue;
						}
					}
					else
					{
						printf("hai immesso un comando non valido\n");
						continue;
					}
				}
				else if (i == listener)
				{ // se quello che scorro è proprio il listener
					printf("nuovo client connesso!\n");
					// prenotazione: accetto nuove connessioni da parte dei client
					lunghezza_indirizzo = sizeof(cl_addr);
					newfd = accept(listener, (struct sockaddr *)&cl_addr, &lunghezza_indirizzo); // creo socket
																								 // di comunicazione per inviare e ricevere i dati
					FD_SET(newfd, &master);														 // aggiungo il socket del nuovo client connesso
					// lo metto in quello originale perché deve continuare ad essere monitorato
					// quello di copia viene inizializzato con il master e serve solo per la select
					if (newfd > fdmax)
					{
						fdmax = newfd;
					} // se sfora aggiorno fdmax
				}
				else
				{
					int y = check_device(i);
					if (!y)
					{ // se non è un file descriptor gia visto
						int fd = 0;
						ricevo_intero(i, &fd);

						if (fd < 10)
						{
							// sono un kitchen device
							kd_file_descriptor[num_kd] = i;
							num_kd++;
						}
						else if (fd >= 10 && fd < 80)
						{
							// sono un client
							client_file_descriptor[num_cli] = i;
							num_cli++;
						}
						else if (fd >= 80)
						{
							// sono un table device
							td_file_descriptor[num_td].fd = i; //salvo in ordine i tavoli connessi
							//mi devo calcolare il tavolo tramite il codice prenotazione da associare
							num_td++;
						}
						y = check_device(i); // lo aggiorno, prima era 0
						continue;			 // se è la prima volta non sarà un comando allora mi riblocco sulla select
					}

					if (y == 1) // siamo in un client
					{			// se non è il listener ad essere pronto ma è un client che richiede una prenotazione, è il socket connesso
						int numero = 0;
						ricevo_intero(i, &numero);
						
						int k = 0;
						switch (numero)
						{
						case 4: // ho ricevuto esc
						{
							for (k = 0; k < 70; k++)
							{
								if (client_file_descriptor[k] == i)
								{
									client_file_descriptor[k] = 0;
								}
							}
							num_cli--;
							close(i);
							FD_CLR(i, &master);
							break;
						}
						case 1: // siamo nella find
						{
							// ricevo la lunghezza del campo della struttura
							printf("Ricevo dati di ricerca:\n");
							// cognome
							ricevo_stringa(i, p.cognome);
							printf("cognome: %s\n", p.cognome);

							// numero persone
							ricevo_intero(i, &p.numero_persone);
							printf("numero persone: %d\n", p.numero_persone);
							// data
							ricevo_stringa(i, p.data);
							printf("data: %s\n", p.data);
							// ora
							ricevo_intero(i, &p.ora);
							printf("ora : %d\n", p.ora);

							// struttura dati prenotazione completata, ora devo fare check sulla disponibilità
							printf("* cerco tavolo libero *\n");

							struct tavolo tav_papabili[20]; // tavoli che possono ospitare almeno il num persone richiesto
							for (k = 0; k < 20; k++)
							{
								if (p.numero_persone <= tavoli[k].num_persone && ((tavoli[k].num_persone - p.numero_persone) <= 3))
								{									   // se è troppo grosso il tavolo
									tav_papabili[k].id = tavoli[k].id; // salvo l'id
								}
								else
								{
									tav_papabili[k].id = 0; // lo 0  indica che non è papabile e non sarà nemmeno prenotabile
								}
							}
							int x;
							struct tavolo tav_prenotabili[20]; // tavoli che rispettano i dettagli richiesti dall'utente
							// con questo for cerco i tavoli prenotabili da mostrare all'utente che ha cercato
							bool trovato = false;
							for (k = 0; k < 20; k++)
							{							   // per ogni tavolo scorro tutte le prenotazioni relative, ma mi concentro solo sui papabili col primo if
								tav_prenotabili[k].id = 0; // all'inizio lo considero non prenotabile

								for (x = 0; x < 1000; x++)
								{ // scorro l'array delle prenotazioni
									if (!tav_papabili[k].id)
									{
										break; // se il tavolo non è papabile manco scorro le prenotazioni
									}
									if (!booking[x].effettuata && x == 0)
									{												// se non ci sono proprio prenotazioni
										tav_prenotabili[k].id = tav_papabili[k].id; // il papabile è prenotabile
										break;
									}
									if (!booking[x].effettuata) // se sono arrivato in fondo alle prenotazioni effettuate
									{
										tav_prenotabili[k].id = tav_papabili[k].id; // il papabile è prenotabile
										break;
									}

									// se coincidono: id del tavolo papabile con quello prenotato, la data di prenotazione
									if (booking[x].effettuata && tav_papabili[k].id == booking[x].id && !strcmp((const char *)p.data, (const char *)booking[x].data))
									{
										if (p.ora + 2 <= booking[x].ora || p.ora >= booking[x].ora + 2)
										{												// se l'ora non è nelle due ore vicine ad un'ora già prenotata
											tav_prenotabili[k].id = tav_papabili[k].id; // il papabile è prenotabile
										}
										else
										{
											tav_prenotabili[k].id = 0; // il papabile non è prenotabile, esco dal for interno escludendo questo papabile
																	   // se lo fosse stato in precedenza, lo 0 sovrascrive e non si considera più prenotabile
											break;
										}
									}
								}
								if (tav_prenotabili[k].id)
								{ // se almeno un tavolo è prenotabile
									trovato = true;
								}
							}
							if (!trovato)
							{ // se la lista è vuota
								printf("non ho trovato nessun tavolo\n");
								invio_intero(i, 0);
							}
							else
							{ // se non è vuota invio 1 per escludere l'if di controllo nel client e invio i tavoli
								invio_intero(i, 1);
								int array[20];
								for (k = 0; k < 20; k++)
								{
									array[k] = 0;
									if (tav_prenotabili[k].id != 0)
									{
										array[k] = tav_prenotabili[k].id; // array[0] = 1 (cioè T1) e cosi via
									}
								}

								// invio i tavoli prenotabili, sono indicati dentro array
								uint32_t lunghezza = 0;
								lunghezza = 80; // array di 20 int
								uint32_t l = htonl(lunghezza);
								if (send(i, (void *)&l, sizeof(l), 0) != sizeof(l))
								{
									perror("errore in fase di send");
								}
								if (send(i, (void *)array, lunghezza, 0) != lunghezza)
								{
									perror("errore in fase di send");
								}
							}

							break;
						}
						case 2:
						{
							printf("* processo la book *\n");
							// se è la book
							// devo salvare la prenotazione
							ricevo_intero(i, &t.numero_tavolo);
							// ricevo nuovamente i dati
							// cognome
							ricevo_stringa(i, p.cognome);
							// numero persone
							ricevo_intero(i, &p.numero_persone);
							// data
							ricevo_stringa(i, p.data);
							// ora
							ricevo_intero(i, &p.ora);

							// faccio un check se il tavolo nel frattempo è stato prenotato
							bool top = true;
							for (k = 0; k < 1000; k++)
							{ // se è stata effettuata, stesso num tavolo, stessa data e ora vicina allora segnalo che non può piu sceglierlo
								if (booking[k].effettuata && booking[k].id == t.numero_tavolo && !strcmp(booking[k].data, (char *)p.data) && !(p.ora + 2 <= booking[k].ora || p.ora >= booking[k].ora + 2))
								{
									top = false;
									break;
								}
							}
							if (!top)
							{
								invio_intero(i, 1); // 1 indica che non è piu disponibile
								break;				// esco dallo switch
							}
							else
							{
								invio_intero(i, 2); // 2 indica che è ancora disponibile
							}

							// calcolo il numero della sala
							t.sala = calcola_sala(t.numero_tavolo);
							
							send(i, (void *)&t.sala, sizeof(uint32_t), 0);
							// calcolo codice concatenando tavolo, data, ora
							char codice[20] = "";
							char null[1] = {'T'};
							strcat(codice, null); // concateno T
							char str[6];
							sprintf(str, "%d", t.numero_tavolo);
							strcat(codice, str); // concateno num tavolo
							strcat(codice, (char *)p.data);
							char str2[6];
							sprintf(str2, "%d", p.ora);
							strcat(codice, str2); // concateno ora
							for (k = 0; k < 1000; k++)
							{
								if (!booking[k].effettuata)
								{
									booking[k].effettuata = true;				   // segno come prenotata
									booking[k].ora = p.ora;						   // salvo ora
									booking[k].numero_persone = p.numero_persone;  // salvo num pers
									strcpy(booking[k].cognome, (char *)p.cognome); // salvo cognome
									strcpy(booking[k].data, (char *)p.data);	   // salvo data
									strcpy((char *)booking[k].codice, codice);	   // salvo codice
									booking[k].id = t.numero_tavolo;			   // salvo num tavolo
									time(&booking[k].rawtime);					   // inizializzo il timestamp della prenotazione
									break;
								}
							}
							invio_stringa(i, (uint8_t *)codice);

							printf("Prenotazione completata, codice %s attivo per accedere al tavolo il giorno della prenotazione\n", codice);
							for (k = 0; k < 70; k++)
							{
								if (client_file_descriptor[k] == i)
								{
									client_file_descriptor[k] = 0;
								}
							}
							num_cli--;
							close(i);
							FD_CLR(i, &master);
							break;
						}
						}
					}
					else if (y == 2) // siamo in un kd
					{
						int numero = 0;
						ricevo_intero(i, &numero);
						int k = 0;
						switch (numero)
						{
						case 6: // ho ricevuto l'accettazione di una comanda da parte di un kd
						{
							printf("* accettazione comanda *\n");
							contatore_comande_pendenti--;
							int x = 0;
							bool presente = false;
							for (k = 0; k < 400; k++) // scorro le comande in attesa
							{
								if (arrivate[k].valida)
								{ // prendo la piu vecchia
									presente = true;
									invio_intero(i, arrivate[k].id_comanda); // invio id comanda
									invio_intero(i, arrivate[k].tavolo);
									for (x = 0; x < 8; x++)
									{
										invio_intero(i, arrivate[k].piatti[x].quantita); // invio le quantita per ogni piatto, anche se sono 0
									}
									arrivate[k].valida = 0; // già accettata
									arrivate[k].status = 2; // in preparazione
									// posso anche non resettare le altre impostazioni
									break;
								}
							}
							if (!presente)
							{
								invio_intero(i, 0); // 0 indica nessuna comanda presente
								break;				// esco dal case
							}
							else
							{																					  // invio stato in prepazione al td corrispondente
								//recupero file descriptor del tavolo
								int fdes = 0;
								for(x = 0; x <20 ;x++){
									if(td_file_descriptor[x].tavolo == arrivate[k].tavolo){
										fdes = td_file_descriptor[x].fd;
										break;
									}
								}
								invio_intero(fdes, 0);					  // stop = 0, non stoppo
								invio_intero(fdes, 2);					  // 2--> in preparazione
								invio_intero(fdes, arrivate[k].id_comanda); // invio id comanda
								//invio numero comande aggiornate dopo la take
								for (k = 0; k < 10; k++)
								{ // invio notifiche a tutti i kitchen device
									if (kd_file_descriptor[k] != 0)
									{
										invio_intero(kd_file_descriptor[k], 0);							 // stop = 0, non stoppo
										invio_intero(kd_file_descriptor[k], contatore_comande_pendenti); // invio numero di comande pendenti
									}																	 // se esiste
								}
							}
							break;
						}

						case 7: // ho ricevuto la set
						{
							printf("* set comanda *\n");
							int tavolo = 0;
							int id_comanda = 0; // id comanda in servizio
							ricevo_intero(i, &id_comanda);
							ricevo_intero(i, &tavolo); // tavolo riguardante
							for (k = 0; k < 400; k++)
							{
								// non servono controlli perché gia corretto l'id
								if (arrivate[k].id_comanda == id_comanda)
								{
									arrivate[k].status = 3; // in servizio
								}
							}
							//recupero file descriptor del tavolo
							int fdes = 0;
							int x = 0;
							for(x = 0; x <20 ;x++){
								if(td_file_descriptor[x].tavolo == tavolo){
									fdes = td_file_descriptor[x].fd;
									break;
								}
							}
							// segnalo che è in servizio quella comanda
							invio_intero(fdes, 0);		  // stop = 0, non stoppo
							invio_intero(fdes, 3);		  // 3 -->in servizio
							invio_intero(fdes, id_comanda); // invio id_comanda

							break;
						}
						}
					}
					else if (y == 3)
					{ // siamo un table device
						
						int numero = 0;
						ricevo_intero(i, &numero);
						int k = 0;
						switch (numero)
						{
						case 3: // test del codice inserito nel table device
						{
							printf("sto testando il codice inserito\n");
							uint8_t codi[15];
							ricevo_stringa(i, codi); // ricevo il codice prenotazione inserito nel td
							int tav = 0;
							bool trovata = false;
							int j = 0;
							//  controllo che sia esistente nelle prenotazioni booking
							for (k = 0; k < 1000; k++)
							{
								if (!booking[k].effettuata) // se sono arrivato in fondo alla lista
									break;
								if (!strcmp((const char *)booking[k].codice, (const char *)codi))
								{
									//se ho trovato mi salvo il file descriptor associato	
									for ( j = 0; j < 20; j++)
									{
										if(td_file_descriptor[j].fd == i){
											td_file_descriptor[j].tavolo = booking[k].id; //tavolo associato al suo fd
											tav = booking[k].id;
											break;
										}
									}
									trovata = true;
									break;
								}
							}
							uint8_t no[2] = "no";
							uint8_t okk[2] = "ok";
							if (!trovata)
							{ // se ha visto tutta la lista e non esiste invio "no"
								invio_stringa(i, no);
								invio_intero(i,100); //100 indica scorretto
							}
							else
							{ // se esiste
									invio_stringa(i, okk);
									invio_intero(i,tav);//gli dico che tavolo è quello della prenotazione
							}

							break;
						}
						case 4: // se il comando è "comanda"
						{
							printf("* salvataggio nuova comanda *\n");
							contatore_comande_pendenti++;
							int tab_dev = 0;
							ricevo_intero(i, &tab_dev);
							tab_dev--;
							int r = 0;			   // quantita
							int u = 0;
							char cod[3];
							for (k = 0; k < 8; k++)
							{
								ricevo_stringa(i, (uint8_t *)cod);
								u = calcola_codice_piatto(cod);
								// in "u" ho il codice del piatto della comanda
								ricevo_intero(i, &r); // sommo ad ogni comanda la quantità relativa alla precedente
								// in "r" ho la quantita del piatto
								qpiatti[tab_dev][u].quantita += r;		  // tab_dev ho il numero di tavolo, in "u" ho il codice piatto
								arrivate[indice].piatti[u].quantita += r; // metto direttamente in posizione u (piatti[0] contiene il piatto di codice 0, cioè A1)
							}
							arrivate[indice].id_comanda = indice + 1; // setto come id l'indice per il salvataggio + 1
							arrivate[indice].tavolo = tab_dev + 1;	  // setto il tavolo della comanda
							arrivate[indice].valida = 1;			  // diventa valida
							arrivate[indice].status = 1;			  // in attesa
							indice++;								  // aggiorno indice array accettate
							invio_intero(i, arrivate[indice - 1].id_comanda);

							for (k = 0; k < 10; k++)
							{ // invio notifiche a tutti i kitchen device
								if (kd_file_descriptor[k] != 0)
								{
									invio_intero(kd_file_descriptor[k], 0);							 // stop = 0, non stoppo
									invio_intero(kd_file_descriptor[k], contatore_comande_pendenti); // invio numero di comande pendenti
								}																	 // se esiste
							}
							if(indice == 400){
								indice = 0;
							}	

							break;
						}
						case 5: // se è il conto
						{
							printf("*sto facendo il conto*\n");
							int t = 0;
							ricevo_intero(i, &t); // ricevo il numero di table device
							int quantit = 0;
							for (k = 0; k < 8; k++)
							{
								quantit = qpiatti[t - 1][k].quantita; // invio quantità ordinata per ogni piatto
								invio_intero(i, quantit);
							}
							int m = 0;
							// azzero i piatti ordinati dal tavolo y
							for (m = 0; m < 8; m++)
							{
								qpiatti[t - 1][m].quantita = 0;
							}
							for (k = 0; k < 20; k++)
							{
								if (td_file_descriptor[k].fd == i)
								{
									td_file_descriptor[k].fd = 0;
									td_file_descriptor[k].tavolo= 0;
								}
							}
							num_td--;
							FD_CLR(i, &master);
							close(i);
							break;
						}
						}
					}
				}
			}
		}
	}
	printf("chiudo il listener\n");
	close(listener);
	fflush(stdout);
	for (i = 0; i < 1000; i++)
	{
		free(booking[i].cognome);
		free(booking[i].codice);
		free(booking[i].data);
	}
	free(p.data);
	free(t.codice);
	free(p.cognome);
	return 0;
} // main
