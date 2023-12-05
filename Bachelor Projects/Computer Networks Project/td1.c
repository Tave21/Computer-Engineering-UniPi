#include "funzioni.h"

int NUMERO_TABLE_DEVICE  = 1; // cambiando questo, gestisco n-esimo table-device (tavolo)
// numero di comandi
int com = 0;
bool check_quantita = true;
struct piatto
{
   uint8_t id[3]; // A1...
   uint32_t quantita;
};
struct piatto appoggio[8]; // fino a 8 piatti per comanda, 8 piatti nel menu, identifica i-esima comanda da inviare

bool fun(char *tok)
{ // funzione che dato un pezzo salva id del piatto e quantità nell'array struttura piatti
   if (tok == NULL)
   {
      return true;
   }
   char *st = malloc(20 * sizeof(char));
   char *rest = tok;

   st = strtok_r(rest, "-", &rest);
   int u = calcola_codice_piatto(st); // A1 --> 0 ...
   if (u == 10)
   {
      // codice non valido di piatto
      return false;
   }
   strcpy((char *)appoggio[u].id, st); // il primo pezzo sarà l'id
   st = strtok_r(rest, "-", &rest);
   check_quantita = true;
   if(atoi(st) <= 0 || atoi(st) > 10) {
      //numero non valido, limite di 10 piatti a coppia id_piatto-numero_piatti
      check_quantita = false;
      return false;
   }
   int j = 0;
   for(j = 0; j < strlen(st);j++){
      if(!isdigit(st[j])){
         check_quantita = false;
         return false;
      } 
   }
   appoggio[u].quantita += atoi(st); // il secondo la quantita che converto in intero
   // ora l array struttura è aggiornata
   return true;
}

bool contax(const char *buf, char *comando)
{
   int num_parametri = 1;
   char string[50];

   strcpy(string, buf);
   string[strcspn(string, "\n")] = 0; // tolgo il ritorno carrello dalla fine stringa
   char *pezzo = strtok(string, " "); // prendo il primo pezzo, ovvero il comando
   strcpy(comando, pezzo);
   if ((!strcmp("conto", comando)) || (!strcmp("menu", comando) || !strcmp("help",comando)))
   {
      com = num_parametri; // sarà 1, comanda oppure conto
   }
   else if (!strcmp("comanda", pezzo))
   { // se è comanda estraggo i pezzo
      while (pezzo != NULL)
      {
         num_parametri++;
         pezzo = strtok(NULL, " ");
         if((num_parametri-1) == 1 && pezzo == NULL){ //se scrivo "comanda" e basta 
            return false;
         }
         // passo il "codice-quantità" del singolo piatto
         if (!fun(pezzo))
         {
            return false;
         }
      }
      num_parametri--;
      com = num_parametri;
   }
   else
   {
      com = 20; // codice errore, non ha inserito un comando valido
   }
   return true;
}

int main(int argc, char *argv[])
{
   //posso assegnare staticamente il numero del tavolo quando mando in esecuzione, utile per controllare se il codice immesso corrisponde al tavolo relativo 
   //int ntd = atoi(argv[2]);
   // dichiaro array che associa il codice di un piatto al suo id: A1 --> 0, A2 -->1, P1 --> 2, ..... D2 --> 7
   char arr[8][3];
   strcpy(arr[0], "A1\0");
   strcpy(arr[1], "A2\0");
   strcpy(arr[2], "P1\0");
   strcpy(arr[3], "P2\0");
   strcpy(arr[4], "S1\0");
   strcpy(arr[5], "S2\0");
   strcpy(arr[6], "D1\0");
   strcpy(arr[7], "D2\0");
   // inizializzo piatti
   int q = 0;
   for (q = 0; q < 8; q++)
   {
      appoggio[q].quantita = 0;
   }

   int ret;                   // sd socket di comunicazione
   //int porta = atoi(argv[1]); // salvo il numero di porta indicato
   const char *freccia = "   --> ";
   const char *fre = "--> ";
   const char *comando1 = "help";
   const char *text1 = "mostra i dettagli dei comandi";
   const char *comando2 = "menu";
   const char *text2 = "mostra il menu dei piatti";
   const char *comando3 = "comanda";
   const char *text3 = "invia una comanda";
   const char *comando4 = "conto";
   const char *text4 = "chiedi il conto";

   int costi[8] = {7, 8, 10, 6, 20, 15, 5, 5};
   char *comando = (char *)malloc(sizeof(char)*8);

   int i = 0;
   fd_set master;
   fd_set read_fds;
   int fdmax;

   struct sockaddr_in my_addr;

   /* Creazione socket */
   int sd = socket(AF_INET, SOCK_STREAM, 0);

   memset(&my_addr, 0, sizeof(my_addr));
   my_addr.sin_family = AF_INET;
   my_addr.sin_port = htons(4242);
   inet_pton(AF_INET, "127.0.0.1", &my_addr.sin_addr); // setto l'ip del mio socket, loopback, da presentation to numeric

   ret = connect(sd, (struct sockaddr *)&my_addr, sizeof(my_addr));

   if (ret < 0)
   {
      perror("connect non riuscita\n");
      exit(0);
   }
   invio_intero(sd, NUMERO_TABLE_DEVICE + 79);
   uint8_t *codice = (uint8_t *)malloc(sizeof(uint8_t) * 15); // codice di prenotazione per entrare nel table device

   while (1)
   {
      printf("Prego inserire Codice di Prenotazione:\n");
      fgets((char *)codice, 15, stdin);
      codice[strcspn((char*)codice, "\n")] = 0; // tolgo il ritorno carrello dalla fine stringa
      // invio comando di test
      invio_intero(sd, 3);
      // invio codice al server per verificare se è corretto
      invio_stringa(sd, codice);

      uint8_t buf[3];

      // ricevo risposta dal server
      ricevo_stringa(sd, buf);
      ricevo_intero(sd,&NUMERO_TABLE_DEVICE); //riceve 100 se è errato ma si ricicla nel while 
      /*
      if(NUMERO_TABLE_DEVICE != ntd){
         strcpy(buf,"err\0");
      }
      */
      
      if (!strcmp((char *)buf, "ok"))
      { // controllo se il server dà l'ok: il codice deve essere esistente e corrispondere al tavolo preciso
         printf("%s\n", buf);
         break; // sblocco il device
      }
      else if (!strcmp((char *)buf, "no"))
      { // errore, nessuna prenotazione corrispondente
         printf("Nessuna prenotazione corrispondente o di un altro tavolo, riprovare\n");
         continue;
      }
      /* //se volessi controllare anche se il codice immesso corrisponde al preciso tavolo scommento questo
      else //ricevo "err"
      {
         printf("Il codice inserito non corrisponde a questo table device, cambiare tavolo\n");
         continue;
      }
      */
      
   }

   printf("Sei entrato nel table device numero : %d!\n", NUMERO_TABLE_DEVICE); //qui potrei mettere ntd come ultimo campo, se facessi controllo codice-tavolo
   // rendo disponibile il menu delle operazioni

   printf("Lista comandi:\n -%s %s %s\n -%s %s %s\n -%s %s %s\n -%s%s %s\n", comando1, freccia, text1, comando2, freccia, text2, comando3, fre, text3, comando4, freccia, text4);
   // ciclo finché non immette un comando corretto o non si preme esc, fino a quel momento continuo a ricevere comandi

   // Reset FDs
   FD_ZERO(&master);
   FD_ZERO(&read_fds);

   FD_SET(0, &master);  // aggiungo standard input in ascolto
   FD_SET(sd, &master); // aggiungo socket di comunicazione al server

   // Aggiorno il massimo
   fdmax = sd;
   int comande_non_in_servizio = 0; //contatore delle comande non ancora in servizio, se ci fossero allora il cliente non può richidere il conto

   // main loop
   while (1)
   {

      // Inizializzo il set read_fds, manipolato dalla select()
      read_fds = master;

      // Mi blocco in attesa di descrittori pronti in lettura
      // imposto il timeout a infinito
      ret = select(fdmax + 1, &read_fds, NULL, NULL, NULL);
      if (ret < 0)
      {
         perror("ERRORE SELECT:");
         exit(1);
      }

      for (i = 0; i <= fdmax; i++)
      {
         // controllo se i è pronto
         if (FD_ISSET(i, &read_fds))
         {

            if (i == sd) // il server mi ha contattato
            {
               int stop = 0;
               ricevo_intero(i, &stop); // controllo se il server ha inviato il segnale di disconessione
               if (stop == 1024)
               {
                  free(codice);
                  free(comando);
                  close(sd);
                  exit(0);
               }
               int status = 0;
               ricevo_intero(sd, &status); // ricevo status della comanda
               int comanda = 0;
               ricevo_intero(sd, &comanda); // ricevo id comanda
               if (status == 2)
               { // stampo lo stato attuale appena ricevuto della comanda
                  printf("comanda com%d in preparazione\n", comanda);
               }
               else
               {
                  comande_non_in_servizio--;
                  printf("comanda com%d in servizio\n", comanda);
               }
            }
            else if (i == 0) // standard input
            {

               char ingr[50];
               bool result = false;
               while (fgets(ingr, 50, stdin))
               { // per prendere diverso numero di parametri input da stdin
                  result = contax(ingr, comando);
                  break;
               }
               if (!result)
               {
                  if (!check_quantita)
                  {
                     printf("Hai inserito una quantità errata\n");
                  }
                  else
                  {
                     printf("Uno dei codici che hai inserito è errato\n");
                  }
                  continue;
               }
               if (!strcmp(comando, comando3))
               { // se è la comanda invio: 4 (codice comanda per lo switch dei comandi nel server , vedi server)
                  invio_intero(sd, 4);
                  invio_intero(sd, NUMERO_TABLE_DEVICE);
               }
               else if (!strcmp(comando, comando4))
               { // se è il conto invio codice : 5
                  if(comande_non_in_servizio > 0){
                     printf("Non puoi richiedere il conto se ci sono ancora comande in attesa/in preparazione\n");
                     continue;
                  }
                  invio_intero(sd, 5);
               }
               // verifico quale comando è stato immesso e se risulta valido
               if (com == 1)
               { // è il conto richiesto oppure il menu
                  if (!strcmp(comando, comando2))
                  { // ha immesso menu, lo mostro
                     printf("A1 - Antipasto di terra           7\n");
                     printf("A2 - Antipasto di mare 	     	  8\n");
                     printf("P1 - Spaghetti ai muscoli        10\n");
                     printf("P2 - Rigatoni all'amatriciana     6\n");
                     printf("S1 - Frittura di calamari        20\n");
                     printf("S2 - Arrosto misto               15\n");
                     printf("D1 - Crostata di mele             5\n");
                     printf("D2 - Zuppa inglese                5\n");
                  }
                  else if (!strcmp(comando, comando4))
                  {                                         // ha immesso conto
                     invio_intero(sd, NUMERO_TABLE_DEVICE); // invio il numero del table device per fargli trovare il conto indicato
                     int z = 0;
                     int ws = 0;
                     int costo = 0;
                     int totale = 0;
                     for (z = 0; z < 8; z++)
                     {
                        ricevo_intero(sd, &ws); // ricevo la quantita in ordine, dal primo piatto all'ultimo
                        costo = costi[z] * ws;
                        totale += costo;
                        if (ws)
                        { // se la quantita è diversa da 0 stampo
                           printf("%s %d %d\n", arr[z], ws, costo);
                        }
                     }
                     printf("Totale: %d\n", totale);
                     close(sd);
                     exit(0);
                  }
                  else if (!strcmp(comando, "help"))
                  {
                     // siamo nella help
                     printf("Per conto e menu basta digitare il preciso comando\n");
                     printf("Per ordinare una comanda digitare: comanda cod1-quantita1 cod2-quantita2... \n");
                  }
                  else
                  {
                     printf("Non hai inserito un comando valido, riprova\n");
                  }
               }
               else if (com <= 19)
               {
                  // devo gestire la comanda
                  printf("COMANDA RICEVUTA\n");
                  int l = 0;

                  for (l = 0; l < 8; l++)
                  {                            // ciclo per quanti piatti sono stati inseriti
                     if (!appoggio[l].quantita) // se è uno ordinato
                     {
                       calcola_id_piatto(l,(char*)appoggio[l].id);    
                     }
                     invio_stringa(sd, appoggio[l].id);      // invio il codice del singolo piatto
                     invio_intero(sd, appoggio[l].quantita); // invio la quantità relativa al piatto
                     appoggio[l].quantita = 0;
                  }
                  int id_com = 0;
                  printf("attendo\n");
                  ricevo_intero(sd, &id_com);
                  printf("com%d in attesa\n", id_com);
                  comande_non_in_servizio++;
               }
               else
               { // com = 20
                  printf("non hai inserito un comando valido, riprova\n");
               }
            }
         }
      }
   }
   free(codice);
   free(comando);
   printf("CHIUDO IL SOCKET DI COMUNICAZIONE!\n");
   fflush(stdout);
   close(sd);
}