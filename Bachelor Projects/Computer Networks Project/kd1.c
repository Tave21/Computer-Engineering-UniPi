#include "funzioni.h"

// numero di comandi
int com = 0;

#define NUMERO_KITCHEN_DEVICE 1 // cambiando questo, gestisco n-esimo kitchen-device (ipad del cuoco)
struct piatto
{
    uint32_t cod;      // 1-->A1, 2--> A2 ecc..  codice del piatto
    uint32_t quantita; // per piatto
};

struct comanda
{
    uint32_t id_comanda;     // id della comanda, es: 1--> com1
    uint32_t tavolo;         // id del tavolo, es: 1 --> T1
    struct piatto piatti[8]; // fino a 8 piatti per comanda, 8 piatti nel menu
};

struct comanda accettate[10]; // pongo il limite sul numero di comande accettate

void show(char arr[8][3])
{ // mostra le comande accettate
    int i = 0;
    int x = 0;
    bool ok = false;
    for (i = 0; i < 10; i++)
    {
        if (accettate[i].id_comanda != 0)
            printf("com%d T%d\n", accettate[i].id_comanda, accettate[i].tavolo);
        for (x = 0; x < 8; x++)
        {
            if (accettate[i].piatti[x].quantita)
            { // se la quantita è positiva
                ok = true;
                printf("%s  %d\n", arr[x], accettate[i].piatti[x].quantita);
            }
        }
    }
    if (!ok)
    {
        // nessuna comanda presente
        printf("Nessuna comanda attualmente accettata\n");
    }
}

void conto(char *buf, char *comando, char *codice_comanda, char *tavolo)
{
    sscanf(buf, "%s %s %s", comando, codice_comanda, tavolo);
    return;
}

int main(int argc, char *argv[])
{
    char *tavolo = (char *)malloc(10);         // tavolo della comanda accettata
    char *codice_comanda = (char *)malloc(10); // codice di set della comanda
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
    // inizializzo piatti in tutte le comande accettabili
    int q = 0;
    int i = 0;
    int contatore_accettate = 0;
    for (i = 0; i < 10; i++)
    {
        accettate[i].tavolo = 0;                                    // inizializzo tavolo
        accettate[i].id_comanda = 0;                                // inizializzo codice comanda
        for (q = 0; q < 8; q++)
        {
            accettate[i].piatti[q].cod = calcola_codice_piatto(arr[q]); // inserisco tutti i codici numerici delle comande
            accettate[i].piatti[q].quantita = 0;
        }
    }

    int nkd = atoi(argv[2]);
    const char *freccia = "  --> ";
    const char *fre = "   --> ";
    const char *comando1 = "take";
    const char *text1 = "accetta una comanda";
    const char *comando2 = "show";
    const char *text2 = "mostra le comande accettate  (in preparazione)";
    const char *comando3 = "set";
    const char *text3 = "imposta lo stato della comanda";

    char *comando = (char *)malloc(sizeof(char) * 8);

    int ret, k;

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
    invio_intero(sd, NUMERO_KITCHEN_DEVICE - 1); // mi identifico

    // Reset FDs
    FD_ZERO(&master);
    FD_ZERO(&read_fds);

    FD_SET(0, &master);  // aggiungo standard input in ascolto
    FD_SET(sd, &master); // aggiungo socket di comunicazione al server

    // Aggiorno il massimo
    fdmax = sd;

    printf("Sei entrato nel kitchen device numero : %d!\n", nkd);
    // rendo disponibile il menu delle operazioni
    printf("Lista comandi:\n -%s %s %s\n -%s %s %s\n -%s %s %s\n", comando1, freccia, text1, comando2, freccia, text2, comando3, fre, text3);

    // main loop
    while (1)
    {

        // Inizializzo il set read_fds, manipolato dalla select()
        read_fds = master;

        // Mi blocco in attesa di descrottori pronti in lettura
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
                    ricevo_intero(i, &stop); // verifico se è un segnale di stop
                    if (stop == 1024)
                    {
                        close(sd);//chiudo il socket di comunicazione col server
                        exit(0);
                    }
                    int numero = 0; // numero di comande pendenti che ricevo dal server, aggiornate
                    ricevo_intero(i, &numero);
                    if(numero == 0){
                        printf("Nessuna comanda più disponibile\n");
                    }
                    else {
                        printf("\nComande :");
                        for (k = 0; k < numero; k++)
                        {
                            printf(" *");
                        }
                        printf("\n");
                    }

                }
                else if (i == 0) // standard input
                {
                    char ingr[50];
                    while (fgets(ingr, 50, stdin))
                    { // per prendere diverso numero di parametri input da stdin
                        conto(ingr, comando, codice_comanda, tavolo);
                        break;
                    }
                    if (!strcmp(comando, comando1))
                    { // se è la take invio: 6 (codice take per lo switch dei comandi nel server , vedi server)
                        if (contatore_accettate == 10)
                        {
                            printf("non puoi accettare più di 10 comande\n");
                            continue;
                        }
                        invio_intero(sd, 6);
                        int id_comanda = 0;
                        // accetto la comanda piu vecchia
                        ricevo_intero(sd, &id_comanda);
                        if (id_comanda == 0)
                        {
                            // nessuna comanda disponibile
                            printf("Nessuna comanda disponibile\n");
                            continue; // torno sulla select
                        }
                        int tavolo = 0;
                        ricevo_intero(sd, &tavolo);
                        printf("com%d T%d\n", id_comanda, tavolo);
                        int quantita = 0;
                        char st[3];
                        int k = 0;
                        int ind = 0;
                        for (k = 0; k < 10; k++)
                        {
                            if (accettate[k].id_comanda == 0)
                            { // salvo nella prima componente con id comanda non valido
                                accettate[k].id_comanda = id_comanda;
                                accettate[k].tavolo = tavolo;
                                ind = k;//k è la posizione dell'array accettate in cui viene salvata la presente comanda
                                break;
                            }
                        }

                        contatore_accettate++;//aumento il contatore

                        for (k = 0; k < 8; k++)
                        {
                            ricevo_intero(sd, &quantita);
                            // stampo solo se la quantità è diversa da 0
                            if (quantita != 0)
                            {
                                calcola_id_piatto(k, st);
                                printf("%s %d\n", st, quantita);
                                accettate[ind].piatti[k].quantita = quantita;
                            }
                        }
                    }
                    else if (!strcmp(comando, comando2))
                    { // se è la show mostro comande accettate
                        show(arr);
                    }
                    else if (!strcmp(comando, comando3))
                    {
                        // controllo input
                        if (tavolo[0] != 'T')
                        {
                            printf("codice tavolo non corretto, riprova\n");
                            continue;
                        }
                        tavolo++;
                        int id_tavolo = atoi(tavolo); // prendo solo il numero
                        if (id_tavolo <= 0 || id_tavolo > 20)
                        {
                            printf("codice tavolo non corretto, riprova\n");
                            continue;
                        }
                        
                        if (strncmp(codice_comanda, "com", 3) != 0)
                        {
                            printf("id comanda errato\n");
                            continue;
                        }
                        codice_comanda += 3;
                        int id_comanda = atoi(codice_comanda);
                        if (id_comanda <= 0)
                        {
                            printf("id comanda errato\n");
                            continue;
                        }
                        //controllo se il tavolo indicato corrisponde proprio a quella comanda accettata
                        bool controllo = true;
                        for (k = 0; k < 10; k++)
                        {
                            if (accettate[k].id_comanda == id_comanda)
                            { // se sono nell'id comanda controllo il tavolo
                                if(accettate[k].tavolo != id_tavolo){
                                    controllo = false;
                                    break;
                                }
                            }
                        }
                        if(!controllo){
                            printf("tavolo non corrispondente all'id comanda\n");
                            continue;
                        }
                        bool chec = false;
                        for (k = 0; k < 10; k++)
                        {
                            if (accettate[k].id_comanda == id_comanda)
                            {
                                // ho trovato la comanda completata e la levo
                                chec = true;                 // trovata
                                accettate[k].id_comanda = 0; // inizializzo codice comanda
                                // 0 come se fosse posizione libera
                                for (q = 0; q < 8; q++)
                                { // azzero le quantita
                                    accettate[k].piatti[q].quantita = 0;
                                }
                            }
                        }

                        if (!chec)
                        {
                            printf("id comanda non presente tra le accettate\n");
                            continue;
                        }
                        contatore_accettate--;

                        // siamo nella set, invio 7 (codice set)
                        invio_intero(sd, 7);
                        invio_intero(sd, id_comanda);
                        invio_intero(sd, id_tavolo);

                        // inoltro al table device
                        printf("COMANDA IN SERVIZIO\n");
                    }
                    else
                    {
                        printf("comando non valido, riprova\n");
                    }
                }
            }
        }
    }
    free(comando);
    free(tavolo);
    free(codice_comanda);
    printf("CHIUDO IL LISTENER!\n");
    fflush(stdout);
    close(sd);
}