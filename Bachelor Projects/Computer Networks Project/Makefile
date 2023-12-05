all: server cli td kd

# make rule per il server
server: server.c funzioni.h
	gcc server.c -o server -Wall

# make rule per il client
cli: client.c funzioni.h
	gcc client.c -o cli -Wall

# make rule per il table_device
td:	td1.c funzioni.h
	gcc td1.c -o td -Wall

# make rule per il kitchen_device
kd:	kd1.c funzioni.h
	gcc kd1.c -o kd -Wall

clean:
	rm *o server cli td kd
