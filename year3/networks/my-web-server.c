#include <stdio.h>
#include <sys/ioctl.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in.h>
#include <pthread.h>
#include <string.h>
#include <stdlib.h>
#include <netdb.h>
#include <fcntl.h> // for open
#include <unistd.h> // for close

#define SERVER "Server: Dynamic Thread\n"
#define CONTENT "Content-Type: text/html\r\n"
void *parseHttpReq(void *arg);          //Parse the HTTP request
void badRequest(int sock);              //Send HTTP header for bad request (400)
void serve(int sock, char *path);       //Serve the file
void goodRequest(int sock);             //Send HTTP header for good request (200)
void notGet(int sock);                  //Send HTTP header for unimplemented method (501)
void notFound(int sock);                //Send HTTP header when a file is not found (404)
void outputFile(int sock, FILE *html);  //Reads input from file and sends it to client

int main(int argc, char *argv[])
{
    int listenSock = 0;
    int port = 0;
    int result = 0;
    struct sockaddr_in server;
    pthread_t thread;
    int val = 1;

    //Determine if port number was input
    if(argc != 2)
    { fprintf(stderr, "Port number not input!\n");
        exit(1);
    }
    
    //Create socket
    listenSock = socket(AF_INET, SOCK_STREAM, IPPROTO_TCP);
    if(listenSock == -1)
    {
        fprintf(stderr, "Could not creat a socket!\n");
        exit(1);
    }

    //Set socket option
    result = setsockopt(listenSock, SOL_SOCKET, SO_REUSEADDR, &val, sizeof(val));
    
    //Get port number
    port = atoi(argv[1]);
    
    //Set up address structure
    bzero(&server, sizeof(server));
    server.sin_family = AF_INET;
    server.sin_addr.s_addr = htonl(INADDR_ANY);
    server.sin_port = htons(port);

    //Bind socket to port
    result = bind(listenSock, (struct sockaddr*)&server, sizeof(server));
    if(result != 0)
    {
        fprintf(stderr, "Could not bind to port!\n");
	    close(listenSock);
        exit(1);
    }

    //Listen for client connections
    result = listen(listenSock, 5);
    if(result == -1)
    {
        fprintf(stderr, "Cannot listen on socket!\n");
        close(listenSock);
        exit(1);
    }

    while(1)
    {
        struct sockaddr_in client = {0};
	    int newSock = 0;
        int clientL = sizeof(client);

        //accept connections
        newSock = accept(listenSock, (struct sockaddr*)&client, &clientL);
        if(newSock == -1)
        {
            fprintf(stderr, "Cannot accept connection!\n");
            close(listenSock);
            exit(1);
        }
        else
        {
            //Create a thread for each incoming connection
            result = pthread_create(&thread, NULL, parseHttpReq, (void*) newSock);
            if(result != 0)
            { 
                fprintf(stderr, "Could not create thread!\n");
                close(listenSock);
                exit(1);
            }
            pthread_detach(thread);
            sched_yield();
        }//End of else -- could accept
    }//End of while
    close(listenSock);
    return 0;
}//End of main

void *parseHttpReq(void *arg)
{
    int sock;
    char buffer[1024];
    int readIn;
    char *method, *path, *version;
    int v1, v2, m;

    //cast sock back to int
    sock = (int)arg;

    //Get input from client
    readIn = recv(sock, buffer, 1024, 0);
    buffer[readIn] = '\0';

    //parse the HTTP method, path to the file and the HTTP version
    method = (char*)malloc(sizeof(buffer)+1);
    strcpy(method, buffer);	//copy header data into method
    method = strtok(method, " ");
    printf("%s\n", method);
    path = strtok(path, " ");
    printf("%s\n", path);
    version = strtok(version, "\r\n");
    printf("%s\n", version);

    //Find the HTTP version
    v1 = strcmp(version, "HTTP/1.0\r\n");
    v2 = strcmp(version, "HTTP/1.1\r\n");
    if(!v1)
	    strcpy(version, "HTTP/1.0");
    else
        strcpy(version, "HTTP/1.1");
    
    ///Send back a 400 error message
    if((!v1) || (!v2))
        badRequest(sock);
    else
    {
        //See if the correct method is used -- only handle GET
        m = strcmp(method, "GET");
        if(m == 0)
	        serve(sock, path);	//Send file
	    else
	        notGet(sock);
    }
}	//End of parseHttpReq

void badRequest(int sock)
{
    char buffer[1024];
    //Send HTTP Response line by line
    strcpy(buffer, "HTTP/1.0 400 Bad Request\r\n");
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, SERVER);
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, CONTENT);
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, "\r\n");
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, "<html>\n<head>\n<title>Bad Request</title>\n</head>\r\n");
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, "<body>\n<p>400 Bad Request.</p>\n</body>\n</html>\r\n");
    write(sock, buffer, strlen(buffer));
}   //End of badRequest

void goodRequest(int sock)
{
    char buffer[1024];

    //Send HHTP Response line by line
    strcpy(buffer, "HTTP/1.0 200 Ok\r\n");
    send(sock, buffer, strlen(buffer), 0);
    strcpy(buffer, SERVER);
    send(sock, buffer, strlen(buffer), 0);
    strcpy(buffer, CONTENT);
    send(sock, buffer, strlen(buffer), 0);
    strcpy(buffer, "\r\n");
}   //End of good request 

void notGet(int sock)
{
    char buffer[1024];
    //Send HTTP Response line by line
    strcpy(buffer, "HTTP/1.0 501 Method Not Implemented\r\n");
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, SERVER);
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, CONTENT);
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, "\r\n");
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, "<html>\n<head>\n<title>Method Not Implemented</title>\n</head>\r\n");
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, "<body>\n<p>501 HTTP request method not supported.</p>\n</body>\n</html>\r\n");
    write(sock, buffer, strlen(buffer));
}//End end of notGet

void notFound(int sock)
{
    char buffer[1024];

    //Send HTTP Response line by line
    strcpy(buffer, "HTTP/1.0 404 Not Found\r\n");
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, SERVER);
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, CONTENT);
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, "\r\n");
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, "<html>\n<head>\n<title>Not Found</title>\n</head>\r\n");
    write(sock, buffer, strlen(buffer));
    strcpy(buffer, "<body>\n<p>404 Request file not found.</p>\n</body>\n</html>\r\n");
    write(sock, buffer, strlen(buffer));
}//End of notFound

void outputFile(int sock, FILE *html)
{ 
    char buffer[1024];    //Only supports small files
    //Read in file and store in buffer
    fgets(buffer, sizeof(buffer), html);
    while(!feof(html))
    {
        send(sock, buffer, strlen(buffer), 0);
	fgets(buffer, sizeof(buffer), html);
    }//End of while
}//End of output file

void serve(int sock, char *path)
{
    FILE *html = NULL;
    char buffer[1024];
    int num = 1;

    //determine file name
    if(strcmp(path, "/"))
	strcpy(path, "index.html");
    //Open file
    html = fopen(path, "r");
    if(html == NULL)
        notFound(sock);
    else
    {
	goodRequest(sock);
	outputFile(sock, html);
    }//Send file

    //Close file
    fclose(html);
}//End of serve
