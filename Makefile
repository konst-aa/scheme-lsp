server: requests responses server.scm	
	csc -o server requests.o responses.o -uses requests -uses responses server.scm

requests: responses requests.sld
	csc -c responses.o -uses responses -J requests.sld -unit requests -o requests.o

responses: responses.sld
	csc -c -J responses.sld -unit responses -o responses.o

clean:
	rm -f *.o
	rm -f out
	rm -f result
	rm -f *.link
	rm -f *.import.scm
	rm -f *.c
