server: server.scm
	csc -o server server.scm
clean:
	rm -f *.o
	rm -f out
	rm -f result
	rm -f *.link
	rm -f *.import.scm
	rm -f *.c
