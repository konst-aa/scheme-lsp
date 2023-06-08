responses := responses.o -uses responses
utils := utils.o -uses utils
error-codes := error-codes.o -uses error-codes
text-document := text-document.o -uses text-document
workspace := workspace.o -uses workspace

modules := $(responses) $(utils) $(error-codes) $(text-document) $(workspace)

server: responses utils error-codes text-document workspace server.scm	
	csc $(modules) server.scm -o server

responses: utils responses.sld
	csc -c $(utils) -J responses.sld -unit responses -o responses.o

utils: utils.sld
	csc -c -J utils.sld -unit utils -o utils.o

error-codes: error-codes.sld
	csc -c -J error-codes.sld -unit error-codes -o error-codes.o

text-document: utils responses text-document.sld
	csc -c -J $(utils) $(responses) text-document.sld -unit text-document -o text-document.o

workspace: utils workspace.sld
	csc -c -J $(utils) workspace.sld -unit workspace -o workspace.o

clean:
	rm -f *.o
	rm -f out
	rm -f result
	rm -f *.link
	rm -f *.import.scm
	rm -f *.c
