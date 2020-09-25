c this is a program written in FORTRAN 77.
c Any line preceded by a C is seen as a comment by the compiler. 
c the first 6 columns in a F77 source file are reserved for labels.
c any code must stay between columns 7-72. anything after that is treated as a comment, i.e. it is ignored.
c This is known as "fixed-source" fortran code, and it wasn't replaced until fortran 90.
c this was for one reason alone, and that is to fit on punched cards.

c every program starts with PROGRAM programName, and is closed by end program.  
                                      
       PROGRAM HELLO !inline comments are shown like this.
       
c this is a declaration of a character type variable with a width of 16 bytes.
 
       CHARACTER*16 MESSAGE
       
c this is the dreaded format statement. This is how fortran compilers were told explicitly how to 
c handle input data, as everything was originally treated as a numeric value.
c A means alphanumeric, and 16 is the number of characters to expect.
c needless to say, this can be quite unwieldy, even for such a simple example.
c luckily, this was changed in later versions.

10     FORMAT (A11)
       MESSAGE = 'HELLO WORLD'
c This is the way that we write data to anything. 
c the number 6 means "To the terminal", and 10 is the format label to use.
c We then denote the data to be written.
       WRITE(6,10) MESSAGE
       END PROGRAM
