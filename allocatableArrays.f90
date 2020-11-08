program countOccurrences
    implicit none
    integer :: n,extraCount                                             !set a counter array for other characters
    integer, dimension(26)::letterCount                                 !set a counter array for letters
    integer, dimension(10):: digitCount                                 !set a counter array for digits                                
    character(:), allocatable :: inputFile                              !create a dynamic array to hold input document
    common /arrays/ n, letterCount,digitCount, extraCount               ! use a common block to pass messages to subroutines
              
    open(10, file="lorem.txt", action="read", &                         !open file containing text to be processed
    form="unformatted", access="stream")
    
    inquire(10,size=n)                                                  !check the size of the input
    allocate(character(n)::inputFile)                                   !set array size to match input size
    
    read(10)inputFile                                                   !read in file to array
    close(10)
    
    call countLetters(inputFile)                                        !process text
    print *, "Done."
    deallocate(inputFile)
end program countOccurrences

subroutine countLetters(datum) 
    implicit none
    integer, dimension(26)::letterCount
    integer, dimension(10):: digitCount
    character, dimension(26)::  upperSet,lowerSet                       !initialize two arrays for letters
    character, dimension(10):: digitSet                                 !initialize one array for digits
    integer :: n, i, extraCount = 0, j                                  !type generic variables
    character, dimension(24):: exclusion                                !set an array for counting extra characters
    common /arrays/ n, letterCount,digitCount,extraCount
    character, dimension(n):: datum                                     !set array to hold input
    !Data Initialization
    digitSet = (/'0','1','2','3','4','5','6','7','8','9'/)              !initialize digit arrays
    upperSet =(/'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T' & !initialize uppercase array
      ,'U','V','W','X','Y','Z'/)
    lowerSet =(/'a','b','c','d','e','f','g','h','i','j','k','l','m','n','o','p','q','r','s','t' & !initialize lowercase array
      ,'u','v','w','x','y','z'/)
    exclusion = (/'!','@','#','$','%','^','&','*','(',')','{','}','[',']','|','\','/','?','>','<','.',',',' ',' '/) !initialize excluded characters
    !End Data
    
    !Count Letters
    open(10, file="letterCount.txt")                                    !open or create output file
    do 21, i=1,24                                                       !count all non alphanumeric characters
      do 25, j=1,n
          if(exclusion(i)==datum(j))then
          extraCount = extraCount + 1
          end if
25      continue
21  continue
     do 20,i=1,n                                                        !do this loop until file is processed. N is file length.
        do 30, j=1,26                                                   !Do this for every letter of the alphabet
     if((datum(i).eq.lowerSet(j)).or.(datum(i).eq.upperSet(j)))then     !if the letter in the file is equal to the current letter
        letterCount(j)=letterCount(j)+1                                 !count that letter
     end if
30   continue
20   continue
        do 50,i=1,n                                                     !do this N times
            do 60,j=1,10                                                !for each number from 0-9
          if(datum(i).eq.digitSet(j))then                               !if digit in file is equal to current digit
            digitCount(j)=digitCount(j)+1                               !count that digit
          end if
60   continue
50   continue
     print *,"-----------------"
     do 40,i=1,26                                                       !print the letters to letterCount.txt
     write(10,*)upperSet(i),":",letterCount(i)
     print *,'|',upperSet(i),':',letterCount(i),"|"
40   continue
     print *, "----------------|"
     close(10)                                                          !close letterCount.txt to prepare digitCount.txt
     open(10,file='digitCount.txt')                                     !open digitCount.txt
     do 70, i=1,10
        write(10,*)digitSet(i),':',digitCount(i)                        !write to file
        print *,'|',digitSet(i),':',digitCount(i),"|"
70   continue
end subroutine countLetters                                             
