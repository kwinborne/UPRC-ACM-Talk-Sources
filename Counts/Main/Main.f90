! This program takes in a file containing text data, i.e. alphanumeric and non-alphanumeric characters
! and counts the occurrences of each. It will then print the results to the terminal, and output
! the results to a file for later use or storage.
!
! Contains the following routines, in order of call:
! 
! subroutine initCounts - Initialize the environment for processing. Opens the file, reads it in, and
! calls the next routine.
!
! subroutine initArrays - Initialize the arrays used to check against, and arrays used to store counts.
! The count is set to 0 initially to make sure it is in a known state. 
! 
! subroutine countLetters - Processes the data from the file and counts each character
!
! subroutine printCounts - prints the results to the terminal, and saves them to letterCount.txt and digitCount.txt.
! Excluded characters are counted and saved with letterCount.txt.
!
! The following variables are used:
! arrays: alphaSet - contains A-Z and a-z.
! arrays: digitSet - 0-9
! arrays: exclusion - contains excluded characters. Hard coded to allow modification.
! arrays: inf - array that holds the data read in from the file data.txt
!
! integer: nSize - size of inf
! integer: extraCount - count of excluded characters
! integer: i,j,k - generics to use in loops.
!
!
! Changes from last version include a cleaner code, updated style, and local subroutines.
!
! Written on 09/08/2020 by Kiyle Winborne using VIM and Gfortran





program CountOccurences
    implicit none
    integer, save :: nsize, extraCount
    integer :: i,j,k
    real :: start_time, finish_time

  

    character,save, dimension(52) :: alphaSet
    character,save, dimension(10) :: digitSet
    character,save, dimension(24) :: exclusion
    
    integer, dimension(10) :: digitCount
    integer, dimension(26) :: letterCount

    character(:),save,allocatable :: inf
    call cpu_time(start_time)
    call initCounts
    call cpu_time(finish_time)
    print *, "Array set up time: ", finish_time - start_time
    call cpu_time(start_time)
    call countLetters
    call cpu_time(finish_time)
    print *, "Processing time: ", finish_time - start_time
   

   contains

   subroutine initCounts
     open(10, file="data.txt", action="read", form="unformatted", access="stream") ! Open data file 
     inquire(10, size = nsize )                                                    ! Get its size
     allocate(character(nsize) :: inf)                                             ! make inf that size
     print *, "Ready"                                                              ! Marker to show that the program is doing the thing.
     read(10) inf                                                                  ! Read data from the file
     close(10)
     call initArrays
    end subroutine initCounts


    subroutine initArrays
    exclusion = (/'!','@','#','$','%','^','&','*','(',')','{','}','[',']','|','\','/','?','>','<','.',',',' ',' '/) 
    ! Generate capital letters
    do i=1,26
    alphaSet(i:i) = achar(iachar('A') + i -1)
    end do
    ! Generate lowercase 
    do i=27,52
    alphaSet(i:i) = achar(iachar(alphaSet(i-26))+32)
    end do
    ! Generate digits
    do i=1,10
    digitSet(i:i) = achar(iachar('0') + i -1)
    end do
    ! Initialize letter counts
    do i=1, 26
     letterCount(i) = 0
    end do
    ! Initialize digit counts
    do i=1,10
     digitCount(i) = 0
    end do
    end subroutine initArrays



subroutine countLetters
    implicit none
       !Count non-alphanumeric characters
    do i=1,24
    do j=1,nsize
      if(inf(j:j) == exclusion(i))then
        extraCount = extraCount + 1
      end if
    end do
    end do
    print *, "/:", extraCount
                                                    
    !Count Letters
 
    do i=1,52
      do j=1,nsize
    if(inf(j:j) .eq. alphaSet(i))then
      if(i>=27)then
        letterCount(i-26)=letterCount(i-26) + 1
      elseif(i<27)then
        letterCount(i)=letterCount(i) + 1
      end if
    end if   
    end do
    end do
    
    !Count digits
    do i=1, 10
    do j=1, nsize
      if(inf(j:j)==digitSet(i))then
         digitCount(i) = digitCount(i) + 1
      end if
    end do 
    end do
    call printCounts

                    
  end subroutine countLetters                               

  subroutine printCounts
    ! Print letter counts
    open(10, file = "results.txt")
    write(10,*) "/:", extraCount
    do i=1,26 
     write(10,*) alphaSet(i), ":" , letterCount(i)  
     print *,alphaSet(i), ":" , letterCount(i)  
    end do

    ! Print digit counts
    do i=1,10
     write(10,*)digitSet(i), ":", digitCount(i)
     print *, digitSet(i), ":" , digitCount(i)
    end do
    close(10)

  end subroutine printCounts


   
   end program CountOccurences
