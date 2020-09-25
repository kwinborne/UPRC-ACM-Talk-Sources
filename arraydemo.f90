program arraytest
implicit none ! implicit typing is disabled here.
integer, dimension(10) :: numbers = [1,2,3,4,5,6,7,8,9,10] ! dimension 10 means that it is an array of length 10.
character, dimension(12):: chars = ['H','e','l','l','o',' ','W','o','r','l','d','!']  
integer i ! because of implicit typing being disabled, iterator i must be declared somewhere.
do i=1,10 ! do this statement from 10 times.
print *,numbers(i)
end do

do i=1,12
print *, trim(chars(i)) ! write to console, and trim whitespace from each character.
end do

end program arraytest
