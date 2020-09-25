program test_prog

integer :: i

call execute_command_line("x a b", exitstat=i)
print *, "Exit status of x was ", i

call execute_command_line("x a b", wait=.false.)
print *, "running x in the background"
end program test_prog
