module class_Circle
    implicit none
    private
    real :: pi = 3.14159
    
        type, public :: Circle ! Name used to instantiate a circle object.
        
            real :: radius ! instance variable, that is, the radius of a circle object
            
        contains ! "this class contains the following methods"
        ! The following 
        
        
            procedure :: area => circle_area ! type-bound procedure used to create a class method.
            procedure :: print => circle_print ! same as above.
            
        end type Circle
        
    contains
     function circle_area(this) result(area) ! This is the declaration of the circle area method.
     
      class(Circle), intent(in) :: this ! This line shows that the parameter to be passed ("this") is to be an input to the method.
      
      real :: area ! This declares a real, i.e. floating point variable named area
      
      area = pi * this%radius**2 ! This calculates the area. Note the % sign. This is seen in other languages as this.radius. Fortran uses %.
      
      end function circle_area
      
      
      subroutine circle_print(this) 
      
        class(Circle), intent(in):: this
        
        real :: area
        
        area = this%area()
        
        print *, 'Circle : r = ', this%radius, ' area = ', area
        
    end subroutine circle_print
    
end module class_Circle


program circle_test ! Note that this says "program" and not "module". This will actually be run when the compiled source is executed.
    use class_Circle  ! This shows that we are needing the module class_Circle
    implicit none ! Explain explicit typing here.
    type(Circle) :: c ! Instantiate a circle and name it C.
    c = Circle(1.5) ! Give C a radius of 1.5 
    call c%print ! Call C's print method. In C++ or Java it would be c.print
    end program circle_test ! That's all folks'
