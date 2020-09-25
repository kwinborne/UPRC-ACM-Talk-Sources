module circle_object
implicit none
private
real :: pi = 3.14159
type, public :: Circle
    real :: radius
    real :: area
    character :: color
    
    contains
    procedure:: area => get_area(radius )
    procedure :: areaSetter => set_area
    procedure :: radiusGetter => get_radius
    procedure :: radiusSetter => set_radius
    procedure :: getColor => get_color
    procedure :: setColor => set_color
    
    end type Circle
    
    contains
    function set_area(this) result(area)
    class(Circle), intent(in):: this
    real :: area
    area = pi* this%radius**2
    end function set_area
    
    function set_radius(this)result(radius)
    class(Circle), intent(in)::this
    real::radius
    radius = this%radius
    end function set_radius
    
    function set_color(this) result(color)
    class(Circle), intent(in)::this
    character :: color
    color = this%color
    end function set_color
    
    subroutine get_area
    print *, area
    end subroutine get_area
    
    subroutine get_radius
    print *, radius
    end subroutine get_radius
    
    subroutine get_color
    print*, color
    end subroutine get_color
end module circle_object
    
program circle_test
use circle_object
implicit none
type(Circle) :: c
call c%set_radius(1.5)
call c%get_radius
call c%set_area(c%radius)
call c%set_color("blue")
call c%get_color
end program circle_test
    
    
    
