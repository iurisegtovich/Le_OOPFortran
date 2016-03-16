!see ref http://compgroups.net/comp.lang.fortran/array-of-polymorphic-objects/606273

!array of polymorphic objects
!vs
!linked list

module m1 !nesse tópico vamos definir listas polimórficas
implicit none
  type, abstract :: t
  contains
    procedure(AbsInt1), deferred :: disp
    procedure(AbsInt1), deferred :: increment
  end type


  abstract interface
    subroutine AbsInt1(z)
      import t
      implicit none
      class(t) :: z
    end subroutine
  end interface

  type, extends(t) :: t1
    integer :: i
    integer, pointer :: pi => null()
  contains
    procedure :: disp => disp1
    procedure :: increment => increment1
  end type
  
  type, extends(t) :: t2
    real :: r
    real, pointer :: pr => null()
  contains
    procedure :: disp => disp2
    procedure :: increment => increment2
  end type
  
  type, extends(t) :: t3
    logical :: l
    logical, pointer :: pl => null()
  contains
    procedure :: disp => disp3
    procedure :: increment => increment3
  end type

  !  type(t) :: t_o_constructor !Error: 't_o_constructor' at (1) is of the ABSTRACT type 't'

  type(t1) :: t1_o_constructor
  type(t2) :: t2_o_constructor
  type(t3) :: t3_o_constructor
  
!  type(t), pointer :: t_p_src_constructor, t_p_mold_constructor, t_p_dots_constructor !Error: 't_o_constructor' at (1) is of the ABSTRACT type 't'
  type(t1), pointer :: t1_p_src_constructor, t1_p_mold_constructor, t1_p_dots_constructor
  type(t2), pointer :: t2_p_src_constructor, t2_p_mold_constructor, t2_p_dots_constructor
  type(t3), pointer :: t3_p_src_constructor, t3_p_mold_constructor, t3_p_dots_constructor
  
!  type(t), pointer :: t_p_src_target, t_p_mold_target, t_p_dots_target !Error: 't_o_target' at (1) is of the ABSTRACT type 't'
  type(t1), pointer :: t1_p_src_target, t1_p_mold_target, t1_p_dots_target
  type(t2), pointer :: t2_p_src_target, t2_p_mold_target, t2_p_dots_target
  type(t3), pointer :: t3_p_src_target, t3_p_mold_target, t3_p_dots_target
  
  class(t), pointer :: c_p_src_constructor, c_p_mold_constructor, c_p_dots_constructor
  class(t1), pointer :: c1_p_src_constructor, c1_p_mold_constructor, c1_p_dots_constructor
  class(t2), pointer :: c2_p_src_constructor, c2_p_mold_constructor, c2_p_dots_constructor
  class(t3), pointer :: c3_p_src_constructor, c3_p_mold_constructor, c3_p_dots_constructor
  
  class(t), pointer :: c_p_src_target, c_p_mold_target, c_p_dots_target
  class(t1), pointer :: c1_p_src_target, c1_p_mold_target, c1_p_dots_target
  class(t2), pointer :: c2_p_src_target, c2_p_mold_target, c2_p_dots_target
  class(t3), pointer :: c3_p_src_target, c3_p_mold_target, c3_p_dots_target
  
  class(t), pointer :: cl_p_allocate_constructor(:), cl_p_src_constructor(:), cl_p_mold_constructor(:), cl_p_dots_constructor(:)
  class(t1), pointer :: cl1_p_allocate_constructor(:), cl1_p_src_constructor(:), cl1_p_mold_constructor(:), cl1_p_dots_constructor(:)
  class(t2), pointer :: cl2_p_allocate_constructor(:), cl2_p_src_constructor(:), cl2_p_mold_constructor(:), cl2_p_dots_constructor(:)
  class(t3), pointer :: cl3_p_allocate_constructor(:), cl3_p_src_constructor(:), cl3_p_mold_constructor(:), cl3_p_dots_constructor(:)
  
  class(t), pointer :: cl_p_allocate_target(:), cl_p_src_target(:), cl_p_mold_target(:), cl_p_dots_target(:)
  class(t1), pointer :: cl1_p_allocate_target(:), cl1_p_src_target(:), cl1_p_mold_target(:), cl1_p_dots_target(:)
  class(t2), pointer :: cl2_p_allocate_target(:), cl2_p_src_target(:), cl2_p_mold_target(:), cl2_p_dots_target(:)
  class(t3), pointer :: cl3_p_allocate_target(:), cl3_p_src_target(:), cl3_p_mold_target(:), cl3_p_dots_target(:)
  
  type t_ptr
    class(t), pointer :: p
  end type
  
  type(t_ptr), allocatable :: clist_allocate_constructor(:), clist_src_constructor(:), clist_mold_constructor(:), clist_dots_constructor(:)
  type(t_ptr), allocatable :: clist_allocate_target(:), clist_src_target(:), clist_mold_target(:), clist_dots_target(:)
  
!  class(t), allocatable :: cptr2(:), cptr3(:) !proibido CLASS se não for POITER ou DUMMY
  
contains

!#8 aqui começa a execução
  subroutine s1()

  integer, pointer :: j => null()
  real, pointer :: s => null()
!  logical, pointer :: m => null()
  integer, pointer :: jj(:)
  
  allocate(j)
  j=2
  
  !Test different allocation syntax for a intrinsic type scalar:
!  allocate(s); s=1.
!  print*, s; stop !OK
  allocate(s, source = 2.)
!  print*, s; stop !OK gets type and value
!  allocate(s, mold = 3.)
!  print*, s; stop !does not get value
!  allocate(real:: s); s=4
!  print*, s; stop
!  allocate(real:: s, source = 5.) !Error: SOURCE tag at (1) conflicts with the typespec at (2)
!  print*, s; stop
!  allocate(real:: s, mold = 6.) !Error: MOLD tag at (1) conflicts with the typespec at (2)
!  print*, s; stop

!works with dimension arrays
  allocate(jj(3),source=[1,2,3])
  print*, jj

!  t1_o_constructor = t1(i=1,pi=1) !Error: The element in the structure constructor at (1), for pointer component 'pi' should be a POINTER or a TARGET

  t1_o_constructor = t1(i=1,pi=j)

  call t1_o_constructor%disp
  
  j=3
  
  call t1_o_constructor%disp
  
  !conclusão, t1%pi aponta para j local
  
  !now we try a personal constructor
  t2_o_constructor = allocate_t2(rv=1., prv=s)

  call t2_o_constructor%disp
  
  s=s+1.
  
  call t2_o_constructor%disp
  
  !the value of s was copied, but the pointer component of the object is independently allocated, not pointing to s
  
  t3_o_constructor = t3(l=.true.,pl=null())

  call t3_o_constructor%disp

allocate( t1_p_src_constructor )
allocate( t1_p_mold_constructor )
allocate( t1_p_dots_constructor )

allocate( t2_p_src_constructor )
allocate( t2_p_mold_constructor )
allocate( t2_p_dots_constructor )

allocate( t3_p_src_constructor )
allocate( t3_p_mold_constructor )
allocate( t3_p_dots_constructor )

allocate( t1_p_src_target, source= t1_p_src_constructor )
allocate( t1_p_mold_target, mold= t1_p_mold_constructor )
allocate( t1:: t1_p_dots_target )

allocate( t2_p_src_target, source= t2_p_src_constructor )
allocate( t2_p_mold_target, mold= t2_p_mold_constructor )
allocate( t2:: t2_p_dots_target )

allocate( t3_p_src_target, source= t3_p_src_constructor )
allocate( t3_p_mold_target, mold= t3_p_mold_constructor )
allocate( t3:: t3_p_dots_target )



!allocate( c_p_src_constructor, source=!XXX
!allocate( c_p_mold_constructor, mold=!XXX
!allocate( !XXX:: c_p_dots_constructor

!!!  c1_p_src_constructor
!!!  c1_p_mold_constructor
!!!  c1_p_dots_constructor

!!!  c2_p_src_constructor
!!!  c2_p_mold_constructor
!!!  c2_p_dots_constructor

!!!  c3_p_src_constructor
!!!  c3_p_mold_constructor
!!!  c3_p_dots_constructor

!  allocate( c_p_src_target, source=!XXX
!  allocate( c_p_mold_target, mold=!XXX
!  allocate( !XXX:: c_p_dots_target

!!!  c1_p_src_target
!!!  c1_p_mold_target
!!!  c1_p_dots_target

!!!  c2_p_src_target
!!!  c2_p_mold_target
!!!  c2_p_dots_target

!!!  c3_p_src_target
!!!  c3_p_mold_target
!!!  c3_p_dots_target

!  cl_p_allocate_constructor =[!XXX]
!  allocate( cl_p_src_constructor(!XXX), source=!XXX
!  allocate( cl_p_mold_constructor(!XXX), mold=!XXX
!  allocate( !XXX:: cl_p_dots_constructor(!XXX)

!!!  cl1_p_allocate_constructor(:)
!!!  cl1_p_src_constructor(:)
!!!  cl1_p_mold_constructor(:)
!!!  cl1_p_dots_constructor(:)

!!!  cl2_p_allocate_constructor(:)
!!!  cl2_p_src_constructor(:)
!!!  cl2_p_mold_constructor(:)
!!!  cl2_p_dots_constructor(:)

!!!  cl3_p_allocate_constructor(:)
!!!  cl3_p_src_constructor(:)
!!!  cl3_p_mold_constructor(:)
!!!  cl3_p_dots_constructor(:)

!  cl_p_allocate_target = [c_p_src_constructor, c_p_mold_constructor, c_p_dots_constructor] !Error: Variable must not be polymorphic in intrinsic assignment at (1) - check that there is a matching specific subroutine for '=' operator
! can't use "automatic allocation" syntax with the array_pointer, only with array_allocatable

!  allocate( cl_p_src_target(3) ) !Error: Allocating cl_p_src_target of ABSTRACT base type at (1) requires a type-spec or source-expr

! all elements are of the same derived type
  allocate( cl_p_src_target(3), source=c_p_src_constructor)
  
!  compiler error in source expressiom
!  using class abstract objects in the array constructor
!  allocate( cl_p_src_target(3), source=[c_p_src_constructor,c_p_src_constructor,c_p_src_constructor]) !internal compiler error: in gfc_conv_array_constructor_expr, at fortran/trans-expr.c:5618
!  using fixed type works
!  allocate( cl_p_src_target(3), source=[t1_p_src_constructor,t1_p_src_constructor,t1_p_src_constructor])
! using different types dont work
!  allocate( cl_p_src_target(3), source=[t1_p_src_constructor,t2_p_src_constructor,t3_p_src_constructor]) !internal compiler error: in gfc_conv_array_constructor_expr, at fortran/trans-expr.c:5618
!  using concrete class doensn work too
!  allocate( cl_p_src_target(3), source=[c1_p_src_constructor,c1_p_src_constructor,c1_p_src_constructor]) !internal compiler error: in gfc_conv_array_constructor_expr, at fortran/trans-expr.c:5618
  
!  can't make multitype pointer
! not with objects
!  allocate( cl_p_src_target(3), source=[c_p_src_constructor, c_p_mold_constructor, c_p_dots_constructor])
!  neither with constructors
!can make single type array with different initializations
!  allocate( cl_p_src_target(3), source=[t1(1),t1(2),t1(3)])
!cant use different constructors
!  allocate( cl_p_src_target(3), source=[t1(1),t2(2.),t3(.true.)]) !Error: Element in TYPE(t1) array constructor at (1) is TYPE(t2)
!the first element in the array constructor enforces the type of the array
!  the way for polimorphic array is allocatable array of pointer to class: see t_ptr, clist
  
!  allocate( cl_p_mold_target(!XXX), mold=!XXX
!  allocate( !XXX:: cl_p_dots_target(!XXX)

!!!  cl1_p_allocate_target(:)
!!!  cl1_p_src_target(:)
!!!  cl1_p_mold_target(:)
!!!  cl1_p_dots_target(:)

!!!  cl2_p_allocate_target(:)
!!!  cl2_p_src_target(:)
!!!  cl2_p_mold_target(:)
!!!  cl2_p_dots_target(:)

!!!  cl3_p_allocate_target(:)
!!!  cl3_p_src_target(:)
!!!  cl3_p_mold_target(:)
!!!  cl3_p_dots_target(:)

!  clist_allocate_constructor =[!XXX]
!  allocate( clist_src_constructor(!XXX), source=!XXX
!  allocate( clist_mold_constructor(!XXX), mold=!XXX
!  allocate( !XXX:: clist_dots_constructor(!XXX)


!  clist_allocate_target = []
!  allocate( clist_src_target(!XXX), source=!XXX
!  allocate( clist_mold_target(!XXX), mold=!XXX
!  allocate( !XXX:: clist_dots_target(!XXX)













!  allocate(c_o1, source = t1(n=2) )
!  allocate(c_o1, source = t2(r=3.) )
!  allocate(c_o2, source = t3(l=.true.) )
!  
!  call c_o1%disp 
!  call c_o1%disp 
!  call c_o2%disp 
!  
!  !forma 1a
!  allocate(clist_src(2))
!  allocate(clist_src(1)%p, source = t1(n=5) )
!  allocate(clist_src(1)%p, source = t2(r=6.) )
!  allocate(clist_src(2)%p, source = t3(l=.false.) )
!  
!  call clist_allocate(1)%p%disp 
!  call clist_allocate(1)%p%disp 
!  call clist_allocate(2)%p%disp 
!  
!  !forma 1b
!  clist_allocate = [t_ptr(c_o1),t_ptr(c_o2),t_ptr(c_o3)] !Aqui os objetos recebidos pelo do construtor são "apontados para", pointed to, ver Dependency injection

!  call clist_allocate(1)%p%disp 
!  call clist_allocate(1)%p%disp 
!  call clist_allocate(2)%p%disp 
!  
!  call c_o1%increment
!  call c_o1%increment
!  call c_o2%increment
!  
!  call c_o1%disp 
!  call c_o1%disp 
!  call c_o2%disp 
!  
!  call clist_allocate(1)%p%disp 
!  call clist_allocate(1)%p%disp 
!  call clist_allocate(2)%p%disp 
!  
!  !forma1a
!!  allocate(cptr1(2)) !Allocating cptr1 of ABSTRACT base type at (1) requires a type-spec or source-expr
!  
!  !forma1b
!  allocate(cptr1(2), source = [t1(n=7), t1(n=8), t1(n=9)]) !esta construção exige todos os elementos serem do mesmo subtipo, o que não satisfaz diversa aplicações
!!  allocate(cptr2(2), source = [t2(r=8.), t1(n=7), t3(l=.true.)]) !Error: Element in TYPE(t2) array constructor at (1) is TYPE(t1)

!  allocate(cptr3, mold = c_o1) !mold? XXX
!  allocate( t1:: cptr5 ) !class:: obj XXX
!  
!  call cptr1(1)%disp 
!  call cptr1(1)%disp 
!  call cptr1(2)%disp 

!  !rotina concluída, voltamos a quem chamou, no caso foi o program
  end subroutine
  
  subroutine disp1(z)
    class(t1) :: z
    print*, 
    print*, 'disp1'
    print*, 'c1%i= ', z%i
    if(associated(z%pi)) print*, 'c1%pi= ', z%pi
 end subroutine

  subroutine disp2(z)
    class(t2) :: z
    print*, 
    print*, 'disp2'
    print*, 'c2%r= ', z%r
       if(associated(z%pr)) print*, 'c2%pr= ', z%pr
 end subroutine
 
  subroutine disp3(z)
    class(t3) :: z
    print*, 
    print*, 'disp3'
    print*, 'c3%l= ', z%l
    if(associated(z%pl)) print*, 'c3%pl= ', z%pl
 end subroutine
 
   subroutine increment1(z)
    class(t1) :: z
    print*, 
    print*, 'increment1'
    z%i=z%i*11
    if(associated(z%pi)) z%pi=z%pi*11
 end subroutine

  subroutine increment2(z)
    class(t2) :: z
    print*, 
    print*, 'increment2'
    z%r=exp(z%r)
    if(associated(z%pr)) z%pr=exp(z%pr)
 end subroutine
 
  subroutine increment3(z)
    class(t3) :: z
    print*, 
    print*, 'increment3'
    z%l=.not.z%l
    if(associated(z%pl)) z%pl=.not.z%pl
 end subroutine

  function allocate_t2(rv,prv) result(z)
    type(t2) :: z
    real, value :: rv
    real, value :: prv
    z%r=rv
    allocate(z%pr,source=prv)
  end function

end module

!#1 !o programa começa necessariamente aqui
program main
!necessitamos acesso a rotina s1, onde estão todos os exemplos deste tópico
!olhemos também a seção de definições de types e declarações de variaveisa e objetos do m1 antes de continuar 
use m1, only: s1 !{1}
implicit none
!#7 !vamos à implementação
!a ordem de execução desse programa será PROGRAM -> s1 -> disp -> s1 -> disp1 -> disp -> disp1 -> s1 -> program
!vamos à rotina s1
call s1() !{8}

!#n
!tópico concluído
end program


