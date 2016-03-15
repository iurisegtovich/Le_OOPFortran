!see ref http://compgroups.net/comp.lang.fortran/array-of-polymorphic-objects/606374

!array of polymorphic objects
!vs
!linked list

module m1 !nesse tópico vamos definir listas polimórficas

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

  type, extends(t) :: t2
    integer :: n
  contains
    procedure :: disp => disp2
    procedure :: increment => increment2
  end type
  
  type, extends(t) :: t3
    real :: r
  contains
    procedure :: disp => disp3
    procedure :: increment => increment3
  end type
  
  type, extends(t) :: t4
    logical :: l
  contains
    procedure :: disp => disp4
    procedure :: increment => increment4
  end type
  
  class(t), pointer :: c_o1, c_o2, c_o3, c_o4
  
  type t_ptr
    class(t), pointer :: p
  end type
  
  type(t_ptr), allocatable :: clist1(:), clist2(:)
  
  class(t), pointer :: cptr1(:), cptr2(:), cptr3(:) !essa aqui não vão ser uteis, explorar opções e concluir que não tem saída se não a de fazer t_ptr XXX
  
  class(t), pointer :: cptr4, cptr5
  
!  class(t), allocatable :: cptr3(:), cptr4(:) !proibido CLASS se não for POITER ou DUMMY
  
contains

!#8 aqui começa a execução
  subroutine s1()

  allocate(c_o1, source = t2(n=3) )
  allocate(c_o2, source = t3(r=4.) )
  allocate(c_o3, source = t4(l=.true.) )
  
  call c_o1%disp 
  call c_o2%disp 
  call c_o3%disp 
  
  !forma 1a
  allocate(clist1(3))
  allocate(clist1(1)%p, source = t2(n=5) )
  allocate(clist1(2)%p, source = t3(r=6.) )
  allocate(clist1(3)%p, source = t4(l=.false.) )
  
  call clist1(1)%p%disp 
  call clist1(2)%p%disp 
  call clist1(3)%p%disp 
  
  !forma 1b
  clist2 = [t_ptr(c_o1),t_ptr(c_o2),t_ptr(c_o3)] !Aqui os objetos recebidos pelo do construtor são "apontados para", pointed to, ver Dependency injection

  call clist2(1)%p%disp 
  call clist2(2)%p%disp 
  call clist2(3)%p%disp 
  
  call c_o1%increment
  call c_o2%increment
  call c_o3%increment
  
  call c_o1%disp 
  call c_o2%disp 
  call c_o3%disp 
  
  call clist2(1)%p%disp 
  call clist2(2)%p%disp 
  call clist2(3)%p%disp 
  
  !forma2a
!  allocate(cptr1(3)) !Allocating cptr1 of ABSTRACT base type at (1) requires a type-spec or source-expr
  
  !forma2b
  allocate(cptr2(3), source = [t2(n=7), t2(n=8), t2(n=9)]) !esta construção exige todos os elementos serem do mesmo subtipo, o que não satisfaz diversa aplicações
!  allocate(cptr3(3), source = [t3(r=8.), t2(n=7), t4(l=.true.)]) !Error: Element in TYPE(t3) array constructor at (1) is TYPE(t2)

  allocate(cptr4, mold = c_o1) !mold? XXX
  allocate( t2:: cptr5 ) !class:: obj XXX
  
  call cptr2(1)%disp 
  call cptr2(2)%disp 
  call cptr2(3)%disp 

  !rotina concluída, voltamos a quem chamou, no caso foi o program
  end subroutine
  
  subroutine disp2(z)
    class(t2) :: z
    print*, 
    print*, 'disp2'
    print*, 'c2%n = '
    print*, z%n
 end subroutine

  subroutine disp3(z)
    class(t3) :: z
    print*, 
    print*, 'disp3'
    print*, 'c3%r = '
    print*, z%r
 end subroutine
 
  subroutine disp4(z)
    class(t4) :: z
    print*, 
    print*, 'disp2'
    print*, 'c4%l = '
    print*, z%l
 end subroutine
 
   subroutine increment2(z)
    class(t2) :: z
    print*, 
    print*, 'increment2'
    z%n=z%n*11
 end subroutine

  subroutine increment3(z)
    class(t3) :: z
    print*, 
    print*, 'increment3'
    z%r=exp(z%r)
 end subroutine
 
  subroutine increment4(z)
    class(t4) :: z
    print*, 
    print*, 'increment4'
    z%l=.not.z%l
 end subroutine

end module

!#1 !o programa começa necessariamente aqui
program main
!necessitamos acesso a rotina s1, onde estão todos os exemplos deste tópico
!olhemos também a seção de definições de types e declarações de variaveisa e objetos do m1 antes de continuar 
use m1, only: s1 !{2}

!#7 !vamos à implementação
!a ordem de execução desse programa será PROGRAM -> s1 -> disp -> s1 -> disp2 -> disp -> disp2 -> s1 -> program
!vamos à rotina s1
call s1() !{8}

!#n
!tópico concluído
end program


