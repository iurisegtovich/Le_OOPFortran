program main

!isso é uma variável do type integer
  integer :: i
!esse type é intrinseco do fortran

!isso é um type novo (também conhecido com Struct) sendo definido agora
  type t1
  end type
!esse type não serve para nada

!isso é um type novo que empacota um inteiro e um real
  type t2
    integer :: n
    real :: r
  end type
  
!isso é uma variável do tipo t1, também chamada de objeto do tipo t1
  type(t1) :: t1_o1
!essa não vai servir para nada
  
!isso é uma variável do tipo t2, também chamada de objeto do tipo t2
  type(t2) :: t2_o1
  
!isso é uma atribuição de valor a variável i
  i=1

!isso imprime o valor da variável i
  print*, char(11)//'i = '
  print*, i
  
!isso são atribuições de valor às variáveis n e r do objeto t2_o1
  t2_o1%n=1
  t2_o1%r=2.
  print*, char(11)//'t2_o1%n, t2_o1%r = '
  print*, t2_o1%n, t2_o1%r
  
!também é possível atribuir os valores as variáveis usando o construtor automático do type
  t2_o1=t2(n=3,r=4.)
  print*, char(11)//'t2_o1%n, t2_o1%r = '
  print*, t2_o1%n, t2_o1%r
  
!vamos tentar usar o construtor do t1
  t1_o1=t1()
!não gera erro, mas não há variáveis nesse tipo para se atribuir valores

end program
