!integer; type; assignment; constructor

program main

!isso é uma variável do type integer
  integer :: i
!esse type é intrinseco do fortran

!isso é um type novo, (também conhecido com Struct) sendo definido agora, que empacota um inteiro e um real
  type t
    !os valores a seguir são valores DEFAULT para as variáveis de objetos desse tipo, e que podem ser modificadas
    integer :: n = -1
    real :: r = -2.
  end type
  
!isso é uma variável do tipo t, também chamada de objeto do tipo t
  type(t) :: t_o1, t_o2
  
!isso é uma atribuição de valor (ASSIGNMENT) a variável i
  i=1

!isso imprime o valor da variável i
  print*, 
  print*, 'i = '
  print*, i
  
  
!para acessar uma variável desse tipo, usamos o TOKEN %, que dá a idéia de posse
!a seguir queremos imprimir o valor da vriável n pertencente ao objeto t_o1, e o da variável r pertencente ao objeto t_o1
  print*, 
  print*, 't_o1%n, t_o1%r = '
  print*, t_o1%n, t_o1%r
!ao pedir a impressão dos valores das variaveis desse objeto, recebemos os valores padrão
  
!isso são atribuições de valor às variáveis n e r do objeto t_o1
  t_o1%n=1
  t_o1%r=2.
  print*, 
  print*, 't_o1%n, t_o1%r = '
  print*, t_o1%n, t_o1%r
  
!também é possível atribuir os valores as variáveis usando o construtor automático do type
  t_o1=t(n=3,r=4.)
!o construtor sobrescreve os valores de com inicialização DEFAULT
  print*, 
  print*, 't_o1%n, t_o1%r = '
  print*, t_o1%n, t_o1%r
  
!também é possível atribuir os valores as variáveis usando o construtor automático do type apenas para algumas das variáveis, caso outras já possuam valores de inicialização default
  t_o2=t(r=4.)
!o construtor sobrescreve os valores de com inicialização DEFAULT
  print*, 
  print*, 't_o2%n, t_o2%r = '
  print*, t_o2%n, t_o2%r
  
end program
