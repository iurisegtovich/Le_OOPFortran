program main

!isso é um type novo que empacota um inteiro e um real
  type t2
    integer :: n
    real :: r
  end type
  
!isso é um type novo que extende a funcionalidade do t2
  type, extends(t2) :: t3
    character( len = 1 ) :: c
  end type
!ele empacota um inteiro e um real, além de um character
  
!isso é uma variável do tipo t2, também chamada de objeto do tipo t2
  type(t2) :: t2_o1
  
!isso são duas variáveis do tipo t2, e que são ponteiros
  type(t2), pointer :: t2_op1, t2_op2
  
!isso é uma variável da classe t2, que precisa ser um ponteiro,
!pois a palavra chave CLASS significa que não está definido se o objeto pertence ao tipo t2 ou se pertence a uma extensão qualquer desse tipo
!e logo a memória necessária não pode ser determinada neste ponto
  class(t2), pointer :: c2_o1, c2_o2, c2_o3
  
  type(t3), pointer :: t3_op1
  
!é possível atribuir os valores as variáveis usando o construtor automático do type
  t2_o1=t2(n=3,r=4.)
  print*, char(11)//'t2_o1%n, t2_o1%r = '
  print*, t2_o1%n, t2_o1%r
  
!para o objeto que é ponteiro, primeiro, memória precisa ser alocada
  allocate(t2_op1)
  t2_op1=t2(n=5,r=6.)
  print*, char(11)//'t2_op1%n, t2_op1%r = '
  print*, t2_op1%n, t2_op1%r

!a alocação de memória e atribuição de valor podem ser feitas simultaneamente com a palavra chave SOURCE
  allocate(t2_op2, source = t2(n=7,r=8.) )
  print*, char(11)//'t2_op2%n, t2_op2%r = '
  print*, t2_op2%n, t2_op2%r
  
!para uma variável de class, e portanto sendo pointer, é essencial usar a forma allocate/source
  allocate(c2_o1, source = t2(n=9,r=10.) )
  print*, char(11)//'c2_o1%n, c2_o1%r = '
  print*, c2_o1%n, c2_o1%r
!o tipo especifico do objeto e definido pelo objeto a disposição da SOURCE, no caso foi um objeto do tipo t2

!para uma variável de class, é possível usar a forma allocate/source com um objeto de uma extensão da classe dessa variável
  allocate(c2_o2, source = t3(n=11,r=12.,c='a') )
  print*, char(11)//'c2_o2%n, c2_o2%r = '
  print*, c2_o2%n, c2_o2%r
!nesse caso foi um objeto do tipo t3
  
!como o programa reconhece o objeto como da classe t2, só é possível acessar diretamente as variáveis disponíveis nessa classe
!a não ser usando procedimentos atrelados ao tipo, a se ver no próximo tópico
!ou, contigentemente, usando um bloco de controle especial:
  select type(c2_o2)
  class is(t2)
  print*, char(11)//'c2_o2%n, c2_o2%r = '
  print*, c2_o2%n, c2_o2%r
  class is (t3)
  print*, char(11)//'c2_o2%n, c2_o2%r, c2_o2%c = '
  print*, c2_o2%n, c2_o2%r, c2_o2%c
  end select
  
!esse é um objeto reconhecido como do tipo t3
  allocate(t3_op1, source = t3(n=13,r=14.,c='b') )
  print*, char(11)//'t3_op1%n, t3_op1%r, t3_op1%c = '
  print*, t3_op1%n, t3_op1%r, t3_op1%c
!nesse caso é possível acessar diretamente todas as variáveis do tipo
  
  
end program


