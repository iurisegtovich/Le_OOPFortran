!#type, abstract; procedure(abstract interface), deferred

!#0 !podemos ler esse código seguindo os marcadors !#1 ..até !#n, pulando ao ver um link !{1} ...!{n}
!olhemos o marcador em PROGRAM primeiramente {1}

!#2 !voltando aqui, olhemos a seção de definições de types e declarações de variaveisa e objetos do m1 antes de continuar
module m1 !nesse tópico vamos definir TYPE BOUND PROCEDURES,
!a interface deles é realzada automaticamente
!mas para isso precisamos organizart os tipos e as rotinas em um mesmo modulo

!isso é um type novo que empacota um inteiro e um real
!e que tem um type bound procedure cujo binding name é disp
  type, abstract :: t
    !em geral um tipo abstrato é um tipo que não tem variáveis a serem trabalhadas, apenas interface de abstração para procedures a se extender
  contains
    !aqui está o TYPE BOUND PROCEDURE, ele fica referido na seção contaisnde um type
    !como o type é abstract, esse procedure é DEFERRED
    !uqer dizer que precisamos saber o que entra e sai dele, mas não precisamos implementar a funcionalidade deles, isso fica para as extensões fazerem
    !estudemos a estrutura dessa interface antes de continuar
    procedure(AbsInt1), deferred :: disp !{3}
    !#4 isso conclui esse type
  end type

!uma abstract interface
  abstract interface
    subroutine AbsInt1(z)
      import t
      implicit none
      class(t) :: z
      !nesse caso, a rotina não recebe nada
    end subroutine
  end interface

!isso é um type novo que extende a funcionalidade do t
  type, extends(t) :: t2
    integer :: n
    character( len = 1 ) :: c
  contains
    !aqui fazemos que o procedimento disp é OVERRIDEN pelo procedimento disp2
    !quer dizer que ao chamar o disp de um t2, vai rodar a rotina disp2
    !estudemos a estrutura desse antes de continuar
    procedure :: disp => disp2 !{5}
    !#6 isso conclui esse type
  end type
!ele empacota um inteiro e um real, além de um character
  
!aqui um extensão alternativa
  type, extends(t) :: t3
    real :: r
    logical :: l
  contains
    !aqui fazemos que o procedimento disp é OVERRIDEN pelo procedimento disp2
    !quer dizer que ao chamar o disp de um t2, vai rodar a rotina disp2
    !estudemos a estrutura desse antes de continuar
    procedure :: disp => disp3 !{5}
    !#6 isso conclui esse type
  end type
  
!não é possível usar a linha a seguir para degrar um objeto do tipo t
!  type(t), pointer :: t_op1
!pois o tipo abstrato não permite objetos gerado diretamente dele, mas apenas extensões
  
!isso é uma variável da classe t, que precisa ser um ponteiro,
!pois a palavra chave CLASS significa que não está definido se o objeto pertence ao tipo t ou se pertence a uma extensão qualquer desse tipo
!e logo a memória necessária não pode ser determinada neste ponto
  class(t), pointer :: c_o1, c_o2
!é possível fazer um ponteiro da classe abstrata t, em que ele deverá ser alocado usando o construtor de alguma das extensões de t
  
  !terminamos o cabeçalho, vamos à execução {7}
  
contains

!#8 aqui começa a execução
  subroutine s1()
    !a alocação de memória e atribuição de valor podem ser feitas simultaneamente com a palavra chave SOURCE
  allocate(c_o1, source = t2(n=7,c='a') )
  allocate(c_o2, source = t3(r=8.,l=.true.) )

!é possível imprimir o valor de suas variáveis usando o typebound procedure implementado
!para chamar um TYPEBOUND procedure, usamos a sintaxe " OBJETO % PROCEDIMENTO ", o que passa o objeto como primeiro argumento esperado pela rotina
!e traz a interpretação de que pedimos o procedimento tal pertencente ao objeto tal com suas propriedades
  call c_o1%disp !executa-se o que foi visto no item 3
  call c_o2%disp !executa-se o que foi visto no item 3
  
  !rotina concluída, voltamos a quem chamou, no caso foi o program
  end subroutine
  
  !#3 !o tipo t é abstrato, então não pode ter implementação de disp, mas podemos fazer uma rotina disp1 no módulo que aceita objetos de qualquer tipo da classe t e trabalhe com as propriedades disponiveis nela
  subroutine disp2(z)
    !o argumento ser da CLASS tal, e não do TYPE tal, significa ser um argumento polimórfico isso é, do tipo t ou qualquer tipo que seja extensão
    class(t2) :: z
    !o que a gente escolheu fazer aqui foi imprimir os valores das variáveis
    print*, 
  print*, 'disp2'
  print*, 'c2%n, c2%c = '
    print*, z%n, z%c
    !rotina concluída, retornemos o estudo ao item {4}
 end subroutine

  !#5 essa é a rotina TYPEBOUND para o tipo t2, ela espera receber um argumento da classe t2, isso é, do tipo t2 ou qualquer extensão
  !ela não vai aceitar argumento do tipo t
  subroutine disp3(z)
    class(t3) :: z
    !e depois retornamos para fazer mais alguma coisa, no caso imprimir a propriedade que esse tipo tem em extensão das do tipo superior
    print*, 
  print*, 'disp3'
  print*, 'c3%r, c3%l = '
    print*, z%r, z%l
    
    !e assim concluímos essa rotina {6}
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


