!procedure(abstract interface), pointer

!#0 !podemos ler esse código seguindo os marcadors !#1 ..até !#n, pulando ao ver um link !{1} ...!{n}
!olhemos o marcador em PROGRAM primeiramente {1}

!#2 !voltando aqui, olhemos a seção de definições de types e declarações de variaveisa e objetos do m1 antes de continuar
module m1 !nesse tópico vamos definir POINTER TO PROCEDURES,
!a interface deles é realzada manualmente
!para isso precisamos organizart os tipos e as rotinas em um mesmo modulo
implicit none
!isso é um type novo que empacota um inteiro e um real
!e que tem um pointer to procedure denominado p1, obedecendo a interface sp1, e inicializado como apontando para null()
!e que tem um pointer to procedure denominado p2, obedecendo a interface sp2, e inicializado como apontando para sp2
  type t2
    integer :: n
    real :: r
!AbsInt1 e AbsInt2 são os nomes das abstract interface que regem os dois ponteiros a seguir, respectivamante.
    procedure(AbsInt1), pointer, nopass :: p11 => null() !{ver a interface agora}
!#voltando
!é recomendado inicializar todos os pointer to procedure que tenham implementação definida para inicializar, usando o null()
!pois dessa forma, ao tentar acessar esse ponteiro não inicializado haverá um segmentation fault depurável
!e caso contrário esse ponteiro seria selvagem WILD POINTER e a execução poderia pular para qualquer função do programa, imprevisivelmente, podendo gerar erros de difícil depuração ou resultados corrompidos misteriosos

!a palavra chave NOPASS indica que o restante da estrutura não deve ser requisitado pelo procedimento
    procedure(AbsInt2), pointer :: p21 => null() !{ver a interface agora}
!#voltando
!sem a palavra chave NOPASS, é implícito que é pass, então essa é equivalente à
!    procedure(AbsInt2), pointer, pass :: p2 => null()

!essa próxima segue a mesma interface que a anterior
!mas com uma inicialização padrão diferente de null(),
!mas que pode ser sobrescrita por um construtor ou manualmente
    procedure(AbsInt2), pointer :: p22 => sp22 !{ver a implementação sp22 agora}
!#voltando
  end type
  
!uma abstract interface
  abstract interface
    subroutine AbsInt1()
      implicit none
      !nesse caso, a rotina não recebe nada
    end subroutine
  end interface
  
!a outra abstract interface
  abstract interface
    subroutine AbsInt2(z)
      import t2 !o comando import é necessário para a interface saber quais DERIVED TYPES ou variáveis do módulo são requeridos dentro do procedimento
      implicit none !tem que chamar o implicit none dentro da interface
      !o argumento automatico tem que ser polimorfico, i.e. ser da classe t2 que significa aceitar o tipo t2 ou qualquer extensão
      class(t2) :: z
    end subroutine
  end interface
  
!isso é um type novo que extende a funcionalidade do t2
  type, extends(t2) :: t3
    character( len = 1 ) :: c
    procedure(AbsInt3), pointer :: p31 => sp31
  end type
!ele empacota um inteiro e um real, além de um character e um pointer to procedure novo

!a outra abstract interface
  abstract interface
    subroutine AbsInt3(z)
      import t3 !o comando import é necessário para a interface saber quais DERIVED TYPES ou variáveis do módulo são requeridos dentro do procedimento
      implicit none !tem que chamar o implicit none dentro da interface
      !o argumento automatico tem que ser polimorfico, i.e. ser da classe t2 que significa aceitar o tipo t2 ou qualquer extensão
      class(t3) :: z
    end subroutine
  end interface
  
!isso são duas variáveis do tipo t2, e que são ponteiros
  type(t2), pointer :: t2_opa, t2_opb
  
!isso é uma variável da classe t2, que precisa ser um ponteiro,
!pois a palavra chave CLASS significa que não está definido se o objeto pertence ao tipo t2 ou se pertence a uma extensão qualquer desse tipo
!e logo a memória necessária não pode ser determinada neste ponto
  class(t2), pointer :: c2_o1, c2_o2
!isso é uma variável do tipo t3 não polimórfica
  type(t3), pointer :: t3_op1
  
!terminamos o cabeçalho, vamos à execução {7}
  
contains

!#8 aqui começa a execução
  subroutine s1()
    !a alocação de memória e atribuição de valor podem ser feitas simultaneamente com a palavra chave SOURCE
    !inclusive a atribuição de rotinas compatíveis com as interfaces abstratas aos pointer to procedures do type que não tenham sido inicializadas
  allocate(t2_opa, source = t2(n=7,r=8.,p11=sp11a,p21=sp21a) )
  allocate(t2_opb, source = t2(n=7,r=8.,p11=sp11b,p21=sp21b) )

!é possível imprimir o valor de suas variáveis usando o typebound procedure implementado
!para chamar um TYPEBOUND procedure, usamos a sintaxe " OBJETO % PROCEDIMENTO ", o que passa o objeto como primeiro argumento esperado pela rotina
!e traz a interpretação de que pedimos o procedimento tal pertencente ao objeto tal com suas propriedades
  call t2_opa%p11
  call t2_opa%p21
  call t2_opa%p22
  
  call t2_opb%p11
  call t2_opb%p21
  call t2_opb%p22

  !para uma variável de class, e portanto sendo pointer, é essencial usar a forma allocate/source
    allocate(c2_o1, source = t2(n=7,r=8.,p11=sp11a,p21=sp21a) )
  !o tipo especifico do objeto e definido pelo objeto a disposição da SOURCE, no caso foi um objeto do tipo t2

  !é possível imprimir o valor de suas variáveis usando o typebound procedure implementado para a classe t2
    call c2_o1%p11
    call c2_o1%p21
    call c2_o1%p22
  !como o tipo é o t2, vai rodar a implementação sp1
  !então executa-se o que foi visto no item 3

  !para uma variável de class, é possível usar a forma allocate/source com um objeto de uma extensão da classe dessa variável
    allocate(c2_o2, source = t3(n=11,r=12.,c='a',p11=sp11a,p21=sp21a,p31=sp31) )
  !nesse caso foi um objeto do tipo t3

  !é possível imprimir o valor de suas variáveis usando o typebound procedure implementado para a classe t2
    call c2_o2%p11
    call c2_o2%p21
    call c2_o2%p22

!              como o programa reconhece o objeto como da classe t2, só é possível acessar diretamente as variáveis disponíveis nessa classe
!              a não ser usando procedimentos atrelados ao tipo TYPE BOUND PROCEDURES, a se ver no próximo tópico

  !esse é um objeto reconhecido como do tipo t3
    allocate(t3_op1, source = t3(n=13,r=14.,c='b',p11=sp11a,p21=sp21a,p31=sp31) )

  !é possível imprimir o valor de suas variáveis usando o typebound procedure implementado para a classe t2
    call t3_op1%p11
    call t3_op1%p21
    call t3_op1%p22
    call t3_op1%p31
                
!como o programa reconhece o objeto como da classe t2, só é possível acessar diretamente as variáveis disponíveis nessa classe
!a não ser usando procedimentos atrelados ao tipo, a se ver no próximo tópico
!ou, contigentemente, usando um bloco de controle especial:

    call c2_o1%p11
    call c2_o1%p21
    call c2_o1%p22
  select type(c2_o1)
  class is(t2)
    !nothing else
  class is (t3)
    call c2_o1%p31
  end select

    call c2_o2%p11
    call c2_o2%p21
    call c2_o2%p22
  select type(c2_o2)
  class is(t2)
    !nothing else
  class is (t3)
    call c2_o2%p31
  end select




  !rotina concluída, voltamos a quem chamou, no caso foi o program
  end subroutine
  
  !#3 essa é a rotina TYPEBOUND para o tipo t2, TYPE BOUND quer dizer que ela espera receber como 1o argumento, um objeto da classe t2
  subroutine sp11a()
    !essa rotina não recebe argumento nenhum, não há muito o que elea possa fazer nesse exemplo
    print*, 
  print*, 'sp11a'
 end subroutine
 
   subroutine sp11b()
    !essa rotina não recebe argumento nenhum, não há muito o que elea possa fazer nesse exemplo
    print*, 
  print*, 'sp11b'
 end subroutine
 
  subroutine sp21a(z)
    class(t2) :: z
    print*, 
  print*, 'sp21a, r, n'
    print*, z%r, z%n
  end subroutine
 
  subroutine sp21b(z)
    class(t2) :: z
    print*, 
  print*, 'sp21b, r, n'
    print*, z%r, z%n
  end subroutine
 
  subroutine sp22(z)
    class(t2) :: z
    print*, 
  print*, 'sp22, r, n'
    print*, z%r, z%n
  end subroutine
  
  subroutine sp31(z)
    class(t3) :: z
    call z%p21
    print*, 
  print*, 'sp31, c'
    print*, z%c
  end subroutine

end module

!#1 !o programa começa necessariamente aqui
program main
!necessitamos acesso a rotina s1, onde estão todos os exemplos deste tópico
!olhemos também a seção de definições de types e declarações de variaveisa e objetos do m1 antes de continuar 
use m1, only: s1 !{2}
implicit none
!#7 !vamos à implementação
!a ordem de execução desse programa será PROGRAM -> s1 -> sp1 -> s1 -> sp4 -> sp1 -> sp4 -> s1 -> program
!vamos à rotina s1
call s1() !{8}

!#n
!tópico concluído
end program


!obj class t2 não terá acesso ao sp4, só com select type ou com os type bound
