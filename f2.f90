program main

!isso é uma variável do type integer
  integer :: i
!esse type é intrinseco do fortran

!isso é uma variável do type integer, que é um ponteiro
  integer, pointer :: ip

!isso é um type novo que empacota um inteiro e um real
  type t2
    integer :: n
    real :: r
  end type
  
!isso é uma variável do tipo t2, também chamada de objeto do tipo t2
  type(t2) :: t2_o1
  
!isso são duas variáveis do tipo t2, e que são ponteiros
  type(t2), pointer :: t2_op1, t2_op2
  
!isso é uma atribuição de valor a variável i
  i=1
  
!para atribuir valor a variável ip, ela primeiro precisa ter memória alocada
  allocate(ip)
  ip=2

!isso imprime o valor da variável i e ip
  print*, char(11)//'i, ip = '
  print*, i, ip
  
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
  
end program


