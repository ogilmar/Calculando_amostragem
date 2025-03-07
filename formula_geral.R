#Formula geral para calculo de amostra


#Estabelecendo parametros 

N = 20000 #N e a populacao total a ser estudada
Z = 1.96 #Valor da tabela Z. Para intervalo de confianca de 95 %, utilize o valor 1.96
p= 0.5 #proporcao estimada (0.5 ? a m?xima variabilidade estimada)
e= 0.02 #margem de erro em percentual escrito em valores decimais

# definindo o tamanho da amostra

P = (N*(Z^2)*p*(1-p))
E = e^2*(N-1)+Z^2*p*(1-p)

Amostra=P/E
Amostra