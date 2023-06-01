# arrasta
Projeto arrasta
Onde se definem áreas não sobrepostas denominadas estímulos, que se destacam contra o fundo.
Os estimulos são divididos em duas classes: moveis e alvos.
Os moveis ao receber toques mantidos tem a posição redefinida em tempo real pela posição do toque mantido.
Os estímulos tem propriedades definidas: p1-forma, p2-arquivo contido (bmp, jpg, tif, mov, gif), p3-posição.
Para cada alvo são definidos n moveis efetivos [n=>0 (n igual ou maior que zero)].
Acerto. Verdadeiro quando um pixel de um movel efetivo e igual a um pixel de um alvo. A ocorrencia de um acerto aciona uma rotina chamada consequencia.
Tipos de consequencia para acerto: C1- apaga tudo menos o móvel e o alvo coincidentes, C2- uma animação e ativada, C3- uma rotina externa é ativada.
