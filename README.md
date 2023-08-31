# arrasta
Projeto arrasta
Onde se definem áreas não sobrepostas denominadas estímulos, que se destacam contra o fundo.
Os estimulos são divididos em duas classes: moveis e alvos.
Os moveis ao receber toques mantidos tem a posição redefinida em tempo real pela posição do toque mantido.
Os estímulos tem propriedades definidas: p1-forma, p2-arquivo contido (bmp, jpg, tif, mov, gif), p3-posição.
Para cada alvo são definidos n moveis efetivos [n=>0 (n igual ou maior que zero)].
Acerto. Verdadeiro quando um pixel de um movel efetivo e igual a um pixel de um alvo. A ocorrencia de um acerto aciona uma rotina chamada consequencia.
Tipos de consequencia para acerto: C1- apaga tudo menos o móvel e o alvo coincidentes, C2- uma animação e ativada, C3- uma rotina externa é ativada.

Project Drag-on
On the monitor screen, non-overlapping areas called stimuli are defined, which stand out against the background.
Stimuli are divided into two classes: rover and target.
The rover, while receiving a sustained touch, has its position redefined in real-time by the position of the sustained touch.
The stimuli have defined properties: p1-shape, p2-contained file (bmp, jpg, tif, mov, gif), p3-position.
For each target, n effective rovers [n=>0 (n equal to or greater than zero)] are defined.
Hit. True when a pixel of an effective rover is equal to a pixel of a target. The occurrence of a hit triggers a routine called consequence.
Types of consequence for hit: C1- erases everything but the matching rover and target, C2- an animation is activated, C3- an external routine is activated.

