# **Eventos de Tentativas: Explorando os Registros dos Arquivos .timestamps**
As implementações dos eventos de tentativas do Arrasta foram divididas nas duas seguintes unidades:

[Controls Trials Abstract](https://github.com/eep-lab/arrasta/blob/main/src/units/controls.trials.abstract.pas)

[Controls Trials DragDrop](https://github.com/eep-lab/arrasta/blob/main/src/units/controls.trials.dragdrop.pas)

## **Eventos de Tentativas**
Ao conduzir sessões experimentais, o Arrasta captura e registra detalhadamente a sequência de eventos ao longo de cada sessão à medida que ocorrem, para fornecer o relatório completo dos resultados relevantes, com informações acuradas. Os arquivos **.timestamps** desempenham um papel crucial nesse empreendimento.

Os eventos de tentativas são os seguintes: 
- TS
- TE
- Stimuli.Start
- R.Latencia
- R
- Correto _<Modelo\>-<Comparacao\>_
- Errado _<Modelo\>-<Comparacao\>_
- Outro _<Modelo\> <X\> <Y\>_
- Acerto1
- Acerto2 _<Quantidade de Erros\>_

Este documento destina-se a fornecer uma visão clara e concisa dos registros dos eventos de tentativas gerados nos arquivos **.timestamps** da base de relatórios criados após cada sessão experimental. A base de relatórios é composta por três arquivos de texto (nomeados com as extensões _.data_, _.ini_ e ._timestamps_) e desempenha um papel central na coleta, organização e
análise dos resultados obtidos durante o experimento, registrando todos os detalhes relevantes de cada sessão experimental.

Por meio da exploração de uma tabela informativa, você será guiado(a) através dos diversos eventos documentados, compreendendo seus propósitos individuais.

**É importante ressaltar que o registro de tempo desde o início é apresentado em segundos, com 7 casas decimais.**

<div align="center">
  <p>Tabela de Eventos de Tentativas: Significados e Descrições</p>
</div>

| Evento | Significado | Descrição |
| - | - | - |
| TS | Trial Start - Início da Tentativa | Registra o momento exato em que uma tentativa começa. É o registro do tempo desde o início da sessão experimental, apresentado em segundos, com 7 casas decimais, e desde o início da primeira tentativa. |
| TE | Trial End - Fim da Tentativa | Registra o encerramento de uma tentativa individual em uma sessão experimental. Ele ocorre quando todas as etapas e elementos da tentativa foram concluídos, incluindo a apresentação de estímulos, a coleta de respostas e qualquer processo associado à tentativa em questão. |
| Stimuli.Start | Início dos Estímulos | Registra o início da apresentação dos estímulos em uma determinada tentativa durante uma sessão experimental. Esse evento é responsável por capturar quando os estímulos são apresentados na tela ao participante em teste. |
| R.Latencia | Resposta - Latência | Representa a **_latência_** da resposta durante uma tentativa em uma sessão experimental. **_Latência é o período de tempo decorrido entre a apresentação de um estímulo e a subsequente resposta do participante em teste._** |
| R | Resposta ao Modelo | Representa a resposta dada pelo participante em uma sessão experimental. Esse evento captura a resposta fornecida pelo participante em relação ao estímulo em questão. |
| Correto _<Modelo\>-<Comparacao\>_ | Consequência Correta | Indica que a resposta emitida pelo participante durante uma tentativa foi considerada correta de acordo com a comparação específica. A notação _<Modelo\>-<Comparacao\>_ se refere a uma associação entre um estímulo modelo apresentado e o estímulo comparação (ou resposta) esperado para essa associação. |
| Errado _<Modelo\>-<Comparacao\>_ | Consequência Incorreta | Indica que a resposta emitida pelo participante durante uma tentativa é considerada incorreta de acordo com a comparação específica. |
| Outro _<Modelo\> <X\> <Y\>_ | Posicionamento Fora do Alvo | Representa uma situação em que um estímulo modelo é **_posicionado_** em uma área específica da tela, definida pelas coordenadas X e Y, que não corresponde a nenhuma ação de comparação específica. Isso ocorre quando o estímulo modelo é arrastado e solto em um local que não foi designado como um ponto de comparação. **_Posicionado: momento em que o sujeito retirou o dedo do estímulo tocado._** |
| Acerto1 | Tentativa Finalizada *sem* a Ocorrência de Consequências Incorretas | Representa uma situação onde o participante **_finalizou uma tentativa_** sem erros durante ela. Esse evento indica que todas as respostas fornecidas pelo sujeito durante a tentativa foram consideradas corretas. **_Finalizou uma tentativa: momento em que o sujeito retirou o dedo do estímulo tocado._** |
| Acerto2 _<Quantidade de Erros\>_ | Tentativa Finalizada *com* a Ocorrência de Consequências Incorretas | Representa uma situação onde o participante finalizou uma tentativa com erros durante ela. Esse evento indica que houveram respostas incorretas fornecidas pelo sujeito durante a tentativa. A notação _<Quantidade de Erros\>_ indica o número de consequências incorretas durante a tentativa. |

## **Exemplo de Arquivo .timestamps de uma Sessão Experimental Real**
Ao clicar no link a seguir, você terá acesso a um arquivo que ilustra um exemplo do arquivo .timestamps gerado ao final de uma sessão experimental real. Isso permitirá uma melhor visualização da sua estrutura e das métricas essenciais para a análise do comportamento.

[Clique Aqui para Acessar o Arquivo](https://github.com/eep-lab/arrasta/blob/main/docs/ReferenceExample.timestamps)