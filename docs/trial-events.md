# **Eventos de Tentativas: Explorando os Registros dos Arquivos .timestamps**
As implementações dos eventos de tentativas do Arrasta foram divididas nas duas seguintes unidades:

[Controls Trials Abstract](https://github.com/eep-lab/arrasta/blob/main/src/units/controls.trials.abstract.pas)

[Controls Trials DragDrop](https://github.com/eep-lab/arrasta/blob/main/src/units/controls.trials.dragdrop.pas)

## **Eventos de Tentativas**
Ao conduzir sessões experimentais, é fundamental capturar e analisar cada aspecto do processo para obter resultados valorosos e informações acuradas. Os arquivos **_.timestamps_** desempenham um papel crucial nesse empreendimento, registrando uma trilha detalhada de eventos ao longo de cada sessão.

Este documento destina-se a fornecer uma visão clara e concisa dos eventos de tentativas gerados nos arquivos **_.timestamps_** da base de relatórios criados após cada sessão experimental. Por meio da exploração de uma tabela informativa, você será guiado(a) através dos diversos eventos documentados, compreendendo seus propósitos individuais.

<div align="center">
  <p>Tabela de Eventos de Tentativas: Significados e Descrições</p>
</div>

| Evento | Significado | Descrição |
| - | - | - |
| TS | Trial Start - Início da Tentativa | Representa o início de uma tentativa e é registrado no momento exato em que ela começa. |
| TE | Trial End - Fim da Tentativa | Representa o encerramento de uma tentativa individual em uma sessão experimental. Ele ocorre quando todas as etapas e elementos da tentativa foram concluídos, incluindo a apresentação de estímulos, a coleta de respostas e qualquer processo associado à tentativa em questão. |
| Stimuli.Start | Início dos Estímulos | Representa o início da apresentação de estímulos em uma determinada tentativa durante uma sessão experimental. Esse evento é responsável por capturar quando os estímulos são apresentados na tela ao participante em teste. |
| R.Latencia | Resposta - Latência | Representa a latência da resposta durante uma tentativa em uma sessão experimental. Latência é o período de tempo decorrido entre a apresentação de um estímulo e a subsequente resposta do participante em teste. |
| R | Resposta ao Modelo | Representa a resposta gerada pelo participante em uma sessão experimental. Esse evento captura a resposta fornecida pelo participante em relação ao estímulo em questão. |
| Correto _<Modelo\>-<Comparacao\>_ | Consequência Correta | Indica que a resposta fornecida pelo participante durante uma tentativa é considerada correta de acordo com a comparação específica. A notação _<Modelo\>-<Comparacao\>_ se refere a uma associação entre um estímulo modelo apresentado e o estímulo comparação (ou resposta) esperado para essa associação.|
| Errado _<Modelo\>-<Comparacao\>_ | Consequência Incorreta | Indica que a resposta fornecida pelo participante durante uma tentativa é considerada incorreta de acordo com a comparação específica. |
| Outro _<Modelo\> <X\> <Y\>_ | Posicionamento Fora do Alvo | Representa uma situação em que um estímulo modelo é posicionado em uma área específica da tela, definida pelas coordenadas X e Y, que não corresponde a nenhuma ação de comparação específica. Isso ocorre quando o estímulo modelo é arrastado e solto em um local que não foi designado como um ponto de comparação. |
| Acerto1 | Tentativa Finalizada **sem** a Ocorrência de Consequências Incorretas | Representa uma situação onde o participante finalizou uma tentativa sem erros durante ela. Esse evento indica que a todas as respostas fornecidas pelo sujeito durante a tentativa foram consideradas corretas. |
| Acerto2 _<Quantidade de Erros\>_ | Tentativa Finalizada **com** a Ocorrência de Consequências Incorretas | Representa uma situação onde o participante finalizou uma tentativa com erros durante ela. Esse evento indica que houveram respostas incorretas fornecidas pelo sujeito durante a tentativa. A notação _<Quantidade de Erros\>_ indica o número de consequências incorretas durante a tentativa. |