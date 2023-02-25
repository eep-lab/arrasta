Passo 1:
- Baixar FPUPDELUXE

Passo 2:
- Configurar pasta de origem do projeto FPUPDELUXE
- Marcar o trunck no FPC version e no Lazarus version
- Marcar o checkbox do help nas configurações gerais do projeto FPUPDELUXE
- Instalar o OPM nos componentes

Passo 3:
- Fazer o fork do projeto olavo-arrasta
- git clone --recursive https://github.com/fellipecastro7/stimulus_control.git

Passo 4:
- Dentro do Lazarus, abrir o projeto
- Mudar a build para win64
- Instalar as depêndencias usando o inspetor do projeto olavo-arrasta

Passo 5:
- Baixar o código fonte do Castle Game Engine: https://github.com/castle-engine/castle-engine/archive/refs/tags/snapshot.zip
- Descompactar em qualquer lugar
- O projeto possui como dependência o pacote castle_base.lpk, basta abrir para a o Lazarus saber aonde ele está, não precisa compilar e nem instalar:
  - {local-do-castle-engine}/packages/
- O "castle_base" depende da biblioteca "Vampyre". Abra e compile os dois pacotes da versão embarcada do Vampyre:
  - src/vampyre_imaginglib/src/Packages/VampyreImaginePackage.lpk
  - src/vampyre_imaginglib/src/Packages/VampyreImaginePackageExt.lpk
- Fonte: https://castle-engine.io/wp/2022/01/28/more-integration-with-vampyre-imaging-library-used-by-default-with-both-fpc-and-delphi/
