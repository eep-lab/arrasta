Padrão de commit:
- docs: após a introdução de uma documentação no projeto
- fix: após a resolução de um bug
- feat: após o término do trabalho de uma nova funcionalidade
- bump: deprecated, usar o wip
- wip: work in progress, especifica um trabalho em andamento. Necessidade de squash ou rebase, todos os commits com essa chave serão transformados em outra chave
- refact: refatoração, modificações para legibilidade, otimização etc
- ide: modificações nas especificações da IDE pouco relevantes

Link: https://www.conventionalcommits.org/pt-br/v1.0.0-beta.4/

Obs: sempre que forem feitos comandos do git, especificar aqui.

Padrão de pull (atualizar um braço local com mudanças remotas):
- Deixar o braço local suficientemente limpo (faça o commit das suas mudanças e de preferência crie um novo braço)
- Selecione o braço que receberá as atualizações (git checkout -b nomedobraço)
- git pull upstream olavo-arrasta

Padrão de rotina de trabalho:
- git checkout olavo-arrasta
- git pull
- git checkout -b nomedonovobraço
- git add arquivosmodificados
- git commit -m mensagem
- git push origin nomedonovobraço
- No github, fazer o pull request