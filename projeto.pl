% 106322 Raquel Rodrigues
:- set_prolog_flag(answer_write_options,[max_depth(0)]). % para listas completas
:- ['dados.pl'], ['keywords.pl']. % ficheiros a importar.

% Funcoes auxiliares.

/* lista_periodos/1
 verdadeiro se o seu argumento for uma lista de periodos */

lista_periodos([]).
lista_periodos([P|Periodos]) :-
    is_periodo(P),
    lista_periodos(Periodos).

/* is_periodo/1
  verdadeiro se P for um periodo valido. */

is_periodo(P) :-
    member(P, [p1,p2,p3,p4]).

/* periodo/2
 verdadeiro se o evento ID decorre durante o periodo introduzido,
 correpondente ao segundo argumento. */

periodo(ID, p1) :-
    horario(ID,_,_,_,_,p1_2);
    horario(ID,_,_,_,_,p1).
periodo(ID, p2) :-
    horario(ID,_,_,_,_,p1_2);
    horario(ID,_,_,_,_,p2).
periodo(ID, p3) :-
    horario(ID,_,_,_,_,p3_4);
    horario(ID,_,_,_,_,p3).
periodo(ID, p4) :-
    horario(ID,_,_,_,_,p3_4);
    horario(ID,_,_,_,_,p4).

/* eventosSemSalasPeriodo_aux/2
 verdadeiro se EventosSemSala for uma lista constituida pelos eventos
 sem sala que decorrem no periodo Periodo. */

eventosSemSalasPeriodo_aux(Periodo, EventosSemSala) :-
    eventosSemSalas(TodosSemSala),
    findall(ID, (member(ID, TodosSemSala), periodo(ID, Periodo)),
            EventosSemSalaDesordenados),
    sort(EventosSemSalaDesordenados, EventosSemSala).

/* organizaEventos_aux/4
 organizaEventos_aux(ListaEventos,Periodo,EventosNoPeriodo,
 EventosNoPeriodoDesordenados) e verdadeiro se EventosNoPeriodo for a
 lista, ordenada e sem elementos repetidos, de IDs dos eventos da
 ListaEventos que ocorrem no periodo Periodo. */

organizaEventos_aux([],_,EventosNoPeriodo,EventosNoPeriodoDesordenados) :-
    !, %se a lista for vazia, nao se verificam outras clausulas.
    sort(EventosNoPeriodoDesordenados, EventosNoPeriodo).
    %ordenar a lista no caso terminal.

organizaEventos_aux([El|Resto], Periodo, EventosNoPeriodo, ListaRes) :-
    periodo(El, Periodo),!,
    %se o elemento pertence ao Periodo, e desnecessario verificar a clausula seguinte.
    organizaEventos_aux(Resto, Periodo, EventosNoPeriodo, [El|ListaRes]).
organizaEventos_aux([El|Resto], Periodo, EventosNoPeriodo, ListaRes) :-
    not(periodo(El, Periodo)),
    organizaEventos_aux(Resto, Periodo, EventosNoPeriodo, ListaRes).

/* semestre/3
 semestre(Disciplina, Curso, S) e verdadeiro se Disciplina for uma
 disciplina do Curso que decorre no semestre S. */

semestre(Disciplina, Curso, s1) :-
    procuraDisciplinas(Curso, LDCurso),
    member(Disciplina, LDCurso),
    evento(ID, Disciplina,_,_,_), !,
    %basta encontrar um ID de evento com a disciplina Disciplina.
    (periodo(ID, p1); periodo(ID, p2)).
semestre(Disciplina, Curso, s2) :-
    procuraDisciplinas(Curso, LDCurso),
    member(Disciplina, LDCurso),
    evento(ID, Disciplina,_,_,_), !,
    (periodo(ID, p3); periodo(ID, p4)).

/* organizaDisciplinas/5
 organizaDisciplinas(ListaDisciplina, Curso, Semestres, S1, S2) e
 verdadeiro se S1 e S2 forem as listas de disciplinas de
 ListaDisciplinas do curso Curso que decorrem no semestre 1 e no
 semestre 2, respetivamente, e Semestres for a lista contendo S1 e S2
 ordenadas. */

organizaDisciplinas([], _, Semestres, S1, S2) :-
    !,
    S1 \== [],
    S2 \== [],
    sort(S1, Semestre1),
    sort(S2, Semestre2),
    append([Semestre1], [Semestre2], Semestres).
    %concatenar as listas com uma lista cada, para obter a lista final.

organizaDisciplinas([Disciplina|Resto], Curso, Semestres, S1, S2) :-
    semestre(Disciplina, Curso, s1),
    !, %se pertence ao semestre 1 nao verificamos se pertence ao semestre 2
    organizaDisciplinas(Resto, Curso, Semestres, [Disciplina|S1], S2).
organizaDisciplinas([Disciplina|Resto], Curso, Semestres, S1, S2) :-
    semestre(Disciplina, Curso, s2),
    !, %nao e preciso verificar se nao pertence a nenhum semestre
    organizaDisciplinas(Resto, Curso, Semestres, S1, [Disciplina|S2]).
organizaDisciplinas([Disciplina|Resto], Curso, Semestres, S1, S2) :-
    not(semestre(Disciplina, Curso, _)),
    !, organizaDisciplinas(Resto, Curso, Semestres, S1, S2).

/* duracao/2
 verdadeiro se Duracao for a duracao do evento identificado por ID. */

duracao(ID, Duracao) :-
    horario(ID,_,_,_,Duracao,_).

/* evolucaoNAnos/4
 verdadeiro se Evolucao for a lista ordenada da evolucao das horas
 associadas ao curso Curso e EvolucaoAnosAnteriores for a
 lista da evolucao das horas associadas ao curso dos anos anteriores ao
 ano N. */

evolucaoNAnos(_, 4, Evolucao, Evolucao).
evolucaoNAnos(Curso, N, EvolucaoAnosAnteriores, Evolucao) :-
    N =< 3, !,
    setof((N, Periodo, NumHoras), horasCurso(Periodo, Curso, N, NumHoras),
          EvolucaoAnoN),
    NSeguinte is N + 1,
    append(EvolucaoAnosAnteriores, EvolucaoAnoN, Res),
    evolucaoNAnos(Curso, NSeguinte, Res, Evolucao).

/* numHorasOcupadas_aux/4
 numHorasOcupadas(ListaEventos, HoraInicioDada, HoraFimDada, SomaHoras)
 e verdadeiro se SomaHoras for o numero total de horas sobrepostas
 entre os eventos de ListaEventos e o slot associado a HoraInicioDada e
 HoraFimDada. */

numHorasOcupadas_aux([],_,_,0).

numHorasOcupadas_aux([ID|Resto], HoraInicioDada, HoraFimDada, SomaHoras) :-
    horario(ID,_,HoraInicioEvento, HoraFimEvento, _, _),
    ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas),
    !, numHorasOcupadas_aux(Resto, HoraInicioDada, HoraFimDada, SomaHorasResto),
    SomaHoras is Horas + SomaHorasResto.

numHorasOcupadas_aux([ID|Resto], HoraInicioDada, HoraFimDada, SomaHorasResto) :-
    horario(ID,_,HoraInicioEvento, HoraFimEvento, _, _),
    not(ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, _)),
    !, numHorasOcupadas_aux(Resto, HoraInicioDada, HoraFimDada, SomaHorasResto).

/* ocupacaoCritica_aux/6
 ocupacaoCritica(ListaSemana, Periodo, HoraInicio, HoraFim, Threshold,
 Resultados) e verdadeiro se Resultados for uma lista de tuplos do tipo
 casosCriticos(DiaSemana, TipoSala, Percentagem) em que DiaSemana,
 TipoSala e Percentagem sao, respetivamente, um elemento da lista
 ListaSemana, um tipo de sala e a sua percentagem de ocupacao, durante
 o periodo Periodo, no intervalo de tempo entre HoraInicio e HoraFim, e
 Percentagem esta acima do Threshold. */

ocupacaoCritica_aux([], _, _, _, _, []).
ocupacaoCritica_aux([Dia|Resto], Periodo, HI, HF, Threshold, Res) :-
    findall(casosCriticos(Dia, TipoSala, Percentagem),
            (numHorasOcupadas(Periodo,TipoSala,Dia,HI,HF,SomaHoras),
             ocupacaoMax(TipoSala,HI,HF,Max),
             percentagem(SomaHoras,Max,PercentagemI), PercentagemI > Threshold,
             Percentagem is ceiling(PercentagemI)),
            Res1),
    ocupacaoCritica_aux(Resto, Periodo, HI, HF, Threshold, Res2),
    append(Res1, Res2, Res).


% Funcoes Principais

/* eventosSemSalas/1
 verdadeiro se o seu argumento for uma lista, ordenada
 e sem elementos repetidos, de IDs de eventos sem sala. */

eventosSemSalas(EventosSemSala) :-
    setof(ID, D^T^NA^evento(ID, D,T,NA,semSala), EventosSemSala).

/* eventosSemSalasDiaSemana/2
 verdadeiro se EventosSemSala for uma lista, ordenada e sem elementos
 repetidos, de IDs de eventos sem sala que decorrem em DiaDaSemana. */

eventosSemSalasDiaSemana(DiaDaSemana,EventosSemSala) :-
    eventosSemSalas(TodosSemSala),
    findall(IDEvento, (member(IDEvento, TodosSemSala),
                       horario(IDEvento,DiaDaSemana,_,_,_,_)),
            EventosSemSalaDesordenados),
    sort(EventosSemSalaDesordenados, EventosSemSala).

/* eventosSemSalasPeriodo/2
 verdadeiro se ListaPeriodos for uma lista de periodos e
 EventosSemSala e uma lista ordenada e sem elementos repetidos, de IDs
 de eventos sem sala nos periodos de ListaPeriodos */

eventosSemSalasPeriodo(ListaPeriodos,EventosSemSala) :-
    lista_periodos(ListaPeriodos),
    maplist(eventosSemSalasPeriodo_aux, ListaPeriodos, ListaEventosSemSala),
    append(ListaEventosSemSala, EventosSemSala_desordenada),
    sort(EventosSemSala_desordenada, EventosSemSala).

/* organizaEventos/3
 verdadeiro se EventosNoPeriodo for a lista, ordenada e sem elementos
 repetidos, de IDs dos eventos de ListaEventos que ocorrem no periodo
 Periodo. */

organizaEventos(ListaEventos, Periodo, EventosNoPeriodo) :-
    is_periodo(Periodo),
    organizaEventos_aux(ListaEventos, Periodo, EventosNoPeriodo, []).
    %solucao iterativa

/* eventosMenoresQue/2
 verdadeiro se ListaEventosMenoresQue for a lista ordenada e sem
 elementos repetidos dos identificadores dos eventos com duracao menor
 ou igual a Duracao */

eventosMenoresQue(Duracao, ListaEventosMenoresQue) :-
    setof(ID, DS^HI^HF^DuracaoEvento^P^(horario(ID,DS,HI,HF,DuracaoEvento,P),
                                        DuracaoEvento =< Duracao), ListaEventosMenoresQue).

/* eventosMenoresQueBool/2
 verdadeiro se o evento identificado por ID tiver duracao menor ou
 igual a Duracao */

eventosMenoresQueBool(ID, Duracao) :-
    eventosMenoresQue(Duracao, ListaEventosMenoresQue),
    member(ID,ListaEventosMenoresQue).

/* procuraDisciplinas/2
 verdadeiro se ListaDisciplinas for a lista ordenada alfabeticamente do
 nome das disciplinas do curso Curso. */

procuraDisciplinas(Curso, ListaDisciplinas) :-
    setof(Disciplina, ID^A^NT^T^NA^S^(turno(ID,Curso,A,NT),
                                      evento(ID,Disciplina,T,NA,S)), ListaDisciplinas).

/* organizaDisciplinas/3
 verdadeiro se Semestres for uma lista com duas listas. */

organizaDisciplinas(ListaDisciplinas, Curso, Semestres) :-
    procuraDisciplinas(Curso, ListaDisciplinasCurso),
    not(intersection(ListaDisciplinasCurso, ListaDisciplinas, [])),
    %o predicado falha se a intersecao for vazia
    organizaDisciplinas(ListaDisciplinas, Curso, Semestres, [], []).

/* horasCurso/4
 verdadeiro se TotalHoras for o numero de horas total dos eventos
 associados ao curso Curso, no ano Ano e periodo Periodo. */

horasCurso(Periodo, Curso, Ano, TotalHoras) :-
    findall(ID, turno(ID, Curso, Ano, _), ListaEventos),
    organizaEventos(ListaEventos, Periodo, EventosNoPeriodo),
    maplist(duracao, EventosNoPeriodo, DuracaoEventos),
    sum_list(DuracaoEventos, TotalHoras).

/* evolucaoHorasCurso/2
 verdadeiro se Evolucao for uma lista de tuplos na forma
 (Ano,Periodo,NumHoras), ordenada por ano e periodo, em que NumHoras e
 o total de horas associadas ao curso Curso, no ano Ano e no periodo
 Periodo. */

evolucaoHorasCurso(Curso, Evolucao) :-
    evolucaoNAnos(Curso, 1, [], Evolucao).

/* ocupaSlot/5
 verdadeiro se Horas for o numero de horas sobrepostas entre o evento
 (associado a HoraInicioEvento e HoraFimEvento) e o slot (associado a
 HoraInicioDada e HoraFimDada). */

ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, _) :-
    (HoraInicioDada  >= HoraFimEvento; HoraFimDada =< HoraInicioEvento), !, fail.
    %o predicado falha se nao existirem sobreposicoes.

ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    HoraInicioEvento >=  HoraInicioDada, HoraFimEvento =< HoraFimDada,
    !, Horas is HoraFimEvento - HoraInicioEvento.

ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    HoraInicioEvento >=  HoraInicioDada, HoraFimEvento >= HoraFimDada,
    !, Horas is HoraFimDada - HoraInicioEvento.

ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    HoraInicioEvento =<  HoraInicioDada, HoraFimEvento >= HoraFimDada,
    !, Horas is HoraFimDada - HoraInicioDada.

ocupaSlot(HoraInicioDada, HoraFimDada, HoraInicioEvento, HoraFimEvento, Horas) :-
    HoraInicioEvento =<  HoraInicioDada, HoraFimEvento =< HoraFimDada,
    !, Horas is HoraFimEvento - HoraInicioDada.

/* numHorasOcupadas/6
 verdadeiro se SomaHoras for o numero de horas ocupadas nas salas do
 tipo TipoSala, no intervalo de tempo definido entre HoraInicio e
 HoraFim, no dia da semana DiaSemana, e no periodo Periodo. */

numHorasOcupadas(Periodo, TipoSala, DiaSemana, HoraInicio, HoraFim, SomaHoras) :-
    salas(TipoSala, Salas),
    findall(ID, (horario(ID,DiaSemana,_,_,_,_), evento(ID,_,_,_,Sala),
                 member(Sala, Salas)), ListaEventos),
    organizaEventos(ListaEventos, Periodo, EventosNoPeriodo),
    numHorasOcupadas_aux(EventosNoPeriodo, HoraInicio, HoraFim, SomaHoras).

/* ocupacaoMax/4
 verdadeiro se Max for o numero de horas possiveis de ser ocupadas por
 salas do tipo TipoSala, no intervalo de tempo definido entre
 HoraInicio e HoraFim. */

ocupacaoMax(TipoSala, HoraInicio, HoraFim, Max) :-
    salas(TipoSala, Salas),
    length(Salas, N), %N corresponde ao numero de salas
    Intervalo is HoraFim - HoraInicio,
    Max is Intervalo*N.

/* percentagem/3
 verdadeiro se Percentagem for a divisao de SomaHoras por Max,
 multiplicada por 100. */

percentagem(SomaHoras, Max, Percentagem) :-
    Percentagem is (SomaHoras/Max)*100.

/* ocupacaoCritica/4
 verdadeiro se Resultados for uma lista ordenada de tuplos do tipo
 casosCriticos(DiaSemana, TipoSala, Percentagem) em que DiaSemana,
 TipoSala e Percentagem sao, respetivamente, um dia da semana, um tipo
 de sala e a sua percentagem de ocupacao, no intervalo de tempo entre
 HoraInicio e HoraFim, e Percentagem esta acima do Threshold. */

ocupacaoCritica(HoraInicio, HoraFim, Threshold, Resultados) :-
    ListaSemana = [segunda-feira, terca-feira, quarta-feira, quinta-feira, sexta-feira],
    ocupacaoCritica_aux(ListaSemana, p1, HoraInicio, HoraFim, Threshold, Res_p1),
    ocupacaoCritica_aux(ListaSemana, p2, HoraInicio, HoraFim, Threshold, Res_p2),
    ocupacaoCritica_aux(ListaSemana, p3, HoraInicio, HoraFim, Threshold, Res_p3),
    ocupacaoCritica_aux(ListaSemana, p4, HoraInicio, HoraFim, Threshold, Res_p4),
    append([Res_p1, Res_p2, Res_p3, Res_p4], Res),
    sort(Res, Resultados).

% Funcoes auxiliares de ocupacaoMesa

/* cab1/2
  cab1(Pessoa, OcupacaoMesa) e verdadeiro se Pessoa for a pessoa que
  fica na cabeceira 1 e se OcupacaoMesa for o respetivo plano de
  ocupacao da mesa.*/

cab1(X4, [[_,_,_],[X4,_],[_,_,_]]).

/* cab2/2
  cab1(Pessoa, OcupacaoMesa) e verdadeiro se Pessoa for a pessoa que
  fica na cabeceira 1 e se OcupacaoMesa for o respetivo plano de
  ocupacao da mesa.*/

cab2(X5, [[_,_,_],[_,X5],[_,_,_]]).

/* honra/3
  honra(Pessoa1, Pessoa2, OcupacaoMesa) e verdadeiro se Pessoa1 estiver
  numa das cabeceiras e Pessoa2 ficar a sua direita, e se OcupacaoMesa
  for o respetivo plano de ocupacao da mesa.*/

honra(X4, X6, [[_,_,_],[X4,_],[X6,_,_]]). honra(X5, X3,
  [[_,_,X3],[_,X5],[_,_,_]]).

/* lado/3
  lado(Pessoa1, Pessoa2, OcupacaoMesa) e verdadeiro se Pessoa1 e Pessoa2
  ficarem lado a lado na mesa e OcupacaoMesa for o respetivo plano de
  ocupacao da mesa.*/

lado(P1, P2, [[P1,P2,_],[_,_],[_,_,_]]).
lado(P1, P2, [[P2,P1,_],[_,_],[_,_,_]]).
lado(P1, P2, [[_,P1,P2],[_,_],[_,_,_]]).
lado(P1, P2, [[_,P2,P1],[_,_],[_,_,_]]).
lado(P1, P2, [[_,_,_],[_,_],[P1,P2,_]]).
lado(P1, P2, [[_,_,_],[_,_],[P2,P1,_]]).
lado(P1, P2, [[_,_,_],[_,_],[_,P1,P2]]).
lado(P1, P2, [[_,_,_],[_,_],[_,P2,P1]]).

/* naoLado/3
  naoLado(Pessoa1, Pessoa2, OcupacaoMesa) e verdadeiro se Pessoa1 e
  Pessoa2 nao ficarem lado a lado na mesa e OcupacaoMesa for o respetivo
  plano de ocupacao da mesa.*/

naoLado(P1, P2,  L) :-
    not(lado(P1, P2, L)).

/* frente/3
  frente(Pessoa1, Pessoa2, OcupacaoMesa) e verdadeiro se Pessoa1 e
  Pessoa2 ficarem frente a frente na mesa e OcupacaoMesa for o respetivo
  plano de ocupacao da mesa.*/

frente(P1, P2, [[P1,_,_],[_,_],[P2,_,_]]).
frente(P1, P2, [[P2,_,_],[_,_],[P1,_,_]]).
frente(P1, P2, [[_,P1,_],[_,_],[_,P2,_]]).
frente(P1, P2, [[_,P2,_],[_,_],[_,P1,_]]).
frente(P1, P2, [[_,_,P1],[_,_],[_,_,P2]]).
frente(P1, P2, [[_,_,P2],[_,_],[_,_,P1]]).

/* naoFrente/3
  naoFrente(Pessoa1, Pessoa2, OcupacaoMesa) e verdadeiro se Pessoa1 e
  Pessoa2 nao ficarem frente a frente na mesa e OcupacaoMesa for o
  respetivo plano de ocupacao da mesa.*/

naoFrente(P1, P2, L) :-
    not(frente(P1, P2, L)).

/* pessoaMesa/2
  pessoaMesa(Pessoa, Mesa) e verdadeiro se Pessoa fizer parte do plano
  de ocupacao da mesa Mesa.*/

pessoaMesa(P, Mesa) :-
    cab1(P, Mesa);
    cab2(P, Mesa);
    honra(_,P,Mesa);
    (honra(_,Q, Mesa), frente(P, Q, Mesa));
    (honra(_,Q, Mesa), lado(P, Q, Mesa)).

/* ocupaMesa/2
  ocupaMesa(ListaPessoas, Mesa) e verdadeiro se Mesa for um plano de
  ocupacao de mesa com as pessoas de ListaPessoas.*/

ocupaMesa([], _).
ocupaMesa([Pessoa|ListaPessoas], Mesa) :-
    pessoaMesa(Pessoa, Mesa),
    ocupaMesa(ListaPessoas, Mesa).

/* ocupacaoMesa_aux/2
  ocupacaoMesa_aux(ListaRestricoes, OcupacaoMesa) e verdadeiro se
  OcupacaoMesa for um plano de ocupacao da mesa, tendo em conta as
  restricoes de ListaRestricoes.*/


ocupacaoMesa_aux([], _).
ocupacaoMesa_aux([T_c|Restricoes], OcupacaoMesa) :-
    T_c =.. [F|Args],
    append(Args, [OcupacaoMesa], Novos_Args),
    Novo_T_c =.. [F|Novos_Args],
    Novo_T_c,
    ocupacaoMesa_aux(Restricoes, OcupacaoMesa).

/* ocupacaoMesa/3
  ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) e verdadeiro
  se ListaPessoas for a lista com o nome das pessoas a sentar a mesa,
  ListaRestricoes for a lista de restricoes a verificar e OcupacaoMesa
  for uma lista com tres listas, em que a primeira contem as pessoas de
  um lado da mesa, a segunda as pessoas a cabeceira e a terceira as
  pessoas do outro lado da mesa, tendo em conta as pessoas de
  ListaPessoas que verificam todas as restricoes de ListaRestricoes.*/

ocupacaoMesa(ListaPessoas, ListaRestricoes, OcupacaoMesa) :-
    length(ListaPessoas, 8),
    ocupaMesa(ListaPessoas, OcupacaoMesa),
    aux(ListaRestricoes, OcupacaoMesa).
