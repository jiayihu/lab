# Artificial Intelligence

## Agenti intelligenti

- Un'agente è un'entità che percepisce l'ambiente tramite **sensori** ed agisce su di esso mediante **attuatori**
  - Funzione agente: $f: P^* \rightarrow A$ dove $P^*$ è una sequenza percettiva, $A$ le possibili azioni
  - **Sequenza percettiva**: insieme di percezioni osservate fino a quel momento
- Definizione informale Agente razionale: cerca di raggiungere il più possibile i suoi obiettivi data l'informazione di cui dispone o che può acquisire con le sue azioni
  - Limitazioni computazionali nella ricerca dello spazio delle azioni
- Misura di prestazione: valuta una sequenza di stati dell'ambiente, non di stati dell'agente
  - Definizione formale Agente razionale: sceglie un'azione che massimizza il valore atteso della misura di prestazione, data la sequenza di percezioni ottenuta fino all'istante corrente.
  - Razionalità != 
    - onniscenza: conoscere il risultato effettivo delle sue azioni
    - chiaroveggenza: sequenza percettiva fino al momento corrente
    - successo atteso e non quello reale
  - Razionalità =>
    - esplorazione: intraprendere azionimirate a modificare le percezioni future, **information gathering**
    - apprendimento: modificare la conoscenza pregressa sulla base delle proprie percezioni (scarabeo e vespa)
    - autonomia: apprendere il più possibile per compensare la presenza di conoscenza parziale o erronea (un aspirapolvere che apprende come prevedere dove apparirà lo sporco)
- PEAS (Performance Environment Actuators Sensors)
  - PEAS di un taxi automatizzato
- Tipi di ambiente
  - Completamente/parzialmente osservabile: capacità dell'agente di misurare tutti gli aspetti dell'ambiente che sono rilevanti per la scelta dell'azione
  - Deterministico/stocastico: lo stato successivo dell'ambiente è completamente determinato dallo stato corrente e dall'azione eseguita
  - Episodico/sequenziale: in ogni episodio l'angete riceve una percezione e poi esegue un'azione. Un episodio non dipende dalle azioni intraprese in quelle precedenti e ogni decisione non influenza quelle successive. (Identificare pezzi difettosi vs scacchi)
  - Statico/dinamico: l'ambiente può cambiare mentre un agente sta pensando, per cui non deve continuare ad osservare il mondo oppure non si deve preoccupare del passaggio del tempo.
    - Semidinamico se non l'ambiente non cambia ma la valutazione della prestazione sì, come negli scacchi con orologio
  - Discreto/continuo: si riferisce allo stato e all'insieme di percezioni/azioni. Gli scacchi sono discreti, la guida autonoma ha stato e tempo continui
  - Agente singolo/multiagente
    - Competitivo negli scacchi
    - Cooperativo nella guida autonoma
  - Noto/ignoto: se l'agente conosce i risultati per tutte le azioni (gioco di carte vs videogioco)
- Tipi di agente
  - Agente con tabella: irrealizzabile per complessità spaziale
  - **Reattivo semplice**: scelgono l'azione in base solo alla percezione corrente
    - Riduce la tabella delle azioni da $4^T$ con $T$=vita a $4$.
    - Regola condizione-azione (luci di frenata rosse o riflessi automatici)
    - Richiede ambiente completamente osservabile
  - **Reattivi basati su modello**: tiene traccia dell'ambiente in uno stato interno che dipende dalla storia delle percezioni e quindi riflette parte degli aspetti non osservabili dello stato corrente.
    - Richiede conoscenza sull'evoluzione del mondo indipendentemente dalle sue azioni e informazioni sull'effetto che hanno sull'ambiente le sue azioni. Questa conoscenza è implementata tramite un modello del mondo.
    - Poco flessibile avendo il comportamento hard-coded in regole
  - **Basati su goal**: unione dell'obiettivo al modello per la scelta dell'azione
    - Spesso richiede ricerca e pianificazione per identificare le sequenze, anche lunghe, di azioni.
    - Richiede di prendere in considerazione il futuro
    - Meno efficiente ma più flessibile, può adattare le sue azioni in base a variazioni dell'ambiente, come la pioggia, senza riscrivere le regole
    - Può anche essere modificato cambiando semplicemente l'obiettivo senza riscrivere le regole
  - **Basati su una misura di utilità**: internalizzazione della misura di prestazione. 
    - Gestisce casi di obiettivi in conflitto oppure non raggiungibili con certezza, in ambienti parzialmente osservabili e stocastici.
    - Calcola utilità attesa dei risultati, date le probabilità ed utilità di ciascun risultato.
- Agenti che apprendono
  - Permette di operare in ambienti inizialmente sconosciuti, migliorando la conoscenza iniziale
  - L'apprendimento può essere definito come il processo che modifica ogni suo componente affinché si accordi meglio con l'informazione di feedback disponibile, migliorando così le prestazioni globali dell'agente
  1. **Elemento di apprendimento**: determina se e come modificare l'elemento esecutivo affinché in futuro si comporti meglio, in base all'elemento critico
  2. **Elemento esecutivo**: prende in input le percezioni e decide le azioni
  3. **Elemento critico**: dice a quello di apprendimento come si sta comportando rispetto a uno standard di prestazione prefissato. (Linguaggio scurrile degli altri guidatori). Può essere anche un feedback esterno come ricompensa/penalità.
  4. **Generatore di problemi**: suggerire azioni che portino ad esperienze nuove e significative (provare freni su nuove superfici stradali)

## Risoluzione di problemi

- La formulazione dell'obiettivo, basato sullo stato corrente e sulla misura di prestazione, è il primo passo
- Un **obiettivo** è composto dall'insieme di tutti e soli gli stati del mondo in cui l'obiettivo è soddisfatto. Il compito dell'agente è come agire, ora e nel futuro, per raggiungere uno stato obiettivo.
- La formulazione del problema è il processo di decidere, dato un obiettivo, _quali azioni e stati considerare_. Un agente può decidere cosa fare esaminando le azioni future che porteranno a stati di valore conosciuto.
- **Ricerca**: il processo che cerca una sequenza di azioni che raggiunge l'obiettivo.
- Un problema è composto da:
  - Lo **stato iniziale** $s$
  - L'insieme delle azioni che possono eseguite in $s$.
  - Un modello di transizione $RISULTATO(s, a)$, che restituisce lo stato risultante dall'esecuzione dell'azione $a$ nello stato $s$.
  - Il test-obiettivo che determina se un particolare stato è uno stato obiettivo
  - La funzione costo di cammino. La soluzione ottima è quello che ha costo minore di tutte.
- Spazio degli stati: insieme di tutti gli stati raggiungibili a partire da quello iniziale mediante qualsiasi sequenza di azioni, rappresentato come grafo o **albero di ricerca**.
  - $RICERCA-ALBERO$: processo di espandere i nodi sulla frontiera continua finché si trova una soluzione o finché non vi sono più nodi da espandere
  - Frontiera: insieme di tutti i nodi foglia che possono essere espansi in un dato punto
    - Separa stati esplorati da quelli non esplorati.
    - Implementato come coda FIFO o LIFO
  - I cammici ciclici sono un caso particolare dei cammini ridondanti, che esistono ogni volta che esistono più modi per passare da uno stato a un altro.
    - In alcuni casi è possibile ridefinire un problema per eliminare cammini ridondanti (problema della 8 regine poste nella colonna vuota più a sinistra).
    - Non è invece possibile qualora le azioni siano reversibili.
    - Si può usare un _insieme esplorato_ per ricordare i nodi espansi/visitati. Si parla di $RICERCA-GRAFO$
  - Un nodo è implementato con 4 componenti:
    - stato
    - padre
    - azione
    - costo di cammino
- Criteri di valutazione di un algoritmo di ricerca
  - **Completezza**: garantisce di trovare una soluzione quando esiste
  - **Ottimalità**: trova la soluzione ottima
  - **Complessità temporale**
  - **Complessità spaziale**
  - Nell'informatica in genere la complessità si misura in base a $|V| + |E|$, nell'IA l'insieme di stati ed azioni è spesso infinito.
    - $b$ *branching factor* o numero massimo di successori di un nodo
    - $d$ *depth* del nodo obiettivo più vicino allo stato iniziale
    - $m$, *maximum length* dei cammini nello spazio degli stati

## Ricerca non informata

- Ricerca in ampiezza (BFS)
  - Tutti i nodi a profondità minore devono essere espansi prima che si possa espondere uno dei nodi al livello successivo
  - Usa coda FIFO per la frontiera
  - Il test obiettivo è applicato alla generazione del nodo e non alla sua selezione per limitare la complessità
  - Trova sempre il cammino meno profondo
  - Completo ma non ottimo, salvo che il costo di un cammino sia una funzione monotona crescente della profondità del nodo.
  - Complessità temporale e spaziale $O(b^d)$, espade $b$ nodi ad ogni livello e li memorizza nell'insieme esplorato
  - Ricerca a costo uniforme
    - Espande il nodo $n$ con minimo costo di cammino $g(n)$
    - Usa coda ordinata secondo costo di cammino
    - Il test obiettivo è applicato alla selezione per espansione invece che generazione per garantire di ottimalità
    - Test aggiuntivo nel caso in cui sia trovato un cammino migliore per raggiungere un nodo sulla frontiera
    - Ottimalità:
      1. Ogni volta che un nodo è espanso, il cammino ottimale verso tale nodo è stato trovato. Altrimenti andrebbe in contraddizione con la decisione di scegliere il nodo con minor cost $g(n)$
      2. I costi non sono negativi per cui non si possono ottenere cammini migliori aggiungendo nodi
      - Il primo nodo obiettivo selezionato per l'espansione deve essere la soluzione ottima.
    - Completezza: garantita se il costo di ogni passo è $\ge \epsilon, \epsilon \ge 0$
    - Complessità $O(b^{1+\lfloor C^*/\epsilon\rfloor})$, che può essere $\gg O(b^n)$. L'intuizione è che esplora grandi alberi fatti di piccoli passi prima di considerare i cammini che prevedono passi molti grandi e forse più utili.
- Ricerca in profondità
  - Espande sempre il nodo più profondo nella frontiera
  - Utilizza una coda LIFO per la frontiera
  - Si può usare funzione ricorsiva invece di $RICERCA-GRAFO$
  - Completa con ricerca su grafo in spazi finiti, altrimenti non completa con ricerca con albero o in spazi infiniti
  - Analoghi motivi per l'ottimalità
  - Complessità temporale $O(b^m)$ con $m \gg d$ ed infinito per alberi illimitati
  - Complessità spaziale $O(b*m)$: 
    - Deve memorizzare un solo cammino dalla radice a un nodo foglia, insieme ai rimanenti nodi fratelli non espansi. Un nodo espanso può essere rimosso con tutti i suoi discendenti se esplorato completamente.
  - Variante **ricerca con backtracking**: genera solo un successore quindi complessità spaziale $O(m)$, o addirittura $O(1)$ se si può generare lo stato successore tornando indietro sui propri passi e modificando direttamente la descrizione dello stato.
- Ricerca a profondità limitata $l$:
  - $l$ può essere basato sulla conoscenza a priori del problema, ad esempio _diametro dello spazio degli stati_
  - Mitiga l'incompletezza della ricerca in profondità in spazi deglo stati infiniti
  - I nodi alla profondità $l$ sono trattati come se fossero senza successore
  - Incompleto comunque se $l < d$ e non ottimo se $l > d$
  - Complessità temporale $O(b^l)$ e spaziale $O(bl)$
- Ricerca ad approfondimento iterativo
  - Il limite $l$ viene incrementato progressivamente finché non raggiunge $d$
  - Complessità temporale $O(b^d)$, spaziale $O(bd)$
  - Completa con $b$ finito
  - Ottima con funzione costo di cammino monotona crescente della profondità di nodo
  - I nodi figli diretti del nodo radice sono generati $d$ volte
- Ricerca bidirezionale
  - Eseguire due ricerche BFS in parallelo, una in avanti dallo stato iniziale e l'altra indietro dall'obiettivo. Si spera si incontrino a metà strada. $O(b^{d/2}) + O(b^{d/2}) \ll O(b^d)$
  - Si usa un test di intersezione delle due frontiere invece che test obiettivo. Il controllo può essere in tempo costante con una tabella hash.
  - Ottimale assicurandosi che non esistano altre scorciatoie che colmino il gap
  - Complessità temporale e spaziale $O(b^{d/2})$
  - Infatti almeno una delle due frontiere deve essere mantenuta in memoria per il controllo di intersezione
  - Difficile da utilizzare con obiettivi astratti come 8 regine. Se invece ci sono più stati obiettivi espliciti, si può costruire uno stato obiettivo fittizio i cui predecessori siano tutti gli stati obiettivo.


| criterio  | in ampiezza  | a costo uniforme  | in profondità  | a profondità limitata  | ad approfondimento iterativo  | bidirezionabile  | 
|---|---|---|---|---|---|---|---|---|:-:|
| completezza  | Sì  | Sì | No  | No | Sì  | Sì | 
| ottimalità  | Sì  | Sì | No | No | Sì | Sì | 
| tempo  |  $O(b^d)$ | $O(b^{1+\lfloor C^* / \epsilon\rfloor})$ | $O(b^m)$ | $O(b^l)$  | $O(b^d)$ | $O(b^{d/2})$ |
| spazio  |  $O(b^d)$ | $O(b^{1+\lfloor C^* / \epsilon\rfloor})$ | $O(bm)$ | $O(bl)$ | $O(bd)$  | $O(b^{d/2})$ | 

## Ricerca informata

- Il nodo da espandere viene scelto in base ad una funzione di valutazione $f(n)$, come stima di costo
- Identico alla ricerca a costo uniforme sostituendo $f$ con $g$ per ordinare la coda di priorità
- $f$ include in genere una **funzione euristica** $h(n)$, come costo stimato del cammino più conveniente dal nodo $n$ ad uno stato obiettivo
  - Si basa solo sullo stato del nodo
- Ricerca best-first greedy
  - Espande il nodo più vicino all'obiettivo: $f(n) = h(n)$
  - Non ottimale
  - Non completa: può finire in vicoli ciechi o cicli infiniti, salvo controllo di ripetizione di stati.
  - Complessità temporale e spaziale $O(b^m)$, sebbene con una buona euristica la complessità possa essere ridotta notevolmente.
- Ricerca A*
  - $f(n) = g(n) + h(n)$, costo stimato della soluzione più conveniente che passa per $n
  - Completa ed ottima se $h(n)$ è consistente
  - Ammissibilità: $h(n)$ non sbaglia mai per eccesso la stima del costo per arrivare all'obiettivo, quindi è una stima _ottimista_ pper natura
  - **Consistenza**/monotonicità:
    - $h(n) \le c(n, a, n') + h(n')$, $n'$ successore di $n$ tramite azione $a$ (disuguaglianza triangolare)
    - condizione più forte dell'ammissibilità.
  - Ottimalità per ricerca ad albero: se un nodo obiettivo non ottimale $G_2$ viene generato e si ha $n$ nodo sul cammino minimo verso il nodo obiettivo ottimale $G$, A* sceglierà $n$: $f(G_2) = g(G_2) \ge f(G) \ge f(n)$
    - Non vale per ricerca a grafo, rischia di scartare un nodo già visitato che si trova però sul cammino ottimo
  - Ottimalità per ricerca a grafo:
    1. $f(n)$ è crescente lungo ogni cammino: $f(n') = g(n') + h(n') = g(n) + c(n, a, n') + h(n') \ge g(n) + h(n) = f(n)$
    2. Il cammino trovato per il nodo $n$ selezionato per l'espansione è ottimo.
       - Altrimenti esisterebbe un altro $n'$ lungo il cammino verso $n$, che avrebbe però avuto un $f(n') \le f(n)$ e quindi sarebbe stato selezionato prima
    - Il primo nodo obiettivo selezionato deve quindi essere ottimo, perché ho trovato il cammino minimo verso quel nodo e tutti i nodi obiettivo successivi avranno un costo $\ge$
  - Complessità temporale e spaziale $O(b^{\epsilon d})$
  - A* espande gradualmente il contour in ordine di $f$: contiene tutti i nodi che hanno $f_i \le f_{i+1}$
    - Con ricerca a costo uniforme $h(n) = 0$ le bande sono circolare, con l'euristica $h(n)$ sono allungate verso lo stato obiettivo e si stringono intorno al cammino ottimo.
  - A* espande tutti i nodi con $f(n) \lt C^*$ ed alcuni con $f(n) = C^*$ con $C^*$ costo del cammino ottimale
  - La completezza richiede che esista un numero finito di nodi $f(n) \le C^*$, vera se $f(n) \ge \epsilon$ e $b$ è finito
  - A* non espande alcun nodo $f(n) \gt C^*$, i relativi alberi sono _pruned_
  - A* è ottimamente efficiente, non è possibile espandere meno nodi
  - La complessità temporale è $O(b^{\epsilon d})$ ove $\epsilon = (h^* - h) / h^*$
  - La complessità spaziale è $O(b^{\epsilon d})$, non applicabile per problemi di grandi dimensioni
- Iterative Deepening A* (IDA*)
  - Invece di tagliare a profondità $d$, si taglia a costo $f$, ove il limite è il $f$-costo minimo tra quelli di tutti i nodi che hanno superato il limite nell'iterazione precedente
- Recursive Best-First Search (RBFS):
  - come ricerca ricorsiva in profondità, con spazio lineare
  - Tiene traccia di $f\_limite$, come $f$-costo del miglior cammino alternativo di uno degli antenati del nodo corrente
  - Quando il nodo corrente supera il limite, torna indietro a quello alternativo e sostituisce l'$f$-valore di ogni nodo lungo il ritorno con un valore di backup, il migliore dei suoi nodi figli.
  - Eccessiva rigenerazione dei nodi: in uno spazio di ricerca grande e vicini all'obiettivo, il cammino alternativo tende a diventare quello migliore in assoluto, ma poi la ricerca deve tornare di nuovo indietro sul cammino ottimale per rieseguirlo. Ogni "ripensamento" corrisponde ad un'iterazione di IDA* e ne può ruchiedere diversi per estendere il cammino migliore di un nodo.
  - Ottima se $h(n)$ è ammissibile
  - Complessità spaziale $O(bd)$, temporale difficile da definire ma esponenziale nel caso pessimo
- IDA* e RBFS usano troppa poca memoria, dimenticando ciò che hanno fatto riespandono nuovamente gli stessi stati più volte.
- Memory-bounded A* (MA*) o Simplified MA* (SMA*)
  - Espande la foglia migliore finché la memoria non è piena. A quel punto scarta la foglia peggiore, con $f$-valore più alto, memorizzando nel nodo padre il valore del nodo scartato.
  - Rigenera il sottoalbero dimenticato solo quando _tutti gli altri cammini_ sono peggiori.
  - Se $f$-valore è uguale, espande la foglia più recente e rimuove quella più vecchia. Se c'è una sola foglia la rimuove, perché in ogni caso non ci sarebbe abbastanza memoria.
  - Completo se il cammino fino a $d$ è raggiungibile con la memoria disponibile
  - Ottima se c'è la soluzione tra quelle raggiungibili, altrimenti restituisce la migliore tra quelle disponibili
  - In problemi molto difficili, SMA* passa continuamente da un cammino candidato all'altro e potrà tenere in memoria solo un piccolo sottoinsieme. Dovrà rigenerare ripetutamente i nodi.
    - Le limitazioni di memoria possono rendere temporalmente intrattibile un problema altrimenti risolvibile in A*
- Euristiche
  - 8-puzzle
    - $h_1(n)$ = numero di tasselli fuori posto
    - $h_2(n)$ = somma della distanza Manhattan, distanza in orizzontale e verticale per ogni tassello dalla posizione obiettivo
  - TSP: MST come euristica, limite inferiore al percorso aperto più breve
  - La qualità dell'euristica si misure con $b^*$, fattore di branching effettivo indicato come $N+1 = 1 + b^* + (b^*)^2 + ... + (b^*)^d$
  - Il valore ideale di $b$ è vicino a 1, in modo che i tempi siano ragionevoli anche per problemi di dimensioni grandi. Già 2 significa esplorare tutti i nodi di un albero binario.
  - Se un'euristica $h_2$ domina su $h_1$ si traduce in maggior efficienza perché ci saranno meno nodi espansi: $f(n) < C^* \Leftrightarrow h(n) < C^* - g(n)$
  - Il costo di una soluzione ottima di un problema rilassato è un'euristica ammissibile per il problema originale
    - Un problema rilassato ha meno vincoli sulle azioni possibili
    - Il grafico del problema rilassato corrisponde ad un _supergrafo_ dell'originale, con più archi quindi potenziali scorciatoie verso la soluzione ottima
    - Se invece il problema rilassato è ancora difficile da risolvere, i valori dell'euristica saranno difficili da ottenere 
      - Altrimenti potremmo sempre usare la ricerca in ampiezza come euristica "perfetta"
  - Si può derivare euristiche ammissibili dal costo della soluzione di un sottoproblema (i.e. 4 tasselli)
    - Database di pattern: memorizzare i costi delle soluzioni dei sottoproblemi (programmazione dinamica)

## Algoritmi di miglioramento iterativo / Ricerca locale

- Il cammino non è rilevante, conta trovare una configurazione ottima nello spazio degli stati dato dall'insieme delle configurazioni a stato completo (es 8 regine)
- > Si mantiene un singolo stato corrente e si tenta di migliorarlo iterativamente
- Complessità spaziale costante, quindi adatto per la ricerca online, e possono trovare soluzioni ragionevoli in spazi degli stati grandi o infiniti
- Il panorama dello spazio degli stati mostra una posizione, definita dallo stato, e un'altezza che corrisponde al valore della funzione costo o di obiettivo. Rispettivamente si punta a trovare il minimo e massimo globali.
- Completo se trova sempre un obiettivo, ottimale se trova il minimo/massimo globale
- **Hill climbing**
  - Ciclo che si muove continuamente verso l'alto, nella direzione dei valori crscenti e termina quando raggiunge un picco che non ha vicino di valore più alto
  - I.e. 8 regine: stati con 8 regine e i possibili stati muovono una singola regina. $h$ è il numero di coppie di regine che si stanno attaccando a vicenda
    - $p = 14\%$ delle volte soluzione ottima
  - Procede molto rapidamente verso una soluzione, ma spesso rimane bloccato:
    - **massimo locali**: piccolo più alto degli stati vicini, ma inferiore al massimo globale
    - **creste**: sequenza di massimo locali molto difficili da esplorare
    - **plateau**: area piatta nello spazio degli stati, può essere un massimo locale piatto o una _spalla_ da cui si potrà salire ulteriormente
      - Utilizzo di una _mossa laterale_ per spostarsi nel plateau. Bisogna porre limite massimo di mosse consecutive per evitare ciclo infinito.
        - - $p = 94\%$ delle volte soluzione ottima
  - Hill climbing stocastico: sceglie a caso tra le mosse che vanno verso l'alto
  - Hill climbing con riavvio casuale: serie di ricerche hill climbing con stati inziiali generati casualmente
    - Se la probabilità di ricerca è $p$, sono richiesti $1/p$ riavii in media

      $
      x_i = \begin{cases}
      0 & \text{se la i-esima ricerca non trova la soluzione ottima} \\
      1 & \text{se la i-esima ricerca non trova la soluzione ottima}
       \end{cases}
      $

      $\forall i, P(x_i = 1) = p$ e $P(x_i = 0) = 1 - p$

      La probabilità che la k-esima ricerca sia la prima con soluzione ottima è quindi: $P(\sum_j^{k-1} x_j = 0 \land (x_k = 1)) = (1 - p)^{k - 1}p$

      Il valore atteso quindi è: $\sum_{k=1}^\infty k(1-p)^{k - 1}p = {1 \over p}$
    - 8 regine:
      - Con $p = .14$ sono richieste in media $1 / .14 = 7$ ricerche
      - Con mosse laterali sono richieste in media $1 / .94 = 1.06$ ricerca per la soluzione ottima
  - La bontà della ricerca dipende dal panorama dello spazio degli stati, soprattutto dalla presenza di massimi locali o plateau
- Simulated annealing
  - Combinazione del hill climbing con esplorazione casuale partendo da mosse "cattive"
  - Prende il nome dalla tecnica di raffreddamento del metallo, che viene portato ad alta temperatura e poi raffreddato lentamento ottenendo lo stato con minor configurazione energetica
  - Si usa discesa di gradiente, quindi minimizzazione di costo
  - Scelta casuale della mossa che migliora, con probabilità pesata in base al grado di miglioramento $\Delta E$ e alla temperatura $T$. Quest'ultima decresce gradualmente, quindi all'inizio ci sono maggiori probabilità di scegliere mosse "cattive"
  - Se la temperatura descresce abbastanza lentamente, trova ottimo globale con probabilità tendente a 1
  - $p(x) = e^{E(x) \over kT}$, $k$ è la costante che regola la temperatura
  - Usato per configurazione VLSI

## Ricerca online

- Ricerca offline: la soluzione completa viene calcolata e poi eseguita
- Ricerca online: l'angete opera alternando computazione ed azione, prima esegue un'azione, poi osserva l'ambiente e determina l'azione succesiva
  - Si presta a domini dinamici e in ambienti ignoti, stati ed effetti delle azioni sono sconosciuti all'agente
  - Adatta a problemi di esplorazione
- Problema di ricerca online
  - L'agente conosco solo $AZIONI(s)$, $c(s, a, s')$ e $TEST-OBIETTIVO(s)$
  - > L'agente non può determinare $RISULTATO(s, a)$ se non trovandososi effettivamente in $s$ ed eseguendo $a$
  - La funzione di costo $c(s, a, s')$ non può essere usata finché l'agente non appura che $s'$ è il risultato dell'azione
  - rapporto di competitività rispetto ad un algoritmo che conosco lo spazio di ricerca
    - Può essere infinito nel caso di azioni irreversibili e **vicoli ciechi**
    - Può essere non limitato anche nel caso di spazio degli stati _esplorabile in modo sicuro_ e con azioni reversibili, immaginando un avversario che blocca il cammino migliore con un lungo muro sottile
- $AGENTE-ONLINE-RIP$ (ricerca in profondità)
  - Un agente online può espandere solo nodi fisicamente successori di quello che sta occupando, quindi per evitare di spostarsi continuamente è meglio espande secondo un ordine locale: ricerca in profondità.
  - L'agente mantiene in memoria gli stati risultanti delle azioni $RISULTATO(s, a)$ e prova ogni azione non ancora esplorata
  - Se tutte le azioni sono state provate in uno stato, torna indietro con backtracking (richiede quindi azioni reversibili)
  - Nel caso peggiore attraverso lo spazio degli stati due volte
    - Ottimo per l'esplorazione
    - Non ottimo per il raggiungimento del goal esplorare lunghe distanze quando si è vicini all'obiettivo
    - Variante ad approfondimento iterativo
- Ricerca hill climbing
  - È già una ricerca online in quanto mantiene solo lo stato corrente
  - Non è utile perché si può bloccare a massimo locale e non si può usare riavvio casuale perché l'agente non può trasferirsi in un altro stato magicamente
  - **Random walk**: sceglie a caso una delle possibili azioni, prediligendo quelle non ancora provate
    - Converge _prima o poi_ all'obiettivo ma può essere molto lento, soprattutto se ci sono alte probabilità di tornare indietro
  - Trick LRTA*: arricchire con memoria, memorizzando la miglior stima corrente $H(s)$ ad ogni stato $s$, che inizialmente è l'euristica $h(s)$. Man mano con l'esperienza nello spazio degli stati, il valore viene aggiornato per "sfuggire" ad esempio da minimi locali "appiattendoli"
    - Mantiene memoria dei risultati delle azioni come $AGENTE-ONLINE-RIP$
    - Aggiorna la stima del costo $H(s)$ dello stato appena lasciato, $H(s) = c(s, a, s') + H(s')$
    - Ottimismo in condizioni di incertezza: si privilegiano le azioni non ancora provate in uno stato, poiché si pensa coducano all'obiettivo con il minimo costo possibile $h(s)$.
  - Completo in spazi finiti, non completo in spazi degli stati infiniti poiché può lasciarsi sviare senza possibilità di ritorno
  - Complessità temporale $O(n^2)$ con $n$ stati, nel caso peggiore

## Ricerca con avversari

- Ambienti multiagente competitivi, bisogna considerare le azioni degli altri e gli obiettivi degli agenti sono in conflitto
- **Giochi a somma zero con informazione perfetta**: ambienti deterministici e completamente osservabili, in cui due agenti agiscono alternandosi e i cui valori di unità, a fine partita, sono uguali ma di segno opposto (a somma zero). 
- I giochi sono facili da rappresentare in astratto ma difficili da risolvere
  - Scacchi hanno fattore di ramificazione 35
  - Richiedono abilità di prendere una decisione quando calcolare quella ottima non è realizzabile per inefficienza
  - L'obiettivo è sfruttare al meglio il tempo disponibile
- Un gioco è composto dalle seguenti componenti:
  - $S_0$ stato iniziale
  - $GIOCATORE(s)$: a chi tocca tra $MIN$ e $MAX$
  - $AZIONI(s)$
  - $RISULTATO(s, a)$
  - $TEST-TERMINAZIONE(s)$
  - $UTILITÀ(s, p)$ o funzione di payoff: attribuisce il valore numerico finale nello stato $s$ al giocatore $p$. Un gioco a somma zero ha payoff totale 0 ed uguale per tutte le istanze del gioco
- Stato iniziale, le azioni ed i risultati compongono **l'albero di gioco**
  - Costrutto teorico che non possiamo realizzare nel mondo fisico perché troppo grande con giochi non banali
  - Si usa **albero di ricerca**, porzione dell'albero di gioco esaminato per consentire di determinare quale mossa fare
- A differenza della ricerca normale, in un gioco con avversari $MIN$ può dire la sue: $MAX$ deve elaborare una strategia che valuti la propria mossa, gli stati possibili dalle mosse di $MIN$, le proprie mosse risultati e così via
- La strategia ottima esamina il **valore minimax** di ogni nodo: miglior utilità per $MAX$ contro un avversario che gioca in modo ottimo
  - $
    \text{MINIMAX}(n) = \begin{cases}
    \text{UTILITÀ}(s, MAX) & \text{se TEST-TERMINALE}(s) \\
    max_{a \in AZIONI} \text{VALORE-MINIMAX-RISULTATO}(s, a) & \text{se GIOCATORE(s) = MAX} \\
    min_{a \in AZIONI} \text{VALORE-MINIMAX-RISULTATO}(s, a) & \text{se GIOCATORE(s) = MIN} \\
    \end{cases}
    $
- Algoritmo minimax
  - Calcola ricorsivamente il valore minimax di ogni stato successore fino alle foglie, di cui ottiene $\text{UTILITÀ}$ poiché terminali
  - I valori minimax sono portati su nella fase di ritorno
  - Esegue esplorazione completa in profondità dell'albero di gioco
  - Complessità temporale $O(b^m)$ e spaziale $O(bm)$
  - Completo con alberi di gioco finiti
  - Ottimo contro avversari ottimali, altrimenti ancora meglio perché il minimax è il massimo del minimo vantaggio ottenibile
  - Multiplayer A, B, C: ogni nodo ha un vettore $\lang v_A, v_B, v_C\rang$
    - Stati terminali: il vettore è riempito con $\text{UTILITÀ}$
    - Stati intermedi: il vettore di utilità passato in alto più favore a chi sta scegliendo la mossa
    - Alleanze: conseguenza naturale delle strategie ottime di ogni giocatore
- **Potatura alfa-beta**: pota i rami che non possono influenzare la decisione finale
  - Ogni nodo ha un intervallo dei possibili valori minimax dei successori
    - $\alpha$ il valore migliore, più alto, per $MAX$
    - $\beta$ il valore migliore, più basso, per $MIN+$
  - $\text{MINIMAX(radice)} = max(min(3, 12, 8), min(2, x, y), min(14, 5, 2)) = 3$ a prescindere da $x$ e $y$ che quindi possono essere potati
  - Possiamo potare un nodo appena abbiamo raccolto abbastanza informazioni per concludere che esiste una scelta sicuramente migliore in un nodo precedente
  - L'efficacia della potatura dipende fortemente dall'ordine con cui si esaminano gli stati
    - Per l'ordine perfetto serve poter:
      - conoscere il valore esatto di uno stato:
        - conoscere il valore esatto di utilità per uno figlio
        - conoscere un bound sulla utilità di tutti gli altri figli
        - $E(d + 1) = E(d) + (b - 1)B(d)$
      - conoscere un bound sulla utilità di uno stato:
        - conoscere il valore esatto di utilità per uno stato figlio
        - $B(d + 1) = E(d)$
      - $E(d)$ / $B(d)$ numero minimo di stati da considerare per conoscere il (valore esatto) / bound di utilità di uno stato a profondità d dagli stati terminali
      - $E(m) \le (\sqrt{2b})^m = (\sqrt{2})^mb^{m/2}$ upper-bound
    - Complessità temporale $O(b^{m/2})$ se sono esaminati prima i successori più promettenti tramite ordinamento perfetto
    - Il fattore di ramificazione effettivo diventa $b^* = \sqrt{b}$
      - Scacchi con ordinamento esemplice che cerca prima di catturare i pezzi, poi di minacciarti, poi mosse in avanti e infine quelle indietro: $\sqrt{35}=6$
  - Complessità $O(b^{3m/4})$ con ordine casuale
  - Si può usare ricerca ad approfondimento iterativo per avere informazioni sull'ordinamento delle mosse
  - Utile usare **tabella delle trasposizioni**: hashmap che memorizza le valutazioni delle trasposizioni, permutazioni diverse della stessa sequenza di mosse che portano alla stessa configurazione
    - Negli scacchi raddoppia la profondità raggiungibile
- **Funzioni di valutazione**
  - Limiti alle risorse di tempo per calcolare una mossa
  - Tagliare la ricerca minimax o alfa-beta prima che raggiunga una foglia, usando una funzione di valutazione euristica $\text{EVAL}$ che stima l'utilità della posizione raggiunta
  - $\text{TEST-TAGLIO}$ decide quando applicare $\text{EVAL}$
  - $\text{EVAL}$ fornisce una stima del guadagno atteso in uno stato, analogo all'euristica distanza dall'obiettivo
    - È quello che fanno i giocatori di scacchi
    - Il comportamento corretto è preservato per trasformazioni monotone: conta mantenere l'ordine.
    1. Deve ordinare gli stati terminali nello stesso modo della funzione $\text{UTILITÀ}$, per evitare che si sbagli quando l'agente è capace di "vedere" fino alla fine della partita
    2. Deve essere efficiente
    3. Per gli stati non terminali deve avere una forte correlazione con la probabilità di vincere
       - In genere è in base alle **caratteristiche** dello stato, che prese insieme definisco _categorie o classi di equivalenza_: gli stati di una categoria hanno lo stesso valore per le caratteristiche (i.e. 
       - "due pedoni vs uno")
       - Il valore di ogni categoria può essere stimato dal valore atteso pesando le % di vittoria, pareggio e sconfitta con i rispettivi valori di utilità. Nella pratica richiede troppe categorie.
       - Si preferisce invece l'insieme di valori delle caratteristiche sommato in maniera pesata come **funzione lineare pesata**: $\text{EVAL}(s) = \sum_i^n w_i f_i (s)$
       - Si possono usare anche combinazioni non lineari per includere dipendenze tra caratteristiche (i.e. alfiere con _numero di mosse_ alto)
       - I pesi $w_i$ possono essere stimato con Machine Learning
  - $\text{TEST-TAGLIO}(s, d)$
    - Può usare un limite di profondità, ma va scelto affinché la scelta della mossa avvenga nel tempo allocato
      - Più robusto usare ricerca ad approfondimento iterativo
        - Restituisce la mossa calcolata con la più profonda ricerca completata in tempo
        - Aiuta anche l'ordinamento delle mosse
      - Rischio di errore causa approssimazione
    - Meglio tagliare e valutare in posizioni quiescenti tramite **ricerca di quiescenza**, ad esempio posizioni senza catture
  - Potatura in avanti: la potatura alfa-beta non influenza il risultato finale, quella in avanti potrebbe.
    - $\text{PROBCUT}$: utilizza statistiche da esperienza precedente per potare mosse _probabilmente_ fuori dall'intervallo alfa-beta.
      1. Esegue ricerca poco prfonda per calcolare il valore minimax $v$ portato su di un nodo
      2. Stima la probabilità che il valore $v$ a profondità $d$ sia fuori da $(\alpha, \beta)$
- Giochi stocastici
  - Backgammon: vengono tirati due dati per determinate possibili mosse, 21 casi distinti
  - L'albero di gioco deve includere **nodi di casualità** i cui archi uscenti rappresentano i diversi esiti e relativa probabilità
  - **valore expectiminimax**: i nodi casualità hanno valore atteso minimax, come somma pesata dei minimax in base alla probabilità della mossa $\sum_r P(r) \cdot \text{EXPECTIMINIMAX(RISULTATO(s, r))}$ con $r$ risultato del tiro di dati
  - La funzione di valutazione deve essere una trasformazione lineare positiva della probabilità di vincere, altrimenti valutazioni che preservano l'ordinamento comunque modificano la scelta della mossa migliore
  - Complessità temporale $O(b^m n^m)$ con $n$ numero di tiri di dado distinti
  - Quando entra in gioco l'incertezza le possibilità si moltiplicano enormemente e diventa inutile formulare piani dettagliati
  - Se limitiamo i valori della funzione $\text{UTILITÀ}$ possiamo derivare dei limiti anche per la media dei nodi di casualità e di conseguenza potarne alcuni
- Giochi ad informazione parziale e stocastici (i.e. carte)
  - Approccio naive: come se tutti i dadi fossero stati tirati all'inizio della partita
  - Considerare tutte le possibili distribuzioni di carte nascoste e risolverle una per una come fosse un gioco completamente osservabile. Scegliere poi il miglior risultato pesato sulle probabilità $P(s)$ delle distribuzioni. $argmax_a \sum_s P(s) \text{MINIMAX(RISULTATO(s, a))}$ o $\text{H-MINIMAX}$ se non è possibile
  - $26 \choose 13$ possibili distribuzioni, impossibile risolvere per tutte
    - Approssimazione Monte carlo: prendiamo casualmente N distribuzioni, ognuna con probabilità di essere pescata proporzionale a $P(s)$: $argmax_a {1 \over N} \sum_i^N \text{MINIMAX(RISULTATO}(s_i, a))$
    - Con $N = [100,1000]$ fornisce una buona approssimazione
  - La strategia viene chiamata _media sulla chiaroveggenza_: ipotizza che il gioco diventerà osservabile per tutti dopo la prima mossa. Assunzione falsa
    - Racconto della strada A con montagna d'oro e strada B con bivio, montagna d'oro più grande o autobus
    - Con informazione non completa, il valore di un'azione dipende dallo stato di credenza in cui si trova l'agente
    - Non sceglie mai azioni che ottengano informazioni o le nascondano all'avversario, assume anzi che si sappiano già.

## Constraint Satisfaction Problem

- Finora lo stato era atomico, scatola nera priva di struttura atomica
  - IN CPS si usa una rappresentazione fattorizzata, una serie di variabili con vincoli
  - L'idea è eliminare ampie porzioni dello spazio di ricerca, individuando combinazioni di variabili e valori che violano i vincoli
    - Più efficiente di una ricerca classica sull'intero spazio degli stati
- UN CSP è composto da:
  - $X$ insieme di variabili $\{X_1, ..., X_n\}$
  - $D$ insieme di domini delle variabili $\{D_1, ..., D_n\}$ ove ogni dominio contiene i valori ammessi $\{v_1, ..., v_n\}$
    - Le variabili possono avere domini discreti e finiti o infiniti, oppure domini continui (programmazione lineare). Per domini discreti e infiniti serve linguaggio di vincoli $J_1 + d \le J_2$
  - $C$ è un insieme di vincolo, ogni vincolo è costituito da $\lang ambito, relazione \rang$, ambito è una tupla di variabili che partecipano al vincolo e relazione definisce i valori che possono assumere.
  - I vincoli possono essere lineari (risolvibili) o non lineari (non risolvibili da alcun algoritmo)
  - I vincoli possono essere unari, binari, di ordine superiore (Sudoku) o globali a seconda del numero di variabili interessate
  - Ogni relazione supporta due operazioni:
    1. Appartenenza alla relazione
    2. Enumerazione dei membri stessi
- Assegnamento: dare valori alle variabili
  - Completo/parziale se tutte le variabili hanno un valore o meno
  - Consistente se ogni assegnamento non viola alcun vincolo
  - **Soluzione se completo e consistente**
- Si può visualizzare un CSP come **grafo di vincoli**, i nodi corrispondono alle variabili del problema e un arco connette ogni coppia di variabili che partecipano ad un vincolo
  - Ogni vincolo a dominio finito puà essere ridotto ad un insieme di vincoli binari usando variabili ausiliarie. Si ottiene il grafo duale.
- **Propagazione dei vincoli**: utilizzare i vincoli per ridurre il numero di valori legali per una variabile, che a sua volta può ridurre i valori legali per un'altra variabile e così via.
  - Il concetto è forzare la **consistenza locale**, eliminando i valori inconsistenti dal grafo
  - Consistenza di nodo: tutti i valori del dominio soddisfano i vincoli unari del nodo (nodo-consistente)
  - Consistenza d'arco: ogni valore del dominio di un nodo soddisfa i vincoli binari in cui partecipa (arco-consistente)
    - $X_i$ è arco-consistente rispetto a $X_j$ se $\forall v \in D_i, \exists w \in D_j$ che soddisfa in vincolo binario sull'arco $(X_i, X_j)$
    - Algoritmo AC-3:
      1. Mantiene un insieme degli archi
      2. Estrae un arco $(X_i, X_j)$ e rende $X_i$ arco-consistente rispetto a $X_j$
      3. Se il dominio $D_i$ è rimasto invariato si passa all'arco successivo, altrimenti si aggiungono alla coda tutti gli archi $(X_k, X_i)$ con $X_k$ vicino di $X_j$
      4. Se $D_i$ è vuoto allora il CSP non ha soluzione e AC-3 può ritornare fallimento
      5. Otteniamo altrimenti un CSP equivalente all'oroginale ma con future ricerche più rapide perché le variabili hanno domini più piccoli
      - Complessità $O(cd^3)$, $c$ numero di vincoli binari, $d$ dimensione massima di un dominio. Ogni vincolo richiede al più $O(d^2)$ controlli e può essere riaggiunto al più $d$ volte
      - A volte inutile perché ogni variabile è già arco-consistente
  - Consistenza di cammino: restringe i vincoli binari usando vincoli _implicit_ inferiti considerando triplette di variabili
    - Due variabili $\{X_i, X_j\}$ sono cammino-consistenti rispetto ad una terza variabile $X_m$ se per ogni assegnamento $\{X_i=a, X_j=b\}$ consistente con i vincoli $\{X_i, X_j\}$ esiste un assegnamento di $X_m$ che soddisfa i vincoli su $\{X_i, X_m\}$ e $\{X_m, X_j\}$.
    - Si considera essenzialmente un cammino da $X_i$ a $X_j$ con $X_m$ nel mezzo
    - Algoritmo PC-2
  - k-consistenza: per ogni insieme di $k-1$ variabili e loro assegnamento consistente, è sempre possibile assegnare un valore consistente a ogni $k$-esima variabile
    - Fortemente k-consistente se è $k-1$ consistente, $k-2$ consistente etc. fino a $1$-consistente
  - Vincoli globali
    - $Tuttediverse$: se il numero di variabili $m$ è $\gt$ $n$ numero di possibili valori distinti, allora il vincolo non può essere soddisfatto
      - Semplice algoritmo: per ogni variabile del vincolo con dominio con un solo valore, rimuoviamo il valore dai domini delle altre variabili. Se un dominio rimane vuote o $m \gt n$ allora c'è un'inconsistenza
    - Vincolo delle risorse ($atmost(10, P1, P2, P3, P4)$): controllare se la domma dei valori minimi dei domini $\gt$ 10
    - Propagazione degli estremi per ottenere variabili con estremi consistenti: 
      - $D_1 = [0, 165], D_2 = [0, 385]$
      - $F_1 + F_2 = 420$
      - $D_1 = [35, 165], D_2 = [255, 385]$
  - Sudoku: CSP con 81 variabili, 27 vincoli $Tuttediverse$
    - Per gli schemi più facile si può applicare algoritmo AC-3
    - Naked triples: trovare tre caselle in un'unità riga, colonna o riquadro, che abbiano lo stesso dominio. Possiamo rimuovere allora il dominio dalle altre caselle
- Ricerca con backtracking: essenzialmente ricerca depth-first per CSP con assegnamento di singole variabili
  - Applicare ricerca a profondità limitata avrebbe complessità temporale $O(n!d^n)$
  - I CSP sono **commutativi**: qualsiasi ordine di assegnamento alle variabili produce lo stesso assegnamento parziale indipendentemente
    - Basta considerare una sola variabile in ogni livello dell'albero di ricerca
    - Il numero di foglie si restringe a $d^n$
  - La ricerca con backtracking assegna valori ad una variabile per volta e torna indietro quando non ci sono più valori legali da assegnare
    1. Sceglie man mano una variabile non assegnata
    2. Prova uno ad uno tutti i valori del suo dominio
        - Se viene rilevata un'inconsistenza si ritorna alla chiamata precedente che prova un altro valore
  - Miglioramenti:
    - Ordinamento delle variabili
      - Euristica MRV (Minimum Remaining Values): scegliere la variabili con il minor numero di valori legali.
        - Chiamata anche variabile più vincolata o fail-first: con maggior probabilità di arrivare ad un fallimento, potando così l'albero di ricerca
        - Ad esempio se una variabile non ha più valori sarà scelta subita, rilevando il fallimento
        - Miglioramento di fattore 1000
      - Euristica di grado: cerca di contenere il fattore di ramificazione scegliendo la variabile coinvolta nel maggior numero di vincoli con le altre variabili non assegnate
        - Da usare in grado di pareggio dell'euristica MRV
      - Euristica valore meno vincolante: predilige il valore che lascia più flessibilità alle variabili adiecenti sul grafo dei vincoli
  - Alternanza di ricerca e inferenza
    - Invece che applicare AC-3 prima di iniziare la ricerca, possiamo farlo ogni volta che scegliamo un valore per una variabile
    - Verifica in avanti (**forward checking**)
      1. Ogni volta che una variabile $X$ è assegnata, si stabilisce la consistenza d'arco per ogni variabile non assegnata $Y$ collegata a $X$ da un vincolo
      2. Si cancella dal dominio di $Y$ ogni valore non consistente con quello scelto per $X$
      - Si può usare in combinazione con l'euristica MRV come modo efficiente per calcolarne le informazioni 
      - Problema: non guarda avanti per rendere arco-consistenti le variabili non collegate a $X$
      - MAC (Maintaining Arc Consistency): richiama AC-3 per solo gli archi $(X_i, X_j)$ per le $X_j$ non assegnate adiacenti a $X_i$ e **si propaga in avanti** ricorsivamente quando si apportano modifiche ai deomini delle variabili
  - **Backjumping**: backtracking ad una delle variabili che ha causato il fallimento
    - Invece di fare backtracking cronologico al punto decisionale più recente
    - L'insieme dei conflitti per una variabile $X$ è l'insieme delle variabili precedentemente assegnate che sono collegate a $X$ da vincoli.
    - Il forward checking può fornire l'insieme dei conflitti senza lavoro aggiuntivo
      - Ogni volta che si cancella un valore dal dominio di $Y$ a causa di un assegnamento in $X$, si aggiunge $X$ all'insieme di conflitti di $Y$
      - Se viene cancellato l'ultimo valore dal dominio di $Y$, tutti gli assegnamenti dell'insieme di conflitto di $Y$ vanno aggiunti a quello di $X$
      - Allo stesso tempo ridondante con forward checking: ogni ramo dell'albero potato dal backjumping è parimento potato dalla verifica in avanti
    - Backjumping guidato dai conflitti: ci indica fino a dove risalire in modo da non perdere tempo a modificare variabili che non risolveranno il problema
      - Si applica laddove il backjumping fallisce in quanto questi rileva un fallimento quando il dominio di una variabile diventa vuoto, ma in molti casi un ramo è condannato molto prima che questo si verifichi
      1. Sia $X_j$ la variabile corrente e $conf(X_j)$ l'insieme dei suoi conflitti
      2. Se ogni possibile valore di $X_j$ fallisce, si salta indietro alla variabile più recente $X_i$ in $conf(X_j)$ e si assegna $conf(X_i) \leftarrow conf(X_i) \cup conf(X_j) - \{X_i\}$
    - Insieme **no-good**: insieme minimo di variabili dell'insieme dei conflitti, con i relativi valori, che è la responsabile dell'inconsistenza.
      - Si aggiunge ad una cache separata, per evitare di imbattersi nello stesso problema
- Ricerca locale:
  - Si usa uno stato completo e si modifica il valore di una variabile per volta (i.e. 8 regine)
  - Generalmente l'ipotesi iniziale viola diversi vincoli, che si punta ad eliminare
  - L'euristica **min-conflicts**: scegliere il valore che risulta nel numero minimo di conflitti con le altre variabili
    - Nelle 8 regine, il tempo di esecuzione è quasi indipendente dalle dimensioni del problema! In media 50 passi
    - Funziona bene per le 8 regine perché è un problema facile: le soluzioni sono distribuite densamente nello spazio degli stati
    - Purtroppo presenta solitamente un plateau: potrebbero esserci milioni di assegnamenti che distano di un conflitto dalla soluzione
      - Si può indirizzare la ricerca nel plateau tramite **tabu search**, mantenendo un elencato degli stati visitati di recente per impedire all'algoritmo di ritornarvici
    - **Contraint weighting**: si aggiunge una topografia ai plateau
      1. A ogni vincolo è assegnato un peso numerico $W_i=1$ per tutti
      2. L'euristica è modificare una variabile col valore che porterà il minimo peso totale di tutti i vincoli violato
      3. I pesi sono aggiustati incrementando il peso di ciascun vincolo violato
  - Molto usati in ambiente online, ad esempio nelle linee aeree per ottenere schedule alternativo con numero minimo di cambiamenti
    - Una ricerca con backtracking potrebbe prendere molto più tempo e trovare una soluzione molto diversa dall'originario
- Sfruttare la struttura del grafo dei vincoli
  - Sottoproblemi indipendenti, trovabili cercando componenti connessi
    - Se l'assegnamento $S_i$ è una soluzione di $CSP_i$, allora $\cup_i S_i$ è una soluzione di $\cup_i SCP_i$
    - Complessità $O(d^c n/c)$ se ci sono $n/c$ sottoproblemi risolvibili in $d^c$, invece che $O(d^n)$
  - Alberi: un grafo dei vincoli è un albero quando variabili qualsiasi sono collegate da un solo cammino
    - Può essere risolto in tempo lineare al numero di variabili
    - Directed Arc Consistency (DAC): un CSP è arco orientato consistente con ordinamento di variabili $X_1, ..., X_n \Leftrightarrow$ ogni $X_i$ è arco-consistente con ogni $X_j, j \gt i$
    1. Si ordina topologicamente le variabili scelta la radice dell'albero
    2. Si rende l'albero arco-consistente in $O(nd^2)$
    3. Si percorre la lista di variabili e scegliere qualsiasi valore rimanenti
       1. Poiché ogni collegamento è arco-consistente, si può percorrere la lista delle variabili e scegliere qualsiasi valore rimanente sapendo che ci sarà un valore valido nel figlio
    - Non occorre backtracking, $O(nd^2)$ sul numero di variabili
  - Riduzione di grafo dei vincoli ad albero
    - Rimozione dei nodi
      1. Scegliere sottoinsieme di S, chiamato **cycle cutset**, delle variabili del CSP
        - Trovare il più piccolo insieme di taglio dei ciclo è NP-hard, ma ci sono algoritmi approssimati efficienti
      2. Per ogni assegnmaneto delle variabili in S che soddisfi i vincoli in S
        - Rimuovere dal dominio delle variabili rimanenti tutti i valori non consistenti con gli assegnamenti in S
        - Restituire l'eventuale soluzione insieme all'assegnamento per S
      - Complessità $O(d^c \cdot (n - c)d^2)$ se l'insieme di taglio dei cicli ha dimensioni $c$
        - Funziona bene se il grafo è "quasi-albero", quindi $c$ basso
    - Fusione dei nodi: si scompone in un albero di sottoproblemi collegati.
      - Una scomposizione deve soddisfare:
        1. Ogni variabile del problema originare deve comparire in almeno uno dei sottoproblemi
        2. Se due variabili sono collegate da un vincolo devono comparire insieme con il vincolo in almeno uno dei sottoproblemi
        3. Se una variabile compare in due sottoproblemi sull'albero, deve essere presente in tutti i sottoproblemi che compongono il cammino che li collega
            - Ogni variabile deve avere lo stesso valore in ognuno dei sottoproblemi in cui appare
      - Ogni sottoproblema viene risolto in maniera indipendente
        - Se un sottoproblema non ha soluzione, non ce l'ha nemmeno il problema originale
        - Altrimenti ogni sottoproblema viene vista come una variabile il cui dominio è l'insieme delle soluzioni
        - Usando l'algoritmo degli alberi, si possono risolvere i vincoli sui sottoproblemi 
          - La regola 3 impone alle rispettive soluzioni di concordare sui valori delle variabili comuni
      - La dimensione del sottoproblema più grande $-1$ si chiama **larghezza d'albero**. Quella di un grafo è la minima tra le sue scomposizioni ad albero.
        - Complessità temporale $O(nd^{w+1})$
        - I CSP i cui grafi dei vincoli hanno una larghezza d'albero limitato sono risolvibili in tempo polinomiale. 
        - Trovare la scomposizione larghezza d'albero minima è NP-hard, ma ci sono metodi euristici
  - Struttura dei valori: per ogni soluzione consistente esiste un insieme $n!$ di soluzioni permutando i nomi dei colori (**simmetria di valore**)
  - Si può ridurre quindi di $n!$ lo spazio di ricerca introducendo un vincolo di rottura della simmestra, ad esempio un ponendo un vincolo di ordinamento dei colori

## Agenti logici

- Base di conoscenza (KB knowledge base): insieme di formule, espresse mediante un linguaggio di rappresentazione della conoscenza
  - Ogni formula rappresenta un'asserzione sul mondo
  - Deve prevedere meccanismi per aggiungere nuove formule e per interrogazioni tramite $TELL$ e $ASK$
    - Richiedono **inferenza**: derivazione di nuove formule a partire da quelle conosciute
    - La risposta deve essere una conseguenza di quello che è stato detto $TELL$ in precedenza
  - Può avere una base di conoscenza iniziale (_background knowledge_)
- Un agente basato sulla conoscenza fa tre cose:
  1. Comunica le percezioni alla base di conoscenza attraverso $TELL$
  2. Chiede $ASK$ quale azione eseguire
  3. Registra l'azione nella base di conoscenza con $TELL$ prima di eseguirla
- Un agente basato sulla conoscenza può essere costruito semplicemente dicendogli $TELL$ ciò che deve sapere. QUesto è un approccio dichiarativo, invece di codificare direttamente i comportamenti desiderati con un approccio procedurale.
- PEAS del mondo di Wumpus
  - Misura di performance: +1000 se si esce dalla caverna, -1000 se si muore, -1 per ogni azione, -10 uso freccia
  - Ambiente: griglia 4x4, inizio in [1, 1]. Tutti i riquadri hanno probabilità 0.2 di contenere un pozzo
    - Discreto, statico, monoagente, sequenziale, parzialmente osservabile
  - Attuatori: Avanti, GiraSinistra, GiraDestra, Afferra, Scocca
  - Sensori: Fetore, Brezza, Scintillio, Urto, Ululato
  - La principale difficoltà è l'ignoranza della configurazione dell'ambiente, bisogna usare un ragionamento logico
- Modelli
  - $m$ è un modello di una sentenza $\alpha$ se $\alpha$ è vera in $m$
  - $M(\alpha)$ è l'insieme di tutti i modelli di $\alpha$
  - $KB \models M(KB) \subseteq M(\alpha)$
  - Il model checking è un algoritmo di inferenza logica che enumera tutti i possibili modelli per verificare che $KB \models \alpha$ 
    - I.e. $KB$ = "nulla in [1, 1] e brezza in [2, 1]" e $\alpha$="Non c'è nulla in [1, 2]"
    - Completo se lo spazio dei modelli di $KB$ è finito
    - Corretto, poiché applica solo la definizione di conseguenza logica
    - Complessità temporale $O(2^n)$ e spaziale $O(n)$ (l'enumerazione viene fatta in profondità)
      - La conseguenza logica proposizionale è co.NP-completa, quindi nel caso peggiore ogni algoritmo di inferenza è esponenziale
- Proprietà algoritmi di inferenza
  - Correttezza: preserva la verità
  - Completezza: può derivare ogni formula che è conseguenza logica
- Logica proposizionale
  - La sintassi definisce le formule accettabili
  - Formule atomiche: consistono di un singolo simbolo. Sono vere o false.
  - Formule complesse formate da formule più semplici usando connettivi logici: not, and,, or, implicazione, sse.
  - Due formule sono logicamente equivalenti $\alpha \equiv \beta$ se sono vere nello stesso insieme di modelli
  - Una formula è valida se è vera in tutti i modelli: tautologia, cioè equivalente a True
  - > Date due formule qualsiasi $\alpha$ e $\beta$, $\alpha \models \beta$ sse la formula ($\alpha \Rarr \beta$) è valida, cioè equivalente a True
  - Una formula è soddisfacibile se è vera in qualche modello. Determinare la soddisfacibilità delle formule della logica proposizionale, problema SAT, è NP-completo.
  - > Dimostrazione per refutazione o per contraddizione: $\alpha \models \beta$ sse la formula ($\alpha \land \lnot\beta$) è insoddisfacibile
  - Monotonicità: conoscenze aggiuntive possono solo aiutare a trorre nuove conclusioni, mai invalidare conclusioni già dedotte. Se $KB \models \alpha$ allora $KB \land \beta \models \alpha$
    - Oppure anche: le regole di inferenza possono essere applicate non appena si trovano nella base di conoscenze le premesse necessarie, indipendentemente dal resto delle formule nella KB
- Regole di inferenza: applicabile per derivare una dimostrazione, conclusioni che portano all'obiettivo desiderato
  - **Modus Ponens**: $\alpha \Rarr \beta, \alpha \over \beta$
- Dimostrazione di teoremi: applicando regole di inferenza direttamente alle formule della KB per costruire una dimostrazione della formula desiderata senza consultare i modelli
  - Possiamo applicare uno degli algoritmi di ricerca per trovare una sequenza di passi che costituisca una dimostrazione
    - Stato iniziale: KB
    - Azioni: insieme delle regole di inferenza applicate a tutte le formule che corrispondono alla metà superiore della regola di inferenza
    - Risultato: aggiunta della formula nella metà inferiore della regola di inferenza
    - Obiettivo: Uno stato che contiene la formula che vogliamo dimostrare
  - Trovare una dimostrazione può essere molto efficiente perché si possono ignorare le proposizioni irrilevanti
  - Singola regola di inferenza, la **risoluzione**: unita a qualsiasi algoritmo di ricerca completo dà lungo ad un algoritmo di inferenza completo
  
  $$
  l_1 \lor ... \lor l_k,\ m_1 \lor ... \lor m_n \over l_1 \lor ... \lor l_{i-1} \lor l_{i+1} \lor ... \lor l_k \lor m_1 \lor ... \lor m_{i-1} \lor m_{i+1} \lor ... \lor m_n
  $$

  1. Prende due clausole, disgiunzioni di letterali, con $l_i$ e $m_i$ letterali complementari e ne produce una nuova che contiene tutti i letterali delle due clausole tranne i due complementari
  2. La clausola risultante contiene solo una copia di ogni letterale (**fattorizzazione**)
  - Ogni formula della logica proposizionale è equivalente ad una congiunzione di clausole: **conjunctive normal form (CNF)**
  - Le procedure di inferenza basate sulla risoluzione sfruttano il principio di dimostrazione per assurdo. Algoritmo $\text{CP-RISOLUZIONE}$:
    1. Si converte $KB \land \lnot \alpha$ in CNF
    2. Viene applicata la risoluzione ad ogni coppia di clausole
       1. Ogni coppia che contiene letterali complementari è risolta e la nuova clausola viene aggiunta all'insieme se non presente
    3. Il processo continua finché:
       1. Non è più possibile aggiungere nuove clausole. $KB \not \models \alpha$
       2. La risoluzione tra due clausola dà la clausola vuota. $KB \models \lnot \alpha$ è False, quindi $KB \models \alpha$
        - La clausola vuota è equivalente a False perché è una disgiunzione senza alcun disgiunto ed una disgiunzione è vera solo se è vero almeno uno dei disgiunti
  - L'algoritmo è completo
    - **Chiusura della risoluzione** $RC(S)$ di un insieme di clausole $S$: è l'insieme di tutte le clausole derivabili dall'applicazione ripetuta della regola di risoluzione alle clausole in $S$ o a quelle da loro derivate
      - $RC(S)$ è finito in quanto è finito il numero di clausole distinte costruite con i simboli di $S$ grazie alla fattorizzazione $\implies$ $\text{CP-RISOLUZIONE}$ termina sempre l'esecuzione
    - **Teorema di risoluzione ground**: se un insieme di clausole è insoddisfacibile, la sua chiusura della risoluzione contiene la clausola vuota.
      1. Dimostrazione per contrapposizione: se la chiusura $RC(S)$ non contiene la clausola vuota, $S$ è soddisfacibile
      2. Possiamo costruire un modello di S assegnando adeguati valori di verità a $P_1, ...,P_k$
      3. L'assegnamento dato è un modello di $S$. Dimostriamo ipotizzando l'opposto
      4. In qualche fase $i$ c'è stata un'assegnazione che rende falsa una clausola $C$
         - $i$ non può essere 1 altrimenti $RC(S)$ conterrebbe una clausola vuota
         - Devono esserci due letterali complementari $P_i$ e $\lnot P_i$
         - Dato che $RC(S)$ è chiuso rispetto alla risoluzione, contiene anche il risolvente, che però avrà già tutti i letterali falsi per $P_1, ..., P_{i-1}$
         - Viola l'ipotesi che la prima clausola falsa appaia nella fase $i$
- Clausole di Horn e clausole definite
  - Se le formule della KB hanno certe restrizioni, si possono applicare algoritmi di inferenza più ristretti ed efficienti della risoluzione
  - **Clausola definita**: disgiunzione di letterali di cui _esattamente uno_ è positivo. $\lnot L_{1, 1} \lor \lnot Brezza \lor B_{1,1}$
  - **Clausola di Horn**: disgiunzione di letterali in cui _al massimo uno_ dei letterali è positivo
    - Le clausole definite sono un caso particolare
    - Le clausole senza letterale positivo si chiamano _clausole obiettivo_
    - Sono interessanti perché:
      1. Possono essere riscritte come implicazione $L_{1, 1} \land Brezza \Rarr B_{1,1}$. La premessa si chiama corpo, la conclusione testa. I letterali positivi nel corpo sono fatti.
      2. L'inferenza sulle clausole di Horn può essere fatta con concatenazione in avanti e all'indietro
      3. Si può determinare la conseguenza logica in tempo lineare rispetto alla dimensione della KB
  - Concatenazione in avanti: determina se un singolo simbolo $q$ è conseguenza logica dei _fatti_ nella KB
    1. Se tutte le premesse di un'implicazione sono verificate, la conclusione è aggiunta ai fatti noti
    2. Continua finché non viene aggiunta la query $q$ oppure non sono possibili ulteriori inferenze
    - Corretto: applica il Modus Ponens
    - Completo: ogni formula atomica conseguenza logica sarà derivata
    - Ragionamento guidato dai dati: l'attenzione parte dai fatti conosciuti
  - Concatenazione all'indietro: parte dalla query e lavora a ritroso
    1. Trova tutte le implicazioni nella KB che hanno $q$ come conclusione
    2. Se tutte le premesse di una delle implicazioni possono essere dimostrate vere, allora $q$ è vera
    3. Si ripete il procedimento quindi per le premesse, viste come query, fino a raggiungere un insieme di fatti noti che formano la base della dimostrazione
    - Ragionamento basato sugli obiettivi
    - Complessità spesso meno che lineare sulla dimensione della KB, in quanto coinvolge solo i fatti rilevanti
