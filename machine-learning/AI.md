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
  - Si può derivare euristiche ammissibili dal costo della soluzione di un sottoproblema (e.g. 4 tasselli)
    - Database di pattern: memorizzare i costi delle soluzioni dei sottoproblemi (programmazione dinamica)
  - Algoritmi di miglioramento iterativo
    - Il cammino non è rilevante, conta trovare una configurazione ottima nello spazio degli stati dato dall'insieme delle configurazioni "complete"
    - Si mantiene un singolo stato corrente e si tenta di migliorarlo iterativamente
    - Complessità spaziale costante, quindi adatto per la ricerca online
