# Theorems

## CCS Finito

> Dimostrare che ogni processo del CCS finito termina in un numero finito di passo (senza somme infinite e con).

$$P ::= \alpha.P\ |\ \sum_{i \in I} P_i\ |\ P|Q\ |\ P \backslash L\ |\ P[f]$$

### L'insieme I è finito

Definiamo la dimensione di P come il # di caratteri di P, indicato con |P|.

- $|\alpha.P| = |P| + 1$
- $|\sum_{i \in I} P_i| = \sum_{i \in I} |P_i| + (|I| - 1)$
- $|P \vert Q| = |P| + |Q| + 1$
- $|P \backslash L| = |P| + 1$
- $|P[f]| = |P| + 1$

Claim: la dimensione di P è finita. La dimostrazione avviene per induzione sulla struttura del processo.

- Caso base: $P=0 \implies |0| = 1$ quindi |P| è finito
- Casi induttivi:
  - Ipotesi induttiva. Dato i processi P e Q, $|P|$ è finito e anche $|Q|$ è finito
  - Casi $d = |\alpha.P|$, $|P \backslash L|$, $|P[f]|$ ove per definizione $d = |P| + 1$. Poiché per ipotesi induttiva $|P|$ è finita, allora anche $d = |P+1|$ è finito
  - Caso $d = |P \vert Q|$ ove per definizione $d = |P| + |Q| + 1$. Poiché per ipotesi induttiva $|P|$ e $|Q|$ sono finite, allora anche $|P| + |Q| + 1$ è finito
  - Caso $d = |\sum_{i \in I} P_i|$ ove per definizione $d = \sum_{i \in I} |P_i| + (|I| - 1)$.
    - Per ipotesi induttiva $|P_i|\ i \in I$ è finito, per cui anche $\sum_{i \in I} |P_i|$ è finita in quanto l'insieme $I$ è finito
    - $(|I| - 1)$ è finito in quanto l'insieme $I$ è finito

Claim: $P \xrightarrow{\alpha} P'$ allora $|P'| < |P|$.  La dimostrazione avviene per induzione sulle struttura della derivazione $P \xrightarrow{\alpha} P'$

- $\text{ACT}$ $\cfrac{}{\alpha.P \xrightarrow{\alpha} P}$. Si ha che $|P| \lt 1 + |P| = |\alpha.P|$

- $\text{SUM}j$ $\cfrac{P_j \xrightarrow{\alpha} P_j'}{\sum_{i \in I} P_i \xrightarrow{\alpha} P_j'}$.
  - Ipotesi induttiva $|P_j'| \lt |P_j|$
  - Si ha che $|P_j'| \lt \sum_{i \in I} |P_i| + (|I| - 1) = |\sum_{i \in I} P_i|$ in quanto è incluso anche $|P_j|$ e per ipotesi induttiva $|P_j'| \lt |P_j|$

- $\text{COM1}$ $\cfrac{P \xrightarrow{\alpha} P'}{P|Q \xrightarrow{\alpha} P'|Q}$.
  - Ipotesi induttiva $|P'| \lt |P|$
  - Si ha $|P'|Q| = |P'| + |Q| + 1 \lt |P| + |Q| + 1 = |P|Q|$ per ipotesi induttiva

- $\text{COM2}$ $\cfrac{Q \xrightarrow{\alpha} Q'}{P|Q \xrightarrow{\alpha} P|Q'}$.
  - Ipotesi induttiva $|Q'| \lt |Q|$
  - Si ha $|P|Q'| = |P| + |Q'| + 1 \lt |P| + |Q| + 1 = |P|Q|$ per ipotesi induttiva

- $\text{COM3}$ $\cfrac{P \xrightarrow{\alpha} P', Q \xrightarrow{\alpha} Q'}{P|Q \xrightarrow{\tau} P'|Q'}$.
  - Ipotesi induttiva $|P'| \lt |P|$, $|Q'| \lt |Q|$
  - Si ha $|P'|Q'| = |P'| + |Q'| + 1 \lt |P| + |Q| + 1 = |P|Q|$ per ipotesi induttiva

- $\text{RES}$ $\cfrac{P \xrightarrow{\alpha} P'}{P\backslash L \xrightarrow{\alpha} P' \backslash L}$.
  - Ipotesi induttiva $|P'| \lt |P|$
  - Si ha $|P' \backslash L| = |P'| + 1 \lt |P| + 1 = |P \backslash L|$ per ipotesi induttiva

- $\text{REL}$ $\cfrac{P \xrightarrow{\alpha} P'}{P[f] \xrightarrow{\alpha} P' [f]}$.
  - Ipotesi induttiva $|P'| \lt |P|$
  - Si ha $|P' [f]| = |P'| + 1 \lt |P| + 1 = |P [f]|$ per ipotesi induttiva

Claim: data una derivazione $P \xrightarrow{\alpha_1} P_1 \xrightarrow{\alpha_2}...\xrightarrow{\alpha_n}P_n \implies n \lt |P|$. La dimostrazione segue dal fatto che se $P \xrightarrow{\alpha_1}P_1$ allora $|P_1| \lt |P|$ e  $|P| \ge |P_1| + 1 \ge |P_2| + 1 ... \ge |P_n| + 1$ ma significa che dopo $n$ passi, $|P| \ge |P_n| + n$ ovvero che $n + |P_n| \le |P|$. Poiché $|P_n| > 0$ per qualsiasi $P_n$, allora $n \lt |P|$.

Claim: P termina sempre in un numero finito di passi. Dimostrazione: il numero di passi di derivazione $n$ è finito, ma questo segue dal fatto che $n \lt |P|$ e $|P|$ è finito.

### L'insieme I è infinito

Definisco come $h$ l'altezza massima dell'albero di derivazione di un processo $P$. Inoltre, poiché non ci sono costanti, la LTS corrispondente ad un processo corrisponde all'albero di derivazione.

L'idea è che ad ogni passo "scendo" in giù nel LTS e non ho mai cicli oppure non "risalgo" mai in su.

```
A
↓
B  x
↓  ↑
C →
↓
```

- Caso base: $P=0$. Si ha $h = 0$ quindi finita.
- Caso induttivo: 
  - Ipotesi induttiva: $P$ e $Q$ hanno lunghezza massima finita $h_P$ e $h_Q$.
  - Caso $\alpha . P$: Può fare solo una derivazione, quindi $h+1$ rimane finita a patto che non possa fare prefixing infinito $P = a_0.a_1...a_n.0$.
  - Caso $\sum_{i \in I} P_i$. Per ipotesi induttiva, ciascuno dei processi $P_i$ ha altezza finita $h_i$. Allora $\sum_{i \in I}$ può avere infinite derivazioni possibili verso i $P_i$ ma non varia l'altezza della derivazione. Quindi l'altezza massima risultante è $h_{max} = max \{h_i | i \in I\}$.
    - Poiché assumiamo che non si possa fare prefixing infinito, non è possibile avere $P = \sum_{i \in I} \underbrace{a.a...a}_{\text{i volte}}.0$
  - Caso $P|Q$. 
    - L'albero di derivazione risultante può decidere di fare un passo $\ne \tau$ usando l'albero di P o quello di Q finendo in un nodo in cui potrà ancora fare tutti i passi di Q o P rispettivamente. $h_{P|Q} = h_P + h_Q$ e un passo diminuirà di 1 $h_{P|Q}$.
      - Da notare che in questo caso, poiché in CCS finito non ci sono transizioni cicliche nell'albero di derivazione, un processo finito parallelo $P$ può essere "convertito" in uno sequenziale finito $Q$ tale che $P \sim Q$. Ad esempio $P = a.0 | b.0$ è bisimile a $Q = a.b.0 + b.a.0$ ed entrambi hanno $h=2$. Anzi i rispettivi LTS sono isomorfi, per cui a  maggior ragione $P \sim Q$.
    - Può anche a volte fare un passo $\tau$ se lo stesso passo è presente in entrambi gli alberi di P e Q. $h_{P|Q} = h_P + h_Q$ e un passo diminuirà di 2 $h_{P|Q}$.
    - L'altezza massima di $P|Q$ è dunque quella ottenuta facendo nessuno passo di sincronizzazione. $h_{P|Q} = h_P + h_Q$ ed è finita in quanto $h_P$ e $h_Q$ sono finite per ipotesi induttiva.
  - Caso $P \backslash L$. Restringere canali può solo diminuire il numero di passi possibili, che rimane quindi bounded a $h$.
  - Caso $P[f]$ non cambia il numero di passi possibili perché ha effetto solo verso l' "esterno". L'altezza massima rimane quindi $h$, finita.

Parentesi: isomorfismo => strong bisimilarity ma non viceversa.

- Isomorfismo tra due grafi LTS dei processi $P$ e $Q$ con insiemi di nodi $G$ e $H$ rispettivamente, significa che esiste una funzione biunivoca $f: G \rightarrow H$ tale che $\forall (u, v) \in G, \{u, v\} \in E \iff \exists \{f(u), f(v)\} \in F$. E e F sono gli insiemi di edges rispettivamente per i grafi di $P$ e $Q$.
  - La relazione di bisimulazione è $\mathcal{R} = \{(s_1, s_2) \in G\times H | f(s_1) = s_2 \}$. La dimostrazione penso che sfrutti il fatto che la funzione $f$ sia una funzione, quindi ad ogni stato del dominio corrisponde uno e uno solo stato del codominio, e sia biunivoca per cui esiste l'inversa.
- Strong bisimilarity invece non implica isomorfismo. Esempio:
  - $A = a.A$
  - $B = b.B1; B1 = b.B1;$
  - $A \sim B$ ma i due grafi LTS non sono isomorfi, B ha due vertici, A uno solo e si avrebbe $f(A) = B$ e $f(A) = B1$ ma allora $f$ non è più una funzione.

## Congruenza osservazionale

> Dimostrare che P e Q sono debolmente bisimili se e solo se vale una delle tre (i) P osservazionalmente congruente a Q (ii) P osservazionalmente congruente a tau.Q (iii) tau.P osservazionalmente congruente a Q

$P \approx Q \iff (P \approx_c Q\ \text{oppure}\ \tau.P \approx_c Q\ \text{oppure}\ P \approx_c \tau.Q)$

1. $(P \approx_c Q\ \text{oppure}\ \tau.P \approx_c Q\ \text{oppure}\ P \approx_c \tau.Q) \implies P \approx Q$
     - $P \approx_c Q \implies P \approx Q$
       - Ipotesi $P \approx_c Q$ significa che: 
         - Se $P \xrightarrow{\alpha} P'$ allora $Q \xRightarrow{\alpha}_c Q'$ e $P' \approx Q'$
         - Se $Q \xrightarrow{\alpha} Q'$ allora $P \xRightarrow{\alpha}_c P'$ e $P' \approx Q'$
       - Dimostrazione: 
         - Basterebbe usare il fatto che $\approx_c \subseteq \approx$
         - Se $P \xrightarrow{\alpha}P'$ allora $Q \xRightarrow{\alpha}Q'$ in quanto per ipotesi $Q \xRightarrow{\alpha}_c Q'$ con $\xRightarrow{\alpha}$ non passo nullo. E sempre per ipotesi $P' \approx Q'$.
         - Analogo se $Q \xrightarrow{\alpha}Q'$
         - Dunque $P \approx Q$
   - $\tau.P \approx_c Q \implies P \approx Q$
     - Ipotesi $\tau.P \approx_c Q$ significa che $\tau.P \approx Q$. Devo solo dimostrare allora che $\tau.P \approx Q \implies P \approx Q$.
     - Ipotesi:
       - Se $\tau.P \xRightarrow{\alpha} P'$ allora $Q \xRightarrow{\alpha} Q'$ e $P' \approx Q'$
       - Se $Q \xRightarrow{\alpha} Q'$ allora $\tau.P \xRightarrow{\alpha} P'$ e $P' \approx Q'$
     - Dimostrazione: 
       - Se $P \xrightarrow{\alpha}P'$ $\alpha$ è un passo che può fare anche $\tau.P$ a prescindere che $\alpha$ sia o meno $\tau$. Lo fa con $\tau.P \xrightarrow{\tau}P \xrightarrow{\alpha} P'$, ovvero $\tau.P \xRightarrow{\alpha} P'$. Allora $Q \xRightarrow{\alpha}Q'$ per ipotesi. E sempre per ipotesi $P' \approx Q'$.
       - Se $Q \xrightarrow{\alpha}Q'$ allora $Q \xRightarrow{\alpha} Q'$. A questo punto per ipotesi $\tau.P \xRightarrow{\alpha} P'$ e quindi anche $P \xRightarrow{\alpha} P'$ a prescindere che $\alpha$ sia o meno $\tau$ e $P' \approx Q'$.
   - Analogo per $P \approx_c \tau.Q \implies P \approx Q$. Dovrei dimostrare che $P \approx \tau.Q \implies P \approx Q$. Ma per simmetria $\tau.Q \approx P$ e $Q \approx P$ e allora l'ho già dimostrato nel punto prima.
2. $P \approx Q \implies (P \approx_c Q\ \text{oppure}\ \tau.P \approx_c Q\ \text{oppure}\ P \approx_c \tau.Q)$
   - Ipotesi $P \approx Q$:
     - Se $P \xrightarrow{\alpha} P'$ allora $Q \xRightarrow{\alpha} Q'$ e $P' \approx Q'$
     - Se $Q \xrightarrow{\alpha} Q'$ allora $P \xRightarrow{\alpha} P'$ e $P' \approx Q'$
   - Dimostrazione: controllo l'ipotesi e guardo quale processo tra $P$ e $Q$ risponde ad **almeno una** transizione $\xrightarrow{\tau}$ con una transizione nulla. A questo punto rientro in uno solo dei 4 casi:
     1. Nessuno dei due. Allora dimostro banalmente che $P \approx_c Q$.
        - Se $P \xrightarrow{\alpha} P'$ allora per ipotesi $Q \xRightarrow{\alpha} Q'$ e $\xRightarrow{\alpha}$ **non è nullo** quindi $Q \xRightarrow{\alpha}_c Q'$ e $P' \approx Q'$ per ipotesi.
        - Analogamente se $Q \xrightarrow{\alpha} Q'$ allora per ipotesi $P \xRightarrow{\alpha} P'$ e $\xRightarrow{\alpha}$ **non è nullo** quindi $P \xRightarrow{\alpha}_c P'$ e $P' \approx Q'$ per ipotesi.
     2. Solo $Q$ risponde con una transizione nulla a $P \xrightarrow{\tau} P'$. Allora dimostro che $P \approx_c \tau.Q$
        - Se $P \xrightarrow{\alpha} P'$ allora per ipotesi $Q \xRightarrow{\alpha} Q'$ ma potrebbe essere che $\xRightarrow{\alpha}$ **è nullo** e quindi $\alpha = \tau$. Allora $\tau.Q \xRightarrow{\tau}_c Q = Q'$ e $P' \approx Q'$ per ipotesi. Se invece non è nullo allora semplicemente $\tau.Q \xrightarrow{\tau} Q \xRightarrow{\alpha}_c Q'$ e $P' \approx Q'$ per ipotesi.
        - Se $Q \xrightarrow{\alpha} Q'$ allora per ipotesi $P \xRightarrow{\alpha} P'$ e $\xRightarrow{\alpha}$ **non è nullo** quindi $P \xRightarrow{\alpha}_c P'$ e $P' \approx Q'$ per ipotesi.
     3. Solo $P$ risponde con una transizione nulla a $Q \xrightarrow{\tau} Q'$. Allora dimostro che $\tau.P \approx_c Q$
        - Se $P \xrightarrow{\alpha} P'$ allora per ipotesi $Q \xRightarrow{\alpha} Q'$ e $\xRightarrow{\alpha}$ **non è nullo** quindi $Q \xRightarrow{\alpha}_c Q'$ e $P' \approx Q'$ per ipotesi.
        - Se $Q \xrightarrow{\alpha} Q'$ allora per ipotesi $P \xRightarrow{\alpha} P'$ ma potrebbe essere che $\xRightarrow{\alpha}$ **è nullo** quindi $\alpha = \tau$. Allora $\tau.P \xRightarrow{\tau}_c P = P'$ e $P' \approx Q'$ per ipotesi. Se invece non è nullo allora semplicemente $\tau.P \xrightarrow{\tau} P \xRightarrow{\alpha}_c P'$ e $P' \approx Q'$.
     4. Sia $P$ che $Q$ rispondono con una transizione nulla, ma questo in realtà non può accedere. Supponiamo accada, sicuramente $\alpha = \tau$:
        - Questo significa che $P \xrightarrow{\tau} P'$ e $Q \xRightarrow{\tau} Q'$ passo nullo con $Q' = Q$. Inoltre per ipotesi di weak bisimilarity $P' \approx Q$
        - Analogamente $Q \xrightarrow{\tau} Q'$ e $P \xRightarrow{\tau} P'$ passo nullo con $P' = P$. Inoltre per ipotesi di weak bisimilarity $Q' \approx P$
        - Ricordiamo l'ipotesi iniziale $P \approx Q$
        - Allora, grazie a simmetria abbiamo $P' \approx Q \approx P \approx Q'$. Allora per transitività abbiamo $P' \approx Q'$.
        - A questo punto vediamo che $P \approx_c Q$ in quanto se $P \xrightarrow{\tau} P'$ allora $Q \xRightarrow{\tau}_c Q'$ e $P' \approx Q'$ e analogo se $Q \xrightarrow{\tau} Q'$. Ma questo contraddice l'ipotesi che i due processi "si rispondessero" con la transizione vuota.
        - Versione più leggibile e chiara:
          $$
          a \xrightarrow{\tau} b \newline
          x \xrightarrow{\tau} y \newline
          a \approx x, b \approx x, y \approx a \newline
          b \approx x, x \approx a, a \approx y \newline
          b \approx x \approx a \approx y \newline
          b \approx y
          $$
