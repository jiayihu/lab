# Machine Learning

Created: Jan 23, 2020 2:18 PM
Updated: Jan 23, 2020 3:07 PM

## Supervised learning

- Inductive bias
- Spazio delle ipotesi $H$ con ipotesi $h \in H$
- Ad esempio spazio delle curve polinomiali, con obiettivo minimizzare formula

$$min_{w} 1/n \sum_{i=1}^n (y_i - wx_i)^2$$

- Training/empirical error vs ideal error
- Regularization con coefficiente di penalità alpha

$$min_{w} 1/n \sum_{i=1}^n (y_i - wx_i)^2 + \alpha ||w||^2$$

- Formula per calcolare $w$ da $X$ e $y$
a
$$w = (X^TX + \alpha I)^{-1}X^Ty$$

## PAC, generalization e SRM

- PAC (Probably Approximately Correct)

    $$P(|\sigma - \pi| \ge \epsilon) \le 2e^{-2\epsilon^2 N}$$
    $$N \ge {1\over\epsilon} ln({|H|\over\delta})$$
    $$P(|\sigma - \pi| \ge \epsilon) \le M 2e^{-2\epsilon^2 N}$$

- In genere c'è molto overlapping di bad events: $m_H(N) \ll 2^ N$

- Shattering

    $$\forall S' \subseteq \ S, \exists h \in H, \forall x \in S,\  h(x) = 1\ sse\ x \in S'$$

- VC-dimension: $max{|S|_{S \subseteq X}}: H\ shatters\ S$

    - La VC dimension dello spazio di iperpiani in $R^n$ è $n + 1$
- Generalization error con VC-confidence

    $$error(g) \le error_S(g)+ F({VC(H)\over n}, \delta)$$

    - Inversamente proporzionale a n e $\delta$, direttamente proporzionale ad VC(H)
    - SRM (Structural Risk Minimization) per il miglior trade-off tra A e B

## Decision trees

- Struttura nodi interni e foglie
- Equivalenza con logica proposizionale in disjunctive normal form (DNF)
- Quando usarli
  - Classificazione
  - Facile interpretazione (motivi legali)
  - Veloce valutazione $log(depth)$
- CART e ID3
- Scelta dell'attributo ottimo (non sempre possibile, svantaggio del pre-pruning con XOR)
  - Entropia: $\sum_c^m p_c\ log\ p_c$ con $p_c = {|S_c| \over |S|}$
  - Gini Index: $\ - \sum_c^m p_c^2$
  - Miclassification: $1 - max_c(p_c)$
- Information gain: $G(S, a) = E(s) - \sum_{v\in V(a)} {|S_v| \over |S|} E(S_{a=v})$
- Bias del decision tree: l'information gain favorisce attribute con tanti valori discreti
  - Lo spazio delle ipotesi è l'insieme degli alberi
  - La tecnica è hill-climbing, quindi greedy
- Valori continue
  - Calcolo splitting point nel mezzo tra due istanze di classe diversa
  - L'attributo può essere riutilizzato
- Overfitting
  - Depth massima
  - Numero minimo di istanze in un branch, 5% del dataset
  - Pre-pruning
  - Post-pruning con errore validation set o ipotesi nulla con confidence $1- \delta$
  - Rule post-pruning con regole ordinate per performance
- Istanze con valore mancante per attributo $a$
  - Valore più comune in X
  - Valore più comune in base a y
  - $n = |V(a)|$ rimpiazzi con peso $P(a=v|Tr)$ con $v \in V(a)$
  - Mean imputation
  - Imputation by regression

## Neural networks

- Quando usarli
  - Sia classificazione che regressione
  - Alta dimensionalità
  - Accettabile tempo di learning alto ma tempo di valutazione basso della funzione appresa
  - Non è richiesta comprensibilità (black box)
  - Speech and image recognition
  - Adattamento con online learning
- Perceptron $y = sign(wx + b)$ oppure $y = sign(wx)$ con $w_0 = 1$
  - Calcola la discriminante lineare
  - Equivalenza con funzioni booleane AND, OR, NOT ma non con XOR
- base learning rule (perceptron)
  - Scelto $x \in S$ random
  - $w \leftarrow w + \eta(t - o)x_i$ con $o = sign(w \cdot x)$ e $t \in \{-1, +1\}$
    - Se la predizione è sbagliata $t = 1, o = 0$ e $x_i \ge 0$ allora il peso cresce per avvicinarsi a 1, altrimenti se $x_i \le 0$ decresce
  - Se lo spazio è linearmente separabile, termina in numero di passi finito
- activation function / threshold function
  - hard threshold (non derivabile): $sign(w\cdot x)$
  - sigmoide / logistic function (derivabile): ${1\over 1+e^{-w \cdot x}}$ 
- Hidden layers
  - Senza HL la decision surfaces ha un boundary lineare
  - Con HL le decision surfaces non sono lineari e le varie regioni sono unite dall'AND delle unità output
- Gradiente:
  - > Il gradiente di una funzione  $f : R^n \rightarrow R$ è una funzione vettoriale $\nabla f: R^n \rightarrow R^n$. Spesso definito come il vettore che ha per componenti le derivate parziali della funzione.
  - L'algoritmo discesa di gradiente permette di trovare il vettore $R^n$ che minimizza la funzione $f$, ovvero $x^* = argmin_x f(x)$
- delta rule:
  - attivazione lineare, senza hard-threshold: $o = w\cdot x$
    - $E[w] = {1 \over 2N} \sum_i^n (t^i - o^i)^2$ da minimizzare rispetto a $w$
      1. Assegna a $w_i$ valori random in $[-0.01, 0.01]$ vicini allo zero
      2. $\Delta w_i \gets 0$
      3. $\forall (x, t) \in S$: $\Delta w_i \gets \Delta w_i + \eta(t - wx_i)x_i$
      4. $\forall i \in \{1,...,n\}$: $w_i \gets w_i + \Delta w_i$
  - sigmoide: $o = \sigma(w\cdot x)$, $y = w \cdot x$
    - $\Delta w_i \gets \Delta w_i + \eta(t - o)\sigma(y)(1 - \sigma(y))x_i$
- Multilayer:
  - d unità di ingresso: $x = (x_1, ..., x_d)$
  - N unità nascoste: $y = (y_1, ..., y_N)$
  - c unità di output: $z = (z_1, ..., z_c)$
  - $w_{ji}$ peso da $x_i$ a $y_j$
  - $w_{kj}$ peso da $y_j$ a $z_k$
- Backpropagation-1hl-stocastico:
  1. Calcola $y$ e $z$ con $x$
  2. $\forall z_k$
     - $\delta_k = z_k(1 - z_k)(t - z_k)$
     - $\Delta w_{kj} = \delta_ky_j$
  3. $\forall y_j$
     - $\delta_j = y_j(1 - y_j)\sum_{k=1}^c w_{kj}\delta_k$
     - $\Delta w_{ji} = \delta_jx_i$
  4. $w_{sq} \gets w_{sq} + \eta \Delta w_{sq}$
- Teorema di universalità:
  - > Data una funzione continua $f : R^n \rightarrow R$, esiste un intero M tale che una rete neurale con almeno M unità nascoste sia in grado di calcolare una funzione $\hat{f}: R^n \rightarrow R$ e $max|f(x) - \hat{f}(x)| < \epsilon$ con qualunque $\epsilon > 0$.
- Difficoltà:
  - Topologia della rete e numero unità nascoste
  - Learning rate $\eta$
  - Tempo di apprendimento
    - Aggiunta di un termine momento
  - Minimi locali
    - Apprendimento stocastico 
    - Reti diverse con diverse inizializzazioni o ensemble
- Autoencoders
- Overfitting con troppe iterazioni
  - Regularization sui pesi $w$

## GLM (Generalized Linear Models) & SVM (Support Vector Machines)

- Margin: $\rho = 2r = {2 \over ||w||}$
- Link with SRM: la VC-dimension degli iperpiani ottimali è $VC_{opt}\le min\{\lceil{R^2 \over \rho^2}\rceil\} + 1$
  - Il true risk può essere minizzato massimizzando il margine $\rho = {2 \over ||w||}$, quindi minimizzando $||w||$
- $min_{w,b} {1\over2}||w||^2$ subject to $\forall i\in \{1,...,n\}: y_i(wx_i + b) \ge 1$
  - Convex quadratic problem con linear constraints: può essere risolto analiticamente senza iterazioni come gradient descent
- Duale con coefficienti di Lagrange
- $max_{\alpha} \sum_{i=1}^n\alpha_i - {1 \over 2} \sum_{i,j=1}^n y_i y_j \alpha_i \alpha_j (x_i \cdot x_j)$ subject to $\forall i\in \{1,...,n\}\ \alpha_i \ge 0, \sum_i^n y_i \alpha_i = 0$
- Support vectors $x_i: \alpha_i \ge 0$ 
- $w = \sum_i^n y_i \alpha_i x_i$
- $b = y_k - wx_k$ per $\alpha_k \ge 0$
- $h(x) = sign(wx + b) = sign(\sum_i^n y_i \alpha_i (x_i x) + b)$
- Non linearmente separabile
  - $min_w {1 \over 2}||w||^2 + C\sum_i^n \xi_i$ subject to $\forall i \in \{1,..., n\}\ \xi \ge 0, y_i(wx_i + b -) \ge 1 - \xi_i$
  - Hinge loss, simile a cross-entropy e più robusto di misclassification error o square error
- Kernel functions
  - Basis functions: $x \rightarrow \phi(x)$ con $\phi(x) = [\phi_1(x), ..., \phi_M(x)]$
  - $w = \sum_i^n y_i \alpha_i \phi(x_i)$
  - $h(x) = sign(\sum_i^n y_i \alpha_i (\phi(x_i) \cdot \phi(x)) + b)$
  - $K(x_k, x) = \phi(x_k) \cdot \phi(x) = K(x, x_k)$
  - $h(x) = sign(\sum_i^n y_i \alpha_i (K(x_i, x) + b)$
  - Kernel o gram matrix, simmetrica e positiva per definizione
  - Mercer's condition
- Possible kernels
  - Linear kernel, $K(x, x') = x \cdot x'$
  - Polynomial kernel: $K(x, x') = (x \cdot x' + u)^p$ 
  - Radial basis function: $K(x, x') = exp(-\gamma ||x - x'||^2)$ 
- Regression $min_{w, b, \xi, \xi^*} {1 \over 2}||w||^2 + C\sum_i^n(\xi + \xi^*)$ subject to:
  - $\forall i \in \{1, ..., n\} \xi_i, \xi_i^* \ge 0,\ y_i - (w \cdot x_i + b) \le \epsilon + \xi_i,\  (w \cdot x_i + b) - y_i \le \epsilon + \xi_i^*$
- Kernel trick
  1. Qualsiasi algoritmo che possa essere riscritto come dot product dei suoi input, può sfruttare il kernel trick per miglior efficienza di computazione
  2. Utilizzo di algoritmi con input vettoriale per oggetto non vettoriali. La kernel function misura la similarità tra gli oggetti: $d(x, z) = ||\phi(x) - \phi(z)|| = \sqrt{K(x, x) + K(z, z) - 2K(x, z)}$
     - Stringhe (DNA), alberi, graphi
- Combinazione di kernel: somma, prodotto per costante, prodotto di kernels
  - Si possono anche usare dei pesi e diventa una sorta di ensemble di kernels, in modo da combinare l'informazione di similarità da diversi kernels
- Quando usarli
  - Input non vettoriali, grazie alle funzioni kernel
  - Utilizzo di kernel matrix invece del dataset originale, NLP e bioinformatica
  - Speech and image recognition

## Preprocessing

- Feature categoriche/simboliche
  - Nominali vs Ordinali
  - OneHot Encoding con dummy variables
- features quantitative/numeriche
  - Intervalli vs Reali
  - $\hat{x}_j = {1 \over n}\sum_i^n x_{ij}$ e $\sigma_j = \sqrt{{1 \over n}\sum_i^n (x_{ij}-\hat{x}_j)^2}$
  - Centering: $c(x_{ij}) = x_{ij} - \hat{x}_j$
  - Standardizzazione: $s(x_{ij}) = {c(x_{ij}) \over \sigma_j}$
  - Scaling: $h(x_{ij}) = {x_{ij} - x_{min,j} \over x_{max, j}-x_{min, j}}$
  - Normalizzazione: $g(x) = {x \over ||x||}$
- Il k-NN richiede normalizzazione esempi, come pure k-means o la rete neurale
- Feature selection
  - Forward e backward selection
  - Non adatto per immagini
- Feature extraction
  - PCA: autovettori ed autovalori
  - LDA

## Model selection e validation

- Bias $E[\hat{f}(x)] - E[f(x)]$ e varianza $E[(f(x) - E[f(x)])^2]$
- Underfitting/overfitting
- Cross validation
- K-fold cross-validation
  - $k = |Tr|$ LOOCV
  - Bias/variance col cambiare di k
  - Per selezione degli iper-parametri
- Accuracy, non ideale se ci sono tanti esempi positivi rispetto ai negativi
- Contingency table
  - Precision: $\pi = {TP \over TP + FP}$ (degree of soundness)
  - Recall: $\rho = {TP \over TP + FN}$ (degree of completeness)
- F-measure $F_\beta = {(1 + \beta^2) \pi \rho \over \beta^2 \pi + \rho}$
  - Con $\beta = 1$ si ha $F_\beta = {2 \pi \rho \over \pi + \rho}$
- Multiclass classification
  - One-vs-all
  - One-vs-one (pairwise)
  - Confusion matrix: precision in colonna, recall in riga
  - Micro/macro-averaging

## Apprendimento Bayesiano

- $P(h|D) = {P(D|h) * P(h) \over P(D)}$
- $h_{MAP} = argmax_{h \in H}P(D|h) = h_{ML}$
- Nel caso dell'apprendimento di una funzione reale con target a distribuzione normale
  - $p(d_i|h) = {1 \over \sqrt{2\pi\sigma^2}}e^{{-1 \over 2\sigma^2}(d_i - h)^2}$
  - $h_{ML} = argmax_{h \in H}\ p(D|h) = argmax_{h \in H}\ \prod p(d_i|h) = argmax_{h \in H}\ \sum ln(p(d_i|h))$
- Nel caso di una rete neurale in cui si vuole la probabilità della classificazione invece che 0/1 si vede che la $h_{ML}$ minimizza la cross-entropy, migliore dello square error.
- Classificazione ottimale di Bayes, pesata dalle probabilità a posteriori $P(v_j|D) = \sum_{h \in H}P(h|D) P(v_j|h)$
- Classificatore di Gibbs $E[\epsilon_{Gibbs}] \le 2E[\epsilon_{Bayes}]$
- Naive Bayes
  - Quando usarlo
    - Dataset grandi
    - Classificazione di documenti (newsgroups con accuracy 86%)
  - $v_{MAP} = argmax_{v \in V} P(a_1, ..., a_m|v)P(v) = argmax_{v \in V}P(v)\prod_i^m P(a_i|v)$
  - L'assunzione naive di indipendenza condizionale è spesso violata, ma l'algoritmo funziona comunque senza stimare correttamente le probabilità a posteriori, a patto che $argmax_{v \in V}\hat{P}(v)\prod_i^m \hat{P}(a_i|v) = argmax_{v \in V}P(v)\prod_i^m P(a_i|v)$
  - La probabilità a posteriori del NV tende a 1 o 0 essendo produttoria
  - m-stima di probabilità con valori virtuali $\hat{P}(a_1|v_j) = {n_c + mp \over n + m}$
    - $m$ è chiamato equivalent sample size
- Expectation Maximization (EM)
  - Sceglie ipotesi iniziale random $h = \langle\mu_1, \mu_2\rangle$
  - Passo E: calcola il valore atteso $E[z_{ij}]$ assumendo valga l'ipotesi $E[z_{ij} = {p(x = x_i| \mu = \mu_j) \over \sum_j^2 p(x = x_i| \mu = \mu_j)}]$
  - Passo M: calcola la nuova ipotesi $h_{ML}$, assumendo i valori attesi $E[z_{ij}]$: $\mu_{j} = {\sum_{i}E[z_{ij}] x_i \over \sum_i E[z_{ij}]}$

## Ensemble learning

- Vogliamo learners con alta accuracy ma il più diversi possibili
- Justification:
  - Statistical: by “averaging” the votes of several ”good” classifiers the risk of choosing the wrong classifier is reduced
  - Computational: an ensemble constructed by running the local search from many different starting points may provide a better approximation to the
true unknown function, avoiding to be stuck in local minima
  - Representation: forming weighted sums of hypotheses drawn from H it may be possible to expand the space of representable functions
- Generalization error $E[(y - g(x))^2]$ = noise^2 + bias^2 + variance
- Parallel
  - Voting
    - $P(H(x) \not ={f(x)}) \le e^{-T/2(2\epsilon - 1)^2}$
    - Purtroppo in realtà gli errori dei votanti non è indipendente
  - Bagging
    - Bootstraping
    - The prediction is $H(x) = 1/k \sum_i^k h_i(x)$
    - In teoria il bias rimane invariato e la varianza diminuisce, nella pratica il bias aumenta perché i weak learners non sono indipendenti
    - Funziona bene con unstable learners, con alta varianza, come decision stumps
  - Random forests
    1. Usa bootstrap
    2. Utilizza features random come nodi interni
    3. Aggrega le predizioni
- Sequential/Boosting
  - Usa weak-learners con $accuracy = 0.5 + \epsilon$
  - AdaBoost: $H(x) = sign(\sum_t \alpha_t h_t(x))$
    - Inizialmente $p_t^i = 1 / N$
    - Ad ogni passo, l'errore è $\epsilon$ e $\beta = {1 - \epsilon_t \over \epsilon_t}$ e $p_{t+1}^i = \beta p_t^i$ se l'istanza $x_i$ è stata classificata correttamente, altrimenti rimane $p_t^i$
    - I pesi dei weak learners sono $w_t = log({1 \over \beta_t})$
    - Adaboost non fa overfitting e riduce il bias dei weak-learners, che però hanno bassa varianza
    - Sensibile al rumore
- Stacking
  - Diversi tipi di learners
  - Combinazione non lineare dei learners per aggiustare il loro bias
  - Richiede apprendimento dei parametri della combinazione non lineare su un diverso dataset
  - Learners complementari con diversi bias induttive

## Clustering

- Criteri interni di valutazione
  - Similarità intra-class
  - Similarità inter-class
  - Misura di similarità, ad esempio norma euclidea
- Criteri esterni di valutazione
  - Classificazione esterna ground-truth
  - Purity: $|c| \over |K|$ con $c$ classe dominante e $K$ il cluster
  - RandIndex: simile alla contingency table, considerando coppie di esempi
    - Si possono considerare le equivalenti della precision e della recall
- Algoritmi di partizionamento
  - k-means
    - Centroide $\mu(c) = {1 \over |C|} \sum_{x \in C} x$
    - Ad ogni esecuzione si riduce il valore della funzione obiettivo $V(D, \gamma) = \sum_{k}^K \sum_{i:\gamma(d_i) = k} ||d_i - c_k||^2$
- Algoritmi gerarchici
  - Bottom-up HAC (Hierarchycal Agglomerative Clustering)
      - Single-link: minor similarità di una coppia $O(N^2)$
      - Complete-link: massima similarità di una coppia, $O(N^2 log(N))$
      - Average-link: media similarità di tutte le coppie, $O(N^2 log(N))$
      - Centroid: distanza dei centroidi, $O(N^2 log(N))$
  - Top-down
  - Dendrogramma
