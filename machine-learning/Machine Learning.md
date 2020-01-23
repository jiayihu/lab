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

    - Per un input space $R^n$, la VC-dimension data dall'iperpiano è n+1. Ad esempio $R^2$ con iperpiano una retta ha VC-dimension 3
- Generalization error con VC-confidence

    $$error(g) \le error_S(g)+ F({VC(H)\over n}, \delta)$$

    - Inversamente proporzionale a n e $\delta$, direttamente proporzionale ad VC(H)
    - SRM (Structural Risk Minimization) per il miglior trade-off tra A e B

## Decision trees

- Struttura nodi interni e foglie
- Equivalenza con logica proposizionale in disjunctive normal form (DNF)
- Quando usarli
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
