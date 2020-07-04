# Game theory

## Static games of complete information

- Static: all players move together, without knowledge of everybody else's move
- Complete information: anybody payoff function is known
  - Everybody knows all possible actions of all players
  - All possibile outcomes resulting from these actions
  - The utilities of these outcomes
- Pure strategies: a deterministic plan of action
- **Normal form**: set of strategies and relative utilities for all players
- **Rationality** is common knowledge: everybody is maximizing their own payoff and everybody knows that everybody is maximizing their payoff!
- A joint strategy $s'$ **Pareto** dominates $s$ if it's at least as good for every player and strictly better for some player
  - For every player i: $u_i(s') \ge u_i(s)$
  - For some player i: $u_i(s') \gt u_i(s)$
- $s_i$ is strictly dominated by $s_i'$ if the payoffs for player $i$ is always better by playing $s_i'$
- (Iterated elimination of strictly dominated strategies) **IESDS**
  - Usuful to obtain a smaller game
  - Usually doesn't provide a solution though
  - The order is irrelevant
- **Best response**: in order for a player to be optimizing in a game, he has to choose a best strategy as a response to the strategies of his opponents
  - $BR: S_{-i} \rarr p(S_i)$ (not a function, there can be multiple best response strategies)
- **Nash equilibrium**: for a n-player game, strategies $(s_1^*, ..., s_n^*)$ are Nash equilibrium if each strategy $s_i^*$ is the best response of the player $i$ to other player's best strategies
  - Af a strategy is not the best response, it means there exists an incentive for the player to deviate and choose another strategy
  -  If a joint strategy is the survivor of IESDS the it's a NE
- Dominance
  - $s_i'$ **strictly dominates** $\forall s_{-i}\ s_i$ if $u_i(s_i',s_{-i}) \gt u_i(s_i, s_{-i})$
  - $s_i'$ **weakly dominates** $\forall s_{-i}\ s_i$ if $u_i(s_i',s_{-i}) \ge u_i(s_i, s_{-i})$ and $\exists s_{-i}\ u_i(s_i',s_{-i}) \gt u_i(s_i, s_{-i})$
- Do not eliminate weakly dominated strategies
- Pareto efficiency $\not ={}$ Nash equilibrium
  - **Pareto efficiency**: no way (**in the whole game**) a user can improve without somebody else being worse
  - **Nash equilibrium**: no way a user can improve with a **unilateral change**
- The **simplex** $\Delta A$ is the set of possible probability distributions over a non-empty discrete set $A$
- A mixed strategy is a probabilità distribution over pure strategies
  - Pure strategies can be seen as degenerate probability distributions
- The **support** of a mixed strategy $m_i \in \Delta S_i$ in the set of strategies with positive probabilities $\{s_i \in S_i : m_i(s_i) \gt 0\}$
- A joint mixed strategies is said to be a Nash equilibrium if its expected utilities is better than any other mixed strategies, including degenerate pure strategies
  - No player has incentive to change his move
- IESDS: if a pure strategy $R$ is strictly dominated by a mixed strategy $m$, $R$ can be removed
- If a joint mixed strategy $m$ is a Nash equilibrium, then $\forall s_i \in support(m_i)\ u_i(m) = u_i(s_i, m_{-i})$ (the utility of any strategy in the support set is equal to the expected utility of the mixed strategy)
  - If a player is playing a mixed strategy then he must be indifferent between the actions he is choosing with positive probability
- Every two-player games with two strategies has a NE in mixed strategies
- What does "mixed strategies as probabilities" mean? In the end, players take pure strategies.
  - If the game is played $M$ times, mixed strategy $q$ is like choosing a pure strategy $qM$ times
  - **Beliefs**: the probability $q$ reflects the uncertainty that my opponent has about my choice (which is pure)
- A **belief** of a player $i$ a probability distribution over the strategies of this opponents

## Dynamic games

- A dynamic games involves some players moving first, others moving later
- Complete information, everyone knows the payoffs
  - **Perfect information**: every player can make a decision with full awareness of its state. 
    - All information sets are singleton and there is no choice of Nature
  - **Imperfect information**: some decisions are simultaneous or Nature plays
    - Some information sets contain multiple nodes or there is a choice of Nature
    - Players form beliefs
- If the information set is a singleton $\{x_j\}$ then the node is fully aware of the previous moves
  - If not, i.e. $x_k \in h_i(x_j)$ and $x_j \not ={x_k}$ then $A(x_j) = A(x_k)$
- Pure strategy: specifies an action according to what happened in the game
- A mixed strategy draws probabilities over the strategy set at the beginning only
- A behavioral strategy specifies an independent probability distribution over $A_i(h_i)$ for any information set $h_i(x_j) \in H_i$
  - Behavioral = mixed if perfect recall holds, that is no player forgets information that he previously knew
- The **equilibrium path** containts the decision tree nodes that are reached with positive probability given a joint profile of behavioral strategies
  - In a Nash equilibrium players choose to proceed on the equilibrium path over leaving it and choosing some other path in the game, given the belief that the other players will stick  to their strategy
- Solved with **backward induction**. If terminal payoffs are all different, the solution is unique.
  1. Player 2 sees Player 1's move $a_1^h$ and solves the optimization problem $R_2 = max_{a_2 \in A_2} u_2(a_1^h, a_2)$
  2. Player 1 anticipates 2's reaction and solves $a_1^* = max_{a_1 \in A_1} u_1(a_1, R_2(a_1))$
  3. $(a_1^*, R_2(a_1^*))$ is a Nash equilibrium in pure strategies
- A subgame $G$ contains a single node of the tree and all of its successor nodes, with the requirement $\forall X_j \in G, x_k \in h_i(x_j) \rArr x_k \in G$
- A Nash equilibrium is **subgame-perfect** if the strategies chosen by the players give a NE in every subgame
  - Every finite extensive form game has a SPE
  - For perfect information game with finite horizon, SPE is the outcome of backward induction
    - Cannot be applied to games of imperfect information or with infinite horizon
      - Requires treating subgames of imperfect information and considering possible best response strategies like in Mutually Assured Destruction case
    - Can be extended for other dynamic games taking into account the **credibility** of the threats.
    - Player 1 knows $a_1$ implies response $R_2(a_1)$, so strategies "if $a_1$ then $a_2 \not ={R_2(a_1)}$" are classified as non-credible
  - A SPE requires not only that a Nash equilibrium profile of strategies is a combination of best responses on the equilibrium path, but also that the profile of strategies consist of mutual best responses off the equilibrium path
    - The SPE is a Nash equilibrium in every proper subgames, including those that are not reached in equilibrium
- Multistage games: finite sequence of T normal form **stage games**
  - Stages games are defined independently of each other and include the same set of players
  - Stage games are complete but imperfect information games (simultaneous moves)
  - Total payoffs are evaluated from the sequence of outcomes of the stage games
    - Can use discounted factor $\delta$ which gives total payoff $u_i = \sum_j\delta^j u_i^j$
- If a $s_j^*$ is a NE strategy profile in the $j$th stage game, then there exists a SPE whose equilibrium path is $s_1^*, s_2^*, ..., s_T^*$
  - We can build a SPE combining stage NEs
  - A NE must be played in the final stage game $G_T$
  - If each stage game has a unique NE, then the multistage game has a unique SPE
  - If the last $T$-th stage has **multiple NE**, this enables non-NE to be played in the other stages
    - We can build SPE where some intermediate stages have non-NE strategies
    - Strategic connection is possible if the last stage NEs are considerably different: a "stick" and a "carrot"
      1. Play desired non-NE action in the first stage
      2. Reward opponents with carrot if they do the same
      3. Otherwise threaten opponents with stick
    - $\delta$ must be high enough for the different payoffs of carrot and stick to have impact
      - Relates to **patience** or credibility of threats
      - $\delta = 0$ players don't care about the future
    - The carrot-and-stick procedure can work to create a SPE where the first move is whatever, but higher discount factors will be needed
- A one-stage unimprovable strategy must be optimal
- A repeated game $G(T , \delta)$ is a dynamic game where the same static game $G$ is played as a stage game $T$ times and payoffs are discounted by $\delta$ and cumulated
  - Finitely repeated games with finite horizon
  - Infinitely repeated games with infinite horizon, $\delta \lt 1$
- As a consequence of multi-stage games:
  - The outcome of last stage is a NE
  - If stage game $G$ only has NE $s^*$, then $G(T , \delta)$ has a unique subgame-perfect outcome, which play $s^*$ in every stage
  - With multiple NEs, repeated games tend to introduce cooperation if punishment strategies are available
- In infinitely repeated games $G(\infty, \delta)$ we cannot apply backward induction (no “last” stage)
  - We do not need "external" punishments!
  - Players have the ability to support play that is not a static Nash equilibrium even when the stage game has a unique equilibrium!
  - There may be SPE of $G(\infty, \delta)$ in which no stage's outcome is a NE of G
  - **Grim trigger** strategy
    - For $\delta$ close enough to 1, the joint strategy where both users play GrT is a SPE
    - In any subgame, we have two states:
      1. There was no deviation and the players intend to play cooperatively
      2. There was some previous deviation and this triggers the grim strategy

## Bayesian games

- Game of incomplete information: players can be of different types, which implies them to behave differently and also the other players to have beliefs about it
  - Nature draws a type among all possibilities for all the players
  - Nature reveals type $t_i$ to player $i$ only
  - Players choose their actions
  - Payoffs are computed
- **Common prior assumption**: players don't precisely know the types of their opponents but know the probability distribution of the types
- We change incomplete information into imperfect information
  - For player 1, he wants to average on all possibilities of his opponenent's types, seen as they are facinsg a player using mixed strategies
  - For player 2, he is replaced by a meta-player playing both types as mixed strategy
- **Bayesian Nash equilibrium** BNE: is a set of strategies, one for each type of player, such that no type has incentive to change his or her strategy given the beliefs about the types and what the other types are doing
  - Found as SPE of Nash equilibria
- Dynamic Bayesian games
  - We cannot apply backward induction because there is only one subgame
  - Uncertainty about player types merges all the subtrees
  - If we have a Bayesian NE $s^*$, we say that an information set is on the equilibrium path if, given the distribution of types, it is reached with probability $\gt 0$
  - In an extensive-form Bayesian game, a system of belief $\mu$ is a prob distribution over decision nodes for every information set
  - A **system of belief** $\mu$ assigns a probability distribution over decision nodes to very information set, given a strategy $s^*$
  - **Perfect Bayesian equilibrium** PBE: a pair $(s^*, \mu)$ of BNE $s^*$ and its system of beliefs $\mu$ meeting the 4 requirements:
    1. Players must have a system of beliefs, which assigns probability to nodes given $s^*$
    2. On the equilibrium path, they must follow Bayes' rule on conditional probability
    3. Off the equilibrium path: arbitrary  
       - Weak requirement which leads to "strage" PBE, a better requirement is to have **consistent** strategies
         - $(s^*, \mu)$ strategy is said to be consistent if they are the limit of a sequence of non-degenerate strategies-beliefs pairs: $(s^*, \mu) = \lim_{k\to \infty} (s^*_k, \mu_k)$
         - A **sequential equilibrium** is a consistent PBE
    4. Given the beliefs, players are sequentially rational: that is, they play a best response
  - If $s^*$ is a profile of strategies inducing system of beliefs $\mu$ where every information set is reached with probability $\gt 0 \rArr (s^*, \mu)$ is a PBE
