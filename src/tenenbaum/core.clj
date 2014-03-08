(ns tenenbaum.core
  (:require [clojure.core.async :as async :refer [go chan >! <!! close!]]))


;; Protocols

(defprotocol EvolvingOrganism
  "This class encapsulates the domain logic for the 'organism' you are trying
  to evolve toward a solution.

  meetsGoal - Returns a boolean indicating whether or not this organism has
  successfully evolved toward the goal.

  computeFitness - Returns any kind of data ostensibly indicating how close
  the organism is to reaching the solution.

  crossOver - Returns a Vector of children eg [son daughter] by 'mating' two
  EvolvingOrganisms.  son and daughter must be the same type as the parents.

  timeToMutate - Returns a boolean indicating whether or not the organism
  should be mutated or not.  It is common to produce a random result based on
  a percentage eg (.nextBoolean (java.util.Random.)) gives each organism a 50%
  chance to mutate.  This occurs each generation for each child.

  mutate - Returns a mutated version of this organism.  Only executed if
  timeToMutate returns true."
  (meetsGoal [this])
  (computeFitness [this])
  (crossOver [this that])
  (timeToMutate [this])
  (mutate [this]))


;; Async EvolvedState

(defn goal?
  "Takes two EvolvingOrganisms and returns the result of meetsGoal on each
  in a map eg {:m true :f false}.  Executes meetsGoal on each organism
  asynchronously and in parallel."
  [m f]
  (let [c (chan 2)]
    (go (>! c [:m (meetsGoal m)]))
    (go (>! c [:f (meetsGoal f)]))
    (->> (async/take 2 c)
         (async/into [])
         (<!!)
         (flatten)
         (apply hash-map))))

(defn fitness
  "Takes two EvolvingOrganisms and returns the result of computeFitness on each
  in a map eg {:m 1232 :f 8923} or {:m {:val1 10 :val2 20} :f {:val1 -1}}.
  Executes computeFitness on each organism asynchronously and in parallel."
  [m f]
  (let [c (chan 2)]
    (go (>! c [:m (computeFitness m)]))
    (go (>! c [:f (computeFitness f)]))
    (let [[[k1 v1] [k2 v2]] (->> (async/take 2 c)
                                 (async/into [])
                                 (<!!))]
      {k1 v1 k2 v2})))

(defn mutate-pair
  "Takes two EvolvingOrganisms and mutates them if timeToMutate returns true.
  Returns a vector of mutated EvolvingOrganisms eg [mutated-m mutated-f].
  Executes timeToMutate and mutate on each asynchronously and in parallel."
  [m f]
  (let [c (chan 2)]
    (go (>! c (cond (timeToMutate m) (mutate m) :else m)))
    (go (>! c (cond (timeToMutate f) (mutate f) :else f)))
    (<!! (async/into [] (async/take 2 c)))))


;; Utility

(defn pair-children
  "Given [c1 c2 c3 c4...], returns ([c1 c2] [c2 c3] [c3 c4] [c4 ...] ...)
  Expects cardinality to be even."
  [children]
  (->> [(drop-last children) (rest children)]
       (apply interleave)
       (partition-all 2)))

(defn sort-children
  "Given ([[c1 c1-fitness] [c2 c2-fitness]] [[c3 c3-fitness] [c4 c4-fitness]] ...),
  and a comparison function that takes a list with the structure
  ([c1 c1-fitness] [c2 c2-fitness]), sorts the EvolvingOrganism-fitness pairs
  by the comparison function to get eg
  ([c4 c4-fitness] [c2 c2-fitness] [c3 c3-fitness] [c1 c1-fitness] ...), where
  c4 is closest to the goal according to the comparison function"
  [cfn children]
  (->> children
       (flatten)
       (partition-all 2)
       (sort-by identity cfn)))


;; Genetic Algorithm

(defn offspring
  "Given two children, this algorithm performs the following with good
  performance characteristics
  1. Generates offspring (crossOver).
  2. If either of them meet the goal, return the winning organism.
  3. Otherwise mutate them if necessary.
  4. If either of the mutants meet the goal, return the winning organism.
  5. Return the children/mutants produced by this generation.

  Return characteristics:
  1. None, or one, or both of the children may be mutated.
  2. Mutations, goal checks, and fitness calculations are performed in parallel
  for each child.
  3. 'Winning' Organism includes a :goal keyword at the front of the data, paired
  with the winning organism.  The winning organism is always at the front eg
  [[:goal child2 fitness-2] [child2 fitness-2]]
  4. When no 'winner' has been found, the structure of the return data is the
  same, except there is a nil where :goal would be eg
  [[nil child2 fitness-2] [child1 fitness-1]]"
  [m f]
  (let [[child1 child2] (crossOver m f)
        win (goal? child1 child2)]
    (cond (or (:m win) (:f win))
            (let [fit (fitness child1 child2)]
              (cond (:m win) [[:goal child1 (:m fit)] [child2 (:f fit)]]
                    (:f win) [[:goal child2 (:f fit)] [child1 (:m fit)]]))
          :else (let [[mchild1 mchild2] (mutate-pair child1 child2)
                      fit (fitness mchild1 mchild2)
                      win (goal? mchild1 mchild2)]
                  (cond (:m win) [[:goal mchild1 (:m fit)] [mchild2 (:f fit)]]
                        (:f win) [[:goal mchild2 (:f fit)] [mchild1 (:m fit)]]
                        :else [[nil mchild1 (:m fit)] [mchild2 (:f fit)]])))))

(defn pick-mates
  "Given the parents of each generation, and a pair of children to choose
  from, the parents influence how the children are mated.  Input structure
  includes pairs fitness with each child and parent, and the logic works
  like this:

  1. Fathers dislike potential son-in-laws who are jst like them.
  2. Fathers prefer daughter-in-laws who remind them of theirself.
  3. First generation mates don't have picky parents to deal with

  The logic is thus because (offspring) treats the first EvolvingOrganism
  in a pair as the male, and the latter as a female.  crossOver, which
  produces children, is not necessarilly commutative ie
  (crossOver this that) != (crossOver that this)

  So the parents intervene to help ensure that the 'male' doesn't have the
  same fitness generation after generation.

  Returns the children with their fitness, 'mated', eg
  [[c1 c1-fitness] [c2 c2-fitness]]

  This is kind of weird, because the parents are picking the gender roles
  of each successive generation, even though the kids have already determined
  that they want to mate."
  [[[m mf] [f ff]] [[c1 cf1] [c2 cf2]]]
  (cond (nil? m) [[c1 cf1] [c2 cf2]]
        (= mf c1) [[c2 cf2] [c1 cf1]]
        :else [[c1 cf1] [c2 cf2]]))

(defn do-generation
  "Given a generation ie 0, 1, 2, ..., a list of begetters which have the
  structure ([[m1 m1-fitness] [f1 f1-fitness]] [[m2 m2-fitness] [f2 f2-fitness]] ...),
  a list of children which have the structure
  ([c1 c1-fitness] [c2 c2-fitness] [c3 c3-fitness] ...), a channel to produce
  the 'winning' EvolvingOrganism on, and a comparison function which takes a
  list of the structure ([c1 c1-fitness] [c2 c2-fitness]), this performs a full
  generation by doing the following:

  1. Mates the children into a list of the structure
  ([[m1 m1f] [f1 f1f]] [[m2 m2f] [f2 f2f]] ...)
  2. Generates the offspring of each mated pair in parallel.
  3. Produces the goal on goal-chan as [:goal child] ASAP.
  4. Returns the results of the generation as vector to be used as input for
  performing the next iteration.
  4a. The children which are produced are sorted by cfn, so that the child
  closest to the goal appears at the front of the list."
  [generation begetters children goal-chan cfn]
  (let [mates (map pick-mates begetters (pair-children children))
        kid-chan (chan)]
    (doseq [mate mates]
      (go (let [[[m mf] [f ff]] mate
                [[goal? c1 f1] [c2 f2]] (offspring m f)]
            (>! kid-chan [[c1 f1] [c2 f2]])
            (>! goal-chan [goal? c1]))))
    [(inc generation)
     mates
     (->> (async/take (count mates) kid-chan)
          (async/into [])
          (<!!)
          (sort-children cfn))
     goal-chan
     cfn]))


;; Kick-off!

(defn evolve
  "n - The number of threads you want to run concurrently for each generation,
  where each thread produces the offspring of a pair of mated EvolvingOrganisms.
  2n - The number of children you want vying/procreating each generation. Never
  grows or shrinks from generation to generation.

  max-gen - The maximum number of generations you will allow to run.

  initfn - A function which generates an EvolvingOrganism.  It is a good idea
  to ensure that initfn cannot produce an Organism which meets the goal, since
  the algorithm does not check if the first generation meets the goal or not.

  cfn - A comparison which takes input of the form
  ([organism1 fitness1] [organism2 fitness2]), and returns 1 or -1, 1 if
  organism1 is closer to the goal, and -1 if organism2 is closer to the goal.

  This function creates a channel to accept the 'winning' organism ASAP, by
  specifying input must be of the form [:goal organism].  Any input without
  the :goal keyword at the front will be filtered out.

  This function then iterates over each generation until the goal has been
  reached or the maximum number of generations have been reached.

  It updates an agent every generation with the current generation eg 0,1,2,...,
  the 'alpha-male' of each generation, and the solution (when it receives it).

  Returns the structure of the agent, which is a map of the form
  {:finished <bool> :generation <number> :alphas {0 alpha0 1 alpha1 ...} :solution organism}"
  [n max-gen initfn cfn]
  (let [goal-chan (async/filter> #(= (first %) :goal) (chan))
        progress (agent {:finished false :generation 0 :alphas {} :solution nil})
        adam-eves (sort-children cfn
                   (map (fn [org] [org (computeFitness org)])
                        (repeatedly (* 2 n) initfn)))
        begetters (partition-all 2 (map (fn [ae] [nil nil]) adam-eves))]
    (letfn [(update-progress [generation alpha]
                             (send progress assoc
                                   :generation generation
                                   :alphas (assoc (:alphas @progress) (dec generation) alpha)))
            (progress-fn [[generation begetters children goal-chan cfn]]
                         (update-progress (inc (:generation @progress)) (first (first children)))
                         (do-generation generation begetters children goal-chan cfn))
            (goal-fn [[_ sol]]
                     (send progress assoc
                           :finished true
                           :solution sol)
                     (close! goal-chan))
            (drop-fn [prog]
                     (cond (:finished @progress) nil
                           (> (:generation @progress) max-gen) nil
                           :else true))]
      (go (async/take! goal-chan goal-fn))
      (->> [0N begetters adam-eves goal-chan cfn]
           (iterate progress-fn)
           (drop-while drop-fn)
           (first)
           (doall))
      @progress)))
