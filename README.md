# Tenenbaum

<img src="https://raw.github.com/doubleagent/tenenbaum/master/tenenbaum.jpg"
 alt="Tenenbaum Portrait" title="The woman herself!" align="right"/>

> That Tenenbaum ain't what you think. Florence Nightingale, huh? That'll all
> come crashing down 'fore you can say 'canned tomatoes'. I've seen good bunco,
> and I've seen great bunco. But, when you waltz through Rapture and World War
> Two without even a scratch? You got more than leprechauns watching over you.
> ―Frank Fontaine


Tenenbaum is a library for performing a genetic algorithm with good
performance characteristics on a user-specified domain.

## Usage

Leiningen dependency:

    tenenbaum "0.1.0-SNAPSHOT"

Typically you just need to implement the EvolvingOrganism protocol and call evolve.  Evolve takes your requested population size / 2, the maximum number of generations you want to evolve, and two functions.  A function to create your EvolvingOrganism, and a function to compare them.

Example Usage

```clojure
(ns tenenbaum.core.test
  (:require [tenenbaum.core :refer :all])
  (:import java.util.Random))


(defrecord TenOrg [n]
  EvolvingOrganism
  (meetsGoal [this] (== (:n this) 10))
  (computeFitness [this] (-' 10 (:n this)))
  (crossOver [m f]
             (let [n1 (:n m) n2 (:n f)]
               [(->TenOrg (+' n1 n2)) (->TenOrg (-' n1 n2))]))
  (timeToMutate [this] (.nextBoolean (java.util.Random.)))
  (mutate [this]
          (cond (.nextBoolean (Random.)) (->TenOrg (inc' (:n this)))
                :else (->TenOrg (dec' (:n this))))))

(defn cfn [[m mf] [f ff]]
  (cond (< (Math/abs (double mf)) (Math/abs (double ff))) -1
        :else 1))

(defn initfn [] (->TenOrg (rand-int 8)))

(evolve 15 200 initfn cfn)
```

```clojure
{:finished true, :generation 3, :alphas {2 #genetic.core.test.TenOrg{:n 12}, 1 #genetic.core.test.TenOrg{:n 11}, 0 #genetic.core.test.TenOrg{:n 7}}, :solution #genetic.core.test.TenOrg{:n 10}}
```

## License

Copyright © 2014

Distributed under the Eclipse Public License either version 1.0 or (at
your option) any later version.
