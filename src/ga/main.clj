(ns ga.main (:gen-class ))

(defstruct member :fitness :value)
(defstruct mutator :fitness :mutator :mater :target :threshold)

;;;;;;;;;;;;;;;;;;;;;;;general GA functions;;;;;;;;;;;;;;;;;;;;;;;

(defn- fit?
  "checks if population has any members which match the desired value"
  [population]
  (not (empty? (filter #(== (:fitness @%1) 0) population))))
  
(defn- mutate
  "randomly mutates values in members of the population using the mutator function"
  [population mutator]
  (doseq [member population]
      (let [old-val (:value @member)
            fitness (:fitness @member)
            new-val ((:mutator mutator) old-val (:target mutator) (:threshold mutator) fitness)]
      (swap! member #(assoc %1 :value new-val)))))
      
(defn rank
  "ranks the members of the population using the val-comp function and the target value"
  [population mutator]
  (let [target  (:target mutator)
        fitness (:fitness mutator)]
    (doseq [member population]
      (swap! member #(assoc %1 :fitness (fitness (:value %1) target))))     
    (vec (reverse (sort #(compare (:fitness @%1) (:fitness @%2)) population)))))


(defn- mate
  [members mutator] 
  (let [size (count members)]
        (for [i (range 0 size)]
          (atom (struct member nil 
            ((:mater mutator) (:value @(get members (rand-int (dec size)))) 
                              (:value @(get members (rand-int (dec size))))))))))

(defn evolve
  "mutates the populationtakes then combines the top members
   of the population with some of the bottom members to promote
   genetic diversity, and adds some offspring"
  [population mutator]
  (mutate population mutator)
  (let [promote-size (/ (count population) 5)
        keep-size (- (/ (count population) 2) promote-size)
        mate-size (- (count population) (+ promote-size keep-size))
        parts (split-at keep-size population)]
      (concat (first parts)
              (take promote-size (second parts))
              (mate (vec (take mate-size population)) mutator))))

(defn init-population
  "creates a population using the generator function"
  [mutator width height pop-size member-size]  
    (for [i (range 0 pop-size)] 
      (let [value ((:mutator mutator) width height member-size)]    
        (atom (struct member nil value)))))