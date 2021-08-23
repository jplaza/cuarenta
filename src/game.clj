(ns game
  (:require [clojure.spec.alpha :as s]
            [clojure.string :as string]
            [clojure.math.combinatorics :as combo]
            [clojure.tools.logging :as log]
            [clojure.pprint :as pprint]))

(def cards-ranks [1 2 3 4 5 6 7 :jack :queen :king])
(def suits #{:hearts :diamonds :spades :clubs})
(def actions #{:caida :limpia :levante :jugar :sumar})
(def points-on-score 2)
(def cards-to-deal 5)

(defn pp-str [x]
  (with-out-str (pprint/pprint x)))

(s/def ::player string?)
(s/def ::suit suits)
(s/def ::action actions)
(s/def ::rank (set cards-ranks))

(s/def ::card (s/keys :req-un [::suit ::rank]))
(s/def ::play-intent (s/keys :req-un [::player ::card]))
(s/def ::play (s/keys :req-un [::player ::card ::action]))

(def deck
  (for [suit suits
        rank cards-ranks]
   {:suit suit
    :rank rank}))

(defn new-game
  []
  {:players [{:username "player1"
              :name "Player 1"
              :score 0
              :picked-cards []
              :hand []}
             {:username "player2"
              :name "Player 2"
              :score 0
              :picked-cards []
              :hand []}]
   :table []
   :turn "player1"
   :deck (shuffle deck)
   :plays []})

(def game (atom (new-game)))

(defn par?
  "Computes if a caida action is possible"
  [{:keys [table]} play-intent]
  (let [match (->> table
                   (filter #(= (:rank %) (get-in play-intent [:card :rank])))
                   seq)]
    (when match
      {:action (if (= (last table) (first match)) :caida :levante)
       :cards-to-pick match})))

(defn suma?
  "Computes if a suma action is possible"
  [{:keys [table] :as _game} {:keys [card]}]
  (let [nums (->> table
                  (filter (comp number? :rank)))
        combos (combo/combinations nums 2)
        pairs (->> combos
                   (map (fn [pair] [(apply + (map :rank pair)) pair]))
                   (filter #(= (:rank card) (first %)))
                   (map last)
                   seq)]
    (mapv (fn [pair]
            {:action :levante
             :cards-to-pick pair})
          pairs)))

(defn compute-actions
  "Computes posible actions when a card is played"
  [game play-intent]
  (log/debug "compute actions:\n" (pp-str {:game game :intent play-intent}))
  (let [par (par? game play-intent)
        sum (suma? game play-intent)]
    (cond-> []
      par (conj par)
      sum (concat sum))))

(defn deal
  [game]
  (let [cards (take (* 2 cards-to-deal) (:deck game))
        [hand-1 hand-2] (partition cards-to-deal cards)]
    (-> game
        (update :deck (partial drop (* 2 cards-to-deal)))
        (assoc-in [:players 0 :hand] hand-1)
        (assoc-in [:players 1 :hand] hand-2))))

(defn- count-cards
  [{:keys [picked-cards] :as player}]
  (let [card-count (count picked-cards)
        points-earned (max 0 (- card-count 20))
        result (if (pos? points-earned)
                 0
                 (* 2 (int (Math/ceil (/ (+ points-earned 6) 2)))))]
    (-> player
        (update :score + result)
        (assoc :picked-cards []))))

(defn count-and-shuffle
  "Count cards each player has to determine if they should earn extra points before
  shuffling the deck again."
  [{:keys [players] :as game}]
  (let [updated (mapv count-cards players)]
    (-> game
        (assoc :players (mapv count-cards players))
        (assoc :deck (shuffle deck)))))

;; TODO:
;; - Función que calcula el fin del juego

(defn- next-turn
  [current-turn]
  (if (= "player1" current-turn)
    "player2"
    "player1"))

(defn- pick-cards
  [table cards-to-pick]
  (->> table
       (remove (set cards-to-pick))
       (into [])))

(defmulti make-play (fn [_game play] (:action play)))

(defmethod make-play :jugar
  [{:keys [players] :as game} {:keys [player card]}]
  (let [player-idx (->> players
                        (map-indexed vector)
                        (filter #(= player (:username (second %))))
                        (map first)
                        first)]
    (-> game
        (update :table conj card)
        (update-in [:players player-idx :hand] #(remove (set (vector card)) %))
        (update :turn next-turn))))

(defmethod make-play :levante
  [{:keys [players] :as game} {:keys [cards-to-pick player card]}]
  (let [player-idx (->> players
                        (map-indexed vector)
                        (filter #(= player (:username (second %))))
                        (map first)
                        first)
        cards-to-pick (conj cards-to-pick card)]
    (-> game
        (update :table pick-cards cards-to-pick)
        (update-in [:players player-idx :hand] #(remove (set (vector card)) %))
        (update-in [:players player-idx :picked-cards] #(apply conj % cards-to-pick))
        (update :turn next-turn))))

(defmethod make-play :caida
  [{:keys [players] :as game} {:keys [cards-to-pick player card]}]
  (let [player-idx (->> players
                        (map-indexed vector)
                        (filter #(= player (:username (second %))))
                        (map first)
                        first)
        cards-to-pick (conj cards-to-pick card)
        new-score (+ points-on-score (get-in players [player-idx :score]))]
    (-> game
        (update :turn next-turn)
        (update :table pick-cards cards-to-pick)
        (assoc-in [:players player-idx :score] new-score)
        (update-in [:players player-idx :picked-cards] #(apply conj % cards-to-pick))
        (update-in [:players player-idx :hand] #(remove (set (vector card)) %)))))

(defmethod make-play :caida-y-limpia
  [{:keys [cards-to-pick player]} {:keys [players] :as game}]
  (let [player-idx (->> players
                        (map-indexed vector)
                        (filter #(= player (:username (second %))))
                        (map first)
                        first)
        new-score (+ (* 2 points-on-score) (get-in players [player-idx :score]))]
    (-> game
        (update :table pick-cards cards-to-pick)
        (assoc-in [:players player-idx :score] new-score)
        (update-in [:players player-idx :picked-cards] #(apply conj % cards-to-pick)))))

(defn get-player
  [{:keys [players]} player-id]
  (let [idx (->> players
                 (map-indexed vector)
                 (filter #(= player-id (:username (second %))))
                 (map first)
                 first)]
    (nth players idx)))

(defn get-current-player
  [game]
  (get-player game (:turn game)))

(defn empty-hands?
  [{:keys [players]}]
  (empty? (apply concat (map :hand players))))

(defn out-of-cards?
  [{:keys [deck] :as game}]
  (and (empty? deck) (empty-hands? game)))

;; User interface

(defn translate-card-rank
  [rank]
  (condp = rank
    :jack "J"
    :queen "Q"
    :king "K"
    1 "A"
    (str rank)))

(defn translate-card-suit
  [suit]
  (get {;:hearts "corazones"
        :hearts "♥︎"
        ; :diamonds "diamantes"
        :diamonds "♦︎"
        ; :spades "corazón negro"
        :spades "♠︎"
        ; :clubs "trébol"
        :clubs "♣︎"}
       suit))

(defn card->text
  [{:keys [rank suit]}]
  (format "%s%s" (translate-card-rank rank) (translate-card-suit suit)))

(defmulti prompt-opts :action)

(defmethod prompt-opts :par
  [_]
  "llevar")

(defmethod prompt-opts :caida
  [_]
  "caída")

(defmethod prompt-opts :suma
  [{:keys [cards-to-pick]}]
  (format "llevar %s y %s"
          (card->text (first cards-to-pick))
          (card->text (last cards-to-pick))))

(defn pick-action
  [actions]
  (let [menu (format "Selecciona una opción:\n %s\n"
                     (->> (map prompt-opts actions)
                          (map-indexed (fn [i opt]
                                         (format "%d. %s" (inc i) opt)))
                          (string/join "\n")))
        _ (println menu)
        _ (flush)
        selection (dec (Integer/parseInt (read-line)))]
    (nth actions selection)))

(defn prompt-player
  [game]
  (let [hand (:hand (get-current-player game))
        menu (format "%s estas son tus cartas:\n\n%s\n\nSelecciona la carta a jugar: "
                     (:turn game)
                     (->> hand
                          (map-indexed (fn [i opt]
                                         (format "%d. - %s" (inc i)
                                                 (card->text opt))))
                          (string/join "\n")))
        _ (print menu)
        _ (flush)
        selection (dec (Integer/parseInt (read-line)))]
    {:card (nth hand selection)
     :player (:turn game)}))

(defn display-score
  [game]
  (println (format "**** MARCADOR ****\nplayer1: %d\t player2: %d"
                   (get-in game [:players 0 :score])
                   (get-in game [:players 1 :score]))))

(defn display-table
  [{:keys [table]}]
  (let [spaces (max 0 (dec (count table)))
        space-width 1
        card-count (count table)
        card-width 2
        h-padding 4
        title "MESA"
        width (+ (* card-count card-width) (* spaces space-width) h-padding)
        width-fix (if (odd? (+ (count title) width)) 1 0)
        width (+ width width-fix)
        cards (map card->text table)
        title-padding (/ (- width (count title)) 2)]
    ; (log/debug {:width width :fix width-fix})
    (println (string/join (concat ["\n"]
                                  (repeat title-padding " ")
                                  title
                                  (repeat title-padding ""))))
    (println (string/join (repeat width "=")))
    ; (println (str "|" (string/join (repeat (- width 2) " ")) "|"))
    (println (str "| " (string/join " " cards)
                  (string/join (repeat (+ 1 width-fix) " "))
                  "|"))
    ; (println (str "|" (string/join (repeat (- width 2) " ")) "|"))
    (println (string/join (repeat width "=")) "\n")))

(comment
  (display-table {:table (take 3 (shuffle deck))}))

(defn -main
  []
  (println "¡Bienvenidos! Repartiendo...")
  (swap! game deal)
  (loop [play-intent (prompt-player @game)]
    (let [actions (compute-actions @game play-intent)
          _ (log/debug "actions:\n" (pp-str actions))
          selected-action (cond
                            (empty? actions) (assoc play-intent :action :jugar)
                            (> (count actions) 1) (pick-action actions)
                            :else (merge play-intent (first actions)))]
      (log/debug {:selected-action selected-action})
      (swap! game make-play selected-action))
    (display-score @game)
    (display-table @game)
    (when (out-of-cards? @game)
      (println "¡Se terminó la baraja! Contando cartas levantadas...")
      (swap! game count-and-shuffle)
      (display-score @game))
    (when (empty-hands? @game)
      (println "¡Se terminó la mano! Repartiendo...")
      (swap! game deal))
    (recur (prompt-player @game))))

(comment
 (par? {:table [{:rank 3 :suit :hearts}
                {:rank 4 :suit :hearts}]}
       {:card {:rank 3 :suit :hearts}})

 (compute-actions {:table [{:rank 2 :suit :hearts}
                           {:rank 3 :suit :hearts}
                           {:rank 1 :suit :hearts}
                           {:rank 4 :suit :hearts}]}
                  {:card {:rank 4 :suit :hearts}})

 (display-table @game)

 (make-play {:players [{:username "player1"
                        :name "Player 1"
                        :score 0
                        :hand [{:rank 4 :suit :diamonds}]
                        :picked-cards #{}}
                       {:username "player2"
                        :name "Player 2"
                        :score 0
                        :hand []
                        :picked-cards #{}}]
             :turn "player1"
             :table [{:rank 2 :suit :hearts}
                     {:rank 3 :suit :hearts}
                     {:rank 1 :suit :hearts}
                     {:rank 4 :suit :hearts}]}
            {:card {:rank 4 :suit :diamonds}
             :cards-to-pick [{:rank 4 :suit :hearts}]
             :action :caida
             :player "player1"}))
