(ns optcase.core
  (:gen-class :main true)
  (:refer-clojure :exclude [use import])
  (:require [scad-clj.scad :refer :all]
            [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [clojure.math.numeric-tower :refer :all]
            [optcase.attachwithvectors :refer :all]))


(spit "things/post-demo.scad" ;cleans file
     nil )

(declare retr)
(declare mount-hole-height)
(declare mount-hole-width)
(declare plate-thickness)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some Config
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(def keywidthForSpacing 	14)
(def keySpacing 			5.05)
(def arrXWid				7 )
(def arrYLen				4 )

(def plate-thickness 4)
(def dsa-length 18.25)
(def dsa-double-length 37.5)
(def heightbaseofkeycap 6.1 )

(def edgeheight 15)
(def basethickness 30) ;if you want a curved base, set this to the same as desired basethickness
(def desiredbasethickness 4)
(def overlap	2)

(def leftedgepadding 5)
(def rightedgepadding 5)
(def topedgepadding 5)
(def bottedgepadding 5)

(def mount-hole-width 14)
(def mount-hole-height 14)
(def spacminussize (- keywidthForSpacing mount-hole-height))

(def existencearray [
					
					[false true true true  true  true  true ] 
					[false true true true true  true true ] 
					[false true true true  true  true true ] 
					[true  true true true  true  true true ] ;as seem from origin looking in pos x, pos y
					])

(def keycaparray 	[
					
					[1 1 1 1 1 1 1 ] 
					[1 1 1 1 1 1 1 ] 
					[1 1 1 1 1 1 1 ] 
					[1 1 1 1 1 1 1 ] ;as seem from origin looking in pos x, pos y
					])

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Some handy functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defmacro make-fn [m] 
	"This is used to turn macros like and / or into fn"
  `(fn [& args#] 
    (eval `(~'~m ~@args#))))

(defn average [numbers] (/ (apply + numbers) (count numbers)))

(defn averageofcoord [alternative & more]
	(if (nil? more) alternative
		[
			(average (map first more))
			(average (map second more))
			(average (map last more))
		]))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that initialise the array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn createarray [x y] ;x is across, y is down
	"This makes only the starting array. It will be modified by writingArrayFunctions.
	Simple 2D vector with each element being a map of some key data (anyone get the 'key' pun?)."
	(vec(for [ycoin (range y)]
		(vec (for [xcoin (range x)]
			{:xPosInArr xcoin, 
			 :yPosInArr ycoin,
			 :cpntPos [ (* xcoin (+ keySpacing keywidthForSpacing)) (* ycoin (+ keySpacing keywidthForSpacing)) 0], 
			 :cpntVec [0 0 1],
			 :cpntAng 0}
	)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that read the array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn retr "Should only be used when certain that the key is inside of array. If unsure, use smartretrPntData" 
	[arr x y] ((arr y) x)) 

(declare smartretrPntDataY) ;need to declare smartretrPntDataY because it is referenced before declaration

(defn smartretrPntData [arr xcoin ycoin]
	"Because the webbing loops through -1 to arrXWid (or arrYLen), you need to be careful not to go outside of the array.
	This function catches when you are at -1 or arrXWid and returns the position of a key if it were a continutation on the plane of a key inside the array.
	For instance, if x is -1, it gets position of if x is 0, and moves it [(- 0 mount-hole-width keySpacing ) 0 0] along the plane described by the x = 0 key.
	Similarly if x = arrXWid, it gets the key with the same y value but x = arrXWid - 1 and continues it to the right by about 19mm. This is what the attachpoint 
	can do. Once x value is good, it sorts out Y value in a similar way. 
	There are recursive calls because if the key is at (-1, -1), it will do:
		(-1, -1)
	=	(0, -1) but shifted leftwards in the plane of key (0, -1)
	=   (0,  0) but shifted leftwards in the plane of key (0, -1) but shifted downwards in the plane of key (0, 0)

	The reason for trying to place keys outside the array onto the plane of keys inside of the array, is so that the edges in the keyplate are perpendicular with 
	the keyswitches.

	NOTE: this will only handle when called either just before or just after a list. E.g. if you ask when xcoin = -2, you get an error but -1 is good.
	"
	(cond 
		(= xcoin -1)
			(let [ referencepnt		(smartretrPntData arr 0 ycoin)]
				(assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [(- 0 keywidthForSpacing keySpacing ) 0 0])))
		
		(= xcoin arrXWid)
			(let [referencepnt		(smartretrPntData arr (dec arrXWid) ycoin)]
				(assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [(+ keywidthForSpacing keySpacing ) 0 0])))

		:else
			(smartretrPntDataY arr xcoin ycoin)
		))

(defn smartretrPntDataY [arr xcoin ycoin]
	(cond 
		(= -1 ycoin) 
			(let [
				referencepnt		(smartretrPntData arr xcoin 0 )
				](assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [0 (- 0 mount-hole-width keySpacing ) 0])))

		(= arrYLen ycoin)
			(let [
				referencepnt		(smartretrPntData arr xcoin (dec arrYLen))
				](assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [0 (+ mount-hole-width keySpacing ) 0])))
									
		:else
			(retr arr xcoin ycoin))
	)	

;;;;;;;
;;; Keycaps + Keyswitch

(def dsa-cap 
	{		1 (let [bl2 (/ 18.5 2)
                     m (/ 18 2)
                     key-cap (hull (->> (polygon [[bl2 bl2] [bl2 (- bl2)] [(- bl2) (- bl2)] [(- bl2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[m m] [m (- m)] [(- m) (- m)] [(- m) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 1.9]))
                                   (->> (polygon [[6 6] [6 -6] [-6 -6] [-6 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 7.9])))]
                 (->> key-cap
                      (translate [0 0 heightbaseofkeycap])
                      (color [220/255 163/255 163/255 1])))
             2 (let [bl2 (/ dsa-double-length 2)
                     bw2 (/ 18.25 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[6 16] [6 -16] [-6 -16] [-6 16]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 12])))]
                 (->> key-cap
                      (translate [0 0 (+ 5 plate-thickness)])
                      (color [127/255 159/255 127/255 1])))
            1.5 (let [bl2 (/ 18.5 2)
            		 bw2 (/ 28 2)
                     m (/ 18 2)
                     mw (/ 27.5 2)
                     key-cap (hull (->> (polygon [[bw2 bl2] [bw2 (- bl2)] [(- bw2) (- bl2)] [(- bw2) bl2]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 0.05]))
                                   (->> (polygon [[mw m] [mw (- m)] [(- mw) (- m)] [(- mw) m]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 1.9]))
                                   (->> (polygon [[11 6] [11 -6] [-11 -6] [-11 6]])
                                        (extrude-linear {:height 0.1 :twist 0 :convexity 0})
                                        (translate [0 0 7.9])))]
                 (->> key-cap
                      (translate [0 0 heightbaseofkeycap])
                      (color [0 0.51 0.24 1])))})
(def keyswitch 
	(let [
		hw (/ 15.6 2)
		points 	[
					[hw hw 0] [hw (- hw) 0] [(- hw) (- hw) 0] [(- hw) hw 0]
					[hw hw 1] [hw (- hw) 1] [(- hw) (- hw) 1] [(- hw) hw 1]
					[(- hw 2) (- hw 2) -5] [(- hw 2) (- 0 hw -2) -5] [(- 0 hw -2) (- 0 hw -2) -5] [(- 0 hw -2) (- hw 2) -5]
					[(- hw 3) (- hw 3) 6.6] [(- hw 3) (- 0 hw -3) 6.6] [(- 0 hw -3) (- 0 hw -3) 6.6] [(- 0 hw -3) (- hw 3) 6.6]
					]

		faces	[
				;	[3 2 1 0] 
				;	[4 5 6 7]
					[0 1 5 4]
					[2 3 7 6]
					[1 2 6 5]
					[3 0 4 7]
					[11 10 9 8]
					[12 13 14 15]
					[8 9 1 0]
					[9 10 2 1]
					[10 11 3 2]
					[11 8 0 3]

					[4 5 13 12]
					[5 6 14 13]
					[6 7 15 14]
					[7 4 12 15]

					]
		]
	(union
	(polyhedron points faces)
	(translate [0 0 1] (cylinder (/ 3.30 2) 18.5))
	)))

(defn showkeycaps [arr]
		(for [ycoin (range arrYLen) xcoin (range arrXWid)]
			(let [
			pntData 	(retr arr xcoin ycoin)
			cpntP 		(:cpntPos pntData)
			cpntV 		(:cpntVec pntData)
			cpntA 		(:cpntAng pntData)
			keycapsize  (:keycapsize pntData)
				]
			(when (pntData :existence)
				(attach [cpntP cpntV cpntA]
						[[0 0 0] [0 0 1] 0]
						(union (dsa-cap keycapsize) keyswitch)
				)))))

(defn showsquareatkey [arr]
		(for [ycoin (range -1 (inc arrYLen)) xcoin (range -1 (inc arrXWid))]
			(let [
			pntData 	(smartretrPntData arr xcoin ycoin)
			cpntP 		(:cpntPos pntData)
			cpntV 		(:cpntVec pntData)
			cpntA 		(:cpntAng pntData)
			keycapsize  (:keycapsize pntData)
				]
			
				(attach [cpntP cpntV cpntA]
						[[0 0 1] [0 0 1] 0]
						(cube mount-hole-height mount-hole-width 2)
				))))

;;; Finished Keycaps + Keyswitch
;;;;;;;

;;;;;;;
;;;Making web

(def post-size 0.1)
(def web-post (->> (cube post-size post-size plate-thickness)
                   (translate [0 0 (/ plate-thickness -2)])))
(def edgepost (scale [1 1 3] web-post))


(def post-adj (/ post-size 2))
(def web-post-tr (translate [(- (/ mount-hole-width  2) post-adj) (- (/ mount-hole-height  2) post-adj) 0] web-post))
(def web-post-tl (translate [(+ (/ mount-hole-width -2) post-adj) (- (/ mount-hole-height  2) post-adj) 0] web-post))
(def web-post-bl (translate [(+ (/ mount-hole-width -2) post-adj) (+ (/ mount-hole-height -2) post-adj) 0] web-post))
(def web-post-br (translate [(- (/ mount-hole-width  2) post-adj) (+ (/ mount-hole-height -2) post-adj) 0] web-post))

(defn triangle-hulls [& shapes]
	"TBH I didn't write this, Adereth did. Its just a nice hulling function that makes 
	multiple hulls instead of one big hull. I guess hulls in sets of three shapes as 
	three points will always form a plane. This way the hulls will always be planes (flat)"
  (apply union
         (map (partial apply hull)
              (partition 3 1 shapes))))

(defn putupapost [arr xcoin ycoin pos callingfrom makingwhat callingto buildedgesornot plateorbase]
	(let [
		pntData (smartretrPntData arr xcoin ycoin)
		cpntP 		(:cpntPos pntData)
		cpntV 		(:cpntVec pntData)
		cpntA 		(:cpntAng pntData)
		exists		(:existence pntData)

		edge    ;this determines if the post should be an edge post or not.
			;This is easy if x or y is -1 or arrxwid or arrylen.
			;also easy if x or y is 0 and being called from the left or below. 
			;Remember that the webbing starts with one key and then awakens keys to the right or above it.
			;A littl trickier is if x or y is one less than their max. They have to be making columns or rows 
			;or diags and being called from either this one or from a suitable thing.
			;For instance, diagonals being called from below shouldn't be edged because this would create edge posts one key in from the actual edge.
			;Then, if any key in the currently forming web should be non existent, there needs to be an edge post.
			;For example, |n|c|, n is nonexistent, c is current key. That current key needs to look to the left and see that key exists. It doesn't, so put an edge post up.)
			;Also |c|n| would need to check to the right.
			;This is only two combinations for either row or column, but for diagonals, 4 different keys and each one needs to check 3 keys.
			 
			(or 
			    (= xcoin -1 ) 
				(= ycoin -1 ) 
				(= xcoin arrXWid) 
				(= ycoin arrYLen) 
				(and (= xcoin 0) (= callingfrom :callfromleft))
				(and (= xcoin 0) (= callingfrom :callfromleftbelow ))
				(and (= ycoin 0) (= callingfrom :callfrombelow))
				(and (= ycoin 0) (= callingfrom :callfromleftbelow ))

				(and (= ycoin (dec arrYLen)) (or    (and (= makingwhat :makingcolumns) (= callingfrom :callfromthisone))
													(and (= makingwhat :makingdiag) ( or (= callingfrom :callfromthisone) (= callingfrom :callfromleft)))))
				(and (= xcoin (dec arrXWid)) (or    (and (= makingwhat :makingrows) (= callingfrom :callfromthisone))
													(and (= makingwhat :makingdiag) (= callingfrom :callfromthisone))
													(and (= makingwhat :makingdiag) (= callingfrom :callfrombelow))))

				(let [
				neighbours 	(cond
								(= makingwhat :makingcolumns)
									(cond
										(= callingfrom :callfromthisone)
											[(smartretrPntData arr xcoin ycoin) (smartretrPntData arr xcoin (inc ycoin))]
										(= callingfrom :callfrombelow)
											[(smartretrPntData arr xcoin (dec ycoin)) (smartretrPntData arr xcoin ycoin)]
										)
								(= makingwhat :makingrows)
									(cond
										(= callingfrom :callfromthisone)
											[(smartretrPntData arr xcoin ycoin) (smartretrPntData arr (inc xcoin) ycoin)]
										(= callingfrom :callfromleft)
											[(smartretrPntData arr (dec xcoin) ycoin) (smartretrPntData arr xcoin ycoin)]
										)
								(= makingwhat :makingdiag)
									(cond
										(= callingfrom :callfromthisone)
											[(smartretrPntData arr xcoin ycoin)
											 (smartretrPntData arr (inc xcoin) ycoin)
											 (smartretrPntData arr xcoin (inc ycoin))
											 (smartretrPntData arr (inc xcoin) (inc ycoin))]
										(= callingfrom :callfromleft)
											[(smartretrPntData arr (dec xcoin) ycoin)
											 (smartretrPntData arr xcoin ycoin)
											 (smartretrPntData arr (dec xcoin) (inc ycoin))
											 (smartretrPntData arr xcoin (inc ycoin))]
										(= callingfrom :callfrombelow)
											[(smartretrPntData arr xcoin (dec ycoin))
											 (smartretrPntData arr (inc xcoin) (dec ycoin))
											 (smartretrPntData arr xcoin ycoin)
											 (smartretrPntData arr (inc xcoin) ycoin)]
										(= callingfrom :callfromleftbelow)
											[(smartretrPntData arr (dec xcoin) (dec ycoin))
											 (smartretrPntData arr xcoin (dec ycoin))
											 (smartretrPntData arr (dec xcoin) ycoin)
											 (smartretrPntData arr xcoin ycoin)]
										)
							)
					]
					
					(->>
						(map #(get %1 :existence) neighbours) 
						(map (make-fn not))
						(apply (make-fn or)))))
		
			

		xtrans (cond 
				(= xcoin -1) (- keySpacing leftedgepadding)
				(= xcoin arrXWid) (- rightedgepadding keySpacing)
				; (= exists false)
				; 	(cond 
				; 		(and (= :callfromleft callingfrom) (= :makingrows makingwhat))
				; 			(- 0 keySpacing (- leftedgepadding) spacminussize)
				; 		(and (= :callfromthisone callingfrom) (= :makingrows makingwhat))
				; 			(- keySpacing rightedgepadding (- spacminussize))
				; 		(and (not (or (= xcoin 0) (= xcoin (dec arrXWid)) )) ( or (= :callfromthisone callingfrom) (= :callfrombelow callingfrom)) (= :makingdiag makingwhat))
				; 			(- keySpacing rightedgepadding (- spacminussize))
				; 		(and (not (or (= xcoin 0) (= xcoin (dec arrXWid)) )) ( or (= :callfromleft callingfrom) (= :callfromleftbelow callingfrom)) (= :makingdiag makingwhat))
				; 			(- 0 keySpacing (- leftedgepadding) spacminussize)

				; 		:else
				; 			0
				; 		)
				:else
					0
				)

		ytrans (cond 
				(= ycoin -1) (- keySpacing bottedgepadding)
				(= ycoin arrYLen) (- topedgepadding keySpacing)
				; (= exists false) 
				; 	(cond
				; 		(and (= :callfrombelow callingfrom)   (= :makingcolumns makingwhat))
				; 			(- 0 keySpacing (- topedgepadding) spacminussize)
				; 		(and (= :callfromthisone callingfrom) (= :makingcolumns makingwhat))
				; 			(- keySpacing bottedgepadding (- spacminussize))
				; 		)	

				:else 0
				)

		ztrans (if (= plateorbase :base)
					(+ (+ overlap) (+ edgeheight) (- basethickness) -0.2)	
					0
			)

				
		
		post		(cond
						(= pos :tl) (partial web-post-tl)
						(= pos :bl) (partial web-post-bl)
						(= pos :tr) (partial web-post-tr)
						(= pos :br) (partial web-post-br)
						)
		post 		(if (and buildedgesornot edge) (resize [0 0 edgeheight   ] post) post)
		post 		(if (= plateorbase :base) 	   (resize [0 0 basethickness] post) post)
		post 		(if (and (= plateorbase :base) edge) (->> post
															(resize [0 0 (- basethickness overlap )])
															(translate [0 0  (- overlap )])
															) post)

		]
		(attach 
			[cpntP cpntV cpntA]
			[[0 0 0] [0 0 1] 0]
			(translate [xtrans ytrans ztrans] post)
			))
		)

(defn neigbhourtoexistence? [arr xcoin ycoin buildingwhat]

	(let [existenceofthisone 			( (smartretrPntData arr xcoin ycoin) :existence)
		  existenceofnextrow 			( (smartretrPntData arr xcoin (inc ycoin)) :existence)
		  existenceofnextcol 			( (smartretrPntData arr (inc xcoin) ycoin) :existence)
		  existenceofnextcolandrow		( (smartretrPntData arr (inc xcoin) (inc ycoin)) :existence)]
		  (cond 
			(= buildingwhat :buildingcolumnconnects)
				(or existenceofthisone existenceofnextrow)
			(= buildingwhat :buildingrowsconnects)
				(or existenceofthisone existenceofnextcol)
			(= buildingwhat :buildingdiagonalsconnects)
				(or existenceofthisone existenceofnextcolandrow existenceofnextrow existenceofnextcol)
			)
		))

(defn makeconnectors [arr plateorbase] 
	"Creates posts at the corner of each key switch and hulls them with the posts on other keycaps.
	The edges and corners are created by going one less than the array and one more than the array.
	When the variable is out of bounds, it is caught in the smartretrPntData. If the key doesn't exist 
	it might still be used because it is involved in making and edge or corner for a key that does exist.
	This is dealt with in neigbhourtoexistence."
	(let [
		buildedges 	(cond
						(= plateorbase :plate) true
						(= plateorbase :base) false
					)
		]

	(apply union
		(concat
			;Row connectors
			(for [ycoin (range arrYLen)]
				(color [1 (rand) 1 1] 
				(for  [xcoin (range -1  arrXWid)]
				(when (neigbhourtoexistence? arr xcoin ycoin :buildingrowsconnects)
					(triangle-hulls
						(putupapost arr xcoin       ycoin :tr :callfromthisone :makingrows :here buildedges plateorbase)
						(putupapost arr xcoin       ycoin :br :callfromthisone :makingrows :here buildedges plateorbase)
						(putupapost arr (inc xcoin) ycoin :tl :callfromleft    :makingrows :right buildedges plateorbase)
						(putupapost arr (inc xcoin) ycoin :bl :callfromleft    :makingrows :right buildedges plateorbase)
						)
					))))

			;Columns connectors
			(for [ycoin (range -1 arrYLen)]
				(color [(rand) 1 1 1] 
				(for [xcoin (range arrXWid)]
				(when (neigbhourtoexistence? arr xcoin ycoin :buildingcolumnconnects) 
					(triangle-hulls
						(putupapost arr xcoin       ycoin :tr :callfromthisone :makingcolumns :here buildedges plateorbase)
						(putupapost arr xcoin (inc ycoin) :br :callfrombelow   :makingcolumns :above buildedges plateorbase)
						(putupapost arr xcoin       ycoin :tl :callfromthisone :makingcolumns :here buildedges plateorbase)
						(putupapost arr xcoin (inc ycoin) :bl :callfrombelow   :makingcolumns :above buildedges plateorbase)
						)
					))))

			;Diagonal connectors
			(for [ycoin (range -1 arrYLen)]
				(color [0.2 0.2 (rand) 1] 
				(for [xcoin (range -1 arrXWid)]
				(when (neigbhourtoexistence? arr xcoin ycoin :buildingdiagonalsconnects)
					(triangle-hulls
						(putupapost arr xcoin       ycoin       :tr :callfromthisone :makingdiag :here buildedges plateorbase)
						(putupapost arr xcoin       (inc ycoin) :br :callfrombelow   :makingdiag :above buildedges plateorbase)
						(putupapost arr (inc xcoin) ycoin       :tl :callfromleft    :makingdiag :right buildedges plateorbase)
						(putupapost arr (inc xcoin) (inc ycoin) :bl :callfromleftbelow :makingdiag :aboveleft buildedges plateorbase)
						)
					))))

			;key connector for base
			(when (= plateorbase :base)
				(for [ycoin (range arrYLen)]
				(color [0.2 0.2 (rand) 1] 
				(for [xcoin (range arrXWid)]
				(when ((retr arr xcoin ycoin) :existence)
					(hull
						(putupapost arr xcoin ycoin :tr :callfromthisone :makingkeycentre :here buildedges plateorbase)
						(putupapost arr xcoin ycoin :br :callfromthisone :makingkeycentre :here buildedges plateorbase)
						(putupapost arr xcoin ycoin :tl :callfromthisone :makingkeycentre :here buildedges plateorbase)
						(putupapost arr xcoin ycoin :bl :callfromthisone :makingkeycentre :here buildedges plateorbase)
						)
					))))
				)

			))))

;;;Finished of making web
;;;;;;;

(defn showconnectors [arr]
		(for [ycoin (range arrYLen) xcoin (range arrXWid)]
			(let [
			pntData 	(retr arr xcoin ycoin)
			cpntP 		(:cpntPos pntData)
			cpntV 		(:cpntVec pntData)
			cpntA 		(:cpntAng pntData)
				]

			(connector [cpntP cpntV cpntA]
				))))

(defn makelegs [arr]
	(let [
		topright ((retr arr (- arrXWid 1) (- arrYLen 1)) :cpntPos)
		topleft ((retr arr 2 (- arrYLen 1)) :cpntPos)
		bottomright ((retr arr (- arrXWid 1) 1) :cpntPos)
		bottomleft ((retr arr 0 0) :cpntPos)
		]
		(difference
			(union
				(translate [(+ (topright    0) 0) (- (topright    1) -5) -41] (cylinder 5 60))
				(translate [(+ (topleft     0) -5  ) (- (topleft     1) -10) -27] (cylinder 5 60))
				(translate [(- (bottomright 0) -2 ) (- (bottomright 1) 25) -38] (cylinder 5 60))
				(translate [(+ (bottomleft  0) 12  ) (- (bottomleft  1) -3) -24] (cylinder 5 60))
			)
			(translate [0 0 -77] (cube 200 200 100))
	)))

(defn usbcutouts [positiveornegativeshape]
	(let [
		internalwidth 		28
		wallwdith 			4
		internalheight	 	7
		depth				13]

	
			(->>
			(case positiveornegativeshape
				:positive 
					(hull
						(translate [(- 0 (/ internalwidth 2) (/ wallwdith 2)) 0 0] (cube wallwdith depth internalheight)) 
						(translate [(+   (/ internalwidth 2) (/ wallwdith 2)) 0 0] (cube wallwdith depth internalheight))
						(translate [0 -2.5 (- 3)] (cube internalwidth depth internalheight)))
				:negative
					(translate [0 0 4] (cube (inc internalwidth) 40 10)))
					
			(translate [0 30 -12])
			(rotate -0.2 [1 0 0])
		
		))
	)

(defn sidenub []
	(->> (cylinder 0.6 2.75)
  		 (with-fn 30)
		 (rotate (/ π 2) [1 0 0])
		 (translate [(+ (/ mount-hole-width 2)) 0 1])
		 (hull (->> (cube 0.01 2.75 plate-thickness)
			         (translate [(+ (/ 0.01 2) (/ mount-hole-width 2))
			                     0
			                     (/ plate-thickness 2)])))
		 ))

(defn makesidenubs [arr]
	(for [ycoin (range arrYLen) xcoin (range arrXWid)]
		(let [
			pntData (retr arr xcoin ycoin)
			cpntP 		(:cpntPos pntData)
			cpntV 		(:cpntVec pntData)
			cpntA 		(:cpntAng pntData)
			exist 		(:existence pntData)
			]
			(when exist
				(union
				(attach
					[cpntP cpntV cpntA]
					[[0 0 plate-thickness] [0 0 1] 0]
					(sidenub))

				(attach
					[cpntP cpntV cpntA]
					[[0 0 plate-thickness] [0 0 1] 0]
					(rotate Math/PI [0 0 1] (sidenub)))
				))
	)))
	
(defn promicro [height width length positiveornegativeshape arr]
	(let [
		maxwallthick		3
		midwallthick 		2
		ihw				(/ width 2) ;internal halfwidth
		ihl 			(/ height 2) 

		shape 			(case positiveornegativeshape
							:pos
							(union
								(hull
									(translate [0 0 (- maxwallthick)] 		 (cube (+ width midwallthick) (+ midwallthick length) 0.01))
									(translate [0 0 (+ height midwallthick)] (cube (+ width (* 2 maxwallthick) 20) (+ length (* 2 maxwallthick)) 0.01))
									))
							:neg
							(union
								(hull
									(cube width length 0.01)
									(translate [0 0 height] 				   (cube (+ width midwallthick) (+ length midwallthick) 0.01))
									(translate [0 0 (+ 4 height midwallthick)] (cube (+ width midwallthick) (+ length midwallthick) 0.01))
									)
							(translate [0 10 0]
								(hull
									(cube 8.5 length 0.01)
									(translate [0 0 height] (cube 8.5 length 0.01)))))
						
						)

		keyforattachment (retr arr 5 2 )

		]

		(->> shape
			; (rotate  -0.07 [1 0 0])
			; (rotate  0.04 [0 1 0])
			(translate [0 13 -19])
			(attach [(:cpntPos keyforattachment) (:cpntVec keyforattachment) 0 ] [[0 0 0] [0 0 1] 0])
			)
		
		;(prn (:cpntPos keyforattachment))
		)
	)

(defn microusb [height width length positiveornegativeshape arr]
	(let [
		maxwallthick		2
		midwallthick 		2
		ihw				(/ width 2) ;internal halfwidth
		ihl 			(/ height 2) 

		shape 			(case positiveornegativeshape
							:pos
							(union
								(hull
									(translate [0 0 (- maxwallthick)] 		 (cube (+ width midwallthick) (+ midwallthick length) 0.01))
									(translate [0 0 (+ height midwallthick)] (cube (+ width (* 2 maxwallthick) 10) (+ length (* 2 maxwallthick)) 0.01))
									))
							:neg
							(union
								(hull
									(cube width length 0.01)
									(translate [0 0 height] 				   (cube (+ width midwallthick) (+ length midwallthick) 0.01))
									(translate [0 0 (+ 10 height midwallthick)] (cube (+ width midwallthick) (+ length midwallthick) 0.01))
									)
							(translate [0 10 0]
								(hull
									(cube 8.5 length 0.01)
									(translate [0 0 height] (cube 8.5 length 0.01)))))
						
						)

		keyforattachment (retr arr 2 3 )

		]

		(->> shape
			; (rotate  -0.4 [1 0 0])
			; (rotate  0.04 [0 1 0])
			 (translate [0 2 -21])
			(attach [(:cpntPos keyforattachment) (:cpntVec keyforattachment) 0 ] [[0 0 0] [0 0 1] 0])
			)
		
		;(prn (:cpntPos keyforattachment))
		)
	)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Functions that write the array
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defn moveonXYZ  [arr colOrRow points xmove ymove zmove]
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xPosInArr pntData)
				yval  		(:yPosInArr pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				exist 		(:existence pntData)
				condition 	(case colOrRow
								:row (= points yval)
								:col (= points xval)
								:colrow (and (= (points 0) xval) (= (points 1) yval))
								:all true)
				]
				
				;(prn xcoin ycoin condition)
				{:xPosInArr xval, 
				 :yPosInArr yval,
				 :cpntPos (if condition
				 				[ (+ (cpntP 0) xmove) (+ (cpntP 1) ymove) (+ (cpntP 2) zmove)]
				 				cpntP)
				 :cpntVec cpntV,
				 :cpntAng cpntA
				 :existence exist}
				

				))))))

(defn centrearray [arr]
	(let [
		xcoords  (for [ycoin arr pntData ycoin] 
					((pntData :cpntPos) 0) 
					)
		ycoords  (for [ycoin arr pntData ycoin] 
					((pntData :cpntPos) 1) 
					)
		minx 	(apply min xcoords)
		miny 	(apply min ycoords)
		maxx 	(apply max xcoords)
		maxy 	(apply max ycoords)

		halfrangex (/ (- maxx minx) 2)
		halfrangey (/ (- maxy miny) 2)]

		
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xPosInArr pntData) ; coordinate in array, not coord in 3d
				yval  		(:yPosInArr pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				]
				{:xPosInArr xval, 
				 :yPosInArr yval,
				 :cpntPos [(+ (- (cpntP 0) maxx) halfrangex)
				           (+ (- (cpntP 1) maxy) halfrangey) 
				           (cpntP 2)] , 
				 :cpntVec cpntV,  
				 :cpntAng cpntA}
	)))))))

(defn apply3dequation [arr fxy fpartialx fpartialy xmulti ymulti zmulti]
	"this is a general function. It takes fxy and makes z into fxy. 
	fxy uses xmulti and ymulti to change the scale of the 3d plot. 
	It does not change the actual coordinates as this would interfere
	with switch placement. zmulti is applied to the z coordinates 
	after the function has been applied to get the scale correct.
	fpartialx is the partial derivative of fxy with respect to x.
	fpartialy is the partial derivative of fxy with respect to y.

	The point of including the fpartials is to orientate the switches
	along the normal vector at that point in the equation so it looks 
	like a gradual curve.

	It returns an array like the other array writingArrayFunctions."
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xPosInArr pntData)
				yval  		(:yPosInArr pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				

				newPos 		[
							(cpntP 0) 
							(cpntP 1) 
							(+ (cpntP 2) (* (fxy (* (cpntP 0) xmulti) (* (cpntP 1) ymulti)) zmulti)) ]


				newVec 		[
							(- (* (fpartialx (* (cpntP 0) xmulti) (* (cpntP 1) ymulti)) zmulti) ) 
							(- (* (fpartialy (* (cpntP 1) ymulti) (* (cpntP 0) xmulti)) zmulti))
							1 ]
				]
				

				{:xPosInArr xval, 
				 :yPosInArr yval,
				 :cpntPos newPos, 
				 :cpntVec newVec,
				 :cpntAng cpntA}

		)

	))))
	)

(defn curveit [arr]
	(let [
		arguments [arr (partial #(* (+ (* (expt %1 2) 0.000000025) ( expt %2 2) ) 0.003))

					
					(partial #(+ (* %1 2 0.008 0.000000025)  (* %2 0) ))
					(partial #(+ (* %1 2 0.003)     (* %2 0) ))	
					
					1
					1
					1 ]]

	(apply apply3dequation arguments)))

(defn keyExistence [arr]
	(vec (for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(assoc (retr arr xcoin ycoin) :existence (get-in existencearray [(- (dec arrYLen) ycoin) xcoin]))
	)))))

(defn changeKeyCapSize [arr]
	(vec (for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(assoc (retr arr xcoin ycoin) :keycapsize (get-in keycaparray [(- (dec arrYLen) ycoin) xcoin]))
	)))))

(defn alignkeys [arr & more]
	"Use this if you want to keys to share the same allignment.
	For instance, if you print a 2u key that has two stems, it can sit on two 1u keys that are alligned"
	(let [
	 	vecofkeys 		(nth more 0)
	 	movingkey 		(nth vecofkeys 0)

 		direction		(vecofkeys 2)

	 	anchorkey		(retr arr (get-in vecofkeys [1 0]) (get-in vecofkeys [1 1]))
	 	anchkeypos		(anchorkey :cpntPos)
	 	anchkeyvec		(anchorkey :cpntVec)
	 	anchkeyang		(anchorkey :cpntAng)

	 	u 				(unitv anchkeyvec)
	 	a 				(u 0)
	 	b 				(u 1)
	 	c 				(u 2)
	 	d 				(modofvec [0 b c])

	 	ConD 			(/ c d)
	 	BonD 			(/ b d)
		
	 	startingpnt 	(case direction	
	 						:ontheleft  [-19 0 0]
	 						:ontheright [19  0 0]
	 						)

	 	yaxisinv    	[
	 					 (+ (* (startingpnt 0) d) (* (startingpnt 2) a))
	 					 (startingpnt 1)
	 					 (- (* (startingpnt 2) d) (* (startingpnt 0) a))
	 					]

	 	xaxisinv   		[
	 					 (yaxisinv 0)
	 					 (+ (* (yaxisinv 1) ConD) (* (yaxisinv 2) BonD))
	 					 (- (* (yaxisinv 2) ConD) (* (yaxisinv 1) BonD))
	 					]
        
	 	finalpos		[
	 					(+ (xaxisinv 0) (anchkeypos 0))
	 					(+ (xaxisinv 1) (anchkeypos 1))
	 					(+ (xaxisinv 2) (anchkeypos 2))
	 					]

  	    updatedpos 		(assoc-in arr [ (movingkey 1) (movingkey 0) :cpntPos ] finalpos)


		updatedkey 		(assoc-in updatedpos [(movingkey 1) (movingkey 0) :cpntVec] anchkeyvec)
 

		]

		updatedkey
		)
	)

(defn angleKey [arr colOrRow points angle axis]
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xPosInArr pntData)
				yval  		(:yPosInArr pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				condition 	(case colOrRow
								:row (= points yval)
								:col (= points xval)
								:colrow (and (= (points 0) xval) (= (points 1) yval)))
				]
				

				{:xPosInArr xval, 
				 :yPosInArr yval,
				 :cpntPos cpntP,
				 :cpntVec (if condition 
				 				(case axis
						 			:xaxis [
						 			(cpntV 0)
						 			(- (* (cpntV 1)  (Math/cos angle)) (* (cpntV 2) (Math/sin angle)))
						 			(+ (* (cpntV 1)  (Math/sin angle)) (* (cpntV 2) (Math/cos angle)))
						 			]
						 			:yaxis [
						 			(+ (* (cpntV 2)  (Math/sin angle)) (* (cpntV 1) (Math/cos angle)))
						 			(cpntV 1)
						 			(- (* (cpntV 2)  (Math/cos angle)) (* (cpntV 1) (Math/sin angle)))
						 			]
						 			:zaxis [
						 			(- (* (cpntV 0)  (Math/cos angle)) (* (cpntV 1) (Math/sin angle)))
						 			(+ (* (cpntV 0)  (Math/sin angle)) (* (cpntV 1) (Math/cos angle)))
						 			(cpntV 2)
						 			]
						 			)
					 		cpntV)
					 		,
				 :cpntAng cpntA}
				

				)

			)
		))
	))

(defn rotateKey [arr colOrRow points angle axis]
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xPosInArr pntData)
				yval  		(:yPosInArr pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				condition 	(case colOrRow
								:row (= points yval)
								:col (= points xval)
								:colrow (and (= (points 0) xval) (= (points 1) yval)))
				]
				

				{:xPosInArr xval, 
				 :yPosInArr yval,
				 :cpntPos (if condition 
				 				(case axis
						 			:xaxis [
						 			(cpntP 0)
						 			(- (* (cpntP 1)  (Math/cos angle)) (* (cpntP 2) (Math/sin angle)))
						 			(+ (* (cpntP 1)  (Math/sin angle)) (* (cpntP 2) (Math/cos angle)))
						 			]
						 			:yaxis [
						 			(+ (* (cpntP 2)  (Math/sin angle)) (* (cpntP 1) (Math/cos angle)))
						 			(cpntP 1)
						 			(- (* (cpntP 2)  (Math/cos angle)) (* (cpntP 1) (Math/sin angle)))
						 			]
						 			:zaxis [
						 			(- (* (cpntP 0)  (Math/cos angle)) (* (cpntP 1) (Math/sin angle)))
						 			(+ (* (cpntP 0)  (Math/sin angle)) (* (cpntP 1) (Math/cos angle)))
						 			(cpntP 2)
						 			]
						 			)
					 		cpntP),
				 :cpntVec cpntV,
				 :cpntAng cpntA}
				

				)

			)
		))
	))


(defn rotateAttachpoint [arr colOrRow points angle ]
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xPosInArr pntData)
				yval  		(:yPosInArr pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				condition 	(case colOrRow
								:row (= points yval)
								:col (= points xval)
								:colrow (and (= (points 0) xval) (= (points 1) yval)))
				]
				

				{:xPosInArr xval, 
				 :yPosInArr yval,
				 :cpntPos cpntP
				 :cpntVec cpntV,
				 :cpntAng (if condition (+ cpntA angle) cpntA)}
				

				)

			)
		))
	))

(defn changeNonExistentKeys [arr]
	"This changes the position and vector of non existent keys so they minimally disturb the other keys.
	For instance imagine this arr [[T  T  T]
								   [T4 T3 T]
								   [T1 F  T2]] T is true existence, F is false existence and the middle column has been 
	lowered. This means that F has been lowered. The two T's either side of F would have their row connectors going down 
	to connect with an invisible key for no reason. This would not look good as imagin the row connector between T1 and F. 
	It would be a sharp edge which wouldn't be good structurally and visibly. Ideally the row connectors should connect to 
	a key that is on the same plane as the existing key so that the row connectors look like they are on the same plane as 
	the existing key. 
	This function looks at the 8 neighbours of the non existent key and finds the ideal position of F if it 
	were an extension in the plane described by the neighbour that its connecting to. For instance, T3 would give a position 
	thats exactly [0 (- 0 mount-hole-height keySpacing) 0] relative to T3 and on the plane described by T3. This is done for 
	every neighbour that is not outside of the array and is existent (we don't want to take into keys that don't exists because
	we never make connectors between two non existent keys). The weighted average (using weightingFilter) position of all of these ideal positions is used as the 
	new position for the non existent key. The average normal vector of all suitable neighbours is also used as the new plane for 
	the non existent key.
	I did try a function that used the ideal position according to the key that was making the connector with the non existent key.
	For instance, if T1 is making a row connector with F, the position of F will temporarily be [(+ 0 mount-hole-height keySpacing) 0 0] 
	relative to T1 and on the plane described by T1 (with vector T1). And if T3 were making a column with F, F will temporarily be [0 (- 0 mount-hole-height keySpacing) 0]
	relative to T3 and on the plane described by T3. This was bad as this left gaps between row connector for T1-F, diagonal connector for T4-F,
	and column connector for T3-F. This made all the connectors perfect continuations of the existent key. Thats how I got the idea 
	of averaging the ideal positions and vectors. This local ideal approach can be found commented out in retrforbasegoody."
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]

			(let [
				pntData (retr arr xcoin ycoin)
				existence (pntData :existence)
				]
				;(prn xcoin ycoin)
				(if (not existence)
					(let [weightingFilter 	[1 2 1
											 2 0 2
											 1 2 1] ;weighting filter as column and row connectors are more important 
						 positions 			[
						 						[(+ 0 mount-hole-height keySpacing) (- 0 mount-hole-height keySpacing) 0] ;positions relative to tl, tm, tr, ml ...
						 						[0 									(- 0 mount-hole-height keySpacing) 0]
						 						[(- 0 mount-hole-height keySpacing) (- 0 mount-hole-height keySpacing) 0]
						 						[(+ 0 mount-hole-height keySpacing) 0 								   0]
						 						[0 									0 								   0]
						 						[(- 0 mount-hole-height keySpacing) 0 								   0]
						 						[(+ 0 mount-hole-height keySpacing) (+ 0 mount-hole-height keySpacing) 0]
						 						[0 									(+ 0 mount-hole-height keySpacing) 0]
						 						[(- 0 mount-hole-height keySpacing) (+ 0 mount-hole-height keySpacing) 0]

						 					]
						newcoord 			(->> 
												(for [ychange [1 0 -1] xchange [1 0 -1]] (try (get-in arr [(+ ycoin ychange) (- xcoin xchange)]) (catch Exception e nil)));gets the 8 neighbours
												(map-indexed (fn [idx itm] [idx itm])) ;this is used for keeping track of the positions (tl = 0, tm = 1, tr = 2...)
												;(map #(nil? (%1 1)))
												(remove #(nil? (%1 1) ) ) ;remove if it is nil (outside of the array)
												(remove #(not ((%1 1) :existence))) ;remove if it is a key that doesn't exist
												;(map #(prn ((%1 1) :xPosInArr) ((%1 1) :yPosInArr)))

												(map (fn [itm] (repeat (weightingFilter (itm 0)) (assoc (itm 1) :positions (itm 0))))) ;adds the key the number of times according to the weightingfilter
												(flatten)

												(map (fn [itm]  (attachpoint [(itm :cpntPos) (itm :cpntVec) (itm :cpntAng)] (positions (itm :positions))))) ;find the relative position if it were attached to a neighbouring key

												;#(try (apply averageofcoord %1) (catch Exception e (pntData :cpntPos)) );take the average
												;#(if (not (empty? %1)) (apply averageofcoord) [0 0 0] )
												(apply averageofcoord (pntData :cpntPos))
												)
						newvec 				(->> 
												(for [ychange [1 0 -1] xchange [1 0 -1]] (try (get-in arr [(+ ycoin ychange) (- xcoin xchange)]) (catch Exception e nil))) ;similar thing but for the average vector of the neighbouring keys
												(map-indexed (fn [idx itm] [idx itm]))
												;(map #(nil? (%1 1)))
												(remove #(nil? (%1 1) ) )
												(remove #(not ((%1 1) :existence)))

												(map (fn [itm] (repeat (weightingFilter (itm 0)) (assoc (itm 1) :positions (itm 0)))))
												(flatten)

												(map (fn [itm] (itm :cpntVec)))

												;#( try (apply averageofcoord %1) (catch Exception e (pntData :cpntVec)) )
												(apply averageofcoord (pntData :cpntVec))
												)
							]
						;(prn xcoin ycoin (pntData :cpntPos) newcoord)
						(assoc pntData :cpntPos newcoord :cpntVec newvec) ;updated the keys position with new pos and vec

						)
					(retr arr xcoin ycoin) ;if the key exists, just keep the same data.
					)
				)

			)))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Main Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defn writingArrayFunctions [arr & more]
	"These are the functions that rewrite the array. Each function takes an arr as input 
	and outputs a modified arr ready to be used as input by the next function. "
	(-> 
		(centrearray arr)
		(curveit)

		(rotateKey :col 2 0.06 :zaxis)
		(rotateKey :col 1 0.06 :zaxis)
		(rotateKey :col 0 0.06 :zaxis)
		(rotateAttachpoint :col 2 0.06 )
		(rotateAttachpoint :col 1 0.06 )
		(rotateAttachpoint :col 0 0.06 )
		

		(rotateKey :col 6 -0.06 :zaxis)
		(rotateKey :col 5 -0.06 :zaxis)
		(rotateAttachpoint :col 6 -0.06 )
		(rotateAttachpoint :col 5 -0.06 )

		(angleKey :row 0 0.2 :xaxis)

		(moveonXYZ :col 3 0 2 0)
		(moveonXYZ :col 5 2 -4 0)
		(moveonXYZ :col 6 2 -4 0)
		
		(moveonXYZ :row 0 0 0 -2)

		

		(moveonXYZ :colrow [1 0] -3 -5 0)
		(moveonXYZ :colrow [0 0] -4.5 -12 0)

		(rotateAttachpoint :colrow [1 0] 0.2)
		(rotateAttachpoint :colrow [0 0] 0.4)
		(rotateAttachpoint :colrow [0 1] 0.4)

		(moveonXYZ :colrow [4 0] 1 -4 0)
		(rotateAttachpoint :colrow [4 0] -0.06)

		(moveonXYZ :row 1 0 -1.5 0)
		(moveonXYZ :row 2 0 -0.5 0)

		(alignkeys [[3 0] [4 0] :ontheleft])
		(alignkeys [[2 0] [3 0] :ontheleft])
		;(alignkeys [[5 0] [4 0] :ontheright])
		;(alignkeys [[6 0] [5 0] :ontheright])
		(keyExistence)
		(changeNonExistentKeys)
		(changeKeyCapSize)



	))
	
(defn readingArrayFunctions [arr & more]
	"These functions only read the array and return OpenSCAD shapes hence the arr parameter being 
	passed to each of them individually (unlike the threading of the writingArrayFunctions)"
	;(rotate (/ Math/PI 15) [0 1 0]  
	(union 

		;;MAKING PLATE
		(union 
			(makeconnectors arr :plate)
			(makesidenubs arr)
			) 
			

		;;MAKING  BASE
		; (difference
		; 	(union
		; 		(makeconnectors arr :base)
		; 		(promicro 4.4 18 33.3 :pos arr)
		; 		(microusb 4 8.2 11.5 :pos arr))
		; 	(union
		; 		(promicro 4.4 18 33.3 :neg arr)
		; 		(microusb 4 8.2 11.5 :neg arr))
		; 	)
		;  (rotate (/ Math/PI -15) [0 1 0] (makelegs arr))

		; (difference 
		;  	(makeconnectors arr :base)
		; 	(translate [0 0 -45] 
		; 		(union
		; 			 (cube 200 200 50)
		; 		(->> "Hal Frigaard"
		; 		 	 (text "FontAwesomne" 10)
		; 		 	 (extrude-linear {:height 3})
		; 		 	 (translate [-10 -30 25]))
		; 		(->> "June-Sept-2017"
		; 			 (text "FontAwesomne" 10)
		; 			 (extrude-linear {:height 3})
		; 			 (translate [-10 -18 25]))
		; 		(->> "git.io/v5lM4"
		; 			 (text "FontAwesomne" 10)
		; 			 (extrude-linear {:height 3})
		; 			 (translate [-10 -4 25]))))
		;  	(promicro 4.4 18 33.3 :neg arr)
		;  	(microusb 3 8.2 11.5 :neg arr))


		

		; (->>
		; 		(cube 200 200 50)
		; 		(translate [0 0 -50] )
		; 		)
		
		(showkeycaps arr)
		;(showconnectors arr)
		;(showsquareatkey arr)

	));)

(defn buildarray []
	(-> (createarray arrXWid arrYLen) ;create the array to pass onto the transformation functions
		 	 (writingArrayFunctions)
		 	 (readingArrayFunctions) ;the outcome of this should be code for scad-clj
	))

(spit "things/post-demo.scad"
      (write-scad 
      	(->>
      	(buildarray))))
