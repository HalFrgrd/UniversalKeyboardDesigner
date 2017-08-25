(ns optcase.attachwithvectors
	(:refer-clojure :exclude [use import])
	(:require [scad-clj.model :refer :all]
            [unicode-math.core :refer :all]
            [clojure.math.numeric-tower :refer :all]))

(def defaulDrawingResolution 6) ;low res for vector arrows

(defn sqr [x] ;square x
  (expt x 2))


(defn modofvec [[x y z]] ;modulus of vecotr
	(sqrt (+ (sqr x ) (sqr y) (sqr z) )))

(defn cross [[x1 y1 z1] [x2 y2 z2]] ;cross product
	(vector
		(- (* y1 z2) (* z1 y2))
  		(- (* z1 x2) (* x1 z2))
  		(- (* x1 y2) (* y1 x2))
  		))

(defn dot [[x1 y1 z1] [x2 y2 z2]] ;dot product
	 (+ (* x1 x2) (* y1 y2) (* z1 z2)))

(defn unitv [[x y z]] ;unit vecotr
	(vector 
		(/ x (modofvec [x y z]) )
		(/ y (modofvec [x y z]) ) 
		(/ z (modofvec [x y z]) )))


(defn anglev [[x1 y1 z1] [x2 y2 z2]] ;angle between vectors in radians
	(Math/acos (/ (dot [x1 y1 z1] [x2 y2 z2]) (* (modofvec [x1 y1 z1]) (modofvec [x2 y2 z2])))))

(defn point [p] ;make a small sphere at location p
	(->>(sphere 0.7)
		(translate p)
		(with-fn defaulDrawingResolution)))

(defn vectorz [l l_arrow mark]  ;make a vector in the z direction of length l, arrow length l_arrow, mark (true or false) to show angle
	(let [lb 	(- l l_arrow)]
		(union
		(translate [0 0 (/ lb 2)] 
			(union

				(->>(cylinder [1 0.2] l_arrow);draw tye arrow
					(translate [0 0 (/ lb 2)])
					(with-fn defaulDrawingResolution))

				(when mark
					(->>
						(cube 2 0.3 (* 0.8 l_arrow) )
						(translate [1 0 0])
						(translate [0 0 (+ (/ lb 2))])
						)
					)

				(->> (cylinder 0.5 lb)
					 (with-fn defaulDrawingResolution))
				)
			)
		(->> (sphere 1)
			 (with-fn defaulDrawingResolution))
		))

	)

(defn orientate 
	([v shape] (orientate v [0 0 1] 0 shape)) ;for default values

	([v vref roll shape]
	(let [
		raxis 		(cross vref v) 
		ang 		(anglev vref v)]

	(->> shape
		(rotate ang raxis)
		(rotate roll v)
		)
	)))

(defn drawingvector [v l l_arrow mark]
	(->>(vectorz l l_arrow mark)
		(orientate v)
		))
	
(defn connector [[u v ang]] ;u is position, v is vector, ang is rotation around vector
	
	(union
		(->> (point u)
			(color [0.5 1 1 1]))

		(->> (drawingvector v 8 2 true)
			(color [0.5 1 1 1])
			(rotate ang v)
			(translate u)
			)

		))

(defn attach [[pos1 v roll] [pos2 vref _] shape]
	(let [ 
		; calculation of the roll axis
		raxis 		(cross vref v)

		;calculate the angle between the vectors
		ang 		(anglev vref v)
		]

	(->> shape
		(translate (map #(- %1) pos2))
		(rotate ang raxis)
		(rotate roll v)
		(translate pos1)
		)

	))

(defn attachpoint [[pos vect ang] [x y z]]
	"Like the attach module except this returns the attached coordinates of a point instead of an attached shape.
	this is useful when you want to get the attached coordinates for things like polyhedron."
	(let [ 
		 
	 	u 				(unitv vect)
	 	a 				(u 0)
	 	b 				(u 1)
	 	c 				(u 2)
	 	d 				(modofvec [0 b c])

	 	ConD 			(/ c d)
	 	BonD 			(/ b d)
		

	 	yaxisinv    	[
	 					 (+ (* x d) (* z a))
	 					 y
	 					 (- (* z d) (* x a))
	 					]

	 	xaxisinv   		[
	 					 (yaxisinv 0)
	 					 (+ (* (yaxisinv 1) ConD) (* (yaxisinv 2) BonD))
	 					 (- (* (yaxisinv 2) ConD) (* (yaxisinv 1) BonD))
	 					]
        
	 	finalpos		[
	 					(+ (xaxisinv 0) (pos 0))
	 					(+ (xaxisinv 1) (pos 1))
	 					(+ (xaxisinv 2) (pos 2))
	 					]
	 	]
	 	finalpos
	))
