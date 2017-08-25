;Home of some functions that I wrote but either revised into a better function or no longer used.


(defn newbase [arr & more]
	
	;(base-dual-hulls
	;(concat
		(for [ycoin (range arrYLen) xcoin (range arrXWid)]

		(let [
			pntData 	(retr arr xcoin ycoin)
			cpntP 		(:cpntPos pntData)
			cpntV 		(:cpntVec pntData)
			cpntA 		(:cpntAng pntData)
			existence (:existence pntData)
			pntDataAbv 	(smartretrPntData arr xcoin (inc ycoin))
			cpntPAbv 		(:cpntPos pntDataAbv)
			cpntVAbv 		(:cpntVec pntDataAbv)
			cpntAAbv 		(:cpntAng pntDataAbv)
			existenceAbv (:existence pntDataAbv)
			pntDataRght 	(smartretrPntData arr (inc xcoin) ycoin)
			cpntPRght 		(:cpntPos pntDataRght)
			cpntVRght 		(:cpntVec pntDataRght)
			cpntARght 		(:cpntAng pntDataRght)
			existenceRght (:existence pntDataRght)
			pntDataRghtAbv 	(smartretrPntData arr (inc xcoin) (inc ycoin))
			cpntPRghtAbv 		(:cpntPos pntDataRghtAbv)
			cpntVRghtAbv 		(:cpntVec pntDataRghtAbv)
			cpntARghtAbv 		(:cpntAng pntDataRghtAbv)
			existenceRghtAbv (:existence pntDataRghtAbv)
			baseThickness 4
			depth		14
			]


			(union
			(when (and existence existenceAbv)
				(hull
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntP cpntV cpntA] [[0 0 depth] [0 0 1] 0]))
					(->>
					(cube 16 16 baseThickness)
					(color [1 0.5 0 1])
					(attach [cpntPAbv cpntVAbv cpntAAbv] [[0 0 depth] [0 0 1] 0]))
			
			))
			(when (and existence existenceRght)
				(hull
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntP cpntV cpntA] [[0 0 depth] [0 0 1] 0]))
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntPRght cpntVRght cpntARght] [[0 0 depth] [0 0 1] 0]))
			
			))
			(when (and existence existenceRghtAbv)
				(hull
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntP cpntV cpntA] [[0 0 depth] [0 0 1] 0]))
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntPRghtAbv cpntVRghtAbv cpntARghtAbv] [[0 0 depth] [0 0 1] 0]))
			
			))
			(when (and existenceAbv existenceRght)
				(hull
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntPAbv cpntVAbv cpntAAbv] [[0 0 depth] [0 0 1] 0]))
					(->>
					(cube 16 16 baseThickness)
					(attach [cpntPRght cpntVRght cpntARght] [[0 0 depth] [0 0 1] 0]))
			
			))


			)))
	);))

(defn makenewbase [arr]
	"This is a rather interesting way to make a base for the keyboard plate. It uses openscad polyhedrons, google it.
	I had some issues with using more than 20 polyhedrons and couldn't render my base and export as STL."
	(apply union
	(concat
	;(for [ycoin (range arrYLen) xcoin (range arrXWid)]
	(for [ycoin (range 0 arrYLen) xcoin [0 1 2 3 4 5 6 7]]
		(when ((retr arr xcoin ycoin) :existence)
			; (union
			; (hull
			; 	(centreofcomponentpost arr :diag xcoin ycoin :tl)
			; 	(centreofcomponentpost arr :col xcoin ycoin :tl)
			; 	(centreofcomponentpost arr :row xcoin ycoin :tl)
			; )
			; (hull
			; 	(centreofcomponentpost arr :diag xcoin ycoin :bl)
			; 	(centreofcomponentpost arr :col xcoin ycoin :bl)
			; 	(centreofcomponentpost arr :row xcoin ycoin :bl)
			; 	)
			; (hull
			; 	(centreofcomponentpost arr :diag xcoin ycoin :br)
			; 	(centreofcomponentpost arr :col xcoin ycoin :br)
			; 	(centreofcomponentpost arr :row xcoin ycoin :br)
			; 	)
			; (hull
			; 	(centreofcomponentpost arr :diag xcoin ycoin :tr)
			; 	(centreofcomponentpost arr :col xcoin ycoin :tr)
			; 	(centreofcomponentpost arr :row xcoin ycoin :tr)
			; 	)
			; (hull
			; 	(centreofcomponentpost arr :col xcoin ycoin :tl)
			; 	(centreofcomponentpost arr :col xcoin ycoin :tr)
			; 	(centreofcomponentpost arr :col xcoin ycoin :br)
			; 	(centreofcomponentpost arr :col xcoin ycoin :bl)

			; 	(centreofcomponentpost arr :row xcoin ycoin :tl)
				
			; 	(centreofcomponentpost arr :row xcoin ycoin :bl)
				
			; 	(centreofcomponentpost arr :row xcoin ycoin :br)
				
			; 	(centreofcomponentpost arr :row xcoin ycoin :tr)
			; 	))

			(let [
				points [
					(centreofcomponentpost arr :diag xcoin ycoin :tl :upper)
					(centreofcomponentpost arr :col  xcoin ycoin :tl :upper)
					(centreofcomponentpost arr :col  xcoin ycoin :tr :upper)
					(centreofcomponentpost arr :diag xcoin ycoin :tr :upper)
					(centreofcomponentpost arr :row  xcoin ycoin :tr :upper)
					(centreofcomponentpost arr :row  xcoin ycoin :br :upper)
					(centreofcomponentpost arr :diag xcoin ycoin :br :upper)
					(centreofcomponentpost arr :col  xcoin ycoin :br :upper)
					(centreofcomponentpost arr :col  xcoin ycoin :bl :upper)
					(centreofcomponentpost arr :diag xcoin ycoin :bl :upper)
					(centreofcomponentpost arr :row  xcoin ycoin :bl :upper)
					(centreofcomponentpost arr :row  xcoin ycoin :tl :upper)

					(centreofcomponentpost arr :diag xcoin ycoin :tl :lower)
					(centreofcomponentpost arr :col  xcoin ycoin :tl :lower)
					(centreofcomponentpost arr :col  xcoin ycoin :tr :lower)
					(centreofcomponentpost arr :diag xcoin ycoin :tr :lower)
					(centreofcomponentpost arr :row  xcoin ycoin :tr :lower)
					(centreofcomponentpost arr :row  xcoin ycoin :br :lower)
					(centreofcomponentpost arr :diag xcoin ycoin :br :lower)
					(centreofcomponentpost arr :col  xcoin ycoin :br :lower)
					(centreofcomponentpost arr :col  xcoin ycoin :bl :lower)
					(centreofcomponentpost arr :diag xcoin ycoin :bl :lower)
					(centreofcomponentpost arr :row  xcoin ycoin :bl :lower)
					(centreofcomponentpost arr :row  xcoin ycoin :tl :lower)

					(centreofcomponentpost arr :key  xcoin ycoin :centre :upper) ;24
					(centreofcomponentpost arr :key  xcoin ycoin :centre :lower) ;25

					]

				faces [
					; [0 1 2 3 4 5 6 7 8 9 10 11]
					; [23 22 21 20 19 18 17 16 15 14 13 12]
					[0 1 11] [2 3 4] [5 6 7] [8 9 10] [1 2 24] [2 4 24] [4 5 24] [5 7 24] [7 8 24] [8 10 24] [10 11 24] [11 1 24]
					[13 12 23] [16 15 14] [19 18 17] [22 21 20] [25 14 13] [25 16 14] [25 17 16] [25 19 17] [25 20 19] [25 22 20] [25 23 22] [25 13 23]
					[0 12 13]
					[1 0 13]
					[1 13 14]
					[1 14 2]
					[2 14 15]
					[2 15 3]
					[3 15 16]
					[3 16 4]
					[4 16 17]
					[4 17 5]
					[5 17 18]
					[5 18 6]
					[6 19 7]
					[6 18 19]
					[7 19 20]
					[7 20 8]
					[8 20 21]
					[8 21 9]
					[9 21 22]
					[9 22 10]
					[10 22 23]
					[10 23 11]
					[11 23 12]
					[11 12 0]
				]]
			
			;(translate [(* (rand) 10) (rand) (rand)]
			;(resize [20 20 3]
			;(union
			;(translate [(* 10 xcoin) 0 0]
			(polyhedron points faces ););)
				
		)))

		)))

(defn makenewbasewithextras [arr]
	(union
	(makenewbase arr)
	(difference
	(makelegs arr)

	(translate [0 0 1] (makenewbase arr)
	(translate [0 0 (* (- plate-thickness 0.5) 1)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 2)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 3)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 4)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 5)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 6)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 7)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 8)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 9)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 10)] (makenewbase arr))
	(translate [0 0 (* (- plate-thickness 0.5) 11)] (makenewbase arr))

	))))


(defn centreofcomponentpost [arr component xval yval pos uporlow]
	"mainly to be used with the polyhedron method of making a base"
	(let [
		currentPnt 		(smartretrPntData arr xval yval)
		halfwid 		(/ mount-hole-width 2) 			
		adjacentPnts	(cond 
							(= component :diag)	
							(cond 
								(= pos :tr) [(retrforbase arr xval (inc yval)) (retrforbase arr (inc xval) yval) (retrforbase arr (inc xval) (inc yval))]
								(= pos :br) [(retrforbase arr xval (dec yval)) (retrforbase arr (inc xval) yval) (retrforbase arr (inc xval) (dec yval))]
								(= pos :tl) [(retrforbase arr xval (inc yval)) (retrforbase arr (dec xval) yval) (retrforbase arr (dec xval) (inc yval))]
								(= pos :bl) [(retrforbase arr xval (dec yval)) (retrforbase arr (dec xval) yval) (retrforbase arr (dec xval) (dec yval))]
								)
							(= component :row)
							(cond 
								(= pos :tr) [(retrforbase arr (inc xval) yval)]
								(= pos :br) [(retrforbase arr (inc xval) yval)]
								(= pos :tl) [(retrforbase arr (dec xval) yval)]
								(= pos :bl) [(retrforbase arr (dec xval) yval)]
								)
							(= component :col)
							(cond 
								(= pos :tr) [(retrforbase arr xval (inc yval))]
								(= pos :br) [(retrforbase arr xval (dec yval))]
								(= pos :tl) [(retrforbase arr xval (inc yval))]
								(= pos :bl) [(retrforbase arr xval (dec yval))]
								)
							)

		corners 		(cond 
							(= component :diag)
							(cond 
								(= pos :tr) [[halfwid halfwid 0] [halfwid (- halfwid) 0] [( - halfwid) halfwid 0] [(- halfwid) (- halfwid) 0]]
								(= pos :br) [[halfwid (- halfwid) 0] [halfwid halfwid 0] [( - halfwid) (- halfwid) 0] [(- halfwid) halfwid 0]]
								(= pos :tl) [[(- halfwid) halfwid 0] [(- halfwid) (- halfwid) 0] [halfwid halfwid 0]  [halfwid (- halfwid) 0]]
								(= pos :bl) [[(- halfwid) (- halfwid) 0] [(- halfwid) halfwid 0] [halfwid (- halfwid) 0] [halfwid halfwid 0]]
								)
							(= component :row)
							(cond 
								(= pos :tr) [[halfwid halfwid 0] [(- halfwid) halfwid 0]]
								(= pos :br) [[halfwid (- halfwid) 0] [(- halfwid) (- halfwid) 0]]
								(= pos :tl) [[(- halfwid) halfwid 0] [halfwid halfwid 0]]
								(= pos :bl) [[(- halfwid) (- halfwid) 0] [halfwid (- halfwid) 0]]
							)
							(= component :col)
							(cond 
								(= pos :tr) [[halfwid halfwid 0] [halfwid (- halfwid) 0]]
								(= pos :br) [[halfwid (- halfwid) 0] [halfwid halfwid 0]]
								(= pos :tl) [[(- halfwid) halfwid 0] [(- halfwid) (- halfwid) 0]]
								(= pos :bl) [[(- halfwid) (- halfwid) 0] [(- halfwid) halfwid 0]]
							)
						)
							
		attachedcorners (cond 
					(= component :diag)
						[(attachpoint [(currentPnt :cpntPos) (currentPnt :cpntVec) (currentPnt :cpntAng)] (corners 0))
						(attachpoint [((adjacentPnts 0) :cpntPos) ((adjacentPnts 0) :cpntVec) ((adjacentPnts 0) :cpntAng)] (corners 1))
						(attachpoint [((adjacentPnts 1) :cpntPos) ((adjacentPnts 1) :cpntVec) ((adjacentPnts 1) :cpntAng)] (corners 2))
						(attachpoint [((adjacentPnts 2) :cpntPos) ((adjacentPnts 2) :cpntVec) ((adjacentPnts 2) :cpntAng)] (corners 3))]
					(or (= component :row ) (= component :col))
						[(attachpoint [(currentPnt :cpntPos) (currentPnt :cpntVec) (currentPnt :cpntAng)] (corners 0))
						(attachpoint [((adjacentPnts 0) :cpntPos) ((adjacentPnts 0) :cpntVec) ((adjacentPnts 0) :cpntAng)] (corners 1))]
					(= component :key)
						[(currentPnt :cpntPos)]
						
					)


		cornersaveraged (cond
							(= component :key)
								(currentPnt :cpntPos)
							:else
								(apply averageofcoord attachedcorners)
								)
		averagevec 		(cond 
							(= component :diag)
								(averageofcoord (currentPnt :cpntVec) ((adjacentPnts 0) :cpntVec) ((adjacentPnts 1) :cpntVec) ((adjacentPnts 2) :cpntVec))
							(or (= component :row) (= component :col))
								(averageofcoord (currentPnt :cpntVec) ((adjacentPnts 0) :cpntVec))
							(= component :key)
								(currentPnt :cpntVec)
						)
		post 			(cube 0.1 0.1 3)	
		depthbeneath 	8.1
		offset			-0.1
		slightrans 		(case pos
							:br [(- offset) offset 0]
							:bl [offset offset 0]
							:tr [(- offset) (- offset) 0]
							:tl [offset (- offset) 0]
							:centre [0 0 0]
							)
		; slightrans 		[0 0 0]
		]
		;(prn cornersaveraged)
		;(attach [cornersaveraged averagevec 0] [[0 0 13] [0 0 1] 0] post)


		(case uporlow
			:upper
				(attachpoint [cornersaveraged averagevec 0] [(slightrans 0) (slightrans 1) (- 0 depthbeneath)])

			:lower

				(attachpoint [cornersaveraged averagevec 0] [(slightrans 0) (slightrans 1) (- 0 depthbeneath plate-thickness)])
			)
		
		)


	)

(defn base-dual-hulls [shapes]
	 (apply union
         (map (partial apply hull)
              (partition 2 1 shapes))))

(defn dual-hull [shapes]
	;(prn (partition 2 1 shapes))

	(apply union
		(map (partial hull)
			 (partition 2 1 shapes))))

(defn showfunction [fxy xrange yrange]
	(for [x (range (- xrange) xrange) y (range (- yrange) yrange)] (
		
		(spit "things/post-demo.scad"
      		(point x y (fxy x y) )) :append true)
		)
	)

(defn newcurveitbaby [arr]
	(apply3dequation
		arr
		(partial  #(- (sqrt (- 1000 (expt %1 2) (expt %2 2)))))
		(partial  #(/ %1 (sqrt (- 1000 (expt %1 2) (expt %2 2)))))
		(partial  #(/ %1 (sqrt (- 1000 (expt %1 2) (expt %2 2)))))
		0.25
		0.2
		5
		)
	)



(defn gradualcurve [arr rowangle columnangle]
		"This curves the array gradually."
	(vec (for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]

			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xcoord pntData)
				yval  		(:ycoord pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				xmod		(int (/ arrXWid 2))
				ymod		(int (/ arrYLen 2))

				betterxval  (- xval xmod)
				betteryval  (inc (- yval xmod))

				currentcolangle (* betteryval columnangle)
				currentrowangle (* betterxval columnangle)
				 		

				rowrotated	[
							(cpntP 0) 
							(- (* (cpntP 1) (Math/cos currentcolangle))  (* (cpntP 2) (Math/sin currentcolangle)) ) 
							(* (+ (* (cpntP 1) (Math/sin currentcolangle))  (* (cpntP 2) (Math/cos currentcolangle)) ) 1)
							]

				fullyrotated [
							(- (* (rowrotated 0) (Math/cos currentrowangle))  (* (rowrotated 2) (Math/sin currentrowangle)) ) 
							(rowrotated 1)
							(+ (* (rowrotated 2) (Math/cos currentrowangle))  (* (rowrotated 0) (Math/sin currentrowangle)) ) 

							]

				newVec 		(findnewvec fullyrotated [0 0 1] ) 
				]
				
				(prn betteryval)
				{:xcoord xval, 
				 :ycoord yval,
				 :cpntPos fullyrotated, 
				 :cpntVec newVec,
				 :cpntAng cpntA}



			))))))

(defn findnewvec [[x1 y1 z1] [x2 y2 z2]]
	"Simple function to find vector between two points."
	[(- x2 x1) (- y2 y1) (- z2 z1)]
	)



(defn curveitbaby [arr]
	(vec(for [ycoin (range arrYLen)]
		(vec (for [xcoin (range arrXWid)]
			(let [
				pntData (retr arr xcoin ycoin)
				xval		(:xcoord pntData)
				yval  		(:ycoord pntData)
				cpntP 		(:cpntPos pntData)
				cpntV 		(:cpntVec pntData)
				cpntA 		(:cpntAng pntData)
				

				newPos 		[ (cpntP 0) (cpntP 1) (* (sqrt (+ (expt (cpntP 0) 2) (expt (cpntP 1) 2))) 0.1) ]
				focuspnt	[ 30 0 150]

				newVec 		(findnewvec  [(newPos 0) (newPos 1) (newPos 2)] focuspnt )
				]
				

				{:xcoord xval, 
				 :ycoord yval,
				 :cpntPos newPos, 
				 :cpntVec newVec,
				 :cpntAng cpntA}

		))))))

(defn retrforbase [arr xcoin ycoin callingfrom callingto makingwhat]
	(cond 
		(= xcoin -1)
			(let [ referencepnt		(retrforbase arr 0 ycoin callingfrom callingto makingwhat)]
				(assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [(- 0 mount-hole-width keySpacing ) 0 0])))
		
		(= xcoin arrXWid)
			(let [referencepnt		(retrforbase arr (dec arrXWid) ycoin callingfrom callingto makingwhat)]
				(assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [(+ mount-hole-width keySpacing ) 0 0])))

		:else
			(retrforbasegoody arr xcoin ycoin callingfrom callingto makingwhat)
		))

(def smartcontinuationofedges true) ;to be used with the local ideal non existent key placement found in retrforbasegoody


(defn retrforbasegoody [arr xcoin ycoin callingfrom callingto makingwhat]
	(cond 
		(= -1 ycoin) 
			(let [
				referencepnt		(retrforbase arr xcoin 0 callingfrom callingto makingwhat)
				](assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [0 (- 0 mount-hole-width keySpacing ) 0])))

		(= arrYLen ycoin)
			(let [
				referencepnt		(retrforbase arr xcoin (dec arrYLen) callingfrom callingto makingwhat)
				](assoc referencepnt :cpntPos (attachpoint [(referencepnt :cpntPos) (referencepnt :cpntVec) (referencepnt :cpntAng)] [0 (+ mount-hole-width keySpacing ) 0])))
		
		; (and (not ((retr arr xcoin ycoin) :existence)) smartcontinuationofedges)
		; 					(let [newkeyandpos  (case callingfrom
		; 											:callfromthisone
		; 												(case makingwhat
		; 													:makingcolumns [(retrforbase arr xcoin (inc ycoin) callingfrom callingto makingwhat) 	[0 (- 0 mount-hole-height keySpacing) 0]] 
		; 													:makingrows    [(retrforbase arr (inc xcoin) ycoin callingfrom callingto makingwhat) 	[(- 0 mount-hole-height keySpacing) 0 0]] 
		; 													:makingdiag    (cond 
		; 																																		((smartretrPntData arr xcoin (inc ycoin)) 		:existence) [(smartretrPntData arr xcoin (inc ycoin) ) 			[0 (- 0 mount-hole-height keySpacing) 0]]

		; 																		((smartretrPntData arr (inc xcoin) ycoin) 		:existence) [(smartretrPntData arr (inc xcoin) ycoin  	)		[(- 0 mount-hole-height keySpacing) 0 0]]
																				
		; 																		((smartretrPntData arr (inc xcoin) (inc ycoin)) :existence) [(smartretrPntData arr (inc xcoin) (inc ycoin)) 	[(- 0 mount-hole-height keySpacing) (- 0 mount-hole-height keySpacing) 0]]

		; 																		)
		; 													;[(retr arr xcoin ycoin) [0 0 0]]
		; 													)
		; 											:callfromleft	
		; 												(case makingwhat
		; 													:makingrows    [(retrforbase arr (dec xcoin) ycoin callingfrom callingto makingwhat) 	[(+ mount-hole-height keySpacing) 0 0]] 
		; 													:makingdiag    (cond 
		; 																		((smartretrPntData arr (dec xcoin) ycoin) 		:existence) [(smartretrPntData arr (dec xcoin) ycoin  	)		[(+ mount-hole-height keySpacing) 0 0]]
		; 																		((smartretrPntData arr xcoin (inc ycoin)) 		:existence) [(smartretrPntData arr xcoin (inc ycoin) ) 			[0 (- 0 mount-hole-height keySpacing) 0]]
		; 																		((smartretrPntData arr (dec xcoin) (inc ycoin)) :existence) [(smartretrPntData arr (dec xcoin) (inc ycoin)) 	[(+ mount-hole-height keySpacing) (- 0 mount-hole-height keySpacing) 0]]

		; 																		)
		; 													;[(retr arr xcoin ycoin) [0 0 0]]
		; 													)
		; 											:callfrombelow	
		; 												(case makingwhat
		; 													:makingcolumns [(retrforbase arr xcoin (dec ycoin) callingfrom callingto makingwhat) 	[0 (+ mount-hole-height keySpacing) 0]] 
		; 													:makingdiag    (cond 
		; 																		((smartretrPntData arr (inc xcoin) ycoin) 		:existence)  [(retrforbase arr (inc xcoin) ycoin callingfrom callingto makingwhat) 			[(- 0 mount-hole-height keySpacing) 0 0]]
		; 																		((smartretrPntData arr xcoin (dec ycoin)) 		:existence)  [(retrforbase arr xcoin (dec ycoin) callingfrom callingto makingwhat) 			[0 (+ mount-hole-height keySpacing) 0]]
		; 																		((smartretrPntData arr (inc xcoin) (dec ycoin)) :existence)  [(retrforbase arr (inc xcoin) (dec ycoin) callingfrom callingto makingwhat) 	[(- 0 mount-hole-height keySpacing) (+ mount-hole-height keySpacing) 0]]
		; 																		)
		; 													;[(retr arr xcoin ycoin) [0 0 0]]
		; 													)
		; 											:callfromleftbelow	
		; 												(case makingwhat
		; 													:makingdiag    (cond 
		; 																		((smartretrPntData arr (dec xcoin) ycoin) 		:existence)  [(retrforbase arr (dec xcoin) ycoin callingfrom callingto makingwhat) 			[(+ mount-hole-height keySpacing) 0 0]]
		; 																		((smartretrPntData arr xcoin (dec ycoin)) 		:existence)  [(retrforbase arr xcoin (dec ycoin) callingfrom callingto makingwhat) 			[0 (+ mount-hole-height keySpacing) 0]]
		; 																		((smartretrPntData arr (dec xcoin) (dec ycoin)) :existence)  [(retrforbase arr (dec xcoin) (dec ycoin) callingfrom callingto makingwhat) 	[(+ mount-hole-height keySpacing) (+ mount-hole-height keySpacing) 0]]
		; 																		)
		; 													)

		; 											;[(retr arr xcoin ycoin) [0 0 0]]
		; 											)
		; 							]	
		; 						(assoc (newkeyandpos 0) :cpntPos (attachpoint [((newkeyandpos 0) :cpntPos) ((newkeyandpos 0) :cpntVec) ((newkeyandpos 0) :cpntAng)] (newkeyandpos 1)))
		; 						)
									
		:else
			(retr arr xcoin ycoin))
	)	

(defn getcolPntData [arr x y]
	"Similar to getrowPntData but this deals with x. See getrowPntData and smartretrPntData"
	(cond 
		(= x arrXWid)
			(retr arr (dec x) y)
		(= x -1)
			(retr arr 0 y)
		; (< x -1)
		; 	(retr arr 0 y)
		:else
			(retr arr x y)
		)
	)

(defn getrowPntData [arr x y]
	"Retrieves the pntData at x y in arr and makes sure that y is in the correct range. See smartretrPntData 
	for what it does when y is out of bounds. This only deals with y as it gets getcolPntData to deal with x."
	(cond 
		(= y arrYLen)
			(getcolPntData arr x (dec y))
		(= y -1)
			(getcolPntData arr x 0)
		; (< y -1)
		; 	(getcolPntData arr x 0)
		:else 
			(getcolPntData arr x y) 
			)

	)

(defn smartretrPntData [arr x y]
	"This is used to make the edges of the plate. Because the webbing loops through -1
	to arrXWid (or arrYLen), you need to be careful not to go outside of the array.
	this function catches when you are at -1 or arrYLen, and returns the closest good position
	+ edge padding. For instance if you call the array with arrXWid, this will find arrXWid - 1,
	then it will return arrXWid -1 with an updated x pos. This update will include edge padding.

	getrowPntData is used because inside of getrowPntData it calls getcolPntData. You can't also 
	call getrowPntData inside getcolPntData because then you will get an infinite loop."
	
	(getrowPntData arr x y))
