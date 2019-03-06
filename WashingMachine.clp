(import nrc.fuzzy.*)

(import nrc.fuzzy.jess.*)

(load-package FuzzyFunctions)



(deftemplate machine 

    (slot cloth-type)

    (slot dirt-type)

    (slot dirtiness) 

    )


(deftemplate inputTaken 

    (slot inp)

    (slot isValid)

    )   


;Defining defglobals for holding the smart home variables

;The temperature, thermostat, daylight, lights are all fuzzy variables

;The door and window open or closed is a boolean value

(defglobal ?*clothType* = (new FuzzyVariable "clothType" 0.0 10.0 "type"))

(defglobal ?*dirtType* = (new FuzzyVariable "dirtType" 0.0 10.0 "range"))

(defglobal ?*dirtiness* = (new FuzzyVariable "dirtiness" 0.0 10.0 "amount"))

(defglobal ?*washTime* = (new FuzzyVariable "washTime" 0.0 100.0 "minutes"))

(defglobal ?*WT* = "" )



(defglobal 

    ?*cloth-type* = ""

    ?*dirt-type* = 0

    ?*dirti-ness* =  0

    ?*isValidInp* = 0

    )



;A rule to add terms to all the fuzzy variables and assert the current input

; Rule 1
(defrule rule1_initialize-fuzzy-variables

    

	(inputTaken{inp == 1})(inputTaken{isValid == 1}) 

    ?machine <- (machine)

    =>

    ;Adding terms for cloth type

    

    ;(printout t "?cloth-type" crlf)

    ;(printout ?cloth-type)

    

    (?*clothType* addTerm "silk" (new ZFuzzySet 0 5))

	(?*clothType* addTerm "cotton" (new TrapezoidFuzzySet 3 4 5 7))

    (?*clothType* addTerm "wool" (new SFuzzySet 5 10))



    

    ;Adding terms for dirt type

    (?*dirtType* addTerm "not_greasy" (new ZFuzzySet 0 5))

    (?*dirtType* addTerm "medium_greasy" (new TrapezoidFuzzySet 3 4 5 7))

    (?*dirtType* addTerm "greasy" (new SFuzzySet 5 10))

    

    ;Adding terms for dirtiness

    (?*dirtiness* addTerm "small" (new ZFuzzySet 0 5))

    (?*dirtiness* addTerm "medium" (new TrapezoidFuzzySet 3 4 5 7))

    (?*dirtiness* addTerm "large" (new SFuzzySet 5 10))

    

    ;Adding terms for washTime

    (?*washTime* addTerm "very_short"  (new TriangleFuzzySet 0.0 10.0 20.0))

    (?*washTime* addTerm "short" (new TriangleFuzzySet 20.0 30.0 40.0))

    (?*washTime* addTerm "medium" (new TriangleFuzzySet 40.0 50.0 60.0))

    (?*washTime* addTerm "long" (new TriangleFuzzySet 60.0 70.0 80.0))

    (?*washTime* addTerm "very_long" (new TriangleFuzzySet 80.0 90.0 100.0))

    

    

    ;Asserting the input facts

    ;=====> Change the input facts here

     ;(assert (theClothType (new FuzzyValue ?*clothType*  ?machine_fuzzy.clothType)))

      ;(assert (theDirtType (new FuzzyValue ?*dirtType*  ?machine_fuzzy.dirtType)))

    ;(assert (theDirtiness (new FuzzyValue ?*dirtiness*  ?machine_fuzzy.dirtiness)))





   (assert (theClothType (new FuzzyValue ?*clothType* "wool")))

   ;(assert (theClothType (new FuzzyValue ?*clothType* (new SingletonFuzzySet ?machine.cloth-type))))

   (assert (theDirtType (new FuzzyValue ?*dirtType* "not_greasy")))

   (assert (theDirtiness (new FuzzyValue ?*dirtiness* "large")))

     

)


; Rule 2
	(defrule rule2_takeInput



    =>

    

    (printout t crlf crlf "WELCOME TO THE WASHTIME EVALUATOR" crlf)

    (printout t "$$ This Application calculates the number of minutes you will need to put your laundry in the washing machine" crlf)

    (printout t "**NOTE: PLEASE PROVIDE A VALID INPUT FOR ALL THE QUESTIONS. REFER THE DOCUMENTATION FOR MORE INFO ON HOW TO USE THIS APPLICATION" crlf)
    

    (printout t "Enter information about the clothes you need to wash" crlf crlf)        

    (printout t "Cloth type indicates type of fabric- silk, wool and cotton. Please enter cloth type:(0-10) :" crlf)

    (bind ?*cloth-type* (read t))

    (printout t "Dirt Type indicates grease presence, if any- not_greasy, medium_greasy, greasy. Please enter dirt type:(0-10)" crlf)

    (bind ?*dirt-type* (read t))

    (printout t "Dirtiness indicates 3small, medium or large amounts of dirt. Please enter dirtiness:(0-10)." crlf)

    (bind ?*dirti-ness* (read t))

    

    

    (assert (machine (cloth-type ?*cloth-type*) (dirt-type ?*dirt-type*) (dirtiness ?*dirti-ness*) ))

    (bind ?*isValidInp* 1)

    (assert (inputTaken (inp 1) (isValid ?*isValidInp*)))    

    (printout t crlf crlf "**************Machine Assessment*****************" crlf crlf)

)



; Rule 3
 ;Silk Greasy Large very_long THIS IS WORKING 

(defrule rule3 ;"silk type & greasy & large amount of dirtiness => wash time is very_long"

   

         (inputTaken{inp == 1})(inputTaken{isValid == 1}) 

   (theClothType ?c&:(fuzzy-match ?c "silk"))

    (theDirtType ?dt&:(fuzzy-match ?dt "greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d "large"))

    (machine(cloth-type ?ct))

=>

    ;(printout t (?c momentDefuzzify))

    (bind ?*WT* "very_long")

    (assert(theWashTime (new FuzzyValue ?*washTime* "very_long")))

    

)


;Fuzzy Rules for calculation

 


; Rule 4
(defrule rule4 ;"silk type & not_greasy & small amount of dirtiness => wash time is very_short"

    ;(theClothType ?ct&: (fuzzy-match ?ct "silk"))

    ;(theDirtType ?dt&: (fuzzy-match ?dt "not_greasy"))

    ;(theDirtiness ?d&: (fuzzy-match ?d "small"))

    

     (theClothType ?c&:(fuzzy-match ?c"silk"))

    (theDirtType ?dt&:(fuzzy-match ?dt"not_greasy"))

    (theDirtiness ?d&: (fuzzy-match ?d"small"))

=>

     (printout t "--------" crlf)

    (assert(theWashTime (new FuzzyValue ?*washTime* "very_short"))))


; Rule 5
(defrule rule5 ;"silk type & not_greasy & medium amount of dirtiness => wash time is short"

     (theClothType ?c&:(fuzzy-match ?c"silk"))

    (theDirtType ?dt&:(fuzzy-match ?dt"not_greasy"))

    (theDirtiness ?d&: (fuzzy-match ?d"medium"))

=>

    (printout t "--------": ?wash_time crlf)

    (assert(theWashTime (new FuzzyValue ?*washTime* "short"))))


; Rule 6
(defrule rule6 ;"silk type & not_greasy & large amount of dirtiness => wash time is medium"

     (theClothType ?c&:(fuzzy-match ?c"silk"))

    (theDirtType ?dt&:(fuzzy-match ?dt"not_greasy"))

    (theDirtiness ?d&: (fuzzy-match ?d"large"))

=>

     (printout t "--------" crlf)

    (assert(theWashTime (new FuzzyValue ?*washTime* "medium"))))


; Rule 7
(defrule rule7 ;"silk type & medium_greasy & small amount of dirtiness => wash time is medium"

    (theClothType ?c&:(fuzzy-match ?c"silk"))

    (theDirtType ?dt&:(fuzzy-match ?dt"medium_greasy"))

    (theDirtiness ?d&: (fuzzy-match ?d"small"))

=>

     (printout t "--------" crlf)

     (assert(theWashTime (new FuzzyValue ?*washTime* "medium"))))



; Rule 8
;Silk Medium Medium Long

(defrule rule8 ;"silk type & medium_greasy & medium amount of dirtiness => wash time is medium"

    (theClothType ?c&:(fuzzy-match ?c"silk"))

    (theDirtType ?dt&:(fuzzy-match ?dt"medium_greasy"))

    (theDirtiness ?d&: (fuzzy-match ?d"medium"))

=>

    (printout t "--------" crlf)

    (assert(theWashTime (new FuzzyValue ?*washTime* "long"))))


; Rule 9
;Silk Medium Large Long

(defrule rule9 ;"silk type & medium_greasy & large amount of dirtiness => wash time is long"

    (theClothType ?c&:(fuzzy-match ?c"silk"))

    (theDirtType ?dt&:(fuzzy-match ?dt"medium_greasy"))

    (theDirtiness ?d&: (fuzzy-match ?d"large"))

=>

    (printout t "--------" crlf)

    (assert(theWashTime (new FuzzyValue ?*washTime* "long"))))



;Silk Greasy Small Medium

(defrule rule10 ;"silk type & greasy & small amount of dirtiness => wash time is medium"

    (theClothType ?c&:(fuzzy-match ?c"silk"))

    (theDirtType ?dt&:(fuzzy-match ?dt"greasy"))

    (theDirtiness ?d&: (fuzzy-match ?d"small"))

=>

    (printout t "--------" crlf))

    ;(assert(theWashTime (new FuzzyValue ?*washTime* "medium"))))



;Silk Greasy Medium Long

(defrule rule11 ;"silk type & greasy & medium amount of dirtiness => wash time is long"

   (theClothType ?c&:(fuzzy-match ?c"silk"))

    (theDirtType ?dt&:(fuzzy-match ?dt"greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"medium"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "long"))))



;Woolen not_greasy Small Short

(defrule rule12 ;"wool type & not_greasy & small amount of dirtiness => wash time is short"

      (theClothType ?c&:(fuzzy-match ?c"wool"))

    (theDirtType ?dt&:(fuzzy-match ?dt"not_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"small"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "short"))))


;Woolen not_greasy medium medium

(defrule rule13 ;"wool type & not_greasy & medium amount of dirtiness => wash time is medium"

      (theClothType ?c&:(fuzzy-match ?c"wool"))

    (theDirtType ?dt&:(fuzzy-match ?dt"not_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"medium"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "medium"))))



;Woolen not_greasy large long

(defrule rule14 ;"wool type & not_greasy & large amount of dirtiness => wash time is long"

         (theClothType ?c&:(fuzzy-match ?c"wool"))

    (theDirtType ?dt&:(fuzzy-match ?dt"not_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"large"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "long"))))



;Woolen medium_greasy Small medium

(defrule rule15 ;"wool type & medium_greasy & small amount of dirtiness => wash time is medium"

         (theClothType ?c&:(fuzzy-match ?c"wool"))

    (theDirtType ?dt&:(fuzzy-match ?dt"medium_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"small"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "medium"))))



;Woolen medium_greasy medium medium

(defrule rule16 ;"wool type & medium_greasy & medium amount of dirtiness => wash time is medium"

         (theClothType ?c&:(fuzzy-match ?c"wool"))

    (theDirtType ?dt&:(fuzzy-match ?dt"medium_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"medium"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "medium"))))



;Woolen medium_greasy large long

(defrule rule17 ;"wool type & medium_greasy & large amount of dirtiness => wash time is long"

     (theClothType ?c&:(fuzzy-match ?c"wool"))

    (theDirtType ?dt&:(fuzzy-match ?dt"medium_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"large"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "long"))))



;Woolen greasy Small long

(defrule rule18 ;"wool type & greasy & small amount of dirtiness => wash time is long"

       (theClothType ?c&:(fuzzy-match ?c"wool"))

    (theDirtType ?dt&:(fuzzy-match ?dt"greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"small"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "long"))))



;Woolen greasy medium long

(defrule rule19 ;"wool type & greasy & medium amount of dirtiness => wash time is long"

          (theClothType ?c&:(fuzzy-match ?c"wool"))

    (theDirtType ?dt&:(fuzzy-match ?dt"greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"medium"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "long"))))



;Woolen, greasy, large, very_long

(defrule rule20 ;"wool type & greasy & large amount of dirtiness => wash time is very_long"

          (theClothType ?c&:(fuzzy-match ?c"wool"))

    (theDirtType ?dt&:(fuzzy-match ?dt"greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"large"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "very_long"))))





;Cotton, not_greasy, small, short

(defrule rule21 ;"cotton type & not_greasy & small amount of dirtiness => wash time is short"

           (theClothType ?c&:(fuzzy-match ?c"cotton"))

    (theDirtType ?dt&:(fuzzy-match ?dt"not_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"small"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "short"))))



;Cotton, not_greasy, medium, medium

(defrule rule22 ;"cotton type & not_greasy & medium amount of dirtiness => wash time is medium"

   (theClothType ?c&:(fuzzy-match ?c"cotton"))

    (theDirtType ?dt&:(fuzzy-match ?dt"not_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"medium"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "medium"))))



;Cotton, not_greasy, large, long

(defrule rule23 ;"cotton type & not_greasy & large amount of dirtiness => wash time is long"

      (theClothType ?c&:(fuzzy-match ?c"cotton"))

    (theDirtType ?dt&:(fuzzy-match ?dt"not_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"large"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "long"))))



;Cotton, medium_greasy, small, medium

(defrule rule24 ;"cotton type & medium_greasy & small amount of dirtiness => wash time is medium"

      (theClothType ?c&:(fuzzy-match ?c"cotton"))

    (theDirtType ?dt&:(fuzzy-match ?dt"medium_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"small"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "medium"))))



;Cotton, medium_greasy, medium, long

(defrule rule25 ;"cotton type & medium_greasy & small amount of dirtiness => wash time is medium"

         (theClothType ?c&:(fuzzy-match ?c"cotton"))

    (theDirtType ?dt&:(fuzzy-match ?dt"medium_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"medium"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "long"))))



;Cotton, medium_greasy, large, very_long

(defrule rule26 ;"cotton type & medium_greasy & small amount of dirtiness => wash time is medium"

         (theClothType ?c&:(fuzzy-match ?c"cotton"))

    (theDirtType ?dt&:(fuzzy-match ?dt"medium_greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"large"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "very_long"))))



;Cotton, greasy, small, long

(defrule rule27 ;"cotton type & greasy & small amount of dirtiness => wash time is long"

         (theClothType ?c&:(fuzzy-match ?c"cotton"))

    (theDirtType ?dt&:(fuzzy-match ?dt"greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"small"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "long"))))



;Cotton, greasy, medium, long

(defrule rule28 ;"cotton type & greasy & medium amount of dirtiness => wash time is long"

         (theClothType ?c&:(fuzzy-match ?c"cotton"))

    (theDirtType ?dt&:(fuzzy-match ?dt"greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"medium"))

=>

    (assert(theWashTime (new FuzzyValue ?*washTime* "long"))))



;Cotton, greasy, large, very_long

(defrule rule29 ;"cotton type & greasy & large amount of dirtiness => wash time is very_long"

           (theClothType ?c&:(fuzzy-match ?c"cotton"))

    (theDirtType ?dt&:(fuzzy-match ?dt"greasy"))

     (theDirtiness ?d&: (fuzzy-match ?d"large"))

=>

    ;(bind ?*washTime* "very_long")

    (assert(theWashTime (new FuzzyValue ?*washTime* "very_long"))))







;Fuzzy Rules for calculation

 ;Rule to print the crisp values after the rules fire

(defrule rule30_print-results

    (theClothType ?c)(theDirtType ?dt)

    (theDirtiness ?d)(theWashTime ?wt)

    =>

    (printout t crlf)

    (printout t "The fuzzy settings of the Washing Machine system are : " crlf crlf)

	(printout t "Cloth type is  " (?c momentDefuzzify) crlf crlf)

    (printout t "Dirt type is  " (?dt momentDefuzzify) crlf crlf)

    (printout t "Dirtiness is  " (?d momentDefuzzify) crlf crlf)

    

    (printout t "WashTime is " (?wt momentDefuzzify) " minutes" crlf crlf)

    

)

    

(reset)

(run)