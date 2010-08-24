;;;; By Jess Johnson
;;;; jess@grok-code.com
;;;; http://grok-code.com

(setq *debug* t)
(setq *test-know* t)		

;; iterates through the vocabulary, tries to answer a joke for each pair of vocabulary words
(defun generate ()
  (cond (*test-know* (seed-knowledge-test))
	(t (seed-knowledge)))
  ; answer jokes for M_1 N_1, M_2 N_2 pairs, where N is a noun, M is a modifier N_1 != N_2 
  ; modifiers may be null
  (do* ((literals (cdr *literal-list*) (cdr literals))
	(word1 (car literals) (car literals)))
       ((null (cdr literals))) ; break condition
       (cond ((is-POS 'n (gethash word1 *vocab*))
	      (dolist (word2 (cdr literals))
		      (cond ((is-POS 'n (gethash word2 *vocab*))
			     (do* ((literals-m (append *literal-list* (list nil)) (cdr literals-m))
				   (mod1 (car literals-m) (car literals-m)))
				  ((null (cdr literals-m))) ; break condition
				  (cond ((and (or (null mod1) (is-POS 'm (gethash mod1 *vocab*)))
					      (anim-match word1 mod1))  ; animated qualities have to match -- "serious lemon" is not allowed
					 (dolist (mod2 (append (cdr *literal-list*) (list nil)))
						 (cond ((and (or (null mod2) (is-POS 'm (gethash mod2 *vocab*)))
							     (anim-match word2 mod2)) ; animated qualitites
							(let ((answer (answer-joke word1 word2 mod1 mod2)))
							  (cond (answer (print-joke word1 word2 mod1 mod2 answer)))))))))))))))))
		

;; takes strings or word-props returns true if they have they same animated quatlity   
(defun anim-match (str1 str2)
  (let ((w-prop1 (cond ((word-prop-p str1) str1)
		       (t (gethash str1 *vocab*))))
	(w-prop2 (cond ((word-prop-p str2) str2)
		       (t (gethash str2 *vocab*)))))
    (or (null w-prop1) (null w-prop2) 
	(eq 'b (word-prop-anim w-prop1)) (eq 'b (word-prop-anim w-prop2)) 
	(eq (word-prop-anim w-prop1) (word-prop-anim w-prop2)))))
	
 
;; returns a string that will answer the joke, if possible
;; word1 and word2 are strings
;; mod1 and mod2 are strings or nil if no modifier
(defun answer-joke (word1 word2 mod1 mod2)
  
  (let ((derive-word1 (derive-words word1)) 
	(derive-word2 (derive-words word2))
	(derive-mod1 (derive-words mod1))
	(derive-mod2 (derive-words mod2))
	(answer nil)
	(answer-val 0)   ; heuristic for how good the joke is -- funniest is 10
	(threshold 5))   ; jokes with answer-val strictly less than threshold aren't considered funny, and won't be returned

    (cond (*debug* (format t "answer-joke: ~O ~O and ~O ~O~%" mod1 word1 mod2 word2)))
           ; no modifiers
    (cond ((and (null mod1) (null mod2)) 
	   (dolist (d1 derive-word1)
		   (dolist (d2 derive-word2)
			   (let ((a (make-compound (word-prop-literal d1) (word-prop-literal d2))))
			     (cond (a (cond ((is-POS 'm a) 
					     (setq answer (format nil "I don't know, but it's ~O" (word-prop-literal a)))) 
					    ((is-POS 'n a) 
					     (setq answer (word-prop-literal a))))
				      (setq answer-val 10))))  ; found answer with N compound
			   
			   (cond ((and (> 8 answer-val) (>= 8 threshold) 
				       (member (word-prop-literal d1) (mapcar 'word-prop-literal (word-prop-homophone d2)) :test 'string-equal))
				  (let ((ans-prop (cond ((is-POS 'x d1) d1)
							((is-POS 'x d2) d2)
							((is-POS 'b d1) d1)
							((is-POS 'b d2) d2)
							((is-POS 'm d1) d1)
							((is-POS 'm d2) d2)
							(t d1)))
					(ans-phrase (cond ((or (is-POS 'm d1) (is-POS 'm d2)) 'm))))
				    (cond (ans-phrase (setq answer (format nil "I don't know, but it's ~O" (word-prop-literal ans-prop))))
					  (t (setq answer (word-prop-literal ans-prop)))))
				  (setq answer-val 8)))))           ; found an answer where the 2 derived words are a homophone pair
	  
	   (cond ((and (> 3 answer-val) (>= 3 threshold)) 
		  (setq answer (make-substring-word word1 word2 :POS 'n))
		  (cond (answer (setq answer-val 3)))))  ; found answer N with a substring match
	   
	   (cond ((and (> 3 answer-val) (>= 3 threshold))
		  (dolist (d1 derive-word1)
			  (dolist (d2 derive-word2)
				  (cond ((and (is-POS 'm d1) (is-POS 'n d2))
					 (setq answer (format nil "~O ~O" (word-prop-literal d1) (word-prop-literal d2)))
					 (setq answer-val 3))
					((and (is-POS 'm d2) (is-POS 'n d1))
					 (setq answer (format nil "~O ~O" (word-prop-literal d2) (word-prop-literal d1)))
					 (setq answer-val 3))))))))  ; found answer with an MN
	  ; 1 modifier 
	  ((or (null mod2) (null mod1))

	   (cond ((null mod1)
		  (let ((tmp nil))
		    (setq tmp mod1) (setq mod1 mod2) (setq mod2 tmp)
		    (setq tmp word1) (setq word1 word2) (setq word2 tmp)
		    (setq tmp derive-mod1) (setq derive-mod1 derive-mod2) (setq derive-mod2 tmp)
		    (setq tmp derive-word1)(setq derive-word1 derive-word2) (setq derive-word2 tmp))))

	   (cond ((and (> 7 answer-val) (>= 7 threshold))  
		  (dolist (d derive-word2)
			  (dolist (d-word (append (cond ((gethash word1 *vocab*) (list (gethash word1 *vocab*))))
						  derive-word1))
				  (cond ((make-compound (word-prop-literal d) (word-prop-literal d-word))
					 (dolist (d-mod (append (cond ((gethash mod1 *vocab*) (list (gethash mod1 *vocab*))))
								derive-mod1))
						 (cond ((make-compound (word-prop-literal d) (word-prop-literal d-mod))
							(let ((a1 (make-compound (word-prop-literal d) (word-prop-literal d-word)))
							      (a2 (make-compound (word-prop-literal d) (word-prop-literal d-mod))))
							  (cond ((and (is-POS 'm a1) (is-POS 'n a2))
								 (setq answer (format nil "~O ~O" (word-prop-literal a1) (word-prop-literal a2)))
								 (setq answer-val 8))
								((and (is-POS 'm a2) (is-POS 'n a1))
								 (setq answer (format nil "~O ~O" (word-prop-literal a2) (word-prop-literal a1)))
								 (setq answer-val 8))
								((and (is-POS 'm a1) (is-POS 'm a2))
								 (setq answer (format nil "I don't know, but it's ~O and ~O" 
										      (word-prop-literal a1) (word-prop-literal a2)))
								 (setq answer-val 8))
								((and (is-POS 'n a1) (is-POS 'n a2))
								 (setq answer (format nil "~O and ~O" (word-prop-literal a1) (word-prop-literal a2)))
								 (setq answer-val 8))))))))))))))
	 		
	  ; 2 modifiers
	  (t
	   (let ((a1 (make-substring-word word1 word2 :POS 'm))
		 (a2 (make-substring-word mod1 mod2 :POS 'n)))
	     (cond ((and a1 a2 (anim-match (gethash a1 *vocab*) (gethash a2 *vocab*))
			 (<= (length word1) (length word2)) (<= (length mod1) (length mod2))) 
		    (setq answer (format nil "~O ~O" a1 a2))
		    (setq answer-val 8))))  ; made M N, both formed with substrings
			 
	   (cond ((and (> 8 answer-val) (>= 8 threshold))
		  (let ((a1 (make-substring-word word1 word2 :POS 'n))
			(a2 (make-substring-word mod1 mod2 :POS 'm)))
		    (cond ((and a1 a2 (anim-match (gethash a1 *vocab*) (gethash a2 *vocab*))
				(<= (length word1) (length word2)) (<= (length mod1) (length mod2)))
			   (setq answer (format nil "~O ~O" a2 a1))
			   (setq answer-val 8))))))  ; made M N, both formed with substrings

	   (cond ((and (> 8 answer-val) (>= 8 threshold))
		  (let ((ans-list1 nil)
			(ans-list2 nil))
		    (dolist (d derive-word1)
			    (dolist (m derive-mod1)
				    (setq ans-list1 (append ans-list1 (let ((tmp (make-compound (word-prop-literal d) (word-prop-literal m))))
									(cond (tmp (list tmp))))))))  
		    (dolist (d derive-word2)
			    (dolist (m derive-mod2)
				    (setq ans-list2 (append ans-list2 (let ((tmp (make-compound (word-prop-literal d) (word-prop-literal m))))
								       (cond (tmp (list tmp))))))))
		    (cond ((and ans-list1 ans-list2)
			   
			   (dolist (a1 ans-list1)
				   (dolist (a2 ans-list2)
					   (cond ((and (is-POS 'm a1) (is-POS 'm a2))
						  (setq answer (format nil "I don't know, but its ~O and ~O" 
								       (word-prop-literal a1) (word-prop-literal a2)))
						  (setq answer-val 8))
						 ((and (is-POS 'n a1) (is-POS 'm a2) (anim-match a1 a2))
						  (setq answer (format nil "~O ~O" (word-prop-literal a2) (word-prop-literal a1)))
						  (setq answer-val 8))
						 ((and (is-POS 'm a1) (is-POS 'n a2) (anim-match a1 a2))
						  (setq answer (format nil "~O ~O" (word-prop-literal a1) (word-prop-literal a2)))
						  (setq answer-val 8))))))))))))
						 
    
    ; the joke loses "funny points" if the answer and question contain the same word
    (cond ((substring word1 answer)
	   (setq answer-val (- answer-val 4))))
    (cond ((substring word2 answer)
	   (setq answer-val (- answer-val 4))))
    (cond ((substring mod1 answer)
	   (setq answer-val (- answer-val 4))))
    (cond ((substring mod2 answer)
	   (setq answer-val (- answer-val 4))))

  ; it also loses points if the punchline has been used before
    (cond ((gethash answer *punchline*) 
	   (setq answer-val (- answer-val (* 4 (gethash answer *punchline*))))))
    
  ; only return joke if it is funny enough
    (cond ((>= answer-val threshold)
	   (add-punchline answer) ; record the punchline so it is less likely to be used again
	   
	   (cond ((not (null mod1)) ; record the elements of the question so we don't get more jokes with the question and the punchline switched
		  (add-punchline (format nil "~O ~O" mod1 word1))))
	   (cond ((not (null mod2))
		  (add-punchline (format nil "~O ~O" mod2 word2))))
	   
	   answer))))  

;; adds the the punchline 
(defun add-punchline (str)
  (let ((num-punch (gethash str *punchline*)))
    (cond (num-punch (setf (gethash str *punchline*) (+ num-punch 1)))
	  (t (setf (gethash str *punchline*) 1)))))
  

;; returns 't if str1 is contained in str2 
;; returns nil otherwise (returns nil if str1 is nil)
(defun substring (str1 str2)
  (cond ((null str1) nil)
	((null str2) nil)
	((equal str1 "") 't)
	((equal str2 "") nil)
	(t (or (starts-with str2 str1) 
	       (substring str1 (string-left-trim (make-array 1 :initial-element (aref str2 0)) str2))))))

 ;; returns 't if str1 starts with str2			  
(defun starts-with (str1 str2)
  (do ((i 0 (+ i 1)))
      ((eq (length str2) i) 't) ; break
      (cond ((or (>= i (length str1)) (not (eq (aref str2 i) (aref str1 i))))
	     (return nil)))))

;; arg words should be strings	 
;; returns a word-prop formed by combining the two arg words
;; the word-prop-literal may contain words that are homophones of known words.	 
(defun make-compound (word1 word2 &key POS)
  (let ((ho-list1 (append (list word1)
			  (cond ((gethash word1 *vocab*) (mapcar 'word-prop-literal 
								 (word-prop-homophone (gethash word1 *vocab*)))))))
	(ho-list2 (append (list word2)
			  (cond ((gethash word2 *vocab*) (mapcar 'word-prop-literal 
								 (word-prop-homophone (gethash word2 *vocab*)))))))
	(answer nil))
    
    (cond ((and (not (null (gethash word2 *vocab*))) (is-POS POS (gethash word2 *vocab*))) 
	   (dolist (h1 (cdr ho-list1))
		   (cond ((and (starts-with word2 h1) (> (length word2) (length h1)))
			  (cond ((eq (aref (subseq word2 (length h1)) 0) (char " " 0))  ; there is a space at the break point
				 (setq answer (make-word-prop :literal (format nil "~O~O" word1 (subseq word2 (length h1)))
							      :POS (word-prop-POS (gethash word2 *vocab*))
							      :anim (word-prop-anim (gethash word2 *vocab*)))))
				(t 
				 (setq answer (make-word-prop :literal (format nil "~O-~O" word1 (subseq word2 (length h1)))
							      :POS (word-prop-POS (gethash word2 *vocab*))
							      :anim (word-prop-anim (gethash word2 *vocab*)))))))))))
						       
		   
    (cond ((and (not (null (gethash word1 *vocab*))) (is-POS POS (gethash word1 *vocab*))) 
	   (dolist (h2 (cdr ho-list2))
		   (cond ((and (starts-with word1 h2) (> (length word1) (length h2)))
			  (cond ((eq (aref  (subseq word1 (length h2)) 0) (char " " 0)) ; there is a space at the break point
				 (setq answer (make-word-prop :literal (format nil "~O~O" word2 (subseq word1 (length h2)))
							      :POS (word-prop-POS (gethash word1 *vocab*))
							      :anim (word-prop-anim (gethash word1 *vocab*)))))
				(t 
				 (setq answer (make-word-prop :literal (format nil "~O-~O" word2 (subseq word1 (length h2)))
							      :POS (word-prop-POS (gethash word1 *vocab*))
							      :anim (word-prop-anim (gethash word1 *vocab*)))))))))))

    (dolist (h1 ho-list1)
    	    (dolist (h2 ho-list2)
		    (cond ((null answer)
			   (let ((hash nil))
			     
			     (setq hash (gethash (format nil "~O and ~O" h1 h2) *vocab*))
			     (cond ((and hash (is-POS POS hash))
				    (setq answer (make-word-prop :literal (format nil "~O and ~O" word1 word2)
								 :POS (word-prop-POS hash)
								 :anim (word-prop-anim hash)))))
			     
			     (setq hash (gethash (format nil "~O and ~O" h2 h1) *vocab*))
			     (cond ((and hash (is-POS POS hash))
				    (setq answer (make-word-prop :literal (format nil "~O and ~O" word2 word1)
								 :POS (word-prop-POS hash)
								 :anim (word-prop-anim hash)))))
				   
			     (setq hash (gethash (format nil "~O~O" h1 h2) *vocab*))
			     (cond ((and hash (is-POS POS hash))
				    (setq answer (make-word-prop :literal (format nil "~O~O" word1 word2)
								 :POS (word-prop-POS hash)
								 :anim (word-prop-anim hash)))))
			     
			     (setq hash (gethash (format nil "~O~O" h2 h1) *vocab*))
			     (cond ((and hash (is-POS POS hash))
				    (setq answer (make-word-prop :literal (format nil "~O~O" word2 word1)
								 :POS (word-prop-POS hash)
								 :anim (word-prop-anim hash)))))

			     (setq hash (gethash (format nil "~O ~O" h1 h2) *vocab*))
			     (cond ((and hash (is-POS POS hash))
				    (setq answer (make-word-prop :literal (format nil "~O ~O" word1 word2)
								 :POS (word-prop-POS hash)
								 :anim (word-prop-anim hash)))))
			     
			     (setq hash (gethash (format nil "~O ~O" h2 h1) *vocab*))
			     (cond ((and hash (is-POS POS hash))
				    (setq answer (make-word-prop :literal (format nil "~O ~O" word2 word1)
								 :POS (word-prop-POS hash)
								 :anim (word-prop-anim hash))))))))))

    answer))


;; see if we can combine words by using substrings: cat + parrot = carrot
;; args and return value are strings
;; the suffix is always taken from the shorter word
(defun make-substring-word (word1 word2 &key POS)
  (let* ((small-str (cond ((<= (length word1) (length word2)) word1)
			  (t word2)))
	 (big-str (cond ((<= (length word1) (length word2)) word2)
			(t word1)))
	 (suffix (let ((s (delete-if 'null  (mapcar #'(lambda (x) (cond ((starts-with small-str x) x))) ; chair + parrot != carrot 
						    '("thr" "th" "ch" "str" "st" "spr" "sp" "tr" "sc" "gr" "fl" "fr")))))
		   (cond (s (car s))
			 (t (aref small-str 0)))))
	 (new-str (format nil "~O~O" suffix (string-left-trim (make-array 1 :initial-element (aref big-str 0)) big-str))))
    (cond ((and (gethash new-str *vocab*) (is-POS POS (gethash new-str *vocab*)) 
		(not (equal new-str small-str)) (not (equal new-str big-str)))  
	   new-str))))


;; returns true if word-prop can be that kind of speech
(defun is-POS (POS word-prop)
  (or (null POS)
      (and (eq 'b POS) (not (eq 'x (word-prop-POS word-prop))))
      (and word-prop (eq POS (word-prop-POS word-prop)))))


; returns relations of the literal, and relations of the literal with common sufixes, 
; the return list is made of word-props
; the input is a string literal
(defun derive-words (literal)
  (cond ((null literal) nil)
	((null (gethash literal *vocab*)) (add-suffix (make-word-prop :literal literal)))
	(t (append (word-prop-relation (gethash literal *vocab*))
		   (apply 'append (mapcar 'add-suffix
					  (append (word-prop-relation (gethash literal *vocab*)))))))))
						  
	

;; use some common grammer rules to add suffixes to the literal, return a list of possible words 
(defun add-suffix (word-prop)
  (cond ((is-POS 'n word-prop)
	 (list (make-word-prop :literal (format nil "~Os" (word-prop-literal word-prop))
			       :POS (word-prop-POS word-prop)
			       :anim (word-prop-anim word-prop))))))

	
(defun print-joke (word1 word2 mod1 mod2 answer)
  (princ (format nil "WHAT DO YOU GET WHEN YOU CROSS ~A WITH ~A?" 
		 (cond ((and (gethash word1 *vocab*) (eq 't (word-prop-art (gethash word1 *vocab*))))  ; article
			(cond (mod1 (string-upcase (add-article (format nil "~O ~O" mod1 word1))))         ; mod
			      (t (string-upcase (add-article word1)))))                                    ; no mod
		       (t                                                                              ; no article
			(cond (mod1 (string-upcase (format nil "~O ~O" mod1 word1)))                       ; mod
				(t (string-upcase word1)))))                                               ; no mod
		 (cond ((and (gethash word2 *vocab*) (eq 't (word-prop-art (gethash word2 *vocab*))))  ; article
			(cond (mod2 (string-upcase (add-article (format nil "~O ~O" mod2 word2))))         ; mod
			      (t (string-upcase (add-article word2)))))                                    ; no mod
 		       (t                                                                              ; no article
			(cond (mod2 (string-upcase (format nil "~O ~O" mod2 word2)))                       ; mod
			      (t (string-upcase word2)))))))                                               ; no mod

  (terpri)
  (princ (format nil "~A" answer))
  (terpri) (terpri))

(defun add-article (str)
  (cond ((is-vowel (char str 0)) (format nil "an ~O" str))
	(t (format nil "a ~O" str))))

(defun is-vowel (char)
  (cond ((member char '(#\a #\e #\i #\o #\u #\A #\E #\I #\O #\U )))))

;; Add some knowledge, so we can turn it into jokes
(defun seed-knowledge-test ()
  (add-relation "parrot" "polly" :anim1 't :anim2 't)
  (add-relation "cat" "puss" :anim1 't :anim2 't)
  (add-word "carrot")
  (add-word "super" :POS 'm :anim 'b)
  (add-word "dupe" :POS 'm :anim 'b)
  (add-word "duper" :POS 'm :anim 'b)
 
  )

;; Add some knowledge, so we can turn it into jokes
(defun seed-knowledge ()
  (add-relation "cheetah" "fast" :POS2 'm :anim1 't :anim2 't)
  (add-relation "cheetah" "spots" :anim1 't)
  (add-relation "elephant" "trunk" :anim1 't)
  (add-relation "hamburger" "food")
  (add-relation "hamburger" "meat" :art2 'f)
  (add-relation "dance" "ball" :anim2 'b)
  (add-relation "galaxy" "star" :anim2 'b)
  (add-relation "murderer" "killer" :anim1 't :anim2 't)
  (add-relation "toad" "warts" :anim1 't :art2 'f)
  (add-relation "strawberry" "jam" :anim2 'b :art2 'f)
  (add-relation "road" "traffic")
  (add-relation "bell" "ding")
  (add-relation "cow" "milk" :anim1 't :art2 'f)
  (add-relation "duck" "quack" :anim1 't :anim2 'b)
  (add-relation "bank" "dollars" :art2 'f)
  (add-relation "skunk" "scent" :anim1 't)
  (add-relation "ninja" "chops" :anim1 't :anim2 't)
  (add-relation "assistant" "aide" :anim1 't :anim2 'b)
  (add-relation "pig" "pork" :anim1 't)
  (add-relation "cat" "puss" :anim1 't :anim2 't)
  (add-relation "lemon" "sour" :POS2 'm :anim2 'b)
  (add-relation "rabbit" "hare" :anim1 't :anim2 't)
  (add-relation "lawn sprinkler" "spray")
  (add-relation "cemetery" "grave yard")
  (add-relation "mad" "crazy" :POS1 'm :POS2 'm :anim1 't :anim2 't)
  (add-relation "mad" "angry" :POS1 'm :POS2 'm :anim1 't :anim2 't)
  (add-relation "cap" "hat")
  (add-relation "ant" "bug" :anim1 't :anim2 't)
  (add-relation "aunt" "relative" :anim1 't :anim2 't)
  (add-relation "parent" "relative" :anim1 't :anim2 't)
  (add-relation "scared" "afraid" :POS1 'm :POS2 'm :anim1 't :anim2 't)
  (add-relation "rabbit" "hopping" :POS2 'm :anim1 't :anim2 't)
  (add-relation "rabid" "frothing" :POS1 'm :POS2 'm :anim1 't :anim2 't)
  (add-relation "cereal" "Frosted Flakes" :art2 'f)
  (add-relation "boy" "young man" :anim1 't :anim2 't)
  (add-relation "parrot" "polly" :anim1 't :anim2 't)
  (add-relation "flower" "poppy")
  (add-relation "jelly" "jam" :art1 'f :art2 'f)
  (add-relation "fish" "trout" :anim1 't :anim2 't)
  (add-relation "grave" "serious" :POS1 'b :POS2 'm :anim1 't :anim2 't)
  (add-relation "thief" "robber" :anim1 't :anim2 't)
  (add-relation "grave" "serious" :POS1 'b :POS2 'm :anim1 'b :anim2 't)
  (add-relation "music" "band" :anim2 'b :art1 'f)
  (add-relation "pea" "vegetable" :anim2 'b)
  (add-relation "centipede" "legs" :anim1 't :anim2 't :art2 'f)
  (add-relation "einstein" "relative" :anim1 't :anim2 't :art1 'f)
  (add-relation "jacket" "coat")
  (add-relation "fire" "hot" :POS2 'm) 
  (add-relation "electricity" "power" :art1 'f :art2 'f)
  (add-relation "pond" "lake")
  (add-relation "rain" "wet" :POS2 'm :art1'f)
  (add-relation "alcohol" "drunk" :POS2 'b :anim2 't)
  (add-relation "rabbit" "bunny" :anim1 't :anim2 't)
  (add-relation "car" "automobile")
  (add-relation "country" "nation")
  (add-relation "beach" "sand" :art2 'f)
  (add-relation "dog" "ruff" :anim1 't :POS2 'x)
  (add-relation "cat" "mew" :anim1 't :POS2 'x)
  (add-relation "cat" "purrr" :anim1 't :POS2 'x)
  (add-relation "sandpaper" "rough" :POS2 'm :art1 'f)
  (add-relation "radio" "music" :art2 'f)
  (add-relation "tune" "music" :art2 'f)
  (add-relation "chicken" "egg" :anim1 't)
  (add-relation "extraterrestrial" "alien" :anim1 'b :anim2 't) 
  (add-relation "finals" "exams" :art1 'f :art2 'f)
  (add-relation "port" "serial" :POS2 'm)

  (add-homophone "cereal" "serial" :POS2 'm)
  (add-homophone "hare" "hair" :anim1 't :art2 'f)
  (add-homophone "wars" "warts" :art1 'f :art2 'f)
  (add-homophone "cents" "scents" :art1 'f :art2 'f)
  (add-homophone "afraid" "frayed" :POS2 'm :anim1 't :anim2 'b)
  (add-homophone "parent" "apparent" :POS2 'm :anim1 't)
  (add-homophone "band" "banned" :POS2 'm :anim1 'b :anim2 't)
  (add-homophone "ant" "aunt" :anim1 't :anim2 't)
  (add-homophone "rabbit" "rabid" :POS2 'm :anim1 't :anim2 't)
  (add-homophone "puppy" "poppy" :anim1 't)
  (add-homophone "cracker" "quacker" :POS2 'm :anim2 't)
  (add-homophone "peas" "peace")
  (add-homophone "son" "sun")
  (add-homophone "tune" "toon" :POS2 'x)
  (add-homophone "witch" "which" :anim1 't :POS2 'x)
  (add-homophone "rough" "ruff" :POS1 'm :POS2 'x)
  (add-homophone "mew" "mu" :POS1 'x :POS2 'x)
  (add-homophone "purrr" "per" :POS1 'x :POS2 'x)
  (add-homophone "purrr" "pur" :POS1 'x :POS2 'x)
  (add-homophone "eggs" "ex" :POS2 'x :art1 'f)
  (add-homophone "ade" "aide" :POS1 'x :anim2 't)
  (add-homophone "aid" "aide" :POS1 'x :anim2 't)
  
  
  (add-word "canned" :POS 'm :anim 'b)
  (add-word "cow bell")
  (add-word "pig headed" :POS 'm :anim 't)
  (add-word "star wars" :art 'f)
  (add-word "sour puss" :anim 't)
  (add-word "traffic jam")
  (add-word "dingbat" :anim 't)
  (add-word "milk and crackers" :art 'f)
  (add-word "carrot")
  (add-word "pork chops" :art 'f)
  (add-word "fast food" :art 'f)
  (add-word "dollars and cents" :art 'f)
  (add-word "lemonade" :art 'f)
  (add-word "hair spray" :art 'f)
  (add-word "mad hatter" :anim 't)
  (add-word "hopping mad" :POS 'm :anim 't)
  (add-word "serial killer" :anim 't)
  (add-word "boycrazy" :POS 'm :anim 't)
  (add-word "flower power")
  (add-word "jellyfish" :anim 't)
  (add-word "grave robber" :anim 't)
  (add-word "sour balls" :art 'f)
  (add-word "fastball")
  (add-word "sour milk" :art 'f)
  (add-word "bandaid")
  (add-word "peas and carrots" :art 'f)
  (add-word "peace and quiet" :art 'f)
  (add-word "war and peace" :art 'f)  
  (add-word "raincoat")
  (add-word "fireman")
  (add-word "pancake")
  (add-word "cupcake")
  (add-word "butterfly")
  (add-word "milkman" :anim 't)
  (add-word "doorbell")
  (add-word "sunshine") 
  (add-word "bad" :POS 'm :anim 't)
  (add-word "belly")
  (add-word "boat")
  (add-word "cake")
  (add-word "drunk" :POS 'b :anim 't)
  (add-word "fake" :POS 'm)
  (add-word "jolly" :POS 'm :anim 't)
  (add-word "mare" :anim 't)
  (add-word "bugs bunny" :anim 't :art 'f)
  (add-word "cartoon")
  (add-word "carnation")
  (add-word "sandwich")
  (add-word "snowball")
  (add-word "snowman")
  (add-word "excited" :POS 'm :anim 't)
  (add-word "purple" :POS 'm :anim 'b)
  (add-word "person" :anim 't)

  (sort *literal-list* #'(lambda(x y) (< (length x) (length y)))))
 


;; Everything below this line is the underlying data structure
;; -------------------------------------------------------------
(defstruct word-prop literal relation homophone POS anim art)

(setq *vocab* (make-hash-table :test 'string-equal)) ; holds world knowledge
(setq *literal-list* (list nil)) ;; this is the key list for *vocab* -- the two data structures should be kept consistent

(setq *punchline* (make-hash-table :test 'string=))  ; holds punchlines of jokes that have already been told

(defun add-relation (literal1 literal2 &key POS1 POS2 anim1 anim2 art1 art2)
  (add-word literal1 :POS POS1 :anim anim1 :art art1)
  (add-word literal2 :POS POS2 :anim anim2 :art art2)
  (let ((word-prop1 (gethash literal1 *vocab*))
	(word-prop2 (gethash literal2 *vocab*)))
    (setf (word-prop-relation word-prop2) (remove-duplicates (nconc (list word-prop1) (word-prop-relation word-prop2)))) ;; ??
    (setf (word-prop-relation word-prop1) (remove-duplicates (nconc (list word-prop2) (word-prop-relation word-prop1))))))

(defun add-homophone (literal1 literal2 &key POS1 POS2 anim1 anim2 art1 art2)	
  (add-word literal1 :POS POS1 :anim anim1 :art art1)
  (add-word literal2 :POS POS2 :anim anim2 :art art2)
  (let ((word-prop1 (gethash literal1 *vocab*))
	(word-prop2 (gethash literal2 *vocab*)))
    (setf (word-prop-homophone word-prop2) (remove-duplicates (nconc (list word-prop1) (word-prop-homophone word-prop2))))
    (setf (word-prop-homophone word-prop1) (remove-duplicates (nconc (list word-prop2) (word-prop-homophone word-prop1))))))

;; Add a word to the vocab-list if it isn't already there
;; If the POS in unspecified, it defaults to 'n (noun)
;; Other acceptable parts of speech are 'm (modifier), 'b (both), 'x (neither)
;; Anim (animated) can be 't (true) 'f (false) 'b (both), the default is 'f
;; art (article) can be 't (true) or 'f (false), the default is 't
(defun add-word (literal &key POS anim art)
  (cond ((null (gethash literal *vocab*))
	 (setf (gethash literal *vocab*) (make-word-prop :literal literal 
							 :POS (cond (POS)(t 'n))
							 :anim (cond (anim) (t 'f))
							 :art (cond (art) (t 't))))
	 (nconc *literal-list* (list literal)))))

;; Debug funtions 
(defun print-vocab ()
  (maphash #'(lambda (key val) 
	       (word-prop-literal val)
	       (princ key)
	       (princ ": ")
	       (mapcar #'(lambda(y)
			   (princ (word-prop-literal y))
			   (princ " "))
		       (word-prop-relation val))
	       (princ ": ")
	       (mapcar #'(lambda(y)
			   (princ (word-prop-literal y))
			   (princ " "))
		       (word-prop-homophone val))
	       (terpri))
	   *vocab*))

(defun test ()
  (add-relation 'traffic 'jam)
  (print-vocab))

