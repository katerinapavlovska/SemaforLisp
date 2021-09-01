;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;
;;;; 05_bulls-eye.lisp - příklad k přednášce 5. Návrh stromu dědičnosti
;;;;

#|
Terče jsou zavedeny jako instance třídy bulls-eye. Třídě jsou definovány dvě 
nové vlastnosti: circle-count, radius a center. circle-count smí být jen
kladné číslo, jinak by se nedaly zjistit hodnoty ostatních vlastností.
radius je poloměr největšího kolečka, center střed prvního kolečka. center
je jen ke čtení.

Proti minulé verzi jedna změna: třída je potomkem abstract-picture, takže
uživatel nemůže nastavovat items zprávou set-items.
My k nastavení items použijeme interní zprávu do-set-items.
 
|#

(defclass bulls-eye (abstract-picture)
  ())

(defmethod initialize-instance ((be bulls-eye) &key)
  (call-next-method)
  (do-set-items be (make-be-items 0 0 1 1)))

(defmethod circle-count ((be bulls-eye))
  (length (items be)))

(defmethod set-circle-count ((be bulls-eye) count)
  (unless (plusp count)
    (error "Circle count must be positive."))
  (do-set-items be (make-be-items (x (center be))
                                  (y (center be))
                                  (radius be)
                                  count)))
                                                   
(defmethod radius ((be bulls-eye))
  (radius (car (last (items be)))))

(defmethod center ((be bulls-eye))
  (center (car (items be))))

(defmethod set-radius ((be bulls-eye) value)
  (scale be (/ value (radius be)) (center be)))

(defun make-be-items (cx cy radius count)
  (labels ((iter (i ir)
             (if (zerop i)
                 ir
               (iter (1- i) 
                     (cons (make-be-item cx cy (* radius (/ i count)) (oddp i)) 
                           ir)))))
    (iter count '())))

(defun make-be-item (cx cy radius blackp)
  (move 
   (set-filledp 
    (set-color 
     (set-radius (make-instance 'circle) radius)
     (if blackp :black :light-blue))
    t)
   cx cy))


#|
;;Testy (vyhodnocujte každý řádek zvlášť pomocí F8):

(setf w (make-instance 'window))
(setf be (set-radius (set-circle-count (move (make-instance 'bulls-eye) 145 97)
                                       7)
                     80))
(set-shape w be)
(redraw w)
(set-radius be 60)
(redraw w)
(set-circle-count be 5)
(redraw w)
|#


