#|
Zdrojovy soubor k prvnimu zapoctovemu ukolu. Autor Katerina Pavlovska.

DOKUMENTACE
-----------

TRIDA SEMAPHORE
---------------

Trida semaphore je potomkem tridy abstract-picture. Jej� instance predstavuj� funkcni graficky objekt semafor.


NOV� VLASTNOSTI 
---------------

semaphore-type	Urcuje typ semaforu. Tato vlastnost slouzi zejmena k vytvareni semaforu 		podle predem definovaneho prototypu.
phase-count 	Pocet fazi, ktere mohou u semaforu nastat. F�ze jsou cislovane od 0. 			Urceno pouze ke cteni.
semaphore-phase Cislo faze, ve ktere se semafor aktualne nachazi.
color-list	Seznam barev, kter� predstavuji on-color jednotlivych svetel semaforu. 			Svetla jsou potomkem instance light.
program		Seznam seznamu, reprezentujici chovani semaforu v jednotlivych fazich, 			respektive zapnuti a vypnuti svetel. 


NOV� ZPR�VY


semaphore-type semaphore

Vrac� hodnotu vlastnosti semaphore-type.

phase-count semaphore

Vrac� hodnotu vlastnosti phase-count.

semaphore-phase semaphore

Vrac� hodnotu vlastnosti semaphore-phase.

color-list semaphore

Vrac� hodnotu vlastnosti color-list.

program semaphore

Vrac� hodnotu vlastnosti semaphore

set-semaphore-type semaphore value

Nastav� semaforu n�zev jeho typu. Parametr value je ve tvaru #:<<name>>.

set-semaphore-phase semaphore value

Nastav� hodnotu vlastnosti semaphore-phase na celo��selnou hodnotu value. Pokud je hodnota value vice nez je maxim�ln� povolen� hodnota stanoven� vlastnost� phase-count, nebo mensi nez hodnota 0, je vyvol�na chyba. Zpr�va je zas�l�na z metod next-phase. 

set-program semaphore value

Nastav� hodnotu vlastnosti program na value, p�i�em� value je nepr�zdn� seznam seznam�, jehoz hodnoty nab�vaj� hodnot t nebo nil.

next-phase semaphore

Nastav� vlastnost semaphore-phase na dal�� nejbli��� hodnotu p��padn� na poctaecni pri dosazeen� maxim�ln� hodnoty semaphore-phase (phase-count zmenseen� o 1). 

T��DA CROSSROADS (PICTURE)
--------------------------

Instance reprezentuje k�i�ovatku se sv�teln�mi signaliza�n�mi za��zen�mi.

NOV� VLASTNOSTI

semaphores	Seznam index�, na kter�ch se ve vlastnosti items nach�z� objekty t��dy 			semaphore. Ur�eno pouze ke �ten�.
crossroads-phase	Hodnota aktu�ln� f�ze, ve kter� se k�i�ovatka nach�z�.
phase-count	Po�et f�z�, kter�ch m��e k�i�ovatka nab�vat. Ur�eno pouze ke �ten�.
program 	Seznam seznam�, reprezentuj�c� jednotliv� f�ze k�i�ovatky. Hodnoty 			nab�vaj� ��seln�ch hodnot, p�edstavuj�c�ch f�ze jednotliv�ch semafor�.


NOV� ZPR�VY

semaphores crossroads

Vrac� hodnotu vlastnosti semaphores.

crossroads-phase crossroads

Vrac� hodnotu vlastnosti crossroads-phase. Zpr�va zas�l�na z metody next-phase.

phase-count crossroads

Vrac� hodnotu vlastnosti phase-count. Zpr�va zas�l�na z metody set-crossroads-phase.

program crossroads

Vrac� hodnotu vlastnosti program. Zpr�va zas�l�na z metody set-semaphores.

set-semaphores crossroads

Podle aktu�ln� hodnoty crossroads-phase nastav� dle programu hodnoty semaphore-phase jednotliv�m semafor�m. Svetla semafor� jsou po vol�ni set-semaphores zapnuty/vypnuty dle nov� nastaven� hodnoty semaphore-phase. Zpr�va zas�l�na z metody nex-phase.

set-crossroads-phase crossroads value

Nastav� hodnotu vlastnosti semaphore-phase na celo��selnou hodnotu value. Pokud je hodnota value vetsi nez je maxim�ln� povolen� hodnota stanoven� vlastnost� phase-count, nebo mensi nez hodnota 0, je vyvol�na chyba. Zpr�va je zas�l�na z metod next-phase. 

set-program crossroads list

Nastav� hodnotu vlastnosti program na hodnotu value. Hodnota value mus� b�t ve form� seznamu, jinak je vyvol�na chyba. Hodnoty seznamu mohou nab�vat celo��seln�ch hodnot (pri phase-count 1) nebo b�t seznam, obsahuj�c� celo��seln� hodnoty. Zpr�va je vol�na z metody set-crossroads.

next-phase crossroads

Nastav� vlastnost crossroads-phase na dalsi nejblizsi hodnotu pripadne na pocatecni pri dosazen� maxim�ln� hodnoty crossroads-phase (phase-count zmensen� o 1).
|#

(defvar *semaphore-types*
 '( (:pedestrian  (:red :green) ((t nil) (nil t)))
    (:vehicle (:red :orange :green) ((t nil nil) (t t nil) (nil nil t) (nil t nil)))))

#|------POMOCNE FUNKCE-----|#

#| Finding-type value  Nalezne prepis po zadany typ semaforu. Pokud jej nenalezne, vrati prazdny seznam.|#

(defun finding-type (value)
(finding-type-help value *semaphore-types*))

(defun finding-type-help (value seznam)
  (cond ((equal value (car (car seznam))) (cdr (car seznam)))
        ((equal '() seznam) '())
        (t (finding-type-help value (cdr seznam)))))

#| POMOCNE FUNKCE PRO VYTVORENI GRAFICKEHO ZOBRAZENI SEMAFORU|#


#|make-box position Funkce vytvori cerny polygon (ctverec), ktery je pozadim kazdeho svetla semaforu.|#
(defun make-box (position)
(let ((box (make-instance 'polygon)))
  (progn (set-items box (list (move (make-instance 'point) 0 0)
                                (move (make-instance 'point) 70 0)
                                (move (make-instance 'point) 70 70)
                                (move (make-instance 'point) 0 70)))
    (set-filledp box :black)
    (set-color box :black)
    (move box 0 (* (- position 1) 70)))))

#|make-ligth position type Funkce vytvori svetlo semaforu. Vraci svetlo, ktere je vypnute.|#
(defun make-light (position type)
  (let ((light (make-instance 'light)))
    (progn (move
            (set-radius
             (set-on-color light (nth (- position 1) (nth 0 (finding-type type)))) 
             25) 
            (/ 70 2)
            (+ (/ 70 2) 
               (* ( - position 1) 70))))
    (turn-off light)))

#|make-box-dotimes type Funkce, jejimz parametrem je typ semaforu, vytvori n cernych ctvercu, tedy pozadi celeho semaforu |#
(defun make-box-dotimes (type)
 (let ((list '())
        (index (length (nth 0 (finding-type type)))))
    (dotimes (i index (nreverse list))
      (push (make-light (+ 1 i) type) list))))

#|make-light-dotimes type Funkce, jejiz parametrem je typ semaforu, vytvori n vypnutych svetel.|#
(defun make-light-dotimes (type)
(let ((list '())
        (index (length (nth 0 (finding-type type)))))
    (dotimes (i index (nreverse list))
      (push (make-box (+ 1 i)) list))))

#|set-light light value Funkce, ktera podle vstupnni hodnoty (t nebo nil) vypne/zapne dane svetlo.|#
(defun set-light (light value)
  (if value (turn-on light)
    (turn-off light)))

#|set-lighting number type items  Funkce, ktera dany sezam svetel, vypne/zapne podle seznamu hodnot(t nebo nil)|#
(defun set-lighting (number type items)
  (let* ((index (length (nth 0 (finding-type type))))
         (list (nth number (nth 1 (finding-type type)))))
    (dotimes (i index)
       (set-light (nth i items) (nth i list)))) items)

#|make-semaphore type number Funkce pro dany typ a hodnotu faze vytvori graficke znazorneni semaforu.|#
    
(defun make-semaphore (type number)
  (let* ((items (append (make-box-dotimes type) (make-light-dotimes type))))
    (set-lighting number type items)))

#|selection-items-by-type-recursionn seznam type result  Vybere ze seznamu objekty zadaneho typu. Vraci jejich seznam.|#

(defun selection-items-by-type-recursion (list type result)
  (cond ((eq list '()) result)
        ((typep (car list) type) (selection-items-by-type-recursion (cdr list) type (append result (list (car list)))))
       
        ((typep (car list) 'picture) (selection-items-by-type-recursion (cdr list) type (append result (selection-items-by-type-recursion (items (car list)) type '()))))
        (t (selection-items-by-type-recursion (cdr list) type result))))

#|is-same-lenghtp list number Funkce, ktera vraci t, pokud jsou podseznamy seznamu stejne delky jako zadana hodnota number. Jinak vraci nil.|#

(defun is-same-lengthp (list number)
  (let* ((index (length list))
         (result '()))
         (dotimes (i index)
           (if (equal (length (nth i list)) number) (push t result)))
         (if (equal (length list) (length result)) t nil)))

#| TRIDA SEMAPHORE |#

(defclass semaphore (abstract-picture)
  ((semaphore-type :initform :pedestrian)
   (semaphore-phase :initform 0)
   (items :initform (make-semaphore :pedestrian 0))))


(defmethod semaphore-type ((sem semaphore))
  (slot-value sem 'semaphore-type))


(defmethod phase-count ((sem semaphore))
  (length (nth 1 (finding-type (semaphore-type sem)))))

(defmethod semaphore-phase ((sem semaphore))
  (slot-value sem 'semaphore-phase))

(defmethod color-list ((sem semaphore))
  (nth 0 (finding-type (semaphore-type sem))))

(defmethod program ((sem semaphore))
  (nth 1 (finding-type (semaphore-type sem))))

(defmethod set-semaphore-type ((sem semaphore) type)
  (let* ((semaphore-type (finding-type type)))
    (if semaphore-type
        (progn (setf (slot-value sem 'semaphore-type) type)
         (do-set-items sem  (make-semaphore type 0))
           (set-semaphore-phase sem 0)
         )
      (error "Zadany typ neni defionovan"))) sem)
         

(defmethod set-semaphore-phase ((sem semaphore) value)
  (progn (if (and (>= value 0) (< value (phase-count sem))) (setf (slot-value sem 'semaphore-phase) value)
           (error "Zadana hodnota nen� p��pustn�"))
    (set-lighting value (semaphore-type sem) (items sem) ))sem)

(defmethod next-phase ((sem semaphore))
  (progn (if (= (semaphore-phase sem) (-  (phase-count sem) 1)) (set-semaphore-phase sem 0)
         (set-semaphore-phase sem (+ (semaphore-phase sem) 1)))
  (set-lighting (semaphore-phase sem) (semaphore-type sem) (items sem)))sem)

#| TRIDA CROSSROADS  |#

(defclass crossroads (picture)
  ((crossroads-phase :initform 0)
   (program :initform '())))

(defmethod crossroads-phase ((cros crossroads))
(slot-value cros 'crossroads-phase))

(defmethod phase-count ((cros crossroads))
(length (program cros)))

(defmethod program ((cros crossroads))
(slot-value cros 'program))

(defmethod semaphores ((cros crossroads))
  (selection-items-by-type-recursion (items cros) 'semaphore '() ))

(defmethod set-semaphores-lighting ((cros crossroads))
(let* ((number (crossroads-phase cros))
       (list (nth number (program cros)))
       (index (length list)))
  (dotimes (i index)
    (let* ((actual (nth i (semaphores cros))))
      (progn (set-semaphore-phase actual (nth i list))
          (set-lighting (semaphore-phase actual) (semaphore-type actual) (items actual)))))) cros)   
   
(defmethod set-crossroads-phase ((cros crossroads) value)
  (progn (if (and (>= value 0) (<= value (phase-count cros))) (setf (slot-value cros 'crossroads-phase) value)
    (error "Nep��pustn� hodnota f�ze"))
    )cros)

(defmethod set-program ((cros crossroads) list)
  (if (typep list 'list)
    (if (is-same-lengthp list (length (semaphores cros)))
        (progn (setf (slot-value cros 'program) list)
          (set-crossroads-phase cros 0)
          )
      (error "Program neodpovida poctu semaforu v krizovatce"))
    (error "Program neni typu list"))cros)


(defmethod next-phase ((cros crossroads))
  (progn (if (= (crossroads-phase cros) ( - (phase-count cros) 1)) (set-crossroads-phase cros 0)
           (set-crossroads-phase cros (+ (crossroads-phase cros) 1)))
    (set-semaphores-lighting cros)))


            