;;;; Msellek Sanaa 902325
;;;; Linguaggi di programmazione - Progetto Lisp
;;;; Queste prime tre istruzioni sono prese dal pdf
;;;; e verranno usate dalle principali funzioni del progetto
;;;;
;;;; Le classi diventano delle variabili globali con una hash-table
;;assocciata


(defparameter *classes-specs* (make-hash-table))

(defun add-class-spec (class class-spec)
  (setf (gethash class *classes-specs*) class-spec))

(defun class-spec-parents (class)
  (car (gethash class *classes-specs*)))

(defun class-spec-fields (class)
  (cadr (gethash class *classes-specs*)))

(defun class-spec-methods (class)
  (caddr (gethash class *classes-specs*)))

;; Definisce una nuova classe e memorizza le sue specifiche
;;nella tabella hash *classes-specs*.
(defun def-class (class-name parents-list &rest part)
  (when (is-class class-name)
    (error "Classe già."))

  (unless (parents-exist parents-list)
    (error "Una o piu classi parent non sono definite."))

  (add-class-spec class-name
                  (list parents-list
                        (inherit-field-type
                         (get-fields part)
                         (inherite-fields-from-parents parents-list))
                        (get-methods part)))
  class-name)

(defun parents-exist (parents-list)
  (every #'is-class parents-list))

;; Crea un'istanza di una classe con i valori specificati.
(defun make (class-name &rest value)
  (cond ((and (= (length value) 1)
              (listp (car value))
              (= (length (car value)) 3)
              (eq (caar value) 'oolinst)
              (is-subtype-of (cadar value) class-name))
         (if (is-subtype-of (cadar value) class-name)
             (list 'oolinst class-name (caddar value))
             (error "value non valido per questa classe.")))
        ((is-class class-name)
         (list 'oolinst
               class-name
               (update-fields (append-fields-lists
                               (class-spec-fields class-name)
                               (inherite-fields-from-parents
                                (class-spec-parents class-name)))
                              value)))
        ((and (eq class-name 'T)
              (= (length value) 1))
         (list 'oolinst 'T (car value)))
        ((and (= (length value) 1)
              (subtypep (type-of (car value)) class-name))
         (list 'oolinst class-name (car value)))
        (t (error "value non valido per questo tipo di classe."))))



;; Controlla se un simbolo rappresenta una classe definita.
(defun is-class (class-name)
  (if (gethash class-name *classes-specs*)
      t
      nil))
;; Controlla se un'istanza data è di una classe specificata.
(defun is-instance (instance &optional (class 'T))
  (if (and (listp instance)
           (equal (length instance) 3)
           (equal (car instance) 'oolinst)
           (is-subtype-of (cadr instance) class))
      (if (is-subtype-of (cadr instance) class)
          (is-instance-helper (cddr instance))
          (is-subtype-of (type-of (caddr instance)) (cadr instance)))
      (is-subtype-of (type-of instance) class)))


;; Funzione ausiliaria per validare in modo ricorsivo
;;i campi in un'istanza.
(defun is-instance-helper (fields-list)
  (if (= (length fields-list) 1)
      (is-instance (car fields-list))
      (and (is-instance (car fields-list))
           (is-instance-helper (cdr fields-list)))))

;; Recupera il valore di un campo specificato in un'istanza.
(defun field (instance field)
  (if (and (is-instance instance)
           (listp instance)
           (= (length instance) 3)
           (equal (car instance) 'oolinst))
      (if (field-in-fields-list (symbol-name field) (caddr instance))
          (field-in-fields-list (symbol-name field) (caddr instance))
          (error "field non presente."))
      (error "Istanza prevista come parametro!")))

;; Recupera i valori di campi specificati in un'istanza.
(defun fields (instance &rest fields-name-list)
  (if (and (is-instance instance)
           (listp instance)
           (= (length instance) 3)
           (equal (car instance) 'oolinst))
      (if fields-name-list
          (cond ((= (length fields-name-list) 1)
                 (field instance (car fields-name-list)))
                (t (apply #'fields
                          (field instance (car fields-name-list))
                          (cdr fields-name-list))))
          (error "La lista dei field names è vuota"))
      (error "Istanza non valida!")))

;; Funzione ausiliaria per validare e processare in modo ricorsivo
;; i campi.
(defun recursive-validate-fields (fields)
  (if (null fields)
      '()
      (append (list (validate-field (car fields)))
              (recursive-validate-fields (cdr fields)))))

;; Funzione ausiliaria che valida un'istanza controllandone la
;;struttura e i tipi.
(defun validate-instance (instance)
  (and (listp instance)
       (= (length instance) 3)
       (equal (car instance) 'oolinst)))

;; Funzione ausiliaria che valida un campo e restituisce il suo nome
;;e il tipo.
(defun validate-field (field)
  (cond ((= (length field) 2)
         (list (symbol-name (car field)) (make 'T (cadr field))))
        ((= (length field) 3)
         (list (symbol-name (car field)) (make (caddr field)
					       (cadr field))))))

;; Estrae e valida i campi dalla parte 'part' di def-class.
(defun get-fields (part)
  (when part
    (if (equal (caar part) 'fields)
        (append (recursive-validate-fields (cdar part))
                (get-fields (cdr part)))
        (get-fields (cdr part)))))

;; Eredita i tipi di campo dalle classi genitori.
(defun inherit-field-type (fields-list parents-fields-list)
  (cond ((null fields-list) '())
        ((null parents-fields-list) fields-list)
        ((and (equal (caar fields-list) (caar parents-fields-list))
              (equal (cadadr (car fields-list)) 'T))
         (if (is-instance (list 'oolinst
                                (cadadr (car parents-fields-list))
                                (caddr (cadr (car fields-list)))))
             (let ((new-field (list (caar fields-list)
                                    (list 'oolinst
					  (cadadr
					   (car parents-fields-list))
				          (caddr
					   (cadr
					    (car fields-list)))))))
               (if (is-instance new-field)
                   (cons new-field
			 (inherit-field-type (cdr fields-list)
					     
					     (cdr parents-fields-list
						  )))
                   (error "Valore non valido per un field")))
             (error "Valore non valido per un field")))
        ((equal (caar fields-list) (caar parents-fields-list))
         (if (is-subtype-of (cadadr (car fields-list))
                            (cadadr (car parents-fields-list)))
             (let ((new-field (list (car fields-list))))
               (if (is-instance new-field)
                   (cons new-field (inherit-field-type
				    (cdr fields-list)
				    (cdr parents-fields-list)))
                   (error "Valore di field non valido")))
             (error "Un field è supertype di uno ereditato")))
        (t (append (inherit-field-type (list (car fields-list))
                                       (cdr parents-fields-list))
                   (inherit-field-type (cdr fields-list)
                                       parents-fields-list)))))

;; Concatena due liste di campi, evitando duplicati.
(defun append-fields-lists (list1 list2)
  (if list2
      (if (not (field-in-fields-list (caar list2) list1))
          (append-fields-lists (append list1 (list (car list2)))
                               (cdr list2))
          (append-fields-lists list1 (cdr list2)))
      list1))




;; Estrae e valida i metodi dalla parte 'part' di def-class.
(defun get-methods (part)
  (cond ((null part) '())
        ((equal (caar part) 'methods)
         (append (recursive-process-methods (cdar part))
                 (get-methods (cdr part))))
        (t (get-methods (cdr part)))))

;; Funzione ausiliaria per processare e definire i metodi.
(defun recursive-process-methods (methods)
  (if (null methods)
      '()
      (append (list (process-method (caar methods) (cdar methods)))
              (recursive-process-methods (cdr methods)))))

;; Definisce un metodo e la sua funzione lambda associata.
(defun process-method (method-name method-spec)
  (setf (fdefinition method-name)
        (lambda (this &rest value)
          (apply (eval (invoke-method this method-name))
                 (append (list this) value))))
  (rewrite-method-code method-name method-spec))

;; Modifica il codice di un metodo per utilizzare l'argomento 'this'.
(defun rewrite-method-code (method-name method-spec)
  (list method-name (append (list 'lambda)
                            (list (append (list 'this)
					  (car method-spec)))
                            (cdr method-spec))))

;; Invoca un metodo su un'istanza, controllandone l'esistenza.
(defun invoke-method (instance method-name)
  (cond ((method-in-methods-list method-name
                                 (class-spec-methods
				  (cadr instance))))
        ((parents-method (class-spec-parents (cadr instance))
			 method-name))
        (t (error "Il metodo non esite!"))))

;; Aggiorna i campi in un'istanza con nuovi valori.
(defun update-fields (old-fields value)
  (cond ((null value) old-fields)
        ((field-in-fields-list (symbol-name (car value)) old-fields)
         (update-fields (update-field old-fields
                                      (list (car value) (cadr value)))
                        (cddr value)))
        (t (error "Errore nello specificare i field da caricare."))))

;; Aggiorna un campo specifico in una lista di campi con un nuovo
;;valore.
(defun update-field (old-fields field)
  (cond ((null old-fields) (error "Field non trovato"))
        ((equal (caar old-fields) (symbol-name (car field)))
         (append (list (list (caar old-fields)
                             (make (cadr (cadar old-fields))
				   (cadr field))))
                 (cdr old-fields)))
        (t (append (list (car old-fields))
                   (update-field (cdr old-fields) field)))))

;; Eredita i campi dalle classi genitori.
(defun inherite-fields-from-parents (parents-list)
  (if (not (null parents-list))
      (append-fields-lists
       (append-fields-lists (class-spec-fields (car parents-list))
                            (inherite-fields-from-parents
                             (class-spec-parents (car parents-list))))
       (inherite-fields-from-parents (cdr parents-list)))
      '()))




;; Controlla se una classe è un sottotipo di un'altra classe.
(defun is-subtype-of (subclass parent-class)
  (cond ((equal parent-class 'T) t)
        ((equal subclass parent-class) t)
        ((is-derivated-class (list subclass) parent-class) t)
        ((and (not (is-class subclass))
              (not (is-class parent-class))
              (subtypep subclass parent-class))
         t)))

;; Controlla se una lista di classi include una classe derivata.
(defun is-derivated-class (derivated-classes parent-class)
  (cond ((null derivated-classes) nil)
        ((equal (car derivated-classes) parent-class) t)
        ((is-derivated-class (class-spec-parents
			      (car derivated-classes))
                             parent-class) t)
        ((is-derivated-class (cdr derivated-classes) parent-class)
	 t)))

;; Controlla se un metodo è definito nella lista di metodi.
(defun parents-method (parents-list method-name)
  (cond ((null parents-list) nil)
        ((method-in-methods-list method-name
                                 (class-spec-methods
				  (car parents-list))))
        ((parents-method (class-spec-parents (car parents-list))
			 method-name))
        (t (parents-method (cdr parents-list) method-name))))

;; Controlla se un metodo è definito nella lista di metodi.
(defun method-in-methods-list (method-name methods-list)
  (cond ((null methods-list) nil)
        ((equal method-name (caar methods-list)) (cadar methods-list))
        (t (method-in-methods-list method-name (cdr methods-list)))))

;; Controlla se un campo è presente nella lista di campi.
(defun field-in-fields-list (field fields-list)
  (cond ((null fields-list) nil)
        ((equal field (caar fields-list))
         (if (is-class (cadr (cadar fields-list)))
             (cadar fields-list)
             (caddr (cadar fields-list))))
        (t (field-in-fields-list field (cdr fields-list)))))
