;INSTITUTO TECNOLÓGICO DE COSTA RICA
;LENGUAJES DE PROGRAMACIÓN
;TAREA PROGRAMADA #3
;JOSE DANIEL CHACÓN BOGARÍN
;EVELYN MADRIZ MATA

;Se definen las dos listas globales.
(defvar *listaMetadatos* (list))	;Lista que tiene la información de todos los metadatos
(defvar *listaResultados* (list))	;Lista que tiene toda la información de los resultados de cierta búsqueda.


;Función que obtiene todas las direcciones que satisfagan que el tipo de archivo sea mp3 y esté dentro del árbol de direcciones de la dirección dada.
;E: Dirección.
;S: Lista con direcciones.
;R: Ninguna.
(defun obtener-direcciones (dir)
	(directory (concatenate 'string dir "/**/*.mp3"))
)

;Función que obtiene todos los datos de las direcciones obtenidas.
;E: Dirección por buscar
;S: Lista con metadatos cargados o mensaje de error si no se encontró nada.
;R: Ninguna.
(defun obtener-datos (dir)
	(with-open-file (base "BaseDeDatos.txt" :direction :output :if-exists :append :if-does-not-exist :create)		;Se abre el archivo de metadatos. Si no existe, se crea.
		(dolist (i (obtener-direcciones dir))																		;Se itera la lista que se obtiene de la función obtener direcciones.
			(run-shell-command (concatenate 'string "mp3info -p \"%a,%t,%g\" " (namestring i)) :output "Temp.txt")  ;Se usa namestring porque el tipo de datos de i es pathname. Se llama a la función mp3info desde la línea de comandos
			(with-open-file (temp "Temp.txt")																		;Se abre el archivo de stream que se comunica con la línea de comandos.
				(do ((linea (read-line temp nil)																	;Se recorre ese archivo.
							(read-line temp nil)))
						((null linea))
						(if (eql (buscarEnLista (splitN linea (list) "") *listaMetadatos*) nil)						;Se revisa que los datos no existan en la lista de metadatos
							(escribirAgregar linea base)
						)
				)		
			)
		)
	)
	(if (eql *listaMetadatos* nil)		;Se verifica que hayan habido resultados.
		(sinResultado T)
		(conResultado)
	)
)

;Se agrega la información de los metadatos a la lista y al archivo de base de datos.
;E: String y un stream.
;S: Ninguna.
;R: Ninguna.
(defun escribirAgregar (str archivo)
	(defparameter *listaMetadatos* (append *listaMetadatos* (cons (splitN str (list) "") nil)))
	(format archivo (concatenate 'string str "~%"))
)

;Se carga la base de datos anterior (BaseDeDatos.txt). Si no existe retorna que no hay base de datos para cargar.
;E: Ninguna.
;S: Mensaje de carga satisfactoria o no.
;R: Ninguna. 
(defun cargarBaseAnterior ()
	(with-open-file (base "BaseDeDatos.txt" :if-exists :append :if-does-not-exist nil)							;Se abre el archivo.
		(if (eql base NIL)																						;Se compara que el stream sea nulo. Si es nulo es porque no existía entonces se da un mensaje de error.
			(break)																								
			(do ((linea (read-line base nil)																	;Se lee el archivo línea por línea.
						(read-line base nil)))
				((null linea))
				(defparameter *listaMetadatos* (append *listaMetadatos* (cons (splitN linea (list) "") nil)))	;Se cargan los metadatos a la lista global.
			)
		)
	)
)
					

;Función que busca si la información de una canción ya se encuentra en la lista de metadatos.
;E: Elemento y la lista para buscar.
;S: T si está o NIL si no está.
;R: Ninguna.
(defun buscarEnLista (elemento lista)
	(dolist (i lista)
		(if (equal elemento i)
			(return-from buscarEnLista T)
		)	
	)
	(return-from buscarEnLista nil)
)

;Función split que separa los datos de un string por comas y los mete en una lista.
;E: String, lista y string vacío.
;S: Lista con el string separado.
;R: Ninguna.
(defun splitN (str lista arrastre)
	(if (equal (subseq str 0) "") 																;Se verifica si el string está vacío.
		(return-from splitN (append lista (list arrastre)))										;Se retorna la lista.
		(if (eql (char str 0) #\,)																;Se analiza si el caracter es una coma (para separar)
			(splitN (subseq str 1) (append lista (list arrastre)) "")							;Si es coma, entonces se mete arrastre a la lista (lo que está antes de la coma) y se vacía el arrastre
			(splitN (subseq str 1) lista (concatenate 'string arrastre (string(char str 0))))	;Si no es coma, se concatena el caracter a arrastre y se llama a la misma función sin el primer caracter del string original.
		)
	)
)

;Función que busca en los metadatos con autor como criterio.
;E: String del autor por buscar.
;S: Datos desplegados en la tabla.
;R; Ninguna.
(defun buscarPorAutor (autor)
	(defparameter *listaResultados* (list))												;Se vacía la lista de resultados.
	(dolist (i *listaMetadatos*)														;Se itera la lista de resultados
		(if (not (equal (search autor (first i) :test #'char-equal) nil))				;Se busca si el criterio se encuentra dentro de la parte de autor.
			(defparameter *listaResultados* (append *listaResultados* (cons i nil )))	;Se agregan todos los datos a la lista de resultados sí hubo coincidencia.
		)
	)
	(if (eql *listaResultados* nil)			;Si la lista quedó vacía, se despliega que no hubo resultados.
		(sinResultado nil)		
		(hacerTabla *listaResultados*)		;Si hubo resultados, se llama la función hacer tabla.
	)
)

;Función que busca en los metadatos con título de canción como criterio.
;E: String de la canción por buscar.
;S: Datos desplegados en la tabla.
;R; Ninguna.
(defun buscarPorTitulo (titulo)
	(defparameter *listaResultados* (list))
	(dolist (i *listaMetadatos*)
		(if (not (equal (search titulo (second i) :test #'char-equal) nil))				;Se busca si el criterio se encuentra dentro de la parte de canción.
			(defparameter *listaResultados* (append *listaResultados* (cons i nil )))
		)
	)
	(if (eql *listaResultados* nil)
		(sinResultado nil)
		(hacerTabla *listaResultados*)
	)
)

(defun buscarPorGenero (genero)
	(defparameter *listaResultados* (list))
	(dolist (i *listaMetadatos*)
		(if (not (equal (search genero (third i) :test #'char-equal) nil))				;Se busca si el criterio se encuentra dentro de la parte de género.
			(defparameter *listaResultados* (append *listaResultados* (cons i nil )))
		)
	)
	(if (eql *listaResultados* nil)
		(sinResultado nil)
		(hacerTabla *listaResultados*)
	)
)

;Función que agrega espacios para hacer el formato de tabla.
;E: String y el largo del string máximo de la columna.
;S: String con espacios.
;R: Ninguna.	
(defun agregarEspacios (str largo)
	(let ((strTemp str))										;Se define la variable temporal.
		(dotimes (i (- largo (length str)))						;Se itera la cantidad de veces igual a la resta del largo máximo de la columna, con el largo del string.
			(setf strTemp (concatenate 'string strTemp " "))	;Se agrega un espacio por cada iteración.
		)
		(return-from agregarEspacios strTemp)					;Se retorna el string.
	)
)

;Función que obtiene el largo máximo de la columna dada (definida por la función enviada por parámetro).
;E: Función y la lista por buscar el máximo.
;S: Entero con el número máximo.
;R: Ninguna.
(defun obtenerMaximo (fn lista)
	(let ((tempMax 0))
		(dolist (i lista)
			(if (> (length (funcall fn i)) tempMax)
				(setf tempMax (length (funcall fn i)))
			)
		)
	(return-from obtenerMaximo tempMax)
	)
)

;Interfaz Grafica de usuario
(load "ltk")	;Se carga la librería ltk.

(defun ventana-principal()
	(ltk::with-ltk ()
	
		(ltk::wm-title ltk::*tk*"Sistema de manejo de informacion de archivos de música en mp3")	;Título de la ventana.

		;Frame, texto y scroll para ventana de respuestas

		(defvar *frame* (make-instance 'ltk::frame))												;Se define el frame principal.

		(defvar *texto* (make-instance 'ltk::text :master *frame* 									;Se define el cuadro de texto donde se despliegan resultados.
					:width 60 :height 30
					:wrap "none" ))

		(defvar *scrolly* (make-instance 'ltk::scrollbar :master *frame*))							;Se define un scroll vertical.
		
		(ltk::configure *texto* 
			:yscrollcommand 
			(concatenate 'string (ltk::widget-path *scrolly*) " set"))								;Se configura el scroll al cuadro de texto.
			
		(ltk::configure *scrolly* 
			:command 
			(concatenate 'string (ltk::widget-path *texto*) " yview"))
			
		(defvar *scrollx* (make-instance 'ltk::scrollbar :master *frame*							;Se define un scroll horizontal.
					 :orientation "horizontal"))
					 
		(ltk::configure *texto* 																	;Se configura el scroll horizontal al cuadro.
			:xscrollcommand 
			(concatenate 'string (ltk::widget-path *scrollx*) " set"))
			
		(ltk::configure *scrollx* 
			:command 
			(concatenate 'string (ltk::widget-path *texto*) " xview"))
			
		(ltk::grid *texto* 0 0 :sticky "snew")														;Se coloca el cuadro de texto y scrolls.
		
		(ltk::grid *scrolly* 0 1 :sticky "ns")
		
		(ltk::grid *scrollx* 1 0 :sticky "ew")
		
		
		;fram para botones, entrys y labels
		(defvar *frame1* (make-instance 'ltk::frame))												;Se define el frame de los botones.
		
		(ltk::grid *frame1* 0 0)
		
		(ltk::grid *frame* 0 1)

		(defvar *boton1* (make-instance 'ltk::button 												;Botón para cargar dirección.
			:Text "Cargar"
			:master *frame1*
			:command (lambda () (obtener-datos (ltk::text *entry1*))))
		)

		(defvar *label1* (make-instance 'ltk::label 												;Label de información
			:Text "  Ingrese la ruta
Por ejemplo: /home/evelyn/Escritorio"
			:master *frame1*)
		)

		(defvar *entry1* (make-instance 'ltk::entry 												;Entry para la dirección
			:master *frame1*
			:width 25))

		(ltk::grid *label1* 0 1 :Sticky "e")														;Ubicación de botón, label y entry
		(ltk::grid *boton1* 1 0 :Sticky "n")
		(ltk::grid *entry1* 1 1 :Sticky "n")


		(defvar *boton2* (make-instance 'ltk::button 												;Definición de botón para visualizar todo.
			:Text "Visualizar todos los archivos de mp3"
			:master *frame1*
			:command (lambda () (hacerTabla *listaMetadatos*))))
		(ltk::grid *boton2* 3 0 :columnspan 2 :Sticky "s" )
		
		;Seccion BUsqueda
		(defvar *labelB* (make-instance 'ltk::label 												;Label de información
			:Text "Realizar busqueda por:"
			:master *frame1*)
		)
		
		(ltk::grid *labelB* 5 0 :columnspan 2 :Sticky "w")

		(defvar *boton3* (make-instance 'ltk::button 												;Botón para buscar por título de canción
			:Text "Titulo"
			:master *frame1*
			:command (lambda () 
			(buscarPorTitulo (ltk::text *entry3*))))
		)
			
		(defvar *entry3* (make-instance 'ltk::entry 												;Entry para ingresar la canción
			:master *frame1*
			:width 25)
		)
		
		(ltk::grid *boton3* 7 0 :Sticky "n")														;Ubicación de botones y entries anteriores.
		
		(ltk::grid *entry3* 7 1 :Sticky "n")

		(defvar *boton4* (make-instance 'ltk::button 												;Botón para buscar por artista.
			:Text "Artista"
			:master *frame1*
			:command (lambda () 
			(buscarPorAutor (ltk::text *entry4*))))
		)
		
		(defvar *entry4* (make-instance 'ltk::entry 												;Entry para el artista.
			:master *frame1*
			:width 25)
		)
			
		(ltk::grid *boton4* 9 0 :Sticky "n")														;Ubicación
		
		(ltk::grid *entry4* 9 1 :Sticky "n")

		(defvar *boton5* (make-instance 'ltk::button 												;Botón para buscar por género
			:Text "Genero"
			:master *frame1*
			:command (lambda () 
			(buscarPorGenero (ltk::text *entry5*))))
		)
		
		(defvar *entry5* (make-instance 'ltk::entry 												;Entry para el género
			:master *frame1*
			:width 25)
		)
		
		(ltk::grid *boton5* 11 0 :Sticky "n")														;Ubicación 
		
		(ltk::grid *entry5* 11 1 :Sticky "n")
		
		(cargarBaseAnterior)																		;Llamada a carga de base de datos.
		(if (eql *listaMetadatos* nil)																;Se verifica si la carga de base de datos quedó vacía
			(ltk::append-text *texto* "¡No se encontró base de datos!")								;Avisos de éxito o fracaso.
			(ltk::append-text *texto* "¡Se cargó base de datos previa!")
		)
	)
	
)

;Función que hace la tabla
;E: Lista
;S: Tabla de resultados desplegada en pantalla
;R: Ninguna
(defun hacerTabla (lista)
	(ltk::clear-text *texto*)										;Se vacía el cuadro de texto.
	(if (eql lista nil)												;Se verifica si la lista es vacía
		(ltk::append-text *texto* "Debe definir un directorio.")	;Mensaje de error.
		(hacerTablaAux lista)										;Llamada a función auxiliar.
	)
)	

;Función auxiliar de tabla
;E: Lista
;S: Datos desplegados
;R: Ninguna.
(defun hacerTablaAux (lista)
	(ltk::clear-text *texto*)
	(let ((lenArtista (+ 3 (obtenerMaximo #'first lista))))			;Se obtiene el máximo del artista
		(let ((lenCancion (+ 3 (obtenerMaximo #'second lista))))	;Se obtiene el máximo de la canción
			(ltk::append-text *texto* (concatenate 'string (agregarEspacios "Artista" lenArtista) (agregarEspacios "Canción" lenCancion) "Género"))	;Se ponen los cabezados de la tabla con su cantidad respectiva de espacios
			(ltk::append-newline *texto*)  ;Nueva línea
			(ltk::append-text *texto* (poner-lineas (+ lenArtista lenCancion 6))) ;Se pone una línea divisoria
			(ltk::append-newline *texto*)	;Nueva línea
			(dolist (i lista)	;Se itera la lista
				(ltk::append-text *texto* (concatenate 'string (agregarEspacios (first i) lenArtista) (agregarEspacios (second i) lenCancion) (third i))) ;Se despliega la información de un artista
				(ltk::append-newline *texto*)	;Nueva línea
			)
		)
	)				
)

;Función que pone línea divisoria
;E: Cantidad de líneas por poner
;S: Línea divisoria desplegada en pantalla.
;R: Ninguna
(defun poner-lineas (suma)
	(let ((strTemp ""))
		(dotimes (i suma)
			(setf strTemp (concatenate 'string strTemp "-"))
		)
		(return-from poner-lineas strTemp)
	)
)

;Función que despliega información
;E: Booleano para indica el problema
;S: Problema desplegado en pantalla
;R: Ninguna
(defun sinResultado (cod)
	(ltk::clear-text *texto*)
	(if cod
		(ltk::append-text *texto* (concatenate 'string "¡No se encontraron mp3 en el directorio" (ltk::text *entry1*) "!"))
		(ltk::append-text *texto* "¡No se encontraron resultados!")
	)
)

;Función que despliega información
;E: Ninguna.
;S: Información desplegada en pantalla
;R: Ninguna.
(defun conResultado ()
	(ltk::clear-text *texto*)
	(ltk::append-text *texto* (concatenate 'string "Directorio cargado con éxito. Se encontraron " (write-to-string (length *listaMetadatos*)) " canciones."))
)
(ventana-principal) ;Llamada a función de la ventana para enseñar la interfaz


