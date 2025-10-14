################################################################################
# Proyecto = Sistema de Afores                                                 #
# Owner    = E.F.P                                                             #
# Programa = DISM002                                                           #
# Fecha    = 25/07/1997                                                        #
# By       = Juan Colin                                                        #
# Sistema  = Dispersion                                                        #
################################################################################
DATABASE safre_af
GLOBALS
    DEFINE arr_c        SMALLINT
    DEFINE scr_l        SMALLINT

    DEFINE
        aux_pausa       CHAR(1)

    DEFINE
        g_usuario       CHAR(8),
        g_nombres         CHAR(100),
        HOY             DATE,
        HORA            CHAR(5)

    DEFINE x_reg RECORD
          LIKE  aporte_ventanilla.*

    DEFINE sub RECORD
     paterno          LIKE  afi_mae_afiliado.paterno,
     materno          LIKE  afi_mae_afiliado.materno,
     nombres          LIKE  afi_mae_afiliado.nombres
    END RECORD

    DEFINE g_afore RECORD
          LIKE  tab_afore_local.*

    DEFINE
        x_desc		CHAR(60)
END GLOBALS

MAIN
        OPTIONS PROMPT LINE LAST,
                INPUT WRAP ,
                ACCEPT KEY CONTROL - I ,
                COMMENT LINE LAST
        DEFER INTERRUPT

	OPEN WINDOW v1 AT 3,2 WITH FORM "DISM0021" ATTRIBUTE(BORDER)
	DISPLAY "CTRL-C (SALIR)       APORTACIONES POR VENTANILLA                           " AT 3,1 ATTRIBUTE(REVERSE)
         SELECT *,USER into g_afore.*,g_usuario from tab_afore_local

	MENU "APORTE_VENTANILLA"
	    COMMAND "Agrega" "Agrega APORTE_VENTANILLA"
	             CALL Agrega()
	             CALL Inicializa()
	    COMMAND "Consulta" "Consulta APORTE_VENTANILLA"
	             CALL Consulta()
	    COMMAND "Modifica" "Modifica APORTE_VENTANILLA"
	             CALL Modifica()
	    COMMAND "Salir"    "Salir de Programa"
	             EXIT PROGRAM
	END MENU
	CLOSE WINDOW v1
END MAIN
###############################################################################
FUNCTION Agrega()
#-----------------
        LET x_reg.fecha_pago = TODAY
        LET x_reg.fecha_valor = x_reg.fecha_pago + 1
        LET x_reg.fecha_recep = x_reg.fecha_pago + 2

    INPUT BY NAME x_reg.nss , x_reg.curp , x_reg.monto_en_pesos,
                  x_reg.fecha_pago ,
                  x_reg.fecha_valor , x_reg.entidad_rec , 
                  x_reg.fecha_recep   WITHOUT DEFAULTS

AFTER FIELD nss
     IF x_reg.nss IS NULL THEN
            ERROR "No puede ser nulo el NSS" 
            NEXT FIELD nss
     ELSE
        SELECT n_unico,paterno ,materno ,nombres
         INTO x_reg.curp,sub.paterno,sub.materno ,sub.nombres
         FROM afi_mae_afiliado
         WHERE n_seguro = x_reg.nss

         IF STATUS = NOTFOUND THEN 
            ERROR "No existe afiliado con este NSS"
            NEXT FIELD nss
         ELSE 
           LET g_nombres = sub.paterno CLIPPED, " ", sub.materno CLIPPED , " ",
                          sub.nombres CLIPPED
            DISPLAY BY NAME g_nombres
            DISPLAY BY NAME x_reg.curp
         END IF
     END IF

#	AFTER FIELD curp
         #IF x_reg.curp IS NULL THEN
            #ERROR "No puede ser nulo el CURP" 
            #NEXT FIELD curp
         #END IF

	AFTER FIELD monto_en_pesos
         IF x_reg.monto_en_pesos IS NULL THEN
            ERROR "No puede ser nulo el Monto en Pesos" 
            NEXT FIELD monto_en_pesos
         END IF

        
	AFTER FIELD fecha_pago
         IF x_reg.fecha_pago IS NULL THEN
            ERROR "No puede ser nula la Fecha Pago" 
            NEXT FIELD fecha_pago
         END IF


	AFTER FIELD fecha_valor
         IF x_reg.fecha_valor IS NULL THEN
            ERROR "No puede ser nula la Fecha Valor" 
            NEXT FIELD fecha_valor
         END IF

	AFTER FIELD entidad_rec
         IF x_reg.entidad_rec IS NULL THEN
            ERROR "No puede ser nula la Entidad Recaudadora" 
            NEXT FIELD entidad_rec
         END IF


	AFTER FIELD fecha_recep
         IF x_reg.fecha_recep IS NULL THEN
            ERROR "No puede ser nula la Fecha Recepcion" 
            NEXT FIELD fecha_recep
         END IF

          
        ON KEY ( INTERRUPT )
          EXIT INPUT
          CALL Inicializa()

        ON KEY ( ESC )
         IF x_reg.nss IS NULL THEN
            ERROR "No puede ser nulo el NSS" 
            NEXT FIELD nss
         END IF

         IF x_reg.curp IS NULL THEN
            ERROR "No puede ser nulo el CURP " 
            NEXT FIELD curp
         END IF

         IF x_reg.monto_en_pesos IS NULL THEN
            ERROR "No puede ser nulo el Monto en Pesos " 
            NEXT FIELD monto_en_pesos
         END IF

         IF x_reg.entidad_rec IS NULL THEN
            ERROR "No puede ser nula la Entidad Recaudadora" 
            NEXT FIELD entidad_rec
         END IF

         IF x_reg.fecha_recep IS NULL THEN
            ERROR "No puede ser nula la Fecha Recepcion " 
            NEXT FIELD fecha_recep
         END IF

         LET HORA = TIME 
         LET HORA = HORA [1,5]
         LET HOY  = TODAY

         ERROR "INSERTANDO DATOS"

         INSERT INTO aporte_ventanilla values ( x_reg.nss , 
                                                x_reg.curp ,
                                                x_reg.monto_en_pesos ,
                                                x_reg.fecha_pago ,
                                                x_reg.fecha_valor ,
                                                x_reg.entidad_rec ,
                                                x_reg.fecha_recep ,
                                                g_usuario ,
                                                HOY ,
                                                HORA   ,
                                                0 )
       EXIT INPUT
    END INPUT
 END FUNCTION
###############################################################################
FUNCTION Consulta()
#-----------------
    DEFINE l_record  ARRAY[3000] OF RECORD
      curp    LIKE aporte_ventanilla.curp,
      monto_en_pesos LIKE aporte_ventanilla.monto_en_pesos,
      fecha_pago  LIKE aporte_ventanilla.fecha_pago,
      fecha_valor LIKE aporte_ventanilla.fecha_valor, 
      entidad_rec LIKE aporte_ventanilla.entidad_rec,
      rowid        SMALLINT
    END RECORD
     
    DEFINE
        pos             SMALLINT
     OPEN WINDOW vent_2 AT 03,02 WITH FORM "DISM0022" ATTRIBUTE(BORDER)
      DISPLAY "CTRL-C (SALIR)            APORTACIONES POR VENTANILLA                       "AT 2,1 ATTRIBUTE(REVERSE,BOLD)
    INPUT BY NAME x_reg.nss WITHOUT DEFAULTS

	AFTER FIELD nss
	    IF x_reg.nss IS NULL THEN
	        ERROR "NUMERO DE SEGURO SOCIAL NO EXISTE"
		NEXT FIELD nss
            ELSE
                SELECT n_unico,paterno ,materno ,nombres
                 INTO x_reg.curp,sub.paterno,sub.materno ,sub.nombres
                 FROM afi_mae_afiliado
                 WHERE n_seguro = x_reg.nss

                 IF STATUS = NOTFOUND THEN 
                    ERROR "No existe afiliado con este NSS"
                    NEXT FIELD nss
                 ELSE 
                   LET g_nombres = sub.paterno CLIPPED, " ", sub.materno CLIPPED , " ",
                          sub.nombres CLIPPED
                    DISPLAY BY NAME g_nombres
                    DISPLAY BY NAME x_reg.curp
               
	          END IF
	    END IF
        EXIT INPUT
     END INPUT

	    DECLARE cur1 CURSOR FOR
	    SELECT 
	           curp  ,
 		   monto_en_pesos          ,
		   fecha_pago ,
		   fecha_valor ,
		   entidad_rec ,
                   rowid
	    FROM   aporte_ventanilla
	    WHERE  nss = x_reg.nss

            LET pos = 1
	    FOREACH cur1 INTO l_record[pos].*
                LET pos = pos + 1
	    END FOREACH

	    IF (pos-1) < 1  THEN
                ERROR "NUMERO DE SEGURO SOCIAL NO EXISTE"
	    END IF
            CALL SET_COUNT(pos-1)
	    DISPLAY ARRAY l_record TO scr_1.*
               ON KEY (INTERRUPT)
               EXIT DISPLAY 
            END DISPLAY
            close window vent_2
            CLEAR SCREEN
END FUNCTION

FUNCTION Inicializa()
#--------------------
    INITIALIZE x_reg.* TO NULL
    INITIALIZE g_nombres TO NULL
    DISPLAY BY NAME         g_nombres
    DISPLAY BY NAME         x_reg.nss, 
                            x_reg.curp , 
                            x_reg.monto_en_pesos , 
                            x_reg.fecha_pago , 
                            x_reg.fecha_valor , 
                            x_reg.entidad_rec , 
                            x_reg.fecha_recep  
END FUNCTION
FUNCTION Inicializa1()
#--------------------
    INITIALIZE x_reg.nss TO NULL
    DISPLAY BY NAME         
                            x_reg.nss  
END FUNCTION

FUNCTION Presione_enter()
#-----------------------
	PROMPT "Presione < ENTER > para otra Consulta" FOR CHAR aux_pausa
END FUNCTION

FUNCTION Modifica()
#-----------------------
    DEFINE l_record  ARRAY[3000] OF RECORD
      curp            LIKE aporte_ventanilla.curp,
      monto_en_pesos  LIKE aporte_ventanilla.monto_en_pesos,
      fecha_pago      LIKE aporte_ventanilla.fecha_pago,
      fecha_valor     LIKE aporte_ventanilla.fecha_valor, 
      entidad_rec     LIKE aporte_ventanilla.entidad_rec,
      rowid           SMALLINT
    END RECORD
    DEFINE
        pos             SMALLINT,
        i               SMALLINT

     OPEN WINDOW vent_2 AT 03,02 WITH FORM "DISM0022" ATTRIBUTE(BORDER)
      DISPLAY "CTRL-C (SALIR)            APORTACIONES POR VENTANILLA                       "AT 2,1 ATTRIBUTE(REVERSE,BOLD)

    INPUT BY NAME x_reg.nss  WITHOUT DEFAULTS
        AFTER FIELD nss
            IF x_reg.nss IS NULL THEN
               ERROR "No puede ser nulo NSS"
               NEXT FIELD nss
            ELSE
                SELECT n_unico,paterno ,materno ,nombres
                 INTO x_reg.curp,sub.paterno,sub.materno ,sub.nombres
                 FROM afi_mae_afiliado
                 WHERE n_seguro = x_reg.nss

                 IF STATUS = NOTFOUND THEN 
                    ERROR "No existe afiliado con este NSS"
                    NEXT FIELD nss
                 ELSE 
                   LET g_nombres = sub.paterno CLIPPED, " ", sub.materno CLIPPED , " ",
                          sub.nombres CLIPPED
                    DISPLAY BY NAME g_nombres
                    DISPLAY BY NAME x_reg.curp
               
	          END IF
            END IF


	    DECLARE cur2 CURSOR FOR
	    SELECT 
	           curp  ,
 		   monto_en_pesos          ,
		   fecha_pago ,
		   fecha_valor ,
		   entidad_rec ,
		   rowid  
	    FROM   aporte_ventanilla
	    WHERE  nss = x_reg.nss

            LET pos = 1
	    FOREACH cur2 INTO l_record[pos].*
                LET pos = pos + 1
	    END FOREACH

	    IF (pos-1) < 1  THEN
                ERROR "NUMERO DE SEGURO SOCIAL NO EXISTE"
                NEXT FIELD nss
	    END IF

            CALL SET_COUNT(pos-1)
	    DISPLAY ARRAY l_record TO scr_1.*
               ON KEY (INTERRUPT)
               LET pos = ARR_CURR()
               EXIT DISPLAY 
               CLOSE WINDOW vent_2   
               clear screen
               ON KEY (CONTROL -M )
               LET pos = ARR_CURR()
               EXIT DISPLAY 
            END DISPLAY
          EXIT INPUT
        END INPUT          

           DISPLAY "" AT 1,1                                                   
           DISPLAY "" AT 2,1                                                   
           DISPLAY " ( Esc ) Modifica        (Ctrl-C) Salir " AT 1,1 ATTRIBUTE(BOLD) 
    
       DISPLAY " MODIFICA " AT 1,67 ATTRIBUTE(REVERSE,BOLD)                

           
	    INPUT ARRAY l_record WITHOUT DEFAULTS FROM scr_1.* 

        #	BEFORE FIELD curp
                  #LET arr_c = ARR_CURR()
                  #LET scr_l = SCR_LINE()

        	#AFTER FIELD curp
                 #IF l_record[pos].curp IS NULL THEN
                    #ERROR "No puede ser nulo el CURP" 
                    #NEXT FIELD curp
                 #END IF
       
        	BEFORE FIELD monto_en_pesos
                  LET arr_c = ARR_CURR()
                  LET scr_l = SCR_LINE()

 
	        AFTER FIELD monto_en_pesos
                 IF l_record[pos].monto_en_pesos IS NULL THEN
                    ERROR "No puede ser nulo el Monto en Pesos" 
                    NEXT FIELD monto_en_pesos
                 END IF

        	BEFORE FIELD fecha_valor
                  LET arr_c = ARR_CURR()
                  LET scr_l = SCR_LINE()

	        AFTER FIELD fecha_valor
                 IF l_record[pos].fecha_valor IS NULL THEN
                    ERROR "No puede ser nula la Fecha Valor" 
                    NEXT FIELD fecha_valor
                 END IF

        	BEFORE FIELD entidad_rec
                  LET arr_c = ARR_CURR()
                  LET scr_l = SCR_LINE()

	        AFTER FIELD entidad_rec
                 IF l_record[pos].entidad_rec IS NULL THEN
                    ERROR "No puede ser nula la Entidad Recaudadora" 
                    NEXT FIELD entidad_rec
                 END IF

                 ON KEY ( ESC )
                 for i = 1 to ARR_COUNT()
                 UPDATE aporte_ventanilla
                 SET curp            = l_record[i].curp ,
                     monto_en_pesos  = l_record[i].monto_en_pesos,
                     fecha_pago      = l_record[i].fecha_pago ,
                     fecha_valor     = l_record[i].fecha_valor ,
                     entidad_rec     = l_record[i].entidad_rec, 
                     usuario         = g_usuario
                 WHERE   nss         = x_reg.nss
                 AND     rowid       = l_record[i].rowid

                 END FOR
                  ERROR "Registro modificado"
                    EXIT INPUT
                 ON KEY ( INTERRUPT )
                    EXIT INPUT
                    CLOSE WINDOW vent_2   
                    clear screen
                 END INPUT
           CLOSE WINDOW vent_2                                                 
           clear screen
END FUNCTION
