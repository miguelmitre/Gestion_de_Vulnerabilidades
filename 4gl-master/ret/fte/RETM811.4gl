DATABASE safre_af
GLOBALS

DEFINE hoy DATE

DEFINE enter             CHAR(001),
       f_nss             CHAR(011),
       b                 CHAR(001)

END GLOBALS

MAIN 
    DEFER INTERRUPT      
    OPTIONS
        ACCEPT KEY CONTROL-I ,
        INPUT WRAP           ,
        PROMPT LINE LAST     ,
        MESSAGE LINE LAST

LET hoy = TODAY
OPEN WINDOW retm8111 AT 2,2 WITH FORM "RETM8111" ATTRIBUTE(BORDER) 
DISPLAY "                           <Ctrl-c> Salir                                      " AT 1,1 ATTRIBUTE(REVERSE)
DISPLAY " RETM811            TRABAJADORES CON APORTACION EXTEMPORANEA                   " AT 3,1 ATTRIBUTE(REVERSE)
DISPLAY hoy USING"DD-MM-YY" AT 3,70 ATTRIBUTE(REVERSE)

LET b = 0
WHILE b = 0
INPUT BY NAME f_nss 
AFTER FIELD f_nss 
  IF f_nss IS NULL THEN 
     ERROR"Campo no puede ser nulo..."
     NEXT FIELD f_nss
  END IF
ON KEY(ESC)
   EXIT INPUT

ON KEY(INTERRUPT)
   LET b = 1
   EXIT INPUT
END INPUT
IF b THEN
   DISPLAY "Saliendo del Proceso..." AT 20,2 ATTRIBUTE(REVERSE)
   SLEEP 1
   EXIT PROGRAM 
ELSE
   CALL RETM814(f_nss)
END IF
END WHILE
END MAIN

#########################################################################
FUNCTION dibuja_pantalla(ini_vert,fin_vert,pos_hor)
#dp------------------------------------------------

DEFINE i SMALLINT
DEFINE ini_vert     ,
       fin_vert     ,
       pos_hor      SMALLINT

FOR i = ini_vert TO fin_vert
    DISPLAY "|" AT i,pos_hor
END FOR

END FUNCTION
