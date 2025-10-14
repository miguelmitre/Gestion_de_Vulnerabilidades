##############################################################################
#Owner             => E.F.P.
#Programa TRAB0011 => REVERSO DE ENVIO DE SOLICITUDES ICE-AFO-IMSS
#Fecha creacion    => 11 FEBRERO 2003   
#By                => JESUS DAVID YANEZ MORENO
#Modificado  By    => MARCO ANTONIO GONZALEZ ROJAS
#Fecha de Mod      => 09 DE FEBRERO DEL 2005
#Sistema           => TRA-ICE-IMSS
##############################################################################
DATABASE safre_af
GLOBALS
DEFINE hoy DATE
DEFINE ff CHAR(008)
DEFINE enter CHAR(001)
DEFINE fecha_genera DATE
DEFINE total INTEGER
DEFINE reg_sol_icefa RECORD 
       n_seguro char(011),
       correlativo integer 
END RECORD
DEFINE sw    SMALLINT
END GLOBALS


MAIN 
    OPTIONS INPUT WRAP,
    PROMPT LINE LAST  ,
    ACCEPT KEY CONTROL-I
    DEFER INTERRUPT
CALL ini()
    OPEN WINDOW trab00111 AT 4,4 WITH FORM "TRAB00111" ATTRIBUTE(BORDER)
    DISPLAY" TRAB0011    REVERSO DE ENVIO DE SOLIC. TRA-ICE-AFO-IMSS                       " AT 3,1 ATTRIBUTE(REVERSE)   

    DISPLAY "                          < CTRL-C> Salir                                  " AT 1,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING"DD-MM-YYYY" AT 3,60 ATTRIBUTE(REVERSE)
 LET sw = 1
    INPUT BY NAME fecha_genera WITHOUT DEFAULTS 
     BEFORE FIELD fecha_genera
       IF sw = 1 THEN
            LET fecha_genera = " "
            DISPLAY fecha_genera
            LET sw = 0
       END IF
     AFTER FIELD fecha_genera
      IF fecha_genera IS NULL THEN
        ERROR"Fecha no puede ser nula.."
        SLEEP 2
        NEXT FIELD fecha_genera
      END IF

      LET total = 0

      SELECT COUNT(*) 
      INTO total 
      FROM cta_act_marca  a
      WHERE marca_cod = 250
      AND   fecha_ini = fecha_genera

      IF total = 0 OR total IS NULL THEN
           ERROR"NO HAY SOLICITUDES PARA REVERSAR CON ESTA FECHA..."
           SLEEP 2
           ERROR" "
           NEXT FIELD fecha_genera
      ELSE
           DISPLAY "SE ENCONTRARON ",total," SOLICITUDES ENVIADAS"  AT 14,2
           DISPLAY "PARA REVERSAR <ESC> PARA CONTINUAR" AT 15,2
      END IF

     ON KEY(INTERRUPT)
        ERROR"Proceso Cancelado..."
        SLEEP 2
        EXIT PROGRAM 
     ON KEY(ESC)
      IF fecha_genera IS NULL THEN
        ERROR"Fecha no puede ser nula.."
        SLEEP 2
        NEXT FIELD fecha_genera
      END IF
      DISPLAY"Este proceso desmarcará todas las solicitudes enviadas"
      AT 17,2
      DISPLAY"en la fecha: ",fecha_genera USING"DD/MM/YYYY"
      AT 18,2
      WHILE TRUE
        PROMPT "ESTA SEGURO S/N ? " FOR CHAR enter
        IF enter MATCHES "[sSnN]" THEN
            IF enter MATCHES "[sS]" THEN
                EXIT INPUT
            ELSE
                DISPLAY"PROCESO CANCELADO" AT 19,1 ATTRIBUTE(REVERSE) SLEEP 2
                EXIT PROGRAM
            END IF
        END IF
      END WHILE
END INPUT

           DISPLAY "                                             "  AT 14,2
           DISPLAY "                                  " AT 15,2
DISPLAY "                                                             "
AT 17,2
DISPLAY "                                                             "
AT 18,2
  
DISPLAY "PROCESANDO REVERSO...             " AT 19,2

INSERT INTO reverso_sol_icefa
SELECT A.n_seguro,A.correlativo 
FROM   tra_mae_icefa A
WHERE  A.fecha_genera = fecha_genera
AND    A.status = 2

DECLARE cur_1 CURSOR FOR 
SELECT A.* 
FROM  reverso_sol_icefa A

FOREACH cur_1 INTO reg_sol_icefa.*   

  CALL f_desmarca_cuenta(reg_sol_icefa.n_seguro,
                         250,reg_sol_icefa.correlativo)
  
  LET ff = fecha_genera USING"YYYYMMDD"

  DELETE FROM  safre_af:est_0961
  WHERE  safre_af:est_0961.fecha_genera = ff

  UPDATE tra_mae_icefa
  SET fecha_genera = " ",
      n_envios     = 0,
      status       = 1
  WHERE correlativo = reg_sol_icefa.correlativo

END FOREACH
           DISPLAY "SE REVERSARON  ",total," SOLICITUDES ENVIADAS"  AT 14,2
PROMPT"PROCESO FINALIZADO...<ENTER> PARA SALIR..." FOR CHAR ENTER
END MAIN

FUNCTION ini()
#i-------------  
LET hoy = TODAY
CREATE TEMP TABLE reverso_sol_icefa(n_seguro char(0011),
                                    correlativo integer)
END FUNCTION

FUNCTION f_desmarca_cuenta(vnss,vmarca_entra,vvvv_corr)
#fd----------------------------------------------------

DEFINE ejecuta CHAR(300),
       vvvv_corr INTEGER
DEFINE vnss char(11),
vmarca_entra smallint ,
vmarca_estado smallint,
vcodigo_rechazo smallint,
vusuario   char(008)
DEFINE xcodigo_marca smallint,
xcodigo_rechazo smallint

select user
INTO vusuario
FROM tab_afore_local
group by 1

LET ejecuta = "EXECUTE PROCEDURE reversa_marca(","'",vnss,"'",
						    ",",vmarca_entra,",",
						    vvvv_corr,")"
LET ejecuta = ejecuta CLIPPED
PREPARE clausula_spl2 FROM ejecuta
EXECUTE clausula_spl2

END FUNCTION
