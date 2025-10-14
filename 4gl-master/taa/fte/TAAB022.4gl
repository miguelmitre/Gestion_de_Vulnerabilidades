############################################################################
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa TAAB022  => CONSULTA NSS TRASPASO DE AFORE RECEPTORA ( Previo  ) #
#Fecha             => 30 DE DICIEMBRE DE 2008                              #
#Autor             => FERNANDO HERRERA HERNANDEZ                           #
#Sistema           => TAA ( Operacion 29 )                                 #
# Adecuacion circular 28-20 Liquidacion Bimestral                          #
############################################################################

DATABASE safre_af

GLOBALS

  DEFINE reg ARRAY[30000] OF RECORD
    nss                      CHAR(11),
    curp                     CHAR(18),
    folio                    INTEGER ,
    fecha_valor              DATE,
    tipo_traspaso            CHAR(02),
    saldo                    DECIMAL(15,2),
    num_acciones             DECIMAL(22,6)
  END RECORD

  DEFINE 
    nss                      CHAR(11),
    fecha_valor              DATE    ,
    folio                    INTEGER 

  DEFINE
    g_afore                  RECORD LIKE tab_afore_local.*,
    g_paramgrales            RECORD LIKE seg_modulo.*

  DEFINE
    HOY                      DATE

  DEFINE
    enter                    CHAR(1),
    HORA                     CHAR(8),
    g_usuario                CHAR(8) 

  DEFINE
    i                        SMALLINT,
    pos                      INTEGER,
    sel_where                CHAR(1000),
    cla_where                CHAR(1000),
    total_saldo              DECIMAL(15,2),
    total_acciones           DECIMAL(22,6)

END GLOBALS

MAIN

  DEFER INTERRUPT
    OPTIONS PROMPT LINE LAST,
    INPUT WRAP

    CALL inicio()
    CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------

  LET HOY   = TODAY
  LET HORA  = TIME

  SELECT *, USER
  INTO   g_paramgrales.*, g_usuario
  FROM   seg_modulo
  WHERE  modulo_cod = 'taa'

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

  OPEN WINDOW ventana_1 AT 3,2 WITH FORM "TAAB0221" ATTRIBUTE( BORDER)

  DISPLAY " TAAB022         CONSULTA TRASPASOS DE AFORE RECEPTORA PREVIO                  " AT 3,1 ATTRIBUTE(REVERSE)

  DISPLAY HOY USING "DD-MM-YYYY" AT 3,67 ATTRIBUTE(REVERSE)

  MENU " TRASPASOS PREVIOS RECIBIDOS "
    COMMAND "Consulta" " Consulta Traspasos"
      CALL Consulta()
    COMMAND "Salir " " Salir de Programa"
      EXIT MENU
  END MENU

END FUNCTION

FUNCTION Inicializa()
#iz------------------

  DEFINE j SMALLINT

  INITIALIZE reg TO NULL

  FOR j = 1 TO 12
      DISPLAY reg[i].* TO scr_1[i].* ATTRIBUTE (NORMAL)
  END FOR

  CLEAR FORM

END FUNCTION

FUNCTION Consulta()
#C-----------------

  DISPLAY " NSS         CURP               FOLIO    FVALOR TAA       SALDO      ACCIONES  " AT 5,1 ATTRIBUTE(REVERSE)

  LET int_flag = FALSE

  CONSTRUCT cla_where ON nss,
                         curp,
                         folio,
                         fecha_valor,
                         tipo_traspaso
                    FROM nss,     
                         curp,
                         folio,
                         fecha_valor,
                         tipo_traspaso

  IF int_flag = TRUE THEN
     LET int_flag = FALSE
     ERROR "BUSQUEDA CANCELADA..."
     SLEEP 2
     ERROR ""
     CLEAR SCREEN
     RETURN
  END IF

  LET sel_where = " SELECT a.nss,",
                         " a.curp,",
                         " a.folio,",
                         " a.fecha_valor,",
                         " a.tipo_traspaso,",
                         " sum(saldo),",
                         " sum(num_acciones)",
                  " FROM   det_tra_sdo_previo a ",
                  " WHERE ", cla_where CLIPPED,
                  " GROUP BY 1,2,3,4,5",
                  " ORDER BY folio, nss, curp"

  LET sel_where = sel_where CLIPPED

  #LET sel_where = "echo ",sel_where clipped, " > x.sql"  
  #RUN sel_where

  PREPARE qry_consul FROM sel_where 
  DECLARE cursor_c CURSOR FOR qry_consul

  LET pos            = 1
  LET total_saldo    = 0
  LET total_acciones = 0

  FOREACH cursor_c INTO reg[pos].nss THRU reg[pos].num_acciones
    {CASE reg[pos].tipo_traspaso
      WHEN  1 LET reg[pos].desc_estado = 'ORDINARIO'
      WHEN 12 LET reg[pos].desc_estado = 'UNIFACION'
      WHEN 20 LET reg[pos].desc_estado = 'UNIXASIG'
      WHEN 21 LET reg[pos].desc_estado = 'INDEBIDO'
      WHEN 51 LET reg[pos].desc_estado = 'ASIGNACION'
      WHEN 38 LET reg[pos].desc_estado = 'INTERNET'
      WHEN 24 LET reg[pos].desc_estado = 'SEPARACION'
      WHEN 25 LET reg[pos].desc_estado = 'SEPXASIG'
      WHEN 55 LET reg[pos].desc_estado = 'VERIF ELECT'
      WHEN 57 LET reg[pos].desc_estado = 'ASIG INTER'
    END CASE}

    LET total_saldo    = total_saldo    + reg[pos].saldo
    LET total_acciones = total_acciones + reg[pos].num_acciones

    LET pos = pos + 1

  END FOREACH
  
  DISPLAY total_saldo    TO total_saldo
  DISPLAY total_acciones TO total_acciones

  IF (pos-1) >= 1 THEN
     CALL SET_COUNT(pos-1)
     DISPLAY ARRAY reg TO scr_1.*

     IF int_flag = TRUE THEN
        LET int_flag = FALSE
        ERROR "BUSQUEDA TERMINADA..."
        SLEEP 2
        ERROR ""
        CLEAR FORM
        RETURN
     END IF
  ELSE
     ERROR "REGISTROS CON ESAS CONDICIONES NO EXISTEN"
     SLEEP 2
     ERROR ""
  END IF

END FUNCTION

