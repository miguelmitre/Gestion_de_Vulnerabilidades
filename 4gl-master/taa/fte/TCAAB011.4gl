#########################################################################t#
#Proyecto          => AFORE ( MEXICO )                                     #
#Propietario       => E.F.P.                                               #
#Programa TCAAB011  => CONSULTA NSS TRASPASO DE AFORE CEDENTE               #
#Fecha             => 10 DE OCTUBRE DE 2001                                #
#Autor             => MAURO MUNIZ CABALLERO                                #
#Modifico          => FRANCISCO  LUGO  CORNEJO                             #
#Fecha de Mod.     => 06 DE MARZO DEL 2003
#Sistema           => ACR                                                  #
############################################################################

DATABASE safre_af

GLOBALS

    DEFINE tot              SMALLINT
    DEFINE  lastkey         ,
            band            ,
            i               ,
            arr_c           ,
            total_pa        ,
            g_tipo_traspaso ,
            g_dis_pesos     ,
            pos             ,
            linea           SMALLINT
    

    DEFINE reg ARRAY[20000] OF RECORD
        indice             CHAR(01),
        consec             INTEGER,
        nss                CHAR(11),
        curp               CHAR(18),
        acc_rcv            DEC(16,6),
        part_viv           DEC(16,6)
    END RECORD

    DEFINE     consec             INTEGER
    DEFINE  n_seguro           CHAR(11)

    DEFINE  fecha_presentacion DATE    
    DEFINE  fecha_liquidacion  DATE    

    DEFINE
            g_afore       RECORD LIKE tab_afore_local.*,
            g_paramgrales RECORD LIKE seg_modulo.*
    DEFINE arr    INTEGER

    DEFINE
            HOY       DATE

    DEFINE
            enter     CHAR(1),
            HORA      CHAR(8),
            g_usuario CHAR(8) 

    DEFINE
            sel_where CHAR(1000),
            cla_where CHAR(1000)

    DEFINE   l_sal_pesos       ARRAY[14]  OF  DEC(16,6)
    DEFINE   l_sal_acciones    ARRAY[14]  OF  DEC(16,6)
    DEFINE  liquidada  SMALLINT
    DEFINE  g_folio    INTEGER
    DEFINE  gfecha_liq DATE

END GLOBALS

MAIN

    DEFINE salida SMALLINT
    OPTIONS
    PROMPT LINE LAST, 
    INPUT WRAP
    DEFER INTERRUPT

    CALL inicio()

    LET salida = 1

    WHILE salida  = TRUE
          CALL despliega_folios() RETURNING salida
    END WHILE


---CALL proceso_principal()

END MAIN

FUNCTION inicio()
#i---------------
    DEFINE  l_qry               CHAR(1000)

    LET HOY   = TODAY
    LET HORA  = TIME

    SELECT  *, USER
      INTO  g_paramgrales.*, g_usuario
      FROM  seg_modulo
     WHERE  modulo_cod = 'taa';

    SELECT  a.estado
      INTO  liquidada
      FROM  safre_af:taa_cd_edo_cedente  a
     WHERE  a.descripcion     = "LIQUIDADA"
       AND  a.tipo            =  3;

     
     LET    l_qry    =  " SELECT  TRIM(a.nombre)||' '||TRIM(a.paterno)||' '|| ",
                        "         TRIM(a.materno),",
                        " a.rfc,b.finicta,a.tipo_traspaso,c.descripcion ",
                        " FROM  taa_cd_det_cedido a, afi_mae_afiliado  b ,",
                        " taa_cd_tipo_traspaso c ",
                        " WHERE  a.folio     = ?",
                        " AND    a.n_seguro  = ?",
                        " AND    a.n_seguro  = b.n_seguro",
                        " AND    a.tipo_traspaso = c.tipo_traspaso;" 
     LET    l_qry       =  l_qry    CLIPPED

     PREPARE   qry_nombre      FROM  l_qry
                       
     LET    l_qry    =  " SELECT  subcuenta,SUM(monto_en_acciones),",
                        "         SUM(monto_en_pesos)",
                        " FROM    dis_cuenta",
                        " WHERE   folio    = ?",
                        " AND     nss      = ?",
                        " AND     subcuenta    > 0 ",
                        " AND     tipo_movimiento  <>  0",
                        " GROUP   BY  1;" 
     LET    l_qry       =  l_qry    CLIPPED

     PREPARE   qry_saldos      FROM  l_qry

END FUNCTION

FUNCTION proceso_principal()
#pp-------------------------

    OPEN WINDOW ventana_1 AT 2,2 WITH FORM "TCAAB0111" ATTRIBUTE( BORDER)

    DISPLAY " TCAAB011      CONSULTA TRASPASOS LIQUIDADOS  AFORE CEDENTE                      " AT 3,1 ATTRIBUTE(REVERSE)

    DISPLAY HOY USING "DD-MM-YYYY" AT 3,64 ATTRIBUTE(REVERSE)

    CALL     dibuja_pantalla(6,11,9)
    CALL     dibuja_pantalla(6,11,22)
    CALL     dibuja_pantalla(6,11,42)
    CALL     dibuja_pantalla(6,11,60)

    CALL     dibuja_pantalla(16,20,27)
    CALL     dibuja_pantalla(16,20,53)

    MENU " TRASPASOS "
        COMMAND  "Consulta de Traspasos" "     <Esc>  Continua     <Crtl-c> Salir  "
            CLEAR FORM
            CALL Consulta()
        COMMAND  "Salir " " Salir de Programa"
            EXIT MENU
    END MENU
    CLOSE WINDOW ventana_1
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


    DEFINE sali      SMALLINT
    DEFINE linea     SMALLINT

----DISPLAY "       NSS     FECHA PRESENTACION     FECHA LIQUIDA   TIPO SOLICITUD ESTADO" AT 5,1 ATTRIBUTE(REVERSE)

    LET int_flag = FALSE
    LET sali     = 0

    CONSTRUCT cla_where ON  a.n_seguro
#                           a.tipo_traspaso
                      FROM  n_seguro 
#                           tipo_traspaso


    ON KEY(ESC)
       LET int_flag = FALSE 
       EXIT CONSTRUCT

    
    ON KEY(INTERRUPT)
       LET int_flag = FALSE 
       ERROR "BUSQUEDA CANCELADA..."
       SLEEP 2
       CLEAR FORM
       LET sali = 1
       EXIT CONSTRUCT 

    END CONSTRUCT

    
    IF sali = 1 THEN
       RETURN    
    END IF
    ERROR  " PROCESANDO  INFORMACION ........."
    IF     g_tipo_traspaso       =  1    THEN
           LET      sel_where    = " SELECT  a.n_seguro,a.n_unico",
                    " FROM    taa_cd_det_cedido a ",
                    " WHERE   a.folio     =",g_folio , 
                    "   AND   a.estado    =",liquidada , 
                    "   AND ",cla_where CLIPPED,
                    " ORDER BY 2,1 " 
    ELSE
           LET      sel_where    = " SELECT  b.n_seguro,b.n_unico",
                    " FROM    taa_cd_det_comple a ,",
                    "         taa_cd_det_cedido b  ",
                    " WHERE   a.folio     =",g_folio , 
                    "   AND   a.n_seguro  = b.n_seguro ",
                    "   AND   a.estado    =",liquidada , 
                    "   AND ",cla_where CLIPPED,
                    " ORDER BY 2,1 " 
    END IF

    LET     sel_where      =  sel_where CLIPPED

    PREPARE qry_consul FROM sel_where 

    DECLARE cursor_c CURSOR FOR qry_consul

    LET pos = 1

    FOREACH cursor_c INTO reg[pos].nss,reg[pos].curp
        LET reg[pos].consec    =  pos
 
        SELECT SUM(monto_en_acciones)
           INTO reg[pos].acc_rcv
           FROM safre_af:dis_cuenta
        WHERE  folio           =  g_folio
          AND  nss             =  reg[pos].nss
          AND   subcuenta   NOT  IN (4,8,14) 
     

        SELECT SUM(monto_en_acciones)
           INTO reg[pos].part_viv
           FROM safre_af:dis_cuenta
        WHERE   nss              =  reg[pos].nss
          AND   folio            =  g_folio  
          AND   subcuenta   IN (4,8) 

        LET pos = pos + 1
    END FOREACH
    IF          pos     =  1    THEN
                PROMPT  "  NO HAY FOLIOS LIQUIDADOS  "  FOR  enter
    END IF

    LET tot = pos -1

    SELECT   a.fecha_presentacion,a.fecha_liquidacion
     INTO    fecha_presentacion,fecha_liquidacion
     FROM    taa_cd_ctr_folio a
    WHERE    a.folio              =  g_folio

    DISPLAY  g_folio                  TO  FORMONLY.folio
    DISPLAY  tot                      TO  FORMONLY.t_reg
    DISPLAY  fecha_presentacion       TO  FORMONLY.fecha_presentacion
    DISPLAY  fecha_liquidacion        TO  FORMONLY.fecha_liquidacion 
    DISPLAY  BY NAME  fecha_liquidacion
   
    CALL SET_COUNT(pos-1)
    IF    (pos - 1)                     >=  1    THEN
           INPUT   ARRAY  reg  WITHOUT DEFAULTS  FROM  scr_1.*
           BEFORE  ROW
                LET   arr_c              =  ARR_CURR()
                LET   total_pa           =  ARR_COUNT()
                LET   linea              =  SCR_LINE()

                IF    arr_c              >  total_pa THEN
                      LET    band        =  TRUE
                      EXIT INPUT
                END IF
           DISPLAY  reg[arr_c].*        TO  scr_1[linea].*        ATTRIBUTE(REVERSE) 
           CALL  arma_saldos()

          AFTER FIELD indice

                 IF reg[arr_c].indice IS NOT NULL THEN
                    LET reg[arr_c].indice = NULL
                    DISPLAY  BY NAME reg[arr_c].indice
                 END IF

                 IF arr_c >= (total_pa) THEN
                    LET  lastkey = FGL_LASTKEY()
                      IF  ((lastkey = FGL_KEYVAL("down"))
                       OR  (lastkey = FGL_KEYVAL("return"))
                       OR  (lastkey = FGL_KEYVAL("tab"))
                       OR  (lastkey = FGL_KEYVAL("right")))

                      THEN
                        ERROR "No hay mas opciones en esa direccion."
                        NEXT FIELD indice
                      END IF
                 END IF

           AFTER ROW
              LET     arr_c              =  ARR_CURR()
              LET     linea              =  SCR_LINE()
              DISPLAY  reg[arr_c].*     TO  scr_1[linea].*
           ON  KEY  (CONTROL-N)
               CALL    muestra_saldos()
         
      END INPUT

    ELSE
        ERROR "REGISTROS CON ESAS CONDICIONES NO EXISTEN"
        SLEEP 2
        ERROR ""
    END IF

END FUNCTION
################################################################################
FUNCTION despliega_folios()

    DEFINE f_b  SMALLINT
    DEFINE arr_taa_cd_ctr_folio ARRAY[10000] OF RECORD
           folio               LIKE safre_af:taa_cd_ctr_folio.folio, 
           tipo_traspaso       LIKE safre_af:taa_cd_ctr_folio.tipo_traspaso, 
           fecha_liquidacion   LIKE safre_af:taa_cd_ctr_folio.fecha_liquidacion 
                                        END RECORD


    DEFINE reg_taa_cd_ctr_folio RECORD
           folio               LIKE safre_af:taa_cd_ctr_folio.folio, 
           tipo_traspaso       LIKE safre_af:taa_cd_ctr_folio.tipo_traspaso, 
           fecha_liquidacion   LIKE safre_af:taa_cd_ctr_folio.fecha_liquidacion  
                                        END RECORD


    LET f_b = 1

    OPEN WINDOW taab0112 AT 3,2 WITH FORM "TCAAB0112" ATTRIBUTE(BORDER)
    DISPLAY "<Enter>Seleccionar           <Ctrl -C>Salir                             " AT 1,2 ATTRIBUTE(REVERSE)
    DISPLAY HOY USING "DD-MM-YYYY" AT 1,64 ATTRIBUTE(REVERSE)
    DISPLAY "TCAAB0112     CONSULTA TRASPASOS LIQUIDADOS  AFORE CEDENTE                    " AT 4,2 ATTRIBUTE(REVERSE)

 
    DECLARE cur_dsply_folio CURSOR FOR

    SELECT A.folio,A.tipo_traspaso,A.fecha_liquidacion
    FROM   safre_af:taa_cd_ctr_folio A
    WHERE  A.estado  = liquidada
    ORDER BY A.folio  DESC

    LET i = 1

    FOREACH cur_dsply_folio
    INTO reg_taa_cd_ctr_folio.*

       LET arr_taa_cd_ctr_folio[i].* =
           reg_taa_cd_ctr_folio.*

       LET i = i + 1
 
    END FOREACH

    CALL SET_COUNT(i-1)

    DISPLAY ARRAY arr_taa_cd_ctr_folio
               TO sr_ctr_folio.*

    ON KEY(ESC)
       LET arr_c   = ARR_CURR()
    ON KEY(RETURN)
       LET arr_c = ARR_CURR()
       LET g_folio            = arr_taa_cd_ctr_folio[arr_c].folio
       LET g_tipo_traspaso    = arr_taa_cd_ctr_folio[arr_c].tipo_traspaso
       LET gfecha_liq         = arr_taa_cd_ctr_folio[arr_c].fecha_liquidacion
       CALL proceso_principal()
    ON KEY(INTERRUPT)
       LET f_b = 0
     EXIT DISPLAY
    END DISPLAY

    RETURN f_b   
 
END FUNCTION

FUNCTION  arma_saldos()
    DEFINE   l_scta                   SMALLINT
    DEFINE   l_nombre                 CHAR(50)
    DEFINE   l_rfc                    CHAR(13)
    DEFINE   l_finicta                DATE
    DEFINE   l_tipo_traspaso          CHAR(02)
    DEFINE   l_descripcion            CHAR(40)
    DEFINE   l_pesos                  ,
             l_acciones               DEC(16,6)
    LET      g_dis_pesos              =  1
    EXECUTE  qry_nombre     USING  g_folio,reg[arr_c].nss
         INTO   l_nombre,l_rfc,l_finicta,l_tipo_traspaso,l_descripcion 
    FOR      l_scta     =   1     TO  14
             LET     l_sal_pesos[l_scta]         =  0
             LET     l_sal_acciones[l_scta]      =  0
    END FOR
    DECLARE  cur_saldos     CURSOR  FOR  qry_saldos
    OPEN     cur_saldos     USING   g_folio,reg[arr_c].nss
    FOREACH  cur_saldos     INTO    l_scta,l_acciones,l_pesos
        LET    l_sal_acciones[l_scta]       =  l_acciones
        LET    l_sal_pesos[l_scta]          =  l_pesos
    END FOREACH
    DISPLAY   l_nombre               TO  FORMONLY.nombre
    DISPLAY   l_rfc                  TO  FORMONLY.rfc
    DISPLAY   l_finicta              TO  FORMONLY.fec_afil
    DISPLAY   l_tipo_traspaso        TO  FORMONLY.tipo_traspaso
    DISPLAY   l_descripcion          TO  FORMONLY.descripcion
    CALL     muestra_saldos()
END FUNCTION
FUNCTION  muestra_saldos()
    IF       g_dis_pesos         THEN
             DISPLAY  "                            MONTOS  EN  PESOS                                                    " AT 15,1 ATTRIBUTE(REVERSE)
             DISPLAY  "      <Control-N>   PARA VER MONTOS EN ACC_RCV/PART_VIV                                    " AT 21,1 ATTRIBUTE(REVERSE)

             DISPLAY   l_sal_pesos[1]         TO  FORMONLY.saldo1
             DISPLAY   l_sal_pesos[2]         TO  FORMONLY.saldo2
             DISPLAY   l_sal_pesos[3]         TO  FORMONLY.saldo3
             DISPLAY   l_sal_pesos[4]         TO  FORMONLY.saldo4
             DISPLAY   l_sal_pesos[5]         TO  FORMONLY.saldo5
             DISPLAY   l_sal_pesos[6]         TO  FORMONLY.saldo6
             DISPLAY   l_sal_pesos[7]         TO  FORMONLY.saldo7
             DISPLAY   l_sal_pesos[8]         TO  FORMONLY.saldo8
             DISPLAY   l_sal_pesos[9]         TO  FORMONLY.saldo9
             DISPLAY   l_sal_pesos[10]        TO  FORMONLY.saldo10
             DISPLAY   l_sal_pesos[11]        TO  FORMONLY.saldo11
             DISPLAY   l_sal_pesos[12]        TO  FORMONLY.saldo12
             DISPLAY   l_sal_pesos[13]        TO  FORMONLY.saldo13
             DISPLAY   l_sal_pesos[14]        TO  FORMONLY.saldo14
             LET       g_dis_pesos             =  0
    ELSE
    
             DISPLAY  "          MONTOS  EN  ACCIONES  RCV  Y  PARTICIPACIONES VIVIENDA               " AT 15,1 ATTRIBUTE(REVERSE)

             DISPLAY  "              <Control-N> PARA VER MONTOS EN PESOS                                               " AT 21,1 ATTRIBUTE(REVERSE)
             DISPLAY   l_sal_acciones[1]      TO  FORMONLY.saldo1
             DISPLAY   l_sal_acciones[2]      TO  FORMONLY.saldo2
             DISPLAY   l_sal_acciones[3]      TO  FORMONLY.saldo3
             DISPLAY   l_sal_acciones[4]      TO  FORMONLY.saldo4
             DISPLAY   l_sal_acciones[5]      TO  FORMONLY.saldo5
             DISPLAY   l_sal_acciones[6]      TO  FORMONLY.saldo6
             DISPLAY   l_sal_acciones[7]      TO  FORMONLY.saldo7
             DISPLAY   l_sal_acciones[8]      TO  FORMONLY.saldo8
             DISPLAY   l_sal_acciones[9]      TO  FORMONLY.saldo9
             DISPLAY   l_sal_acciones[10]     TO  FORMONLY.saldo10
             DISPLAY   l_sal_acciones[11]     TO  FORMONLY.saldo11
             DISPLAY   l_sal_acciones[12]     TO  FORMONLY.saldo12
             DISPLAY   l_sal_acciones[13]     TO  FORMONLY.saldo13
             DISPLAY   l_sal_acciones[14]     TO  FORMONLY.saldo14
             LET       g_dis_pesos             =  1
    END IF
END FUNCTION 

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
